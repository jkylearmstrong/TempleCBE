#' Features Percent Missing
#'
#' Count the number of missing elements of each column in a data frame.
#' Allows for threshold setting to easily find columns with higher percent missing data.
#' Allows users to supply list of things that might also be be considered missing.
#'
#' @param data a data frame or tibble or something that could become one
#' @param percent_miss numeric between 0 an 1 will filter out any columns that are missing more than this value
#' @inheritParams SumNa
#'
#' @return a tibble (data frame) with columns: feature, SumNa (sum of NA entries), SumComp, PctNa, PctComp
#'
#' @import dplyr tibble
#'
#' @export features_percent_miss

features_percent_miss <- function(data, percent_miss = 0, na_list = NULL){

  if(is.data.frame(data) | tibble::is_tibble(data)){
    data <- data
  } else if( !(is.data.frame(data) | tibble::is_tibble(data)) ){
    data <- try(tibble::as_tibble(data, .name_repair = "universal"))
  } else( stop("Error: data must be coerceable into a tibble") )

  na_sums <- data %>%
    summarise(across(everything(), \(x){SumNa(x, na_list)})) %>%
    tidyr::pivot_longer(everything() ,names_to = 'feature', values_to = 'SumNa') %>%
    arrange(desc(SumNa)) %>%
    mutate(SumComp = nrow(data) - SumNa) %>%
    mutate(PctNa = SumNa/nrow(data)) %>%
    mutate(PctComp = (1 - PctNa))

  data_out <- na_sums %>%
    filter(PctNa >= percent_miss) %>%
    arrange(desc(PctNa))

  class(data_out) <- c("features_percent_miss", class(data_out))

  return(data_out)
}
