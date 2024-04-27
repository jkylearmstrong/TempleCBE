#' Plot method for features_percent_miss objects
#'
#' This function creates a bar plot showing the percentage of complete values
#' for each feature in a data frame or tibble.
#'
#' @param x An object of class 'features_percent_miss' returned by the
#'   \code{\link{features_percent_miss}} function.
#'
#' @return A ggplot object representing the bar plot.
#'
#' @examples
#' features_percent_miss(mtcars,  na_list = c(""," ","NA")) |>
#'    plot()
#'
#' @import dplyr ggplot2
#' @importFrom stats reorder
#'
#' @export
#' @rdname features_percent_miss

plot.features_percent_miss <- function(x){
  plot_obj <- x %>%
    mutate(feature = reorder(feature, PctComp)) %>%
    ggplot(aes(x=feature, y = PctComp, fill = feature)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    theme(legend.position = "none")

  return(plot_obj)
}
