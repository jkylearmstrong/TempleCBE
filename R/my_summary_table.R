#' Summary Table Function
#'
#' @param data a dataframe or tibble
#' @param var variable to summarize
#' @param na.rm should NA values be removed, TRUE or FALSE, default is FALSE
#' @param data.output should the data be output, TRUE or FALSE, default is TRUE
#' @param table.output should the table be output, TRUE or FALSE, default is FALSE
#'
#' @details
#' The two switches choose between two results, they are not independent. The
#' numeric data frame (`N`, `Mean`, `SD`, `min`, `Max`, `SD_Error`) is returned
#' when `data.output = TRUE` and `table.output = FALSE` (the default). In every
#' other case the formatted table (`Mean (SD)`, `Range`) is returned:
#' `table.output = TRUE` asks for it and takes precedence over `data.output`,
#' and `data.output = FALSE` on its own also gives the table, since there is
#' nothing else to return.
#'
#' @return either a dataframe or readable table 
#'
#' @examples
#' mtcars |> 
#'   dplyr::group_by(cyl) |> 
#'   summary_table(mpg)
#'   
#' mtcars |> 
#'   dplyr::group_by(cyl) |> 
#'   summary_table(mpg, table.output = TRUE)
#'
#' @importFrom dplyr %>% ungroup select summarise mutate
#' @importFrom stats sd
#' @importFrom rlang .data
#'
#' @export
summary_table <- function(data, var, na.rm = FALSE, data.output = TRUE, table.output = FALSE) {
  for (arg in c("na.rm", "data.output", "table.output")) {
    val <- get(arg, envir = environment())
    if (!is.logical(val) || length(val) != 1L || is.na(val)) {
      stop("`", arg, "` must be a single logical (TRUE or FALSE).", call. = FALSE)
    }
  }
  if (table.output == TRUE) {
    data.output <- FALSE
  }

  a_var_name <- data %>%
    ungroup() %>%
    select({{var}}) %>%
    colnames()

  if (length(a_var_name) == 1L && !is.numeric(data[[a_var_name]])) {
    stop("`var` must be a numeric column.", call. = FALSE)
  }

  T1 <- if (na.rm) {
    data %>%
      summarise(
        N = sum(!is.na({{var}})),
        Mean = mean({{var}}, na.rm = TRUE),
        SD = stats::sd({{var}}, na.rm = TRUE),
        min = summary_extreme({{var}}, min),
        Max = summary_extreme({{var}}, max),
        SD_Error = std_error({{var}}, na.rm = TRUE)) %>%
      mutate(variable = a_var_name)
  } else {
    data %>%
      summarise(
        N = length({{var}}),
        Mean = mean({{var}}, na.rm = FALSE),
        SD = stats::sd({{var}}, na.rm = FALSE),
        min = min({{var}}, na.rm = FALSE),
        Max = max({{var}}, na.rm = FALSE),
        SD_Error = std_error({{var}}, na.rm = FALSE)) %>%
      mutate(variable = a_var_name)
  }

  result <- if (data.output) {
    T1
  } else {
    T1 %>%
      mutate(`Mean (SD)` = paste0(round(.data$Mean, 2), " (", round(.data$SD, 2), ")")) %>%
      mutate(Range = paste0(round(.data$min, 2), " to ", round(.data$Max, 2))) %>%
      select(-"Mean", -"SD", -"min", -"Max")
  }

  return(result)
}

# min()/max() over the non-missing values; NA (not Inf with a warning) when there are none
#' @noRd
summary_extreme <- function(x, f) {
  x <- x[!is.na(x)]
  if (length(x) == 0L) NA_real_ else f(x)
}
