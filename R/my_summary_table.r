#' Summary Table Function
#'
#'
#' @param data a dataframe or tibble
#' @param var variable to summarize
#' @param na.rm should NA values be removed, TRUE or FALSE, default is FALSE
#' @param data.output should the data be output, TRUE or FALSE, default is TRUE
#' @param table.output should the table be output, TRUE or FALSE, default is FALSE
#'
#' @return either a dataframe or readable table 
#'
#' @examples
#' mtcars |> 
#'   group_by(cyl) |> 
#'   my_summary_table(mpg)
#'   
#' mtcars |> 
#'   group_by(cyl) |> 
#'   my_summary_table(mpg, table.output = TRUE)
#'   
#'  
#'
#' @import dplyr
#'
#' @export my_summary_table


my_summary_table <- function(data, var, na.rm=FALSE, data.output = TRUE, table.output = FALSE){
  if(table.output == TRUE){
    data.output <- FALSE
  }
  
  a_var_name <- data %>%
    ungroup() %>%
    select({{var}}) %>%
    colnames() 
  
  T1 <- if(na.rm){
    data %>%
      summarise(
        N = sum(!is.na({{var}})),
        Mean = mean({{var}}, na.rm = TRUE),
        SD = sd({{var}}, na.rm = TRUE),
        min = min({{var}}, na.rm = TRUE),
        Max = max({{var}}, na.rm = TRUE),
        SD_Error = sd.error({{var}}, na.rm = TRUE)) %>%
      mutate(variable = a_var_name)} else {
        data %>%
          summarise(
            N = length({{var}}),
            Mean = mean({{var}}, na.rm = FALSE),
            SD = sd({{var}}, na.rm = FALSE),
            min = min({{var}}, na.rm = FALSE),
            Max = max({{var}}, na.rm = FALSE),
            SD_Error = sd.error({{var}}, na.rm = FALSE)) %>%
          mutate(variable = a_var_name)
      }
  
  result <- if(data.output){T1} else {
    T1 %>%
      mutate(`Mean (SD)` = paste0(round(Mean, 2), " (", round(SD, 2), ")")) %>%
      mutate(Range = paste0(round(min, 2), " to ", round(Max, 2))) %>%
      select(-Mean, -SD, -min, -Max) 
  }
  
  return(result)
}
