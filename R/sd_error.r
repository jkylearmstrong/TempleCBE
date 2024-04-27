#' Compute standard error
#'
#'
#' @param x vector of data
#' @param na.rm should NA values be removed, TRUE or FALSE, default is FALSE
#'
#' @return numeric value representing standard error
#'
#' @examples
#' sd.error(mtcars$mpg)
#'
#' @importFrom dplyr if_else
#'
#' @export sd.error


sd.error<- function(x, na.rm=FALSE){
  dplyr::if_else(na.rm,
                 sd(x, na.rm=TRUE)/sqrt(sum(!is.na(x))),
                 sd(x, na.rm=FALSE)/sqrt(length(x))
  )
}
