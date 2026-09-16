#' Presentation-Ready Cox Coefficient Table
#'
#' Formats the \code{table} element of a [cbe_cox_single()] or [cbe_cox_multi()] result
#' into a presentation table, adding a log(HR) column (if not already present) and
#' optional sorting and significance annotation.
#'
#' @param x A \code{cbe_cox} (from [cbe_cox_single()]) or \code{cbe_cox_multi}
#'   (from [cbe_cox_multi()]) object.
#' @param sort One of \code{"none"} (default; original model order), \code{"magnitude"}
#'   (sorts by \code{abs(log(HR))} descending; reference rows, which are always exactly
#'   0, sort last), or \code{"pvalue"} (sorts by p-value ascending; reference rows, which
#'   have no p-value, sort last).
#' @param significance Logical; if \code{TRUE}, appends a \code{sig} column of
#'   significance stars (three asterisks for p < 0.001, two for p < 0.01, one for
#'   p < 0.05) and bolds the \code{p.value} text of significant rows.
#' @return A tibble formatted for presentation, with columns \code{Variable}, \code{Level},
#'   \code{Role}, \code{HR}, \code{log(HR)}, \code{95\% CI}, \code{p.value}, and (if
#'   \code{significance = TRUE}) \code{sig}.
#' @seealso [cbe_cox_single()], [cbe_cox_multi()]
#' @export
cbe_cox_table <- function(x, sort = c("none", "magnitude", "pvalue"), significance = FALSE) {
  sort <- match.arg(sort)

  if (!inherits(x, c("cbe_cox", "cbe_cox_multi"))) {
    stop("`x` must be a `cbe_cox` or `cbe_cox_multi` object.", call. = FALSE)
  }

  tab <- x$table

  if (!"log(HR)" %in% names(tab)) {
    tab[["log(HR)"]] <- round(log(tab$HR), 3)
    tab <- tab[, c("Variable", "Level", "Role", "HR", "log(HR)", "95% CI", "p.value")]
  }

  p_numeric <- suppressWarnings(as.numeric(gsub("^<", "", tab$p.value)))
  p_numeric[tab$p.value == "\u2014"] <- NA_real_
  is_ref <- tab$Role == "Reference"

  if (sort == "magnitude") {
    ord <- order(is_ref, -abs(tab[["log(HR)"]]))
    tab <- tab[ord, , drop = FALSE]
  } else if (sort == "pvalue") {
    ord <- order(is_ref, ifelse(is.na(p_numeric), Inf, p_numeric))
    tab <- tab[ord, , drop = FALSE]
  }

  if (significance) {
    p_num2 <- suppressWarnings(as.numeric(gsub("^<", "", tab$p.value)))
    p_num2[tab$p.value == "\u2014"] <- NA_real_
    stars <- dplyr::case_when(
      is.na(p_num2)   ~ "",
      p_num2 < 0.001  ~ "***",
      p_num2 < 0.01   ~ "**",
      p_num2 < 0.05   ~ "*",
      TRUE            ~ ""
    )
    tab$p.value <- ifelse(nzchar(stars), paste0("**", tab$p.value, "**"), tab$p.value)
    tab$sig <- stars
  }

  tibble::as_tibble(tab)
}
