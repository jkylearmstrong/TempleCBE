#' Tidy Proportional Hazards Diagnostics for Cox Models
#'
#' Standalone proportional hazards (PH) assumption diagnostics, usable on any
#' fitted \code{survival::coxph} model, or on a \code{cbe_cox} or
#' \code{cbe_cox_multi} object. Wraps \code{survival::cox.zph()} with a tidy
#' per-term table, a per-term violation flag, and an automated text summary.
#'
#' @param fit A fitted \code{survival::coxph} model, or a \code{cbe_cox} or
#'   \code{cbe_cox_multi} object.
#' @return An object of class \code{cbe_cox_check} containing:
#'   \itemize{
#'     \item \code{zph}: The \code{survival::cox.zph} object (or \code{NULL} if it could not be computed).
#'     \item \code{zph_table}: Data frame version of \code{zph$table} (one row per term, plus a
#'       \code{GLOBAL} row for multivariable fits).
#'     \item \code{zph_violated}: Named logical vector, one entry per term (excluding \code{GLOBAL}),
#'       \code{TRUE} where the assumption is violated (p < 0.05). For a univariable fit this is a
#'       length-1 named logical vector.
#'     \item \code{zph_text}: Automated summary sentence (a single string for a univariable fit
#'       with one term; a two-element character vector, per-term then global, for a multivariable fit).
#'   }
#' @seealso [cbe_cox_single()], [cbe_cox_multi()]
#' @export
cbe_cox_check <- function(fit) {
  if (inherits(fit, "cbe_cox") || inherits(fit, "cbe_cox_multi")) {
    fit <- fit$model
  }

  if (!inherits(fit, "coxph")) {
    stop("`fit` must be a `coxph`, `cbe_cox`, or `cbe_cox_multi` object.", call. = FALSE)
  }

  zph_res <- tryCatch(
    survival::cox.zph(fit),
    error = function(e) NULL
  )

  if (is.null(zph_res)) {
    return(structure(
      list(
        zph          = NULL,
        zph_table    = data.frame(),
        zph_violated = logical(0),
        zph_text     = "Proportional hazards assumption could not be calculated."
      ),
      class = "cbe_cox_check"
    ))
  }

  zph_tab <- as.data.frame(zph_res$table)
  term_names <- rownames(zph_tab)
  is_global <- term_names == "GLOBAL"
  zph_violated <- stats::setNames(zph_tab$p[!is_global] < 0.05, term_names[!is_global])

  if (sum(!is_global) == 1) {
    p_zph <- zph_tab$p[!is_global][1]
    violated <- p_zph < 0.05
    zph_text <- sprintf(
      "Test of the proportional hazards assumption yields p = %.3f. Therefore, the proportional hazards assumption is %sviolated.",
      p_zph, if (violated) "" else "not "
    )
  } else {
    term_text <- sprintf(
      "%s (p = %.3f, %sviolated)",
      term_names[!is_global], zph_tab$p[!is_global], ifelse(zph_tab$p[!is_global] < 0.05, "", "not ")
    )
    per_term_text <- sprintf("Per-term proportional hazards tests: %s.", paste(term_text, collapse = "; "))
    global_text <- if (any(is_global)) {
      p_global <- zph_tab$p[is_global][1]
      sprintf(
        "Global test of the proportional hazards assumption yields p = %.3f (%sviolated).",
        p_global, if (p_global < 0.05) "" else "not "
      )
    } else {
      NULL
    }
    zph_text <- c(per_term_text, global_text)
  }

  structure(
    list(
      zph          = zph_res,
      zph_table    = zph_tab,
      zph_violated = zph_violated,
      zph_text     = zph_text
    ),
    class = "cbe_cox_check"
  )
}

#' Print Method for cbe_cox_check Object
#'
#' @param x Object of class \code{cbe_cox_check}
#' @param ... Additional arguments (unused)
#' @export
print.cbe_cox_check <- function(x, ...) {
  cat("Proportional Hazards Assumption Check\n")
  cat("--------------------------------------\n")
  cat(paste(x$zph_text, collapse = "\n"), "\n\n")
  if (nrow(x$zph_table) > 0) print(x$zph_table)
  invisible(x)
}
