#' Audit Rank Deficiency of a Model's Fixed-Effects Matrix
#'
#' With sparse or unobserved factor cells, `lme4::lmer()` silently drops redundant columns of
#' the fixed-effects model matrix, and `lm()` / `glm()` return `NA` coefficients. This reports
#' which terms were dropped.
#'
#' @param model A `merMod` object (`lmer`, `glmer`) or an `lm` / `glm` object.
#' @param verbose Emit a markdown callout with [message()] when the model is rank-deficient.
#' @return A list of class `cbe_rank_deficiency` with `rank_deficient` (logical), `n_columns`
#'   (columns of the full model matrix), `rank`, `dropped` (names of the dropped terms) and
#'   `message` (the markdown callout text; `NULL` if full rank), returned invisibly.
#' @seealso [lmer_safely()]
#' @export
#' @examples
#' d <- data.frame(y = c(1, 2, 3, 5, 4, 6), a = 1:6, b = 2 * (1:6))
#' inspect_rank_deficiency(lm(y ~ a + b, d))$dropped
inspect_rank_deficiency <- function(model, verbose = TRUE) {
  if (inherits(model, "merMod")) {
    x <- model@pp$X
    dropped_idx <- attr(x, "col.dropped")
    kept <- colnames(x)
    n_cols <- length(kept) + length(dropped_idx)
    dropped <- if (length(dropped_idx)) {
      nm <- names(dropped_idx)
      if (is.null(nm)) as.character(unname(dropped_idx)) else nm
    } else {
      character(0)
    }
    rank <- length(kept)
    engine_msg <- attr(x, "msgRankdrop")
  } else if (inherits(model, "lm")) {
    cf <- stats::coef(model)
    if (inherits(model, "mlm")) stop("Multivariate `lm` fits are not supported.", call. = FALSE)
    dropped <- names(cf)[is.na(cf)]
    n_cols <- length(cf)
    rank <- model$rank
    engine_msg <- NULL
  } else {
    stop("`model` must be a merMod, lm or glm object.", call. = FALSE)
  }
  deficient <- length(dropped) > 0L
  msg <- NULL
  if (deficient) {
    msg <- paste0(
      "> **Rank-deficient model matrix.** ", length(dropped), " of ", n_cols,
      " fixed-effect column(s) were dropped (rank ", rank, "): ",
      paste0("`", dropped, "`", collapse = ", "), ". ",
      "The cells that define them are empty or confounded with other terms, ",
      "so their effects are not estimated."
    )
    if (!is.null(engine_msg) && nzchar(paste(engine_msg, collapse = ""))) {
      msg <- paste0(msg, " Engine note: ", paste(engine_msg, collapse = " "))
    }
    if (isTRUE(verbose)) message(msg)
  }
  invisible(structure(
    list(rank_deficient = deficient, n_columns = n_cols, rank = rank, dropped = dropped, message = msg),
    class = c("cbe_rank_deficiency", "list")
  ))
}
