#' Sample Candidate Model Formulas
#'
#' Draws a reproducible set of distinct model formulas from a pool of predictors, for the
#' candidate-model step of a nested cross-validation (random or weighted subset search over
#' predictors). It does not look at data: it only chooses which terms go into each formula.
#'
#' @section Terms:
#' Each element of `predictors` is **one term**, written as it should appear in the formula:
#' a variable (`"age"`), a transformation (`"log(creatinine)"`, `"poly(age, 2)"`) or an interaction
#' (`"age:sex"`). A transformed term counts as one term, an interaction counts as one term of its
#' own, and `size` limits count all terms (required ones included). Terms are compared as text, so
#' `"age:sex"` and `"sex:age"` are different terms; list each once.
#'
#' @section Constraints:
#' * `required` terms are in every formula.
#' * `min_size` and `max_size` bound the number of terms.
#' * `hierarchy = TRUE` (default) rejects a formula that has an interaction without all its
#'   variables as terms of their own (`"age:sex"` needs `"age"` and `"sex"`).
#' * `exclude` is a list of character vectors; a formula may not contain all terms of any one vector
#'   (for example `list(c("bmi", "weight"))` keeps two near-duplicates apart).
#'
#' @section Sampling:
#' For each draw the size is chosen uniformly between `min_size` and `max_size`, then the non-required
#' terms are drawn without replacement, with probability proportional to `weights` (equal by default).
#' A draw that breaks a constraint is rejected. Formulas with the same set of terms are the same
#' formula, whatever the order, and are kept once (`unique = TRUE`). If fewer than `n` distinct valid
#' formulas can be found within `n * 50` attempts, the ones found are returned with a warning.
#'
#' @section Data boundaries:
#' No data are passed, so nothing here can use held-out rows. If you give `weights`, compute them
#' from the **training** rows of the current fold only (for instance with [calculate_feature_weights()]
#' on scores obtained in the training fold); weights from the full data would leak the held-out
#' rows into candidate selection.
#'
#' @param predictors Character vector of candidate terms (see Terms).
#' @param response Optional left-hand side as text, for example `"Surv(time, status)"`. Without it
#'   the formulas are one-sided (`~ a + b`).
#' @param required Terms that are in every formula. Need not be in `predictors`.
#' @param n Number of distinct formulas wanted.
#' @param min_size,max_size Smallest and largest number of terms per formula
#'   (`max_size` defaults to all terms).
#' @param weights Optional non-negative numeric vector, one value per entry of `predictors`
#'   (named by term, or in the same order). A weight of 0 keeps a term out unless it is required.
#' @param exclude Optional list of character vectors of terms that may not all appear together.
#' @param hierarchy Logical; see Constraints.
#' @param unique Logical; drop repeated term sets (default `TRUE`).
#' @param seed Optional integer. The random-number state of the caller is restored afterwards.
#' @param env Environment of the formulas (default: the caller's).
#' @return A tibble with one row per formula: `id`, `formula` (list of formulas), `terms` (list of
#'   character vectors, required terms first) and `n_terms`.
#' @seealso [calculate_feature_weights()]
#' @export
#' @examples
#' cand <- sample_candidate_formulas(
#'   c("age", "sex", "bmi", "log(lactate)", "age:sex"),
#'   response = "Surv(time, status)", required = "age", n = 5, max_size = 3, seed = 1
#' )
#' cand$formula[[1]]
#' cand$terms
sample_candidate_formulas <- function(predictors,
                                      response = NULL,
                                      required = character(),
                                      n = 100L,
                                      min_size = 1L,
                                      max_size = NULL,
                                      weights = NULL,
                                      exclude = NULL,
                                      hierarchy = TRUE,
                                      unique = TRUE,
                                      seed = NULL,
                                      env = parent.frame()) {
  if (!is.character(predictors) || length(predictors) < 1L || anyNA(predictors) || any(!nzchar(predictors))) {
    stop("`predictors` must be a non-empty character vector of terms.", call. = FALSE)
  }
  if (anyDuplicated(predictors)) {
    stop("`predictors` has repeated terms: ", paste(predictors[duplicated(predictors)], collapse = ", "), ".", call. = FALSE)
  }
  if (!is.character(required) || anyNA(required)) stop("`required` must be a character vector.", call. = FALSE)
  required <- base::unique(required)
  if (!is.null(response) && (!is.character(response) || length(response) != 1L || !nzchar(response))) {
    stop("`response` must be one string such as \"Surv(time, status)\".", call. = FALSE)
  }
  if (!is.numeric(n) || length(n) != 1L || is.na(n) || n < 1) stop("`n` must be a positive whole number.", call. = FALSE)
  n <- as.integer(n)

  pool <- setdiff(predictors, required)
  all_terms <- c(required, pool)
  max_size <- if (is.null(max_size)) length(all_terms) else as.integer(max_size)
  min_size <- as.integer(min_size)
  if (is.na(min_size) || is.na(max_size) || min_size < 1L) {
    stop("`min_size` and `max_size` must be whole numbers, with `min_size` at least 1.", call. = FALSE)
  }
  if (min_size < length(required)) min_size <- length(required)
  if (max_size > length(all_terms)) max_size <- length(all_terms)
  if (min_size > max_size) {
    stop("No formula fits: `min_size` (", min_size, ") is larger than `max_size` (", max_size,
         ") or the number of terms.", call. = FALSE)
  }

  w <- rep(1, length(pool))
  if (!is.null(weights)) {
    if (!is.numeric(weights) || anyNA(weights) || any(weights < 0)) {
      stop("`weights` must be non-negative numbers without NA.", call. = FALSE)
    }
    if (!is.null(names(weights))) {
      if (!all(predictors %in% names(weights))) {
        stop("`weights` is named but lacks: ", paste(setdiff(predictors, names(weights)), collapse = ", "), ".", call. = FALSE)
      }
      w <- unname(weights[pool])
    } else {
      if (length(weights) != length(predictors)) {
        stop("`weights` must have one value per entry of `predictors`.", call. = FALSE)
      }
      w <- weights[match(pool, predictors)]
    }
  }
  if (!is.null(exclude)) {
    if (!is.list(exclude) || !all(vapply(exclude, is.character, logical(1)))) {
      stop("`exclude` must be a list of character vectors.", call. = FALSE)
    }
  }

  parents_of <- function(term) if (grepl(":", term, fixed = TRUE)) trimws(strsplit(term, ":", fixed = TRUE)[[1]]) else character()
  valid <- function(terms) {
    if (isTRUE(hierarchy)) {
      for (tm in terms) {
        par <- parents_of(tm)
        if (length(par) && !all(par %in% terms)) return(FALSE)
      }
    }
    for (ex in exclude) if (all(ex %in% terms)) return(FALSE)
    TRUE
  }
  if (!valid(required)) stop("The `required` terms already break a constraint.", call. = FALSE)

  if (!is.null(seed)) {
    had <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
    old <- if (had) get(".Random.seed", envir = globalenv()) else NULL
    on.exit({
      if (had) assign(".Random.seed", old, envir = globalenv()) else suppressWarnings(rm(".Random.seed", envir = globalenv()))
    }, add = TRUE)
    set.seed(seed)
  }

  found <- list()
  keys <- character()
  attempts <- 0L
  max_attempts <- n * 50L
  n_free_max <- sum(w > 0)
  while (length(found) < n && attempts < max_attempts) {
    attempts <- attempts + 1L
    k_extra <- sample(seq.int(max(min_size - length(required), 0L), max_size - length(required)), 1L)
    if (k_extra > n_free_max) next
    drawn <- if (k_extra == 0L) character() else pool[sample.int(length(pool), k_extra, prob = if (any(w != 1)) w else NULL)]
    terms <- c(required, drawn)
    if (!valid(terms)) next
    key <- paste(sort(terms), collapse = "\r")
    if (isTRUE(unique) && key %in% keys) next
    keys <- c(keys, key)
    found[[length(found) + 1L]] <- terms
  }
  if (length(found) < n) {
    warning("Only ", length(found), " of ", n, " distinct valid formulas could be found.", call. = FALSE)
  }

  mk <- function(terms) {
    rhs <- paste(terms, collapse = " + ")
    stats::as.formula(paste(if (is.null(response)) "" else response, "~", rhs), env = env)
  }
  tibble::tibble(
    id = seq_along(found),
    formula = lapply(found, mk),
    terms = found,
    n_terms = vapply(found, length, integer(1))
  )
}

#' Weight Features by the Candidate Models That Use Them
#'
#' Turns a set of scored candidate formulas into one weight per feature: how much of the total
#' model score sits in the formulas that contain the feature. Use it to learn, in a training fold,
#' which predictors to favour when sampling the next round of candidates
#' ([sample_candidate_formulas()] `weights`).
#'
#' @section Definition:
#' Each formula gets a share `s_i` of the total score: scores are shifted so the worst is 0
#' (`score - min(score)`, after flipping the sign when `higher_is_better = FALSE`) and divided by their
#' sum; if all scores are equal every formula gets `1 / n`. The weight of a feature is the sum of the
#' shares of the formulas that contain it, a number between 0 and 1: 1 means it is in every
#' formula that scores above the worst, 0 that it is in none. `method = "inclusion"` ignores the scores
#' and gives the plain fraction of formulas that contain the feature. `normalize = "sum"` rescales the
#' weights to add to 1 and `"max"` rescales the largest to 1.
#'
#' @section What counts as a feature:
#' With `level = "variable"` (default) a feature is a variable: `log(x)` and `poly(x, 2)` count for
#' `x`, an interaction `a:b` counts for both `a` and `b`, and a variable counts once per formula
#' however many terms use it. With `level = "term"` a feature is a whole term as written
#' (`"log(x)"`, `"a:b"`).
#'
#' @section Data boundaries:
#' The function only sees formulas and their scores. The scores must be computed on the **training**
#' rows of the fold you are in (for example by an inner cross-validation inside the training set);
#' a score computed on the held-out rows would leak them into the weights.
#'
#' @param candidates A tibble from [sample_candidate_formulas()], or a list of formulas or of
#'   character vectors of terms.
#' @param scores Numeric vector, one score per candidate (needed unless `method = "inclusion"`).
#'   Candidates with an `NA` score are dropped, with a warning.
#' @param method `"score"` (default) or `"inclusion"`.
#' @param higher_is_better Logical; set `FALSE` for losses such as the Brier score.
#' @param level `"variable"` (default) or `"term"`.
#' @param normalize `"none"` (default), `"sum"` or `"max"`.
#' @return A tibble with `feature`, `weight`, `n_formulas` (how many formulas contain it) and `rank`,
#'   ordered by weight (ties by name).
#' @seealso [sample_candidate_formulas()]
#' @export
#' @examples
#' cand <- sample_candidate_formulas(c("age", "sex", "bmi", "lactate"), n = 8, max_size = 3, seed = 2)
#' scores <- c(0.61, 0.70, 0.66, 0.72, 0.58, 0.69, 0.64, 0.71)[seq_len(nrow(cand))]
#' calculate_feature_weights(cand, scores)
calculate_feature_weights <- function(candidates,
                                      scores = NULL,
                                      method = c("score", "inclusion"),
                                      higher_is_better = TRUE,
                                      level = c("variable", "term"),
                                      normalize = c("none", "sum", "max")) {
  method <- match.arg(method)
  level <- match.arg(level)
  normalize <- match.arg(normalize)

  term_sets <- if (is.data.frame(candidates) && "terms" %in% names(candidates)) {
    candidates$terms
  } else if (is.list(candidates) && !is.data.frame(candidates)) {
    lapply(candidates, function(x) {
      if (inherits(x, "formula")) attr(stats::terms(x), "term.labels") else as.character(x)
    })
  } else {
    stop("`candidates` must be a result of `sample_candidate_formulas()` or a list of formulas / term vectors.",
         call. = FALSE)
  }
  n <- length(term_sets)
  if (n < 1L) stop("`candidates` is empty.", call. = FALSE)

  if (method == "score") {
    if (is.null(scores)) stop("`scores` is needed for method = \"score\".", call. = FALSE)
    if (!is.numeric(scores) || length(scores) != n) {
      stop("`scores` must be numeric with one value per candidate (", n, ").", call. = FALSE)
    }
    keep <- !is.na(scores)
    if (!all(keep)) warning(sum(!keep), " candidate(s) with a missing score were dropped.", call. = FALSE)
    if (!any(keep)) stop("No candidate has a score.", call. = FALSE)
    term_sets <- term_sets[keep]
    s <- scores[keep]
    if (!isTRUE(higher_is_better)) s <- -s
    shift <- s - min(s)
    share <- if (sum(shift) > 0) shift / sum(shift) else rep(1 / length(s), length(s))
  } else {
    share <- rep(1 / n, n)
  }

  feats_of <- function(terms) {
    f <- if (level == "term") terms else unlist(lapply(terms, function(tm) {
      all.vars(tryCatch(stats::as.formula(paste("~", tm)), error = function(e) NULL))
    }))
    base::unique(f)
  }
  per <- lapply(term_sets, feats_of)
  features <- sort(base::unique(unlist(per)))
  if (length(features) == 0L) stop("The candidates contain no terms.", call. = FALSE)
  in_f <- vapply(per, function(f) features %in% f, logical(length(features)))
  in_f <- matrix(in_f, nrow = length(features))
  weight <- as.numeric(in_f %*% share)
  n_formulas <- as.integer(rowSums(in_f))
  weight <- switch(normalize,
    none = weight,
    sum = if (sum(weight) > 0) weight / sum(weight) else weight,
    max = if (max(weight) > 0) weight / max(weight) else weight
  )
  out <- tibble::tibble(feature = features, weight = weight, n_formulas = n_formulas)
  out <- out[order(-out$weight, out$feature), , drop = FALSE]
  out$rank <- seq_len(nrow(out))
  out
}
