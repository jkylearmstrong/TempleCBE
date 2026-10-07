# Seeds the RNG for the rest of the calling function and restores the caller's
# RNG state when that function exits -- the convention simulate_cohort() and the
# pi_anonymizer helpers already follow -- so a seeded call is reproducible
# without perturbing the caller's later random draws.
#' @keywords internal
#' @noRd
local_seed <- function(seed, envir = parent.frame()) {
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  restore <- function() {
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }
  do.call(base::on.exit, list(as.call(list(restore)), add = TRUE), envir = envir)
  set.seed(seed)
  invisible(NULL)
}
