# Extracted from test-survival-wrappers-compat.R:56

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "TempleCBE", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
compat_data <- function(n = 60, seed = 11) {
  set.seed(seed)
  do.call(rbind, lapply(seq_len(n), function(i) {
    k <- sample(1:3, 1)
    stops <- c(5, 10, 20)[seq_len(k)]
    x1 <- stats::rnorm(1)
    data.frame(
      subject_id = i,
      tstart = c(0, utils::head(stops, -1)),
      tstop = stops,
      status = c(rep(0L, k - 1), stats::rbinom(1, 1, stats::plogis(1.5 * x1))),
      x1 = x1 + stats::rnorm(k, sd = 0.1),
      x2 = stats::rnorm(k),
      x3 = stats::rnorm(k)
    )
  }))
}
compat_fixture <- function() {
  long <- compat_data()
  rec <- recipes::recipe(~ ., data = long)
  rec <- recipes::update_role(rec, "subject_id", "tstart", "tstop", "status", new_role = "id variable")
  rec <- recipes::step_range(rec, recipes::all_numeric_predictors())
  set.seed(12)
  list(
    long = long,
    split = rsample::group_initial_split(long, group = subject_id, prop = 0.7),
    recipe = rec,
    features = c("x1", "x2", "x3"),
    times = dplyr::arrange(dplyr::distinct(long, tstart, tstop), tstart, tstop)
  )
}
skip_if_no_compat_deps <- function() {
  for (pkg in c("glmnet", "survival", "rsample", "yardstick", "recipes")) skip_if_not_installed(pkg)
}

# test -------------------------------------------------------------------------
for (fn in list(tune_over_alpha, summarize_tune_results)) {
    args <- names(formals(fn))
    expect_identical(args[1:2], c("object", "..."))
    expect_true(all(c("num_alpha_values", "num_fixed", "alphas", "formulas") %in% args))
  }
