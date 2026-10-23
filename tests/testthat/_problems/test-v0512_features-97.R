# Extracted from test-v0512_features.R:97

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "TempleCBE", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
args <- formals(tune_alpha_by_split)
testthat::expect_true("calibrate" %in% names(args))
testthat::expect_identical(args$calibrate, TRUE)
args_sum <- formals(summarize_tune_results)
