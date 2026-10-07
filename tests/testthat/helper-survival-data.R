# Synthetic start/stop survival data shared by the coxnet tests. Each subject
# has 1-3 intervals with continuous stop times, belongs to one site, and can
# only have an event in their last interval; x1 drives the risk.
sim_counting <- function(n = 80, seed = 1, n_sites = 4) {
  set.seed(seed)
  do.call(rbind, lapply(seq_len(n), function(i) {
    k <- sample(1:3, 1)
    stops <- round(cumsum(stats::runif(k, 2, 6)), 2)
    risk <- stats::rnorm(1)
    data.frame(
      subject_id = i,
      site = paste0("site_", (i - 1) %% n_sites + 1),
      tstart = c(0, utils::head(stops, -1)),
      tstop = stops,
      status = c(rep(0L, k - 1), stats::rbinom(1, 1, stats::plogis(1.5 * risk))),
      x1 = risk + stats::rnorm(k, sd = 0.2),
      x2 = stats::rnorm(k),
      x3 = stats::rnorm(k)
    )
  }))
}

# Evaluates `expr`, muffling its warnings, and returns them with the value, for
# tests that check one warning among several (hardhat adds its own).
collect_warnings <- function(expr) {
  warns <- character()
  value <- withCallingHandlers(expr, warning = function(w) {
    warns <<- c(warns, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  list(value = value, warnings = warns)
}

# `cv = FALSE` skips only on what fitting and predicting a coxnet() model need;
# the default also needs rsample and yardstick, for cross-validation.
skip_if_no_coxnet_deps <- function(cv = TRUE) {
  pkgs <- c("glmnet", "survival", "recipes", "hardhat", if (cv) c("rsample", "yardstick"))
  for (pkg in pkgs) {
    testthat::skip_if_not_installed(pkg)
  }
}
