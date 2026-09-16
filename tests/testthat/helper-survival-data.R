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

skip_if_no_coxnet_deps <- function() {
  for (pkg in c("glmnet", "survival", "rsample", "yardstick", "recipes", "hardhat")) {
    testthat::skip_if_not_installed(pkg)
  }
}
