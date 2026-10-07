test_that("coxnet's formula, recipe, and x/y interfaces all reproduce glmnet", {
  skip_if_no_coxnet_deps(cv = FALSE)
  d <- sim_counting()
  y <- survival::Surv(d$tstart, d$tstop, d$status)
  xs <- c("x1", "x2", "x3")

  ref <- glmnet::glmnet(as.matrix(d[xs]), y, family = "cox", alpha = 0.5, cox.ties = "breslow")
  s <- ref$lambda[6]
  ref_beta <- unname(as.matrix(stats::coef(ref, s = s))[, 1])

  by_formula <- coxnet(survival::Surv(tstart, tstop, status) ~ x1 + x2 + x3, data = d, mixture = 0.5, cox.ties = "breslow")
  by_xy <- coxnet(d[xs], y, mixture = 0.5, cox.ties = "breslow")
  by_matrix <- coxnet(as.matrix(d[xs]), y, mixture = 0.5, cox.ties = "breslow")
  d$surv <- y
  by_recipe <- coxnet(recipes::recipe(surv ~ x1 + x2 + x3, data = d), d, mixture = 0.5, cox.ties = "breslow")

  for (fit in list(by_formula, by_xy, by_matrix, by_recipe)) {
    expect_s3_class(fit, "coxnet_model")
    tidied <- generics::tidy(fit, penalty = s)
    expect_named(tidied, c("term", "estimate", "penalty"))
    expect_equal(tidied$term, xs)
    expect_equal(tidied$estimate, ref_beta)
  }
})

test_that("predict.coxnet returns tidymodels-shaped linear predictors and Breslow survival", {
  skip_if_no_coxnet_deps(cv = FALSE)
  d <- sim_counting()
  y <- survival::Surv(d$tstart, d$tstop, d$status)
  fit <- coxnet(survival::Surv(tstart, tstop, status) ~ x1 + x2 + x3, data = d, cox.ties = "breslow")
  s <- fit$fit$lambda[4]
  link <- as.numeric(stats::predict(fit$fit, as.matrix(d[1:4, c("x1", "x2", "x3")]), s = s, type = "link"))

  lp <- predict(fit, d[1:4, ], penalty = s)
  expect_named(lp, ".pred_linear_pred")
  expect_equal(lp$.pred_linear_pred, -link)
  expect_equal(predict(fit, d[1:4, ], penalty = s, increasing = FALSE)$.pred_linear_pred, link)

  sv <- predict(fit, d[1:4, ], type = "survival", penalty = s, eval_time = c(3, 6, 9))
  expect_equal(nrow(sv), 4)
  expect_named(sv$.pred[[1]], c(".eval_time", ".pred_survival"))
  expect_true(all(diff(sv$.pred[[1]]$.pred_survival) <= 0))

  # Against survival's Breslow estimate with the glmnet linear predictor as an offset.
  d$lp <- as.numeric(stats::predict(fit$fit, as.matrix(d[c("x1", "x2", "x3")]), s = s, type = "link"))
  ref <- survival::coxph(survival::Surv(tstart, tstop, status) ~ offset(lp), data = d, ties = "breslow")
  ref_fit <- survival::survfit(ref, newdata = data.frame(lp = link[1]), se.fit = FALSE)
  expected <- ref_fit$surv[findInterval(c(3, 6, 9), ref_fit$time)]
  expect_equal(sv$.pred[[1]]$.pred_survival, expected)

  expect_error(predict(fit, d[1:2, ]), "penalty")
  expect_error(predict(fit, d[1:2, ], type = "survival", penalty = s), "eval_time")
  with_default <- coxnet(survival::Surv(tstart, tstop, status) ~ x1 + x2 + x3, data = d, penalty = s, cox.ties = "breslow")
  expect_equal(predict(with_default, d[1:4, ])$.pred_linear_pred, -link)
})

test_that("coxnet fits right-censored data and validates its inputs", {
  skip_if_no_coxnet_deps(cv = FALSE)
  d <- sim_counting()
  last <- d[!duplicated(d$subject_id, fromLast = TRUE), ]
  right <- coxnet(last[c("x1", "x2", "x3")], survival::Surv(last$tstop, last$status), penalty = 0.01)
  expect_s3_class(right, "coxnet_model")
  expect_equal(nrow(generics::tidy(right)), 3)

  y <- survival::Surv(d$tstart, d$tstop, d$status)
  expect_error(coxnet(d[c("x1", "x2")], d$x3), "Surv")
  expect_error(coxnet(d["x1"], y), "at least two")
  with_na <- d[c("x1", "x2")]
  with_na$x1[1] <- NA
  expect_error(coxnet(with_na, y), "missing")
  expect_error(coxnet(d[c("x1", "site")], y), "numeric")
  expect_error(coxnet(d[c("x1", "x2")], y, alpha = 1), "mixture")
  expect_error(coxnet(d[c("x1", "x2")], y, mixture = 2), "mixture")
  expect_error(coxnet("a"), "not defined")
})

test_that("coxnet rejects Inf and -Inf in the predictors and the follow-up times, naming the columns", {
  # glmnet takes an infinite predictor without a word and gives it a coefficient
  # of 0, so the most important predictor could silently drop out of the model.
  skip_if_no_coxnet_deps(cv = FALSE)
  d <- sim_counting()
  y <- survival::Surv(d$tstart, d$tstop, d$status)
  xs <- d[c("x1", "x2", "x3")]

  with_inf <- xs
  with_inf$x1[3] <- Inf
  with_inf$x3[5] <- -Inf
  expect_error(coxnet(with_inf, y), "finite.*x1, x3")
  expect_error(coxnet(as.matrix(with_inf), y), "finite.*x1, x3")

  d_inf <- d
  d_inf$x2[7] <- Inf
  expect_error(
    coxnet(survival::Surv(tstart, tstop, status) ~ x1 + x2 + x3, data = d_inf, penalty = 0.05),
    "finite.*x2"
  )

  y_late <- survival::Surv(d$tstart, replace(d$tstop, 4, Inf), d$status)
  expect_error(coxnet(xs, y_late), "Follow-up times must be finite")

  # NA keeps its own message.
  with_na <- xs
  with_na$x1[3] <- NA
  expect_error(coxnet(with_na, y), "missing")
})

test_that("strata() and offset() in a formula are refused, not fit as something else", {
  # strata() used to be fit as penalized indicator columns and offset() dropped
  # without a word, so the model was not the one the formula described.
  skip_if_no_coxnet_deps(cv = FALSE)
  d <- sim_counting()
  expect_error(
    coxnet(survival::Surv(tstart, tstop, status) ~ x1 + x2 + offset(x3), data = d, penalty = 0.05),
    "strata\\(\\) and offset\\(\\) are not supported.*offset\\(\\)"
  )
  expect_error(
    coxnet(survival::Surv(tstart, tstop, status) ~ x1 + x2 + strata(site), data = d, penalty = 0.05),
    "strata\\(\\) and offset\\(\\) are not supported.*strata\\(\\)"
  )
  # The namespaced spelling is found too, and so is a special inside another call.
  expect_error(
    coxnet(survival::Surv(tstart, tstop, status) ~ x1 + x2 + survival::strata(site), data = d),
    "not supported"
  )
  expect_error(
    coxnet(survival::Surv(tstart, tstop, status) ~ x1 + x2 + I(stats::offset(x3) * 2), data = d),
    "not supported"
  )
  expect_error(
    coxnet(survival::Surv(tstart, tstop, status) ~ x1 + offset(x3) + survival::strata(site), data = d),
    "offset\\(\\), strata\\(\\)"
  )

  # A column that is merely named `strata` or `offset` is an ordinary predictor.
  d$strata <- d$x3
  d$offset <- d$x2
  fit <- coxnet(survival::Surv(tstart, tstop, status) ~ x1 + strata + offset, data = d, penalty = 0.05)
  expect_equal(generics::tidy(fit)$term, c("x1", "strata", "offset"))
})

test_that("coxnet models don't take over glmnet's own coxnet methods", {
  skip_if_no_coxnet_deps(cv = FALSE)
  # glmnet classes its Cox fits "coxnet"; TempleCBE's class must not collide.
  d <- sim_counting()
  y <- survival::Surv(d$tstart, d$tstop, d$status)
  x <- as.matrix(d[c("x1", "x2", "x3")])
  raw <- glmnet::glmnet(x, y, family = "cox", cox.ties = "breslow")
  expect_s3_class(raw, "coxnet")
  expect_false(any(c("predict.coxnet", "tidy.coxnet", "print.coxnet") %in% ls(asNamespace("TempleCBE"))))
  expect_equal(dim(stats::predict(raw, newx = x[1:2, ], s = raw$lambda[3], type = "link")), c(2L, 1L))

  fit <- coxnet(d[c("x1", "x2", "x3")], y)
  expect_s3_class(fit, "coxnet_model")
  expect_false(inherits(fit, "coxnet"))
})

test_that("a penalty below glmnet's default path is honored, not silently clamped to the path's end", {
  skip_if_no_coxnet_deps(cv = FALSE)
  d <- sim_counting()
  f <- survival::Surv(tstart, tstop, status) ~ x1 + x2 + x3
  base <- coxnet(f, data = d)
  smallest <- min(base$fit$lambda)

  # Without an extended path, everything below `smallest` gave the coefficients at `smallest`.
  fit0 <- coxnet(f, data = d, penalty = 0)
  expect_equal(min(fit0$fit$lambda), 0)
  ref <- survival::coxph(f, data = d, ties = "breslow")
  expect_equal(generics::tidy(fit0)$estimate, unname(stats::coef(ref)), tolerance = 1e-3)
  expect_false(isTRUE(all.equal(generics::tidy(fit0)$estimate, generics::tidy(base, penalty = smallest)$estimate)))

  small <- smallest / 1000
  fit_small <- coxnet(f, data = d, penalty = small)
  expect_equal(min(fit_small$fit$lambda), small)
  expect_equal(fit_small$fit$lambda[seq_along(base$fit$lambda)], base$fit$lambda)
  expect_false(anyNA(generics::tidy(fit_small)$estimate))
  expect_gt(
    max(abs(generics::tidy(fit_small)$estimate - generics::tidy(base, penalty = smallest)$estimate)), 1e-4
  )

  # A penalty already on the path leaves the fit untouched.
  inside <- coxnet(f, data = d, penalty = base$fit$lambda[10])
  expect_equal(inside$fit$lambda, base$fit$lambda)
})

test_that("extend_path continues a decreasing path down to the penalty", {
  path <- c(0.4, 0.2, 0.1)
  ext <- extend_path(path, 0.01)
  expect_equal(ext[1:3], path)
  expect_false(is.unsorted(rev(ext), strictly = TRUE))
  expect_equal(min(ext), 0.01)
  # Zero and penalties far below the path end in the penalty itself, after a geometric floor.
  zero <- extend_path(path, 0)
  expect_equal(c(zero[length(zero) - 1], zero[length(zero)]), c(0.1 * 1e-4, 0))
  expect_equal(min(extend_path(path, 1e-12)), 1e-12)
})

test_that("predict and tidy warn when asked for a penalty below the fitted path", {
  skip_if_no_coxnet_deps(cv = FALSE)
  d <- sim_counting()
  f <- survival::Surv(tstart, tstop, status) ~ x1 + x2 + x3
  fit <- coxnet(f, data = d)
  smallest <- min(fit$fit$lambda)
  expect_warning(generics::tidy(fit, penalty = smallest / 10), "smallest penalty on the fitted path")
  expect_warning(predict(fit, d[1:2, ], penalty = smallest / 10), "smallest penalty")
  expect_no_warning(generics::tidy(fit, penalty = smallest))
  expect_no_warning(generics::tidy(fit, penalty = 10 * max(fit$fit$lambda)))

  # A user-supplied path is used as given: a lower default penalty warns at fit time.
  expect_warning(coxnet(f, data = d, penalty = 1e-6, path = c(0.1, 0.05, 0.01)), "below the smallest penalty on `path`")
})

test_that("coxnet on start/stop data matches survival::coxph at a vanishing penalty, with time-varying covariates", {
  skip_if_no_coxnet_deps(cv = FALSE)
  d <- sim_counting(n = 150, seed = 7)
  f <- survival::Surv(tstart, tstop, status) ~ x1 + x2 + x3
  fit <- coxnet(f, data = d, penalty = 1e-7)
  ref <- survival::coxph(f, data = d, ties = "breslow")
  expect_equal(generics::tidy(fit)$estimate, unname(stats::coef(ref)), tolerance = 1e-3)
  # Splitting a subject's follow-up into more intervals changes nothing when covariates are constant.
  long <- do.call(rbind, lapply(seq_len(nrow(d)), function(i) {
    r <- d[i, ]
    mid <- (r$tstart + r$tstop) / 2
    rbind(transform(r, tstop = mid, status = 0L), transform(r, tstart = mid))
  }))
  fit_split <- coxnet(f, data = long, penalty = 1e-7)
  const <- coxnet(f, data = d, penalty = 1e-7)
  const_ref <- survival::coxph(f, data = d, ties = "breslow")
  split_ref <- survival::coxph(f, data = long, ties = "breslow")
  expect_equal(unname(stats::coef(split_ref)), unname(stats::coef(const_ref)), tolerance = 1e-6)
  expect_equal(generics::tidy(fit_split)$estimate, generics::tidy(const)$estimate, tolerance = 1e-3)
})
