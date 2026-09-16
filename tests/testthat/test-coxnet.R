test_that("coxnet's formula, recipe, and x/y interfaces all reproduce glmnet", {
  skip_if_no_coxnet_deps()
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
  skip_if_no_coxnet_deps()
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
  skip_if_no_coxnet_deps()
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

test_that("coxnet models don't take over glmnet's own coxnet methods", {
  skip_if_no_coxnet_deps()
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
