lencode_lung <- function() {
  lung <- stats::na.omit(survival::lung[, c("time", "status", "sex", "ph.ecog", "age")])
  lung$status <- lung$status - 1
  lung$sex <- factor(lung$sex, labels = c("male", "female"))
  lung$ph.ecog <- factor(lung$ph.ecog)
  lung
}

lencode_mapping <- function(prepped, term) {
  tidied <- recipes::tidy(prepped, number = 1)
  stats::setNames(tidied$value[tidied$terms == term], tidied$level[tidied$terms == term])
}

test_that("step_lencode_coxnet encodes a two-level factor, whose design matrix has a single column", {
  # Regression test: glmnet needs two or more columns, so a binary factor's
  # fit errored, and the error was swallowed into an all-zero encoding.
  skip_if_not_installed("recipes")
  skip_if_not_installed("glmnet")
  skip_if_not_installed("survival")
  lung <- lencode_lung()

  rec <- recipes::recipe(~ age + sex + time + status, data = lung) |>
    step_lencode_coxnet(sex, outcome = c("time", "status"), penalty = 0.001)
  prepped <- recipes::prep(rec, training = lung)
  map <- lencode_mapping(prepped, "sex")

  expect_named(map, c("male", "female"))
  expect_equal(unname(map[["male"]]), 0)
  truth <- unname(stats::coef(survival::coxph(survival::Surv(time, status) ~ sex, data = lung)))
  expect_lt(truth, -0.4)  # women do better: the true log hazard ratio is clearly non-zero
  expect_equal(unname(map[["female"]]), truth, tolerance = 0.05)

  baked <- recipes::bake(prepped, new_data = lung)
  expect_equal(baked$sex, unname(map[as.character(lung$sex)]))
})

test_that("step_lencode_coxnet shrinks toward zero as the penalty grows, and encodes multi-level factors", {
  skip_if_not_installed("recipes")
  skip_if_not_installed("glmnet")
  skip_if_not_installed("survival")
  lung <- lencode_lung()
  encode <- function(penalty) {
    rec <- recipes::recipe(~ age + ph.ecog + time + status, data = lung) |>
      step_lencode_coxnet(ph.ecog, outcome = c("time", "status"), penalty = penalty)
    lencode_mapping(recipes::prep(rec, training = lung), "ph.ecog")
  }
  weak <- encode(0.001)
  strong <- encode(0.05)
  truth <- unname(stats::coef(survival::coxph(survival::Surv(time, status) ~ ph.ecog, data = lung)))
  expect_equal(unname(weak[-1]), truth, tolerance = 0.1)
  expect_true(all(abs(strong[-1]) <= abs(weak[-1]) + 1e-8))
  expect_equal(unname(strong[["0"]]), 0)
  expect_equal(unname(encode(10)), rep(0, 4))  # past lambda_max: every coefficient is 0
})

test_that("step_lencode_coxnet treats missing and unseen levels as the reference level", {
  skip_if_not_installed("recipes")
  skip_if_not_installed("glmnet")
  skip_if_not_installed("survival")
  lung <- lencode_lung()
  train <- lung
  train$sex[c(3, 9)] <- NA  # missing values must not drop rows from the fit or break it

  rec <- recipes::recipe(~ age + sex + time + status, data = train) |>
    step_lencode_coxnet(sex, outcome = c("time", "status"), penalty = 0.001)
  prepped <- recipes::prep(rec, training = train)
  expect_lt(lencode_mapping(prepped, "sex")[["female"]], -0.3)

  new <- lung[1:4, ]
  new$sex <- as.character(new$sex)
  new$sex[c(1, 2)] <- c("female", NA)
  new$sex[3] <- "other"
  # recipes itself notes that `sex` was a factor when prepped and is character now.
  expect_warning(baked <- recipes::bake(prepped, new_data = new), "factor when the recipe was prepped")
  expect_equal(baked$sex[1:3], c(lencode_mapping(prepped, "sex")[["female"]], 0, 0))
  expect_error(recipes::bake(prepped, new_data = new[c("age", "time", "status")]), "sex")
})

test_that("step_lencode_coxnet accepts counting-process outcomes and warns instead of hiding a failed fit", {
  skip_if_not_installed("recipes")
  skip_if_not_installed("glmnet")
  skip_if_not_installed("survival")
  d <- sim_counting(n = 120, seed = 3)
  d$site <- factor(d$site)

  rec <- recipes::recipe(~ site + x1 + tstart + tstop + status, data = d) |>
    step_lencode_coxnet(site, outcome = c("tstart", "tstop", "status"), penalty = 0.001)
  prepped <- recipes::prep(rec, training = d)
  expect_equal(names(lencode_mapping(prepped, "site")), levels(d$site))

  # When glmnet can't fit, say so instead of encoding zeros quietly.
  testthat::local_mocked_bindings(glmnet = function(...) stop("boom"), .package = "glmnet")
  expect_warning(prepped <- recipes::prep(rec, training = d), "encoding all levels to 0.*boom")
  expect_equal(unname(lencode_mapping(prepped, "site")), rep(0, nlevels(d$site)))
})

test_that("step_lencode_coxnet's tunable method is registered with generics::tunable()", {
  skip_if_not_installed("recipes")
  rec <- recipes::recipe(~ ., data = lencode_lung()) |>
    step_lencode_coxnet(sex, outcome = c("time", "status"))
  tun <- generics::tunable(rec$steps[[1]])
  expect_equal(tun$name, c("penalty", "mixture"))
  expect_equal(tun$component, rep("step_lencode_coxnet", 2))
  expect_equal(generics::required_pkgs(rec$steps[[1]]), c("TempleCBE", "glmnet", "survival"))
})
