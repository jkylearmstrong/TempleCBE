famd_data <- function(n = 40, seed = 2026) {
  set.seed(seed)
  df <- as.data.frame(matrix(stats::rnorm(n * 3), nrow = n, dimnames = list(NULL, paste0("x", 1:3))))
  df$f1 <- factor(sample(c("a", "b", "c"), n, replace = TRUE))
  df$f2 <- sample(c("u", "v"), n, replace = TRUE)
  df$y <- stats::rnorm(n)
  df
}

famd_reference_data <- function(df) {
  out <- df[c("x1", "x2", "x3", "f1", "f2")]
  out$f2 <- factor(out$f2)
  out
}

skip_if_no_famd <- function() {
  skip_if_not_installed("recipes")
  skip_if_not_installed("FactoMineR")
}

test_that("step_famd reproduces FactoMineR::FAMD coordinates when training and on new data", {
  skip_if_no_famd()
  df <- famd_data()
  rec <- step_famd(recipes::recipe(y ~ ., data = df), recipes::all_predictors(), num_comp = 3)
  prepped <- recipes::prep(rec, training = df)
  baked <- recipes::bake(prepped, new_data = NULL)

  expect_named(baked, c("y", "FAMD1", "FAMD2", "FAMD3"))
  ref <- FactoMineR::FAMD(famd_reference_data(df), ncp = 3, graph = FALSE)
  expect_equal(unname(as.matrix(baked[c("FAMD1", "FAMD2", "FAMD3")])), unname(ref$ind$coord[, 1:3]))

  new_rows <- recipes::bake(prepped, new_data = df[1:5, ])
  expected <- stats::predict(ref, newdata = famd_reference_data(df)[1:5, ])$coord[, 1:3]
  expect_equal(unname(as.matrix(new_rows[c("FAMD1", "FAMD2", "FAMD3")])), unname(expected))

  one_row <- recipes::bake(prepped, new_data = df[1, ])
  expect_equal(nrow(one_row), 1)
  expect_equal(nrow(recipes::bake(prepped, new_data = df[0, ])), 0)
})

test_that("step_famd keeps more components than variables when FAMD has them, capped at its dimensions", {
  skip_if_no_famd()
  set.seed(1)
  df <- data.frame(x1 = stats::rnorm(30), x2 = stats::rnorm(30), f = factor(sample(letters[1:5], 30, TRUE)))
  # 2 numeric variables + (5 - 1) category dimensions = 6 dimensions from 3 variables.
  five <- recipes::prep(step_famd(recipes::recipe(~ ., data = df), recipes::all_predictors(), num_comp = 5))
  expect_equal(five$steps[[1]]$num_comp, 5)
  expect_equal(ncol(recipes::bake(five, new_data = NULL)), 5)

  capped <- recipes::prep(step_famd(recipes::recipe(~ ., data = df), recipes::all_predictors(), num_comp = 10))
  expect_equal(capped$steps[[1]]$num_comp, 6)
  expect_equal(nrow(generics::tidy(capped, number = 1, type = "coef")), 3 * 6)
})

test_that("step_famd's threshold chooses among all FAMD dimensions", {
  skip_if_no_famd()
  set.seed(1)
  df <- data.frame(x1 = stats::rnorm(30), x2 = stats::rnorm(30), f = factor(sample(letters[1:5], 30, TRUE)))
  full <- FactoMineR::FAMD(df, ncp = 6, graph = FALSE)
  expected <- unname(which(full$eig[, "cumulative percentage of variance"] >= 90)[1])

  prepped <- recipes::prep(step_famd(recipes::recipe(~ ., data = df), recipes::all_predictors(), threshold = 0.9))
  expect_equal(prepped$steps[[1]]$num_comp, expected)
  expect_gt(expected, 3)
  expect_equal(ncol(recipes::bake(prepped, new_data = NULL)), expected)
})

test_that("tidy.step_famd returns variable contributions and component variances", {
  skip_if_no_famd()
  df <- famd_data()
  rec <- step_famd(recipes::recipe(y ~ ., data = df), recipes::all_predictors(), num_comp = 2)
  untrained <- generics::tidy(rec, number = 1)
  expect_true(all(is.na(untrained$value)))

  prepped <- recipes::prep(rec, training = df)
  coef <- generics::tidy(prepped, number = 1, type = "coef")
  expect_named(coef, c("terms", "value", "component", "id"))
  expect_setequal(coef$terms, c("x1", "x2", "x3", "f1", "f2"))
  expect_setequal(coef$component, c("FAMD1", "FAMD2"))
  expect_equal(as.vector(tapply(coef$value, coef$component, sum)), c(100, 100))

  variance <- generics::tidy(prepped, number = 1, type = "variance")
  eig <- prepped$steps[[1]]$res$eig
  expect_equal(
    variance$value[variance$terms == "percent variance"],
    unname(eig[, "percentage of variance"])
  )
  expect_error(generics::tidy(prepped, number = 1, type = "loadings"))
})

test_that("step_famd rejects unmixed selections, missing values, and unseen categories", {
  skip_if_no_famd()
  df <- famd_data()
  numeric_only <- step_famd(recipes::recipe(~ x1 + x2 + x3, data = df), recipes::all_predictors())
  expect_error(recipes::prep(numeric_only), "both numeric and categorical")

  with_na <- df
  with_na$x1[3] <- NA
  expect_error(
    recipes::prep(step_famd(recipes::recipe(y ~ ., data = with_na), recipes::all_predictors())),
    "missing values in: x1"
  )

  prepped <- recipes::prep(step_famd(recipes::recipe(y ~ ., data = df), recipes::all_predictors()))
  unseen <- df[1:2, ]
  unseen$f2 <- c("u", "w")
  expect_error(recipes::bake(prepped, new_data = unseen), "not seen in training in `f2`: w")

  expect_error(step_famd(recipes::recipe(y ~ ., data = df), num_comp = -1), "num_comp")
  expect_error(step_famd(recipes::recipe(y ~ ., data = df), threshold = 1.5), "threshold")
  expect_error(step_famd(recipes::recipe(y ~ ., data = df), options = list(graph = TRUE)), "graph")
})

test_that("step_famd with num_comp = 0 leaves the data unchanged, and keep_original_cols keeps inputs", {
  skip_if_no_famd()
  df <- famd_data()
  none <- recipes::prep(step_famd(recipes::recipe(y ~ ., data = df), recipes::all_predictors(), num_comp = 0))
  expect_named(recipes::bake(none, new_data = NULL), c("x1", "x2", "x3", "f1", "f2", "y"))

  kept <- recipes::prep(step_famd(
    recipes::recipe(y ~ ., data = df), recipes::all_predictors(), num_comp = 2, keep_original_cols = TRUE
  ))
  expect_true(all(c("x1", "f2", "FAMD1", "FAMD2") %in% names(recipes::bake(kept, new_data = NULL))))
})

test_that("step_famd uses frequency weights as FAMD row weights and ignores importance weights", {
  skip_if_no_famd()
  skip_if_not_installed("hardhat")
  df <- famd_data()
  set.seed(9)
  df$w <- hardhat::frequency_weights(sample(1:3, nrow(df), replace = TRUE))
  prepped <- recipes::prep(step_famd(recipes::recipe(y ~ ., data = df), recipes::all_predictors(), num_comp = 2))
  expect_true(prepped$steps[[1]]$case_weights)

  # Like other unsupervised recipe steps, importance weights are not used.
  imp <- famd_data()
  imp$w <- hardhat::importance_weights(stats::runif(nrow(imp), 0.5, 2))
  unweighted <- recipes::prep(step_famd(recipes::recipe(y ~ ., data = imp), recipes::all_predictors(), num_comp = 2))
  expect_false(unweighted$steps[[1]]$case_weights)

  ref <- FactoMineR::FAMD(famd_reference_data(df), ncp = 2, row.w = as.double(df$w), graph = FALSE)
  baked <- recipes::bake(prepped, new_data = NULL)
  expect_equal(unname(as.matrix(baked[c("FAMD1", "FAMD2")])), unname(ref$ind$coord[, 1:2]))
})

test_that("step_famd refuses to overwrite existing columns", {
  skip_if_no_famd()
  df <- famd_data()
  df$FAMD1 <- 1
  rec <- step_famd(recipes::recipe(y ~ ., data = df), x1, x2, x3, f1, f2, num_comp = 2)
  expect_error(recipes::prep(rec), "FAMD1")
})

test_that("step_famd prints its columns and declares tuning parameters and required packages", {
  skip_if_no_famd()
  df <- data.frame(x1 = 1:10, x2 = 10:1, x3 = 5:14, cat1 = factor(rep(c("A", "B"), 5)), cat2 = factor(rep(c("Y", "N"), 5)))
  rec_a <- step_famd(recipes::recipe(~ ., data = df), x1, x2, cat1, num_comp = 2)
  rec_b <- step_famd(recipes::recipe(~ ., data = df), x3, cat2, num_comp = 2)
  msg_a <- testthat::capture_messages(print(recipes::prep(rec_a, training = df)$steps[[1]]))
  msg_b <- testthat::capture_messages(print(recipes::prep(rec_b, training = df)$steps[[1]]))
  expect_match(paste(msg_a, collapse = " "), "x1")
  expect_match(paste(msg_b, collapse = " "), "cat2")

  expect_equal(generics::tunable(rec_a$steps[[1]])$name, c("num_comp", "threshold"))
  expect_setequal(generics::required_pkgs(rec_a$steps[[1]]), c("FactoMineR", "TempleCBE"))
})
