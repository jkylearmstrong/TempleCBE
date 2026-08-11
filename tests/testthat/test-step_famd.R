test_that("step_famd works as expected in recipes pipeline", {
  skip_if_not_installed("recipes")
  library(recipes)

  df <- data.frame(
    x1 = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    x2 = c(10, 9, 8, 7, 6, 5, 4, 3, 2, 1),
    cat1 = factor(c("A", "A", "B", "B", "A", "B", "A", "B", "A", "B")),
    y = c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0)
  )

  rec <- recipe(y ~ ., data = df) %>%
    step_famd(x1, x2, cat1, num_comp = 2)

  expect_s3_class(rec, "recipe")

  prepped <- prep(rec, training = df)
  expect_s3_class(prepped, "recipe")

  baked <- bake(prepped, new_data = NULL)
  expect_true("PC1" %in% names(baked))
  expect_true("PC2" %in% names(baked))

  # Test tidy S3 method
  tidied <- tidy(prepped, number = 1)
  expect_s3_class(tidied, "tbl_df")
  expect_true("terms" %in% names(tidied))

  # Test tunable S3 method
  tunable_df <- tunable(rec$steps[[1]])
  expect_s3_class(tunable_df, "tbl_df")
  expect_true("num_comp" %in% tunable_df$name)

  # Test print S3 method (recipes' print_step() reports via message(), not stdout)
  expect_message(print(rec$steps[[1]]))
})

test_that("step_famd honors num_comp beyond FactoMineR's ncp = 5 default", {
  skip_if_not_installed("recipes")
  skip_if_not_installed("FactoMineR")
  library(recipes)

  # Regression test: a prior bug never passed `ncp` to FactoMineR::FAMD(),
  # so it silently capped every fit at FactoMineR's default of 5 components.
  set.seed(2026)
  df <- as.data.frame(matrix(rnorm(40 * 5), nrow = 40, dimnames = list(NULL, paste0("x", 1:5))))
  df$cat1 <- factor(sample(c("A", "B", "C"), 40, replace = TRUE))

  rec <- recipe(~., data = df) %>%
    step_famd(x1, x2, x3, x4, x5, cat1, num_comp = 6)
  prepped <- prep(rec, training = df)
  baked <- bake(prepped, new_data = NULL)

  expect_true(all(paste0("PC", 1:6) %in% names(baked)))
})

test_that("step_famd selects num_comp via threshold, overriding a larger requested num_comp", {
  skip_if_not_installed("recipes")
  skip_if_not_installed("FactoMineR")
  library(recipes)

  # Regression test: `threshold` was documented and tunable but never
  # actually used to pick the number of components.
  set.seed(2026)
  df <- as.data.frame(matrix(rnorm(40 * 5), nrow = 40, dimnames = list(NULL, paste0("x", 1:5))))
  df$cat1 <- factor(sample(c("A", "B", "C"), 40, replace = TRUE))

  rec <- recipe(~., data = df) %>%
    step_famd(x1, x2, x3, x4, x5, cat1, num_comp = 6, threshold = 0.4)
  prepped <- prep(rec, training = df)

  actual_num_comp <- prepped$steps[[1]]$num_comp
  expect_true(actual_num_comp >= 1 && actual_num_comp < 6)

  baked <- bake(prepped, new_data = NULL)
  expect_equal(sum(grepl("^PC", names(baked))), actual_num_comp)
})

test_that("print.step_famd's output depends on which columns were selected", {
  skip_if_not_installed("recipes")
  library(recipes)

  # Regression test: a prior bug did `names(x$columns)` on a plain character
  # vector (always NULL), so every printed step read identically ("FAMD
  # extraction with " and nothing else) regardless of which columns were
  # actually selected.
  df <- data.frame(
    x1 = 1:10, x2 = 10:1, x3 = 5:14,
    cat1 = factor(rep(c("A", "B"), 5)),
    cat2 = factor(rep(c("Y", "N"), 5))
  )

  rec_a <- recipe(~., data = df) %>% step_famd(x1, x2, cat1, num_comp = 2)
  rec_b <- recipe(~., data = df) %>% step_famd(x3, cat2, num_comp = 2)

  msg_a <- testthat::capture_messages(print(prep(rec_a, training = df)$steps[[1]]))
  msg_b <- testthat::capture_messages(print(prep(rec_b, training = df)$steps[[1]]))

  expect_match(paste(msg_a, collapse = " "), "x1")
  expect_match(paste(msg_b, collapse = " "), "cat2")
  expect_false(identical(msg_a, msg_b))
})

test_that("tidy.step_famd returns real per-component loadings, not fabricated placeholders", {
  skip_if_not_installed("recipes")
  skip_if_not_installed("FactoMineR")
  library(recipes)

  # Regression test: a prior bug hardcoded value = 1.0 and component = "PC1"
  # for every term, regardless of the actual FAMD contributions.
  df <- data.frame(
    x1 = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    x2 = c(10, 9, 8, 7, 6, 5, 4, 3, 2, 1),
    cat1 = factor(c("A", "A", "B", "B", "A", "B", "A", "B", "A", "B"))
  )
  rec <- recipe(~., data = df) %>% step_famd(x1, x2, cat1, num_comp = 2)
  prepped <- prep(rec, training = df)

  tidied <- tidy(prepped, number = 1)
  expect_setequal(tidied$terms, c("x1", "x2", "cat1"))
  expect_true(all(c("PC1", "PC2") %in% tidied$component))
  expect_gt(length(unique(tidied$value)), 1)
})

test_that("bake.step_famd errors informatively if FactoMineR becomes unavailable after prep", {
  skip_if_not_installed("recipes")
  skip_if_not_installed("FactoMineR")
  library(recipes)

  # Regression test: a prior bug silently returned new_data unchanged (no
  # PCs, no warning) in this scenario instead of failing loudly.
  df <- data.frame(x1 = 1:10, x2 = 10:1, cat1 = factor(rep(c("A", "B"), 5)))
  rec <- recipe(~., data = df) %>% step_famd(x1, x2, cat1, num_comp = 2)
  prepped <- prep(rec, training = df)

  testthat::local_mocked_bindings(famd_available = function() FALSE)
  expect_error(bake(prepped, new_data = df), "FactoMineR")
})
