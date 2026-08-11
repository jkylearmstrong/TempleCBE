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

  # Test print S3 method
  expect_output(print(rec$steps[[1]]))
})
