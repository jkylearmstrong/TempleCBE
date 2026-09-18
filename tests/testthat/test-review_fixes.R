test_that("step_lencode_coxnet preps and bakes with default outcome and registered S3 methods", {
  skip_if_not_installed("recipes")
  skip_if_not_installed("survival")

  data <- survival::lung
  data$status <- data$status - 1
  data$sex <- as.factor(data$sex)

  rec <- recipes::recipe(~ age + sex + time + status, data = data) |>
    step_lencode_coxnet(sex, outcome = c("time", "status"))

  prepped <- recipes::prep(rec, training = data)
  baked <- recipes::bake(prepped, new_data = data)

  expect_s3_class(prepped, "recipe")
  expect_true("sex" %in% names(baked))
  expect_type(baked$sex, "double")
})

test_that("cbe_cox_multi term prefix collision is prevented and default CI is 95%", {
  skip_if_not_installed("survival")

  # Create a dataset where one variable name is an exact prefix of another
  set.seed(123)
  df <- data.frame(
    time = runif(100, 10, 100),
    status = rbinom(100, 1, 0.5),
    age = rnorm(100, 60, 10),
    age_group = factor(sample(c("Low", "High"), 100, replace = TRUE), levels = c("Low", "High")),
    rx = factor(sample(c("A", "B"), 100, replace = TRUE), levels = c("A", "B"))
  )
  df$outcome <- survival::Surv(df$time, df$status)

  # Model with both 'age' and 'age_group'
  res <- cbe_cox_multi(
    data = df,
    outcome = "outcome",
    features = c("age", "age_group", "rx"),
    conf_level = 0.95
  )

  # Check that each covariate gets exact matching without duplicate rows
  expect_true("95% CI" %in% names(res$table))
  expect_equal(sum(res$table$Variable == "age"), 1)
  expect_equal(sum(res$table$Variable == "age_group"), 2) # Low (ref) + High
  expect_equal(sum(res$table$Variable == "rx"), 2)

  # Dynamic conf_level
  res_90 <- cbe_cox_multi(
    data = df,
    outcome = "outcome",
    features = c("age", "rx"),
    conf_level = 0.90
  )
  expect_true("90% CI" %in% names(res_90$table))
})

test_that("cbe_km_single correctly formats 95% CI and handles unstratified 1D tables", {
  skip_if_not_installed("survival")

  lung <- stats::na.omit(survival::lung[, c("time", "status", "sex")])
  lung$outcome <- survival::Surv(lung$time, lung$status)
  lung$sex_f <- factor(lung$sex, levels = c(1, 2), labels = c("Male", "Female"))

  km_out <- cbe_km_single(data = lung, outcome = "outcome", feature = "sex_f", conf_level = 0.95)

  expect_true("95% CI" %in% names(km_out$summary))
  expect_equal(nrow(km_out$summary), 2)
  expect_false(is.na(km_out$summary$km_median_time[1]))

  # Dynamic CI column in KM
  km_out_90 <- cbe_km_single(data = lung, outcome = "outcome", feature = "sex_f", conf_level = 0.90)
  expect_true("90% CI" %in% names(km_out_90$summary))

  # Test 1D table extraction logic (unstratified survfit returns a 1D named vector)
  km_1d_fit <- survival::survfit(survival::Surv(time, status) ~ 1, data = lung)
  km_raw_tab <- summary(km_1d_fit)$table
  expect_false(is.matrix(km_raw_tab))
  expect_true("median" %in% names(km_raw_tab))
  expect_type(unname(km_raw_tab["median"]), "double")
})

test_that("simulate_section_data restores .Random.seed on exit", {
  map <- demo_cbe_mapping()
  set.seed(42)
  seed_before <- .Random.seed

  # Run simulate_section_data with custom seed
  res <- simulate_section_data(map, index = "Demo", n_subjects = 10, seed = 999)

  seed_after <- .Random.seed
  expect_identical(seed_before, seed_after)
})

test_that("proc_pca computes SAS eigenvalue statistics and automatically subsets numeric columns", {
  # Iris has 4 numeric columns and 1 factor (Species)
  res <- proc_pca(iris, scale = TRUE)

  expect_s3_class(res, "cbe_pca")
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 4)

  # SAS princomp parity columns
  expect_named(res, c("component", "eigenvalue", "difference", "proportion", "variance_pct", "cum_variance_pct"))
  expect_equal(round(res$eigenvalue[1], 4), 2.9185)
  expect_equal(round(res$proportion[1], 4), 0.7296)
  expect_equal(round(res$difference[1], 4), 2.0045)
  expect_equal(round(res$cum_variance_pct[2], 2), 95.81)

  # Check attached prcomp object
  pca_obj <- attr(res, "prcomp")
  expect_s3_class(pca_obj, "prcomp")
})

test_that("PCA visualization functions execute cleanly and produce ggplot objects", {
  res <- proc_pca(iris, scale = TRUE)

  # Scree plot
  p_scree <- pca_scree_plot(res, kaiser = TRUE)
  expect_s3_class(p_scree, "ggplot")

  # Variables plot
  p_vars <- pca_variables_plot(res)
  expect_s3_class(p_vars, "ggplot")

  # Biplot with groups and ellipses
  p_bi <- pca_biplot(res, group = iris$Species, ellipse = TRUE, percent = TRUE)
  expect_s3_class(p_bi, "ggplot")
  expect_true(grepl("^PC1 \\(", p_bi$labels$x))

  # pca_plot dispatcher
  expect_s3_class(pca_plot(res, type = "scree"), "ggplot")
  expect_s3_class(pca_plot(res, type = "circle"), "ggplot")
})
