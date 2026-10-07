test_that("cbe_variable_roles classifies single table correctly", {
  df <- data.frame(
    patient_id = 1:5,
    visit = c(1, 2, 1, 2, 3),
    age = c(50, 50, 60, 60, 60),
    sbp = c(120, 125, 140, 138, 142),
    death = c(0, 0, 1, 1, 1)
  )

  roles <- cbe_variable_roles(
    df,
    id = "patient_id",
    time = "visit",
    outcome = "death",
    predictors = c("age", "sbp")
  )

  expect_s3_class(roles, "data.frame")
  expect_equal(nrow(roles), 5)
  expect_true(roles$ID_var[roles$columns == "patient_id"])
  expect_true(roles$Time_var[roles$columns == "visit"])
  expect_true(roles$Y_var[roles$columns == "death"])
  expect_true(roles$X_var[roles$columns == "sbp"])

  # Test role extractors
  expect_equal(cbe_get_predictors(roles), c("age", "sbp"))
  expect_equal(cbe_get_outcomes(roles), "death")
  expect_equal(cbe_get_id_cols(roles), "patient_id")
  expect_equal(cbe_get_time_cols(roles), "visit")
})

test_that("cbe_variable_roles handles multi-table databases", {
  db <- list(
    inputs = data.frame(id = 1:3, age = c(40, 50, 60)),
    outcomes = data.frame(id = 1:3, death = c(0, 1, 0))
  )

  roles_db <- cbe_variable_roles(
    db,
    id = "id",
    outcome = list(outcomes = "death"),
    predictors = list(inputs = "age")
  )

  expect_equal(sort(unique(roles_db$dataset_name)), c("inputs", "outcomes"))
  expect_true(roles_db$X_var[roles_db$dataset_name == "inputs" & roles_db$columns == "age"])
  expect_true(roles_db$Y_var[roles_db$dataset_name == "outcomes" & roles_db$columns == "death"])
})
