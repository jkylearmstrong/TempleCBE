test_that("simulate_section_data generates valid cohort data", {
  map <- demo_cbe_mapping()

  sim <- simulate_section_data(map, index = "Demo", n_subjects = 10, seed = 42)
  expect_s3_class(sim, "tbl_df")
  expect_equal(names(sim), c("subject_id", "time_point", "map_mean", "status"))
  # 10 subjects x 4 default time points = 40 rows
  expect_equal(nrow(sim), 40)
  expect_false(anyNA(sim$subject_id))
  expect_false(anyNA(sim$time_point))
})

test_that("simulate_section_data injects missingness at specified rate", {
  map <- demo_cbe_mapping()

  sim_na <- simulate_section_data(map, index = "Demo", n_subjects = 100, seed = 123, missing_rate = 0.25)
  expect_false(anyNA(sim_na$subject_id))
  expect_false(anyNA(sim_na$time_point))
  expect_true(anyNA(sim_na$map_mean))
})

test_that("simulate_section_data errors on invalid section index", {
  map <- demo_cbe_mapping()
  expect_error(simulate_section_data(map, index = "NonExistent"), "No rows in `mapping`")
})
