test_that("tidy_tmerge_cox builds correct start-stop intervals with baseline covariates", {
  measure_df <- tibble::tibble(
    subject_id = c(1, 1, 1, 2, 2),
    time = c(0, 20, 40, 0, 30),
    biomarker = c(1.2, 1.5, 1.8, 0.9, 1.1)
  )

  event_df <- tibble::tibble(
    subject_id = c(1, 2),
    event_time = c(50, 60),
    event_type = c("Death", "Censored")
  )

  baseline_df <- tibble::tibble(
    subject_id = c(1, 2),
    age = c(55, 62),
    trt = c("Drug", "Placebo")
  )

  res <- tidy_tmerge_cox(
    measure_df = measure_df,
    event_df = event_df,
    baseline_df = baseline_df
  )

  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 5)
  expect_named(res, c(
    "subject_id", "time", "biomarker", "event_time", "event_type",
    "age", "trt", "tstart", "tstop", "event", "event_label"
  ))

  # Subject 1 check
  s1 <- res[res$subject_id == 1, ]
  expect_equal(s1$tstart, c(0, 20, 40))
  expect_equal(s1$tstop, c(20, 40, 50))
  expect_equal(s1$event, c(0, 0, 1))
  expect_equal(s1$event_label, c(NA_character_, NA_character_, "Death"))
  expect_equal(unique(s1$age), 55)
  expect_equal(unique(s1$trt), "Drug")

  # Subject 2 check (censored at 60)
  s2 <- res[res$subject_id == 2, ]
  expect_equal(s2$tstart, c(0, 30))
  expect_equal(s2$tstop, c(30, 60))
  expect_equal(s2$event, c(0, 1))
  expect_equal(s2$event_label, c(NA_character_, "Censored"))
})

test_that("tidy_tmerge_cox handles post_event exclude and include modes", {
  measure_df <- tibble::tibble(
    subject_id = c(1, 1, 1),
    time = c(0, 20, 60), # time 60 is post-event (event_time is 50)
    val = c(10, 20, 30)
  )

  event_df <- tibble::tibble(
    subject_id = 1,
    event_time = 50,
    event_type = "Relapse"
  )

  # Exclude mode drops measurements where tstart >= event_time
  res_ex <- tidy_tmerge_cox(
    measure_df = measure_df,
    event_df = event_df,
    post_event = "exclude"
  )
  expect_equal(nrow(res_ex), 2)
  expect_equal(res_ex$tstart, c(0, 20))

  # Include mode pushes event_time forward
  res_in <- tidy_tmerge_cox(
    measure_df = measure_df,
    event_df = event_df,
    post_event = "include"
  )
  expect_equal(nrow(res_in), 3)
  expect_equal(max(res_in$tstop), 60)
})
