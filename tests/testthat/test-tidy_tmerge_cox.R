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
  expect_equal(res_ex$tstop, c(20, 50))
  expect_equal(res_ex$event, c(0, 1))
  expect_equal(res_ex$event_label, c(NA_character_, "Relapse"))

  # Include mode pushes event_time forward to the last measurement, 60: that
  # measurement ends the interval (20, 60] and starts none of its own, so there
  # is no zero-length interval (60, 60] and the event is flagged once.
  expect_no_warning(
    res_in <- tidy_tmerge_cox(
      measure_df = measure_df,
      event_df = event_df,
      post_event = "include"
    )
  )
  expect_equal(nrow(res_in), 2)
  expect_equal(res_in$tstart, c(0, 20))
  expect_equal(res_in$tstop, c(20, 60))
  expect_equal(res_in$event, c(0, 1))
  expect_equal(res_in$event_label, c(NA_character_, "Relapse"))
})

test_that("tidy_tmerge_cox with post_event = 'include' never makes a zero-length interval or flags an event twice", {
  measure_df <- data.frame(
    subject_id = c(1, 1, 1, 2, 2, 3, 3, 4, 4, 4, 5, 5, 5),
    time = c(0, 3, 10, 0, 4, 0, 5, 0, 3, 10, 0, 5, 5),
    val = seq_len(13)
  )
  event_df <- data.frame(
    subject_id = 1:5,
    # 1: event inside the measurements (moves to 10); 2: at the last measurement;
    # 3: after the last measurement; 4: censoring inside the measurements;
    # 5: before a last measurement time that is repeated.
    event_time = c(8, 4, 9, 8, 3),
    event_type = c("Death", "Death", "Death", "Censored", "Death")
  )

  expect_no_warning(
    res <- tidy_tmerge_cox(measure_df, event_df, post_event = "include", censor_types = "Censored")
  )
  expect_true(all(res$tstop > res$tstart))
  expect_equal(as.vector(tapply(res$event, res$subject_id, sum)), c(1, 1, 1, 0, 1))
  expect_equal(res$tstart, c(0, 3, 0, 0, 5, 0, 3, 0))
  # The event time of subjects 1, 4 and 5 moved to their last measurement time.
  expect_equal(res$tstop, c(3, 10, 4, 5, 9, 3, 10, 5))
  expect_equal(res$event, c(0, 1, 1, 0, 1, 0, 0, 1))

  # Default "exclude" keeps the recorded event time instead (here the data are
  # otherwise the same).
  res_ex <- tidy_tmerge_cox(measure_df, event_df, censor_types = "Censored")
  expect_equal(res_ex$tstop[res_ex$subject_id == 1], c(3, 8))
})

test_that("tidy_tmerge_cox warns about subjects without an event time and subjects left without intervals", {
  measure_df <- data.frame(
    subject_id = c(1, 1, 1, 2, 2, 3, 4, 5, 5, 6, 6),
    time = c(0, 3, 6, 0, 4, 0, 2, 5, 9, 0, 2),
    bp = seq_len(11)
  )
  event_df <- data.frame(
    subject_id = c(1, 3, 5, 6, 7),
    # 5: event before the first measurement; 6: row without an event time;
    # 7: has no measurements.
    event_time = c(8, 5, 3, NA, 9),
    event_type = "Death"
  )

  w <- collect_warnings(res <- tidy_tmerge_cox(measure_df, event_df))
  expect_length(w$warnings, 2)
  # 2 and 4 are not in event_df and 6 has no event time: their last measurement
  # ends no interval.
  expect_match(w$warnings[1], "3 subject\\(s\\) in `measure_df` have no `event_time` in `event_df`")
  expect_match(w$warnings[1], "subject\\(s\\): 2, 4, 6\\.")
  # 4 has a single measurement and no event time, and the event time of 5
  # precedes both of its measurements: neither has a row left.
  expect_match(w$warnings[2], "2 subject\\(s\\) in `measure_df` have no interval in the result")
  expect_match(w$warnings[2], "subject\\(s\\): 4, 5\\.")

  # What the warnings describe: 2 and 6 keep the interval before their last
  # measurement, with no event, and lose that last measurement.
  expect_equal(sort(unique(res$subject_id)), c(1, 2, 3, 6))
  expect_equal(res$tstop[res$subject_id %in% c(2, 6)], c(4, 2))
  expect_equal(res$event[res$subject_id %in% c(2, 6)], c(0, 0))

  # Nothing to say when every subject has an event time and a measurement before it.
  ok <- measure_df[measure_df$subject_id %in% c(1, 3), ]
  expect_no_warning(tidy_tmerge_cox(ok, event_df[event_df$subject_id %in% c(1, 3), ]))
})

test_that("tidy_tmerge_cox stops when event_df or baseline_df has more than one row for a subject", {
  measure_df <- data.frame(subject_id = c(1, 1, 2, 2), time = c(0, 3, 0, 4))
  event_df <- data.frame(
    subject_id = c(1, 1, 2),
    event_time = c(8, 9, 7),
    event_type = "Death"
  )

  # With the join as it was, the two rows of subject 1 gave four rows with
  # zero-length intervals and no event.
  expect_error(
    tidy_tmerge_cox(measure_df, event_df),
    "`event_df` must have one row per subject.*subject\\(s\\): 1\\."
  )

  event_ok <- event_df[c(1, 3), ]
  baseline_df <- data.frame(subject_id = c(1, 2, 2, 3, 3), age = c(50, 60, 61, 70, 71))
  expect_error(
    tidy_tmerge_cox(measure_df, event_ok, baseline_df = baseline_df),
    "`baseline_df` must have one row per subject.*subject\\(s\\): 2, 3\\."
  )

  res <- tidy_tmerge_cox(measure_df, event_ok, baseline_df = baseline_df[c(1, 2, 4), ])
  expect_equal(nrow(res), 4)
  expect_equal(res$age, c(50, 50, 60, 60))
})

test_that("tidy_tmerge_cox's censor_types marks censoring times as non-events", {
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

  # Default: every subject with an event_time has event = 1 there, whatever its type.
  default <- tidy_tmerge_cox(measure_df, event_df)
  expect_equal(default$event[default$subject_id == 2], c(0, 1))

  res <- tidy_tmerge_cox(measure_df, event_df, censor_types = "Censored")
  s2 <- res[res$subject_id == 2, ]
  expect_equal(s2$tstop, c(30, 60))
  expect_equal(s2$event, c(0, 0))
  expect_equal(s2$event_label, c(NA_character_, NA_character_))
  s1 <- res[res$subject_id == 1, ]
  expect_equal(s1$event, c(0, 0, 1))
  expect_equal(s1$event_label[3], "Death")
  expect_equal(sum(res$event), 1)
})

test_that("tidy_tmerge_cox warns about zero-length intervals from repeated measurement times", {
  measure_df <- tibble::tibble(subject_id = c(1, 1, 1), time = c(0, 20, 20), val = c(1, 2, 3))
  event_df <- tibble::tibble(subject_id = 1, event_time = 40, event_type = "Death")
  expect_warning(res <- tidy_tmerge_cox(measure_df, event_df), "1 interval\\(s\\).*subject\\(s\\): 1")
  expect_equal(res$tstop - res$tstart, c(20, 0, 20))
})
