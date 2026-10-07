test_that("tumor_wide() returns the expected 45-subject SAS benchmark data", {
  w <- tumor_wide()
  expect_s3_class(w, "tbl_df")
  expect_equal(nrow(w), 45)
  expect_equal(ncol(w), 19)

  # Check columns
  expect_named(w, c("ID", "Time", "Dead", "Dose", paste0("P", 1:15)))

  # Check event and dose distribution matching SAS documentation
  expect_equal(sum(w$Dead == 1), 25)
  expect_equal(sum(w$Dead == 0), 20)
  expect_setequal(unique(w$Dose), c(1.0, 2.5, 10.0))
  expect_equal(sort(unique(w$ID)), 1:45)
})

test_that("tumor_long() transforms wide data to counting process format matching SAS Example 85.7", {
  l <- tumor_long()
  expect_s3_class(l, "tbl_df")
  expect_equal(nrow(l), 102)
  expect_named(l, c("ID", "Time", "Dead", "Dose", "T1", "T2", "NPap", "Status"))

  # Total events and censored intervals
  expect_equal(sum(l$Status == 1), 25)
  expect_equal(sum(l$Status == 0), 77)

  # Verify Subject 1 explicitly against SAS published values
  s1 <- l[l$ID == 1, ]
  expect_equal(nrow(s1), 5)
  expect_equal(s1$T1, c(0, 27, 34, 37, 41))
  expect_equal(s1$T2, c(27, 34, 37, 41, 47))
  expect_equal(s1$NPap, c(0, 5, 6, 8, 10))
  expect_equal(s1$Status, c(0, 0, 0, 0, 1))

  # Invariant checks across all 45 subjects
  for (id in 1:45) {
    subj <- l[l$ID == id, ]
    n_subj <- nrow(subj)

    # First interval starts at 0, last ends at Time
    expect_equal(subj$T1[1], 0)
    expect_equal(subj$T2[n_subj], subj$Time[1])

    # All intervals are positive length
    expect_true(all(subj$T2 > subj$T1))

    # Intervals are contiguous
    if (n_subj > 1) {
      expect_equal(subj$T1[2:n_subj], subj$T2[1:(n_subj - 1)])
      # NPap is non-decreasing over time
      expect_true(all(diff(subj$NPap) >= 0))
      # Status is 0 on intermediate intervals
      expect_true(all(subj$Status[1:(n_subj - 1)] == 0))
    }

    # Status on final row matches Dead
    expect_equal(subj$Status[n_subj], subj$Dead[1])
  }
})

test_that("cbe_cox_multi reproduces SAS PROC PHREG estimates on counting-process tumor data", {
  l <- tumor_long()

  fit <- cbe_cox_multi(
    data = l,
    formula = survival::Surv(T1, T2, Status) ~ Dose + NPap,
    id = ID,
    ties = "breslow"
  )

  expect_s3_class(fit, "cbe_cox_multi")
  expect_true(fit$converged)

  # Check coefficients against SAS/STAT Example 85.7 published output:
  # Dose: coef = 0.06885, StdErr = 0.05620, p = 0.2205, HR = 1.071
  # NPap: coef = 0.11714, StdErr = 0.02998, p < .0001, HR = 1.124
  coefs <- stats::coef(fit$model)
  ses <- sqrt(diag(stats::vcov(fit$model)))

  expect_equal(unname(coefs["Dose"]), 0.06885, tolerance = 1e-4)
  expect_equal(unname(coefs["NPap"]), 0.11715, tolerance = 1e-4)

  expect_equal(unname(ses["Dose"]), 0.05620, tolerance = 1e-4)
  expect_equal(unname(ses["NPap"]), 0.02998, tolerance = 1e-4)

  # Check exponentiated hazard ratios
  hrs <- exp(coefs)
  expect_equal(unname(hrs["Dose"]), 1.071, tolerance = 1e-3)
  expect_equal(unname(hrs["NPap"]), 1.124, tolerance = 1e-3)

  # Proportional hazards check executes without failure
  chk <- fit$zph
  expect_s3_class(chk, "cbe_cox_check")
})
