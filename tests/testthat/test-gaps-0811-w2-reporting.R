# Draft Part 2 reporting/docs features (wave 2, items A4-A8, B6).
#
# A4: predict() -> calibration/decision-curve adapters + Fine-Gray CIF
#     prediction (R/predict_calibration.R, R/fine_gray_predict.R).
# A5: review-tracker apply-back step (R/cbe_review_extract.R).
# A6/A7: README vignette rows + DESCRIPTION scope (docs pins).
# A8: visualize_pipeline_interactive() tests (R/compute_graph.R).
# B6: docs-only journey (README + compute-graph vignette Pattern 6); no
#     wrapper was added, so there is no B6 code test here.
#
# NOTE on S3 dispatch: predict.fine_gray_model() and
# print.fine_gray_model() / print.review_apply() are registered in NAMESPACE
# (roxygen2 run by the coordinator), so the tests below use the generics
# `predict()` / `print()` and exercise dispatch. predict_event_risk()
# is deliberately a plain function dispatching on the model class.

# A4 fixtures -----------------------------------------------------------------

w2_surv_data <- function(seed = 1, n = 120) {
  set.seed(seed)
  data.frame(
    time = stats::rexp(n, 0.05), status = stats::rbinom(n, 1, 0.6),
    age = stats::rnorm(n, 60, 10), sex = stats::rbinom(n, 1, 0.5)
  )
}

w2_competing_data <- function(seed = 1, n = 150) {
  set.seed(seed)
  data.frame(
    time = stats::rexp(n, 0.1),
    type = sample(0:2, n, TRUE, prob = c(0.3, 0.4, 0.3)),
    age = stats::rnorm(n, 60, 8), arm = rep(c("A", "B"), length.out = n)
  )
}

w2_two_chain_graph <- function(dir) {
  origin <- as.POSIXct("2020-01-01 00:00:00", tz = "UTC")
  mk <- function(name, seconds) {
    path <- file.path(dir, name)
    writeLines("x", path)
    stopifnot(Sys.setFileTime(path, origin + seconds))
    path
  }
  a <- FilePath("a", mk("a.csv", 1), stage = "s1")
  rb <- FileOutputs(
    "rb", mk("b.qmd", 1),
    dependencies = list(a),
    output = list(FilePath("b_out", file.path(dir, "b.pdf"), stage = "s1")),
    stage = "s1"
  )
  list(a, rb)
}

# A4: predict() -> risk adapter ------------------------------------------------

test_that("extract_horizon_risk() takes one horizon's event risk from predict() output (A4)", {
  skip_if_not_installed("glmnet")
  d <- w2_surv_data()
  fit <- coxnet(survival::Surv(time, status) ~ age + sex, data = d, penalty = 0.01)
  pr <- predict(fit, d[1:5, ], type = "survival", eval_time = c(5, 10, 20))

  risk <- extract_horizon_risk(pr, 10)
  expect_length(risk, 5L)
  expect_true(all(risk >= 0 & risk <= 1))
  # one minus the survival probability at that horizon, per subject
  expect_equal(risk, 1 - vapply(pr$.pred, function(cv) cv$.pred_survival[2], numeric(1)))
  # a different horizon gives a different (larger) risk
  expect_true(all(extract_horizon_risk(pr, 20) >= risk))

  expect_error(extract_horizon_risk(pr, 7), "not one of the predicted")
  expect_error(extract_horizon_risk(pr, c(5, 10)), "single")
  expect_error(extract_horizon_risk(d[1:5, ], 10), "neither a `.pred`, a `.pred_survival`")
})

test_that("predict_event_risk() works for coxnet, cv_coxnet and joint_model fits (A4)", {
  skip_if_not_installed("glmnet")
  d <- w2_surv_data()
  fit <- coxnet(survival::Surv(time, status) ~ age + sex, data = d, penalty = 0.01)

  risk <- predict_event_risk(fit, d[1:5, ], horizon = 10)
  pr <- predict(fit, d[1:5, ], type = "survival", eval_time = 10)
  expect_equal(risk, extract_horizon_risk(pr, 10))
  expect_error(predict_event_risk(list(), d[1:5, ], 10), "No `predict_event_risk\\(\\)` method")

  # cv_coxnet: refit on all the data at lambda.min by default
  skip_if_not_installed("rsample")
  skip_if_not_installed("yardstick")
  set.seed(2)
  dcv_status <- rep(0, 120)
  dcv_status[seq(3, 120, 3)] <- stats::rbinom(40, 1, 0.4) # events only in the last interval
  dcv <- data.frame(
    subject_id = rep(1:40, each = 3),
    tstart = rep(c(0, 2, 4), 40),
    tstop = rep(c(2, 4, 7), 40),
    status = dcv_status,
    x1 = stats::rnorm(120), x2 = stats::rnorm(120)
  )
  cv <- cv_coxnet(
    survival::Surv(tstart, tstop, status) ~ x1 + x2,
    data = dcv, subject_id = "subject_id", mixture = 1, v = 3,
    eval_time = c(3, 7), nlambda = 8, cox.ties = "breslow"
  )
  # one row per subject (the last interval) for prediction and validation
  dcv_last <- dcv[seq(3, 120, 3), ]
  cv_risk <- predict_event_risk(cv, dcv_last[1:3, ], horizon = 7)
  expect_length(cv_risk, 3L)
  expect_true(all(cv_risk >= 0 & cv_risk <= 1))
  expect_true(all(cv_risk > 0)) # events happen at time 7, so the risk by 7 is positive
  cv_pr <- predict(cv, dcv_last[1:3, ], type = "survival", eval_time = 7)
  expect_equal(cv_risk, extract_horizon_risk(cv_pr, 7))
  expect_equal(
    predict_event_risk(cv, dcv_last[1:3, ], horizon = 7, penalty = "lambda.1se"),
    extract_horizon_risk(predict(cv, dcv_last[1:3, ], type = "survival", eval_time = 7, penalty = "lambda.1se"), 7)
  )

  # joint_model: the .pred_survival list-column, not .pred
  skip_if_not_installed("parsnip")
  set.seed(123)
  dj <- data.frame(
    time = stats::rexp(50, rate = 0.1) + 0.1, status = stats::rbinom(50, 1, 0.6),
    x1 = stats::rnorm(50), x2 = stats::rnorm(50)
  )
  jf <- joint_model(dj, survival::Surv(time, status) ~ x1 + x2, mixture = 1, penalty = 0.05,
                    calibration = FALSE)
  j_risk <- predict_event_risk(jf, dj[1:5, ], horizon = 5)
  expect_length(j_risk, 5L)
  expect_true(all(j_risk >= 0 & j_risk <= 1))
  j_pr <- stats::predict(jf, new_data = dj[1:5, ], eval_time = 5, type = "survival")
  expect_equal(j_risk, extract_horizon_risk(j_pr, 5))
})

test_that("calibration_from_model() / decision_curve_from_model() validate a fitted model (A4)", {
  skip_if_not_installed("glmnet")
  d <- w2_surv_data()
  fit <- coxnet(survival::Surv(time, status) ~ age + sex, data = d, penalty = 0.01)

  ct <- calibration_from_model(fit, d, "time", "status", horizon = 10, bins = 5)
  expect_s3_class(ct, "tbl_df")
  expect_equal(nrow(ct), 5L)
  expect_true(all(c("model", "bin", "n", "mean_pred", "observed", "conf_low", "conf_high") %in% names(ct)))
  expect_equal(attr(ct, "horizon"), 10)
  expect_equal(attr(ct, "metrics")$n, nrow(d))

  dc <- decision_curve_from_model(fit, d, "time", "status", horizon = 10)
  expect_s3_class(dc, "decision_curve")
  expect_true(all(c("model", "threshold", "net_benefit", "treat_all", "prop_above") %in% names(dc)))
  expect_equal(attr(dc, "horizon"), 10)
  expect_true(all(dc$threshold > 0 & dc$threshold < 1))

  expect_error(calibration_from_model(fit, d, "missing", "status", horizon = 10), "not a column")
  expect_error(calibration_from_model(fit, d, "time", "status", horizon = -1), "single positive")
})

# A4: Fine-Gray CIF prediction --------------------------------------------------

test_that("fine_gray() fits and predicts the CIF, matching cmprsk::predict.crr (A4)", {
  skip_if_not_installed("cmprsk")
  d <- w2_competing_data()
  fit <- fine_gray(d, "time", "type", ~ age + arm)
  expect_s3_class(fit, "fine_gray_model")
  expect_equal(fit$n, nrow(d))
  expect_equal(fit$n_event, sum(d$type == 1))
  expect_equal(names(fit$fit$coef), c("age", "armB"))

  # one-shot prediction agrees with fit + predict, at requested times
  pf <- predict(fit, d[1:3, ], times = c(5, 10))
  expect_s3_class(pf, "tbl_df")
  expect_equal(nrow(pf), 3L)
  expect_true(all(vapply(pf$.pred, function(cv) all(cv$.eval_time == c(5, 10)), logical(1))))
  expect_true(all(vapply(pf$.pred, function(cv) all(cv$.pred_cif >= 0 & cv$.pred_cif <= 1), logical(1))))
  ps <- predict_fine_gray(d, "time", "type", ~ age + arm, new_data = d[1:3, ], times = c(5, 10))
  expect_equal(ps, pf)

  # against cmprsk itself: the CIF at the training event times
  mm <- stats::model.matrix(~ age + arm, data = d)[, -1, drop = FALSE]
  crr_fit <- cmprsk::crr(ftime = d$time, fstatus = d$type, cov1 = mm, failcode = 1, cencode = 0)
  pr <- cmprsk::predict.crr(crr_fit, cov1 = mm[1:3, , drop = FALSE])
  pall <- predict(fit, d[1:3, ])
  expect_equal(pall$.pred[[1]]$.eval_time, unname(pr[, 1]))
  expect_equal(
    vapply(pall$.pred, function(cv) cv$.pred_cif, numeric(nrow(pr))),
    unname(unclass(pr)[, -1, drop = FALSE]),
    tolerance = 1e-10
  )

  # the CIF is a non-decreasing step function; before the first event time it is 0
  cif_all <- pall$.pred[[1]]$.pred_cif
  expect_true(all(diff(cif_all) >= -1e-12))
  early <- predict(fit, d[1, ], times = min(pall$.pred[[1]]$.eval_time) / 2)
  expect_equal(early$.pred[[1]]$.pred_cif, 0)

  # a factor event column gives the same fit as numeric codes
  dfac <- d
  dfac$type <- factor(dfac$type)
  fit_fac <- fine_gray(dfac, "time", "type", ~ age + arm)
  expect_equal(fit_fac$fit$coef, fit$fit$coef)

  # failcode 2 predicts the other cause
  fit2 <- fine_gray(d, "time", "type", ~ age + arm, failcode = 2)
  expect_equal(fit2$n_event, sum(d$type == 2))
  expect_error(fine_gray(d, "time", "type", ~ age + arm, failcode = 9), "No row has the event code")
  expect_error(fine_gray(d, "time", "type", ~ 1), "no predictor")
})

# A5: review-tracker apply-back ------------------------------------------------

w2_tracker <- function() {
  data.frame(
    item_id = 1:5,
    file = "manuscript.docx",
    author = c("Reviewer 1", "Reviewer 2", "Reviewer 1", "Reviewer 2", "Reviewer 1"),
    comment = c("Define the cohort", "Report the sample size", "Check Table 2",
                "Clarify the estimand", "Fix the typo"),
    status = c("adopted", "not_adopted", "open", "adopted", "adopted"),
    location = c("Methods, page 4", "", "", "", "Results, page 9"),
    rationale = c("Added the inclusion criteria", "Outside the scope of the study", "",
                  "", "Fixed"),
    stringsAsFactors = FALSE
  )
}

test_that("apply_review_tracker() appends a resolutions appendix and lists what it did not apply (A5)", {
  dir <- withr::local_tempdir()
  report <- file.path(dir, "report.qmd")
  writeLines(c("# Results", "", "The cohort is described here."), report)

  res <- apply_review_tracker(w2_tracker(), report)
  expect_s3_class(res, "review_apply")
  expect_equal(res$n_items, 5L)
  # adopted with a location (1), not adopted with a rationale (2), adopted
  # with a location (5); open (3) and adopted without a location (4) are not applied
  expect_equal(res$n_applied, 3L)
  expect_equal(res$applied$item_id, c(1, 2, 5))
  expect_equal(res$not_applied$item_id, c("3", "4"))
  expect_match(res$not_applied$reason[1], "still open", fixed = TRUE)
  expect_match(res$not_applied$reason[2], "no concrete location", fixed = TRUE)

  out_lines <- readLines(res$output)
  expect_true(any(grepl("^## Review resolutions", out_lines)))
  expect_true(any(grepl("Define the cohort", out_lines, fixed = TRUE)))
  expect_true(any(grepl("Report the sample size", out_lines, fixed = TRUE)))
  expect_true(any(grepl("### Not applied", out_lines, fixed = TRUE)))
  expect_true(any(grepl("Item 3", out_lines, fixed = TRUE)))
  # the report body is unchanged above the appendix
  expect_equal(out_lines[1:3], c("# Results", "", "The cohort is described here."))
  # the source report is untouched: the appendix went to a _reviewed copy
  expect_equal(readLines(report), c("# Results", "", "The cohort is described here."))
  expect_match(res$output, "_reviewed\\.qmd$")

  expect_output(print(res), "3 of 5 tracker item")
})

test_that("apply_review_tracker() guards its inputs (A5)", {
  dir <- withr::local_tempdir()
  report <- file.path(dir, "report.qmd")
  writeLines(c("# Results", "", "Body."), report)

  bad <- w2_tracker()[, c("item_id", "comment", "status")]
  expect_error(apply_review_tracker(bad, report), "missing required column")
  expect_error(apply_review_tracker(w2_tracker()[0, ], report), "no rows")
  expect_error(apply_review_tracker(w2_tracker(), file.path(dir, "missing.qmd")), "existing file")
  txt <- file.path(dir, "report.txt")
  writeLines("plain text", txt)
  expect_error(apply_review_tracker(w2_tracker(), txt), "must be a")

  # an existing output is refused without overwrite = TRUE
  res <- apply_review_tracker(w2_tracker(), report)
  expect_error(apply_review_tracker(w2_tracker(), report), "already exists")
  res2 <- apply_review_tracker(w2_tracker(), report, overwrite = TRUE)
  expect_equal(res2$output, res$output)

  # appending in place needs overwrite = TRUE
  expect_error(apply_review_tracker(w2_tracker(), report, output = report), "already exists")
  res3 <- apply_review_tracker(w2_tracker(), report, output = report, overwrite = TRUE)
  expect_true(any(grepl("^## Review resolutions", readLines(report))))
  expect_equal(res3$n_applied, 3L)
})

# A6/A7: docs pins --------------------------------------------------------------

# DESCRIPTION ships in the built/installed package, so these assertions run
# under R CMD check too — unlike the README pins below, which used to be
# skipped together with them. packageDescription() finds it whether the tests
# run from source, from an installed package or inside R CMD check (where
# "../.." is the check directory, not the package root).
test_that("DESCRIPTION states the full scope (A7)", {
  description <- utils::packageDescription("TempleCBE")$Description
  expect_match(description, "SAS", fixed = TRUE)
  expect_match(description, "Python", fixed = TRUE)
  expect_match(description, "multi-table", fixed = TRUE)
  expect_match(description, "pipeline", fixed = TRUE)
})

test_that("README lists all nine vignettes (A6)", {
  root <- file.path(testthat::test_path(), "..", "..")
  # README.md ships in the built package; only README.qmd is .Rbuildignore-d,
  # so the .md pins run under R CMD check and the .qmd pins only from source.
  # Skip only when neither source is present (installed-package test run).
  readme_files <- c(
    file.path(root, "README.md"),
    file.path(root, "README.qmd")
  )
  readme_files <- readme_files[file.exists(readme_files)]
  skip_if(length(readme_files) == 0L,
          "README.md and README.qmd are not part of the installed package")
  for (f in readme_files) {
    text <- paste(readLines(f, warn = FALSE), collapse = "\n")
    expect_match(text, "competing_risks_and_imputation", fixed = TRUE)
    expect_match(text, "rossi_nested_survival", fixed = TRUE)
    expect_match(text, "From render to publication", fixed = TRUE)
    expect_match(text, "apply_review_tracker", fixed = TRUE)
  }
})

# A8: visualize_pipeline_interactive() ------------------------------------------

test_that("visualize_pipeline_interactive() returns a visNetwork widget for a pipeline graph (A8)", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("visNetwork")
  dir <- withr::local_tempdir()
  objs <- w2_two_chain_graph(dir)

  w <- visualize_pipeline_interactive(objs)
  expect_s3_class(w, c("visNetwork", "htmlwidget"))
  expect_equal(nrow(w$x$nodes), 3L)
  expect_equal(nrow(w$x$edges), 2L)
  # the default title is the study name, "Computational Pipeline" when unset
  expect_equal(w$x$main$text, "Computational Pipeline")
  expect_equal(w$x$submain$text, "Computational Pipeline")

  w2 <- visualize_pipeline_interactive(objs, title = "Study Data Lineage", subtitle = "Q1 freeze",
                                       direction = "UD", font_size = 14, node_size = 30)
  expect_s3_class(w2, c("visNetwork", "htmlwidget"))
  expect_equal(w2$x$main$text, "Study Data Lineage")
  expect_equal(w2$x$submain$text, "Q1 freeze")

  # an igraph input works the same way
  g <- as_igraph(objs)
  w3 <- visualize_pipeline_interactive(g, hierarchical = FALSE)
  expect_s3_class(w3, c("visNetwork", "htmlwidget"))
  expect_equal(nrow(w3$x$nodes), 3L)
})

test_that("export_interactive_pipeline() saves a self-contained HTML page (A8)", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("visNetwork")
  skip_if_not_installed("htmlwidgets")
  skip_if_not_installed("rmarkdown")
  testthat::skip_if_not(
    rmarkdown::pandoc_available(),
    "pandoc is needed to save a self-contained widget"
  )
  dir <- withr::local_tempdir()
  objs <- w2_two_chain_graph(dir)
  out <- file.path(dir, "pipeline.html")
  res <- export_interactive_pipeline(objs, file = out, title = "Study Data Lineage")
  expect_equal(res, out)
  expect_true(file.exists(out))
  html <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_match(html, "Study Data Lineage", fixed = TRUE)
})

test_that("visualize_pipeline_interactive() renders an edgeless graph and names direction in its error (A8, coordinator fix)", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("visNetwork")
  dir <- withr::local_tempdir()
  p <- file.path(dir, "only.csv")
  writeLines("x", p)
  objs <- list(FilePath("only", p))

  # a single node with no edges used to error with
  # "arguments imply differing number of rows: 0, 1"
  w <- suppressWarnings(visualize_pipeline_interactive(objs))
  expect_s3_class(w, c("visNetwork", "htmlwidget"))
  expect_equal(nrow(w$x$nodes), 1L)
  expect_equal(nrow(w$x$edges), 0L)

  # an invalid direction names the argument (match.arg() alone does not)
  expect_error(
    visualize_pipeline_interactive(objs, direction = "sideways"),
    "`direction` must be"
  )
})
