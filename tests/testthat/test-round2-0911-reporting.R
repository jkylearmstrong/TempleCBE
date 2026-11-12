# Round-2 fresh review (2026-11-12), section C items 22-29:
# reporting / rendering / publishing.
#
# 22 zip_reports() rejects non-.qmd/.Rmd report paths instead of shipping
#    the source mislabelled as PDF/DOCX.
# 23 predict.fine_gray_model() carries NA-predictor rows through with NA
#    CIFs; fine_gray() warns (and counts) rows dropped for missing values.
# 24 extract_horizon_risk() errors clearly on 0-row input and returns NA
#    risk for NA-survival rows instead of failing the whole call.
# 25 fine_gray() with variance = FALSE predicts (the old doc claim and
#    guard were wrong: crr() stores the baseline hazard either way).
# 26 review_tracker() multi-path combines $paths from every part.
# 27 pipeline_config() accepts an empty stage_labels / stage_colors as
#    "no labels / no colours".
# 28 render_pipeline() warns on unnamed ... arguments (including dry runs).
# 29 .ask_item() reports a huge item number plainly instead of erroring
#    on `if (NA)`.

r2_competing_data <- function(seed = 1, n = 150) {
  set.seed(seed)
  data.frame(
    time = stats::rexp(n, 0.1),
    type = sample(0:2, n, TRUE, prob = c(0.3, 0.4, 0.3)),
    age = stats::rnorm(n, 60, 8), arm = rep(c("A", "B"), length.out = n)
  )
}

r2_surv_data <- function(seed = 1, n = 120) {
  set.seed(seed)
  data.frame(
    time = stats::rexp(n, 0.05), status = stats::rbinom(n, 1, 0.6),
    age = stats::rnorm(n, 60, 10), sex = stats::rbinom(n, 1, 0.5)
  )
}

# 22 ---------------------------------------------------------------------------

test_that("zip_reports() rejects a report path that is not a .qmd/.Rmd source (22)", {
  skip_if_not_installed("openxlsx")
  d <- tempfile("r2_zip22_")
  dir.create(d)
  txt <- file.path(d, "rep.txt")
  writeLines("HELLO-TXT-CONTENT", txt)
  reports <- data.frame(name = "Other", path = txt, stage = "99_Other")
  expect_error(
    zip_reports(reports, output_formats = c("pdf", "docx"),
                output_dir = file.path(d, "out"), zip_name = "r.zip"),
    "must end in `.qmd` or `.Rmd`"
  )
  # an extensionless path is rejected the same way, and the offending path
  # is named
  noext <- file.path(d, "rep")
  writeLines("HELLO-NOEXT", noext)
  err <- tryCatch(
    zip_reports(data.frame(name = "Other", path = noext),
                output_dir = file.path(d, "out"), zip_name = "r.zip"),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "must end in `.qmd` or `.Rmd`", fixed = TRUE)
  expect_match(err, noext, fixed = TRUE)
  # one bad path among good ones still stops the whole call
  good <- file.path(d, "good.qmd")
  writeLines("qmd source", good)
  writeLines("PDFBYTES", file.path(d, "good.pdf"))
  expect_error(
    zip_reports(
      data.frame(name = c("Good", "Bad"), path = c(good, txt)),
      output_formats = "pdf", output_dir = file.path(d, "out"), zip_name = "r.zip"
    ),
    "must end in `.qmd` or `.Rmd`"
  )
})

test_that("zip_reports() still ships a genuine .qmd/.Rmd source (22)", {
  skip_if_not_installed("openxlsx")
  d <- tempfile("r2_zip22_ok_")
  dir.create(d)
  writeLines("qmd source", file.path(d, "rep.qmd"))
  writeLines("PDFBYTES", file.path(d, "rep.pdf"))
  z <- zip_reports(
    data.frame(name = "Rep", path = file.path(d, "rep.qmd")),
    output_formats = "pdf", output_dir = file.path(d, "out"), zip_name = "r.zip"
  )
  expect_true(file.exists(z))
  contents <- utils::unzip(z, list = TRUE)$Name
  expect_true("pdf/99_Other/rep.pdf" %in% contents)
  td <- tempfile("r2_zip22_unzip_")
  dir.create(td)
  utils::unzip(z, exdir = td)
  expect_equal(readLines(file.path(td, "pdf/99_Other/rep.pdf")), "PDFBYTES")
})

# 23 ---------------------------------------------------------------------------

test_that("fine_gray() warns with the dropped-row count instead of dropping silently (23)", {
  skip_if_not_installed("cmprsk")
  d <- r2_competing_data()
  d$age[c(1, 2, 3, 4, 5)] <- NA
  expect_warning(
    fit <- fine_gray(d, "time", "type", ~ age + arm),
    "dropped 5 of 150 rows with missing values"
  )
  expect_equal(fit$n, 145L)
  expect_equal(fit$n_dropped, 5L)
  # a clean fit neither warns nor drops
  expect_no_warning(fit_clean <- fine_gray(r2_competing_data(), "time", "type", ~ age + arm))
  expect_equal(fit_clean$n, 150L)
  expect_equal(fit_clean$n_dropped, 0L)
})

test_that("predict.fine_gray_model() carries NA-predictor rows through with NA CIFs (23)", {
  skip_if_not_installed("cmprsk")
  d <- r2_competing_data()
  fit <- fine_gray(d, "time", "type", ~ age + arm)
  nd <- d[1:5, ]
  nd$age[3] <- NA
  pr <- predict(fit, nd, times = c(5, 10))
  expect_equal(nrow(pr), 5L)
  expect_true(all(is.na(pr$.pred[[3]]$.pred_cif)))
  expect_equal(pr$.pred[[3]]$.eval_time, c(5, 10))
  # the complete rows agree with predicting on the complete rows alone
  pr_complete <- predict(fit, d[c(1, 2, 4, 5), ], times = c(5, 10))
  expect_equal(pr$.pred[[1]]$.pred_cif, pr_complete$.pred[[1]]$.pred_cif)
  expect_equal(pr$.pred[[4]]$.pred_cif, pr_complete$.pred[[3]]$.pred_cif)
  # a missing factor predictor behaves the same way
  nd2 <- d[1:5, ]
  nd2$arm[2] <- NA
  pr2 <- predict(fit, nd2, times = c(5, 10))
  expect_equal(nrow(pr2), 5L)
  expect_true(all(is.na(pr2$.pred[[2]]$.pred_cif)))
  expect_false(any(is.na(pr2$.pred[[1]]$.pred_cif)))
  # every row NA still returns one row per input row, at the training times
  nd_all <- d[1:5, ]
  nd_all$age <- NA
  pr_all <- predict(fit, nd_all)
  expect_equal(nrow(pr_all), 5L)
  expect_true(all(vapply(pr_all$.pred, function(cv) all(is.na(cv$.pred_cif)), logical(1))))
  expect_equal(pr_all$.pred[[1]]$.eval_time, fit$fit$uftime)
  pr_all_t <- predict(fit, nd_all, times = c(5, 10))
  expect_true(all(vapply(pr_all_t$.pred, function(cv) all(is.na(cv$.pred_cif)), logical(1))))
  expect_equal(pr_all_t$.pred[[1]]$.eval_time, c(5, 10))
  # predicting on the training data is unchanged
  expect_equal(nrow(predict(fit, NULL)), 150L)
})

# 25 ---------------------------------------------------------------------------

test_that("fine_gray(variance = FALSE) predicts CIFs identical to variance = TRUE (25)", {
  skip_if_not_installed("cmprsk")
  d <- r2_competing_data()
  fit_true <- fine_gray(d, "time", "type", ~ age + arm, variance = TRUE)
  fit_false <- fine_gray(d, "time", "type", ~ age + arm, variance = FALSE)
  # the premise of the old guard is false: the baseline hazard is stored
  expect_false(is.null(fit_false$fit$bfitj))
  expect_equal(length(fit_false$fit$bfitj), length(fit_true$fit$bfitj))
  p_true <- predict(fit_true, d[1:5, ], times = c(5, 10))
  p_false <- predict(fit_false, d[1:5, ], times = c(5, 10))
  expect_equal(
    vapply(p_false$.pred, function(cv) cv$.pred_cif, numeric(2)),
    vapply(p_true$.pred, function(cv) cv$.pred_cif, numeric(2))
  )
})

# 24 ---------------------------------------------------------------------------

test_that("extract_horizon_risk() errors clearly on 0-row input (24)", {
  skip_if_not_installed("glmnet")
  d <- r2_surv_data()
  fit <- coxnet(survival::Surv(time, status) ~ age + sex, data = d, penalty = 0.1)
  pr <- predict(fit, d[1:5, ], type = "survival", eval_time = c(5, 10, 20))
  expect_error(extract_horizon_risk(pr[0, ], 10), "has no rows")
})

test_that("extract_horizon_risk() returns NA risk for NA-survival rows (24)", {
  skip_if_not_installed("glmnet")
  d <- r2_surv_data()
  fit <- coxnet(survival::Surv(time, status) ~ age + sex, data = d, penalty = 0.1)
  pr <- predict(fit, d[1:5, ], type = "survival", eval_time = c(5, 10, 20))
  risk <- extract_horizon_risk(pr, 10)
  pr_na <- pr
  pr_na$.pred[[3]]$.pred_survival <- NA_real_
  risk_na <- extract_horizon_risk(pr_na, 10)
  expect_equal(length(risk_na), 5L)
  expect_true(is.na(risk_na[3]))
  expect_equal(risk_na[c(1, 2, 4, 5)], risk[c(1, 2, 4, 5)])
  # a genuinely non-finite or out-of-range survival still errors, naming the row
  pr_inf <- pr
  pr_inf$.pred[[2]]$.pred_survival <- Inf
  expect_error(extract_horizon_risk(pr_inf, 10), "must be finite.*rows: 2")
  pr_big <- pr
  pr_big$.pred[[4]]$.pred_survival <- 1.5
  expect_error(extract_horizon_risk(pr_big, 10), "between 0 and 1.*rows: 4")
})

# 26 ---------------------------------------------------------------------------

test_that("review_tracker() multi-path combines $paths from every part (26)", {
  parts <- list(
    list(paths = list(
      master = "a/review_tracker.xlsx",
      forks = c(rev_a = "a/review_tracker_rev_a.xlsx"),
      manifest = "a/manifest.txt",
      comments_csv = "a/comments.csv"
    )),
    list(paths = list(
      master = "b/review_tracker.xlsx",
      forks = c(rev_b = "b/review_tracker_rev_b.xlsx"),
      manifest = "b/manifest.txt",
      comments_csv = "b/comments.csv"
    ))
  )
  out <- .combine_review_extract_paths(parts)
  expect_equal(out$master, c("a/review_tracker.xlsx", "b/review_tracker.xlsx"))
  expect_equal(out$forks, c(rev_a = "a/review_tracker_rev_a.xlsx", rev_b = "b/review_tracker_rev_b.xlsx"))
  expect_equal(out$manifest, c("a/manifest.txt", "b/manifest.txt"))
  expect_equal(out$comments_csv, c("a/comments.csv", "b/comments.csv"))
  # a single part passes through unchanged
  expect_equal(.combine_review_extract_paths(parts[1]), parts[[1]]$paths)
})

test_that("review_tracker() multi-path branch combines extracts from every part (26)", {
  fake_part <- function(tag) {
    list(
      comments = tibble::tibble(
        file = paste0(tag, ".docx"), comment_text = paste("comment", tag),
        author = "rev", paragraph_num = "1"
      ),
      revisions = tibble::tibble(),
      redlines = tibble::tibble(),
      documents = tibble::tibble(),
      docxwalk = tibble::tibble(),
      summary = tibble::tibble(),
      errors = tibble::tibble(),
      paths = list(master = paste0(tag, "/review_tracker.xlsx"))
    )
  }
  testthat::local_mocked_bindings(
    docx_review_extract = function(p, verbose = FALSE) fake_part(tools::file_path_sans_ext(basename(p))),
    .package = "TempleCBE"
  )
  tr <- review_tracker(c("a.docx", "b.docx"))
  expect_equal(nrow(tr), 2L)
  expect_equal(tr$file, c("a.docx", "b.docx"))
})

# 27 ---------------------------------------------------------------------------

test_that("pipeline_config() accepts an empty stage_labels / stage_colors as empty (27)", {
  pipeline_config(reset = TRUE)
  on.exit(pipeline_config(reset = TRUE), add = TRUE)
  expect_equal(pipeline_config(stage_labels = list())$stage_labels, list())
  expect_equal(pipeline_config(stage_labels = character(0))$stage_labels, list())
  expect_equal(pipeline_config(stage_colors = list())$stage_colors, list())
  expect_equal(pipeline_config(stage_colors = character(0))$stage_colors, list())
  # empty clears labels that were set, and non-empty values still work
  expect_equal(
    pipeline_config(stage_labels = c(s1 = "Stage 1"))$stage_labels,
    c(s1 = "Stage 1")
  )
  expect_equal(pipeline_config(stage_labels = list())$stage_labels, list())
  expect_equal(
    pipeline_config(stage_colors = list(s1 = "#112233"))$stage_colors,
    list(s1 = "#112233")
  )
  # a single NA still clears, and other validations are unchanged
  expect_equal(pipeline_config(stage_labels = NA)$stage_labels, list())
  expect_error(pipeline_config(stage_labels = c("no name")), "non-empty name")
  expect_error(pipeline_config(stage_labels = 1), "named character vector")
})

# 28 ---------------------------------------------------------------------------

test_that("render_pipeline() warns on unnamed ... arguments, including dry runs (28)", {
  d <- tempfile("r2_render28_")
  dir.create(d)
  q <- file.path(d, "doc.qmd")
  writeLines(c("---", "title: t", "format: html", "---", "hi"), q)
  # An unnamed value only reaches `...` once every formal is named: a bare
  # positional extra would fill `pattern` instead (ordinary R matching), so
  # all formals are named here. renv_guard = FALSE keeps the unrelated
  # renv-lockfile warning out of these assertions.
  # Every formal is named from the function itself, so an added or renamed
  # formal cannot break this test; only what the test needs is overridden.
  fm <- formals(render_pipeline)
  fm <- fm[setdiff(names(fm), "...")]
  full_args <- lapply(fm, function(e) if (identical(e, quote(expr = ))) NULL else eval(e, baseenv()))
  full_args$path <- q
  full_args$formats <- "html"
  full_args$dry_run <- TRUE
  full_args$renv_guard <- FALSE
  expect_warning(
    res <- do.call(render_pipeline, c(full_args, list("bogus_value"))),
    "unnamed.*bogus_value"
  )
  expect_equal(res$status[1], "dry_run")
  # an unnamed value among otherwise named dots is caught too
  expect_warning(
    do.call(render_pipeline, c(full_args, list(quiet = TRUE, "second_bogus"))),
    "unnamed.*second_bogus"
  )
  # no dots at all: no warning
  expect_no_warning(do.call(render_pipeline, full_args))
})

# 29 ---------------------------------------------------------------------------

test_that(".ask_item() reports a huge item number plainly (29)", {
  testthat::local_mocked_bindings(
    .read_answer = function(prompt) "99999999999999999999",
    .package = "TempleCBE"
  )
  # no coercion warning leaks out alongside the error
  expect_no_warning(
    err <- tryCatch(
      .ask_item(c("a", "b"), "Render which item? "),
      error = function(e) conditionMessage(e)
    )
  )
  expect_match(err, "No item number 99999999999999999999 \\(1 to 2\\)")
})

test_that(".ask_item() still resolves ordinary numbers and names (29)", {
  testthat::local_mocked_bindings(
    .read_answer = function(prompt) "2",
    .package = "TempleCBE"
  )
  expect_identical(.ask_item(c("a", "b"), "x? "), 2L)
  testthat::local_mocked_bindings(
    .read_answer = function(prompt) "b",
    .package = "TempleCBE"
  )
  expect_identical(.ask_item(c("a", "b"), "x? "), 2L)
  testthat::local_mocked_bindings(
    .read_answer = function(prompt) "0",
    .package = "TempleCBE"
  )
  expect_error(.ask_item(c("a", "b"), "x? "), "No item number 0 \\(1 to 2\\)")
})
