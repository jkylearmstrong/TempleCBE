make_v056_surv_data <- function(n_subj = 36, seed = 101) {
  withr::local_seed(seed)
  do.call(rbind, lapply(seq_len(n_subj), function(i) {
    k <- sample(1:3, 1)
    stops <- c(5, 10, 20)[seq_len(k)]
    x1 <- stats::rnorm(1)
    data.frame(
      id = i,
      tstart = c(0, utils::head(stops, -1)),
      tstop = stops,
      status = c(rep(0L, k - 1), stats::rbinom(1, 1, stats::plogis(1.5 * x1))),
      x1 = x1 + stats::rnorm(k, sd = 0.1),
      x2 = stats::rnorm(k),
      x3 = stats::rnorm(k)
    )
  }))
}

testthat::test_that("P1: pair_paragraphs_by_similarity runs fast on large documents and respects time budget", {
  n <- 1200
  paras <- paste("This is paragraph", seq_len(n), "in the clinical trial report.")
  paras_mod <- paras
  # Edit 50 paragraphs
  paras_mod[seq(1, 500, by = 10)] <- paste("This is paragraph", seq(1, 500, by = 10), "with substantial modifications in trial text.")
  # Add 20 distinct paragraphs
  paras_mod <- c(paras_mod, paste("Completely novel section addition", 1:20))
  # Remove 20 distinct paragraphs
  paras <- c(paras, paste("Xyzk deleted entry", 1:20))

  t0 <- proc.time()["elapsed"]
  # Pair with time budget
  res <- pair_paragraphs_by_similarity(paras, paras_mod, time_budget_sec = 5.0)
  t1 <- proc.time()["elapsed"]

  testthat::expect_lt(as.numeric(t1 - t0), 5.0)
  testthat::expect_s3_class(res$section_summary, "tbl_df")
  testthat::expect_s3_class(res$largest_changes, "tbl_df")
  testthat::expect_gt(res$n_edited, 0L)
  testthat::expect_gt(res$n_added, 0L)
  testthat::expect_gt(res$n_removed, 0L)
  testthat::expect_match(res$diff_summary, "edited")
})

testthat::test_that("P2: find_deprecated_calls and update_deprecated_calls are namespace-safe, local-def safe, and fast", {
  tmp_dir <- tempfile("p2_test_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  # Script 1: defines its own SumNa, calls janitor::clean_names, and calls TempleCBE::sd.error and summarize_tune_results
  s1 <- file.path(tmp_dir, "report.qmd")
  writeLines(c(
    "SumNa <- function(x) { sum(is.na(x)) }",
    "na_cnt <- SumNa(vec)",
    "cleaned <- janitor::clean_names(dat)",
    "dt_like <- data.table::like(dat$col, 'test')",
    "se <- TempleCBE::sd.error(vals)",
    "tuned <- summarize_tune_results(splits)"
  ), s1)

  # Binary / invalid files should be skipped cleanly
  s2 <- file.path(tmp_dir, "model_cache.rds")
  writeBin(as.raw(c(0x1f, 0x8b, 0x08, 0x00, 0xaa, 0xbb)), s2)

  s3 <- file.path(tmp_dir, "notes.rtf")
  writeLines("{\\rtf1\\ansi\\deff0 Dummy RTF content}", s3)

  # 1. find_deprecated_calls
  findings <- find_deprecated_calls(tmp_dir)
  testthat::expect_equal(nrow(findings), 2L)
  testthat::expect_setequal(findings$old_name, c("sd.error", "summarize_tune_results"))
  testthat::expect_false("SumNa" %in% findings$old_name)
  testthat::expect_false("clean_names" %in% findings$old_name)
  testthat::expect_false("like" %in% findings$old_name)

  # 2. update_deprecated_calls in dry_run mode
  dry_out <- update_deprecated_calls(tmp_dir, dry_run = TRUE)
  testthat::expect_equal(nrow(dry_out), 2L)
  lines_s1_dry <- readLines(s1)
  testthat::expect_true(any(grepl("sd\\.error", lines_s1_dry)))
  testthat::expect_true(any(grepl("summarize_tune_results", lines_s1_dry)))

  # 3. update_deprecated_calls in apply mode
  apply_out <- update_deprecated_calls(tmp_dir, dry_run = FALSE)
  lines_s1_applied <- readLines(s1)

  # Local SumNa and janitor::clean_names must remain unchanged
  testthat::expect_true(any(grepl("SumNa <- function", lines_s1_applied)))
  testthat::expect_true(any(grepl("na_cnt <- SumNa\\(vec\\)", lines_s1_applied)))
  testthat::expect_true(any(grepl("janitor::clean_names\\(dat\\)", lines_s1_applied)))
  testthat::expect_true(any(grepl("data.table::like", lines_s1_applied)))

  # Deprecated calls must be renamed
  testthat::expect_true(any(grepl("TempleCBE::std_error\\(vals\\)", lines_s1_applied)))
  testthat::expect_true(any(grepl("tuned <- tune_alpha_by_split\\(splits\\)", lines_s1_applied)))
})

testthat::test_that("P3: database_relationships runs cleanly under lifecycle_verbosity = error", {
  withr::local_options(lifecycle_verbosity = "error")

  db <- list(
    table_a = tibble::tibble(subject_id = 1:5, val = c("a", "b", "c", "d", "e")),
    table_b = tibble::tibble(subject_id = 3:7, num = 10:14)
  )

  testthat::expect_no_error({
    rel <- database_relationships(db)
  })
  testthat::expect_equal(nrow(rel), 1L)
  testthat::expect_equal(rel$shared_keys, 3L)
})

testthat::test_that("P3: Zero internal calls to deprecated functions across entire R/ codebase", {
  dep_map <- templecbe_deprecated_map()
  r_files <- list.files(testthat::test_path("../../R"), pattern = "\\.[Rr]$", full.names = TRUE)
  if (length(r_files) == 0L) {
    r_files <- list.files(system.file("R", package = "TempleCBE"), pattern = "\\.[Rr]$", full.names = TRUE)
  }
  testthat::skip_if(length(r_files) == 0L, "Source R directory not accessible in installed binary mode.")

  r_files <- r_files[!grepl("(deprecated_tools|reexports)\\.[Rr]$", r_files)]

  violations <- character()
  for (f in r_files) {
    lines <- readLines(f, warn = FALSE)
    for (i in seq_along(lines)) {
      l <- lines[i]
      if (grepl("^\\s*#", l)) next
      if (grepl("deprecate_(warn|stop|soft)", l)) next
      for (old_name in names(dep_map)) {
        if (grepl(paste0("^\\s*", gsub("\\.", "\\\\.", old_name), "\\s*(<-|=)\\s*function"), l)) next
        # Check for unadorned or TempleCBE-qualified call to deprecated name
        if (grepl(paste0("(?<![a-zA-Z0-9_.:])", gsub("\\.", "\\\\.", old_name), "\\s*\\("), l, perl = TRUE)) {
          violations <- c(violations, sprintf("%s:%d calls %s", basename(f), i, old_name))
        }
      }
    }
  }

  testthat::expect_equal(violations, character(), info = paste(violations, collapse = "\n"))
})

testthat::test_that("P4: pca_loading_diff and heatmap stop with clear error when fits share no variables", {
  set.seed(42)
  pca1 <- run_pca(mtcars[, c("mpg", "cyl", "disp")])
  pca2 <- run_pca(iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length")])

  testthat::expect_error(
    pca_loading_diff(pca1, pca2),
    "The two fits have no variables in common."
  )

  testthat::expect_error(
    pca_loading_diff_heatmap(pca1, pca2),
    "The two fits have no variables in common."
  )
})

testthat::test_that("P5: tune_alpha_by_split reports serial baseline range and speedup range", {
  set.seed(42)
  long <- make_v056_surv_data(n_subj = 30, seed = 202)
  rec <- recipes::recipe(~ ., data = long) |>
    recipes::update_role("id", "tstart", "tstop", "status", new_role = "id variable")
  splits <- rsample::group_vfold_cv(long, group = id, v = 2)
  times <- unique(long[, c("tstart", "tstop")])

  res <- tune_alpha_by_split(
    object = splits,
    alphas = c(0.2, 0.8),
    recipe = rec,
    feature_names = c("x1", "x2"),
    time_data = times,
    internal_folds = 2,
    num_fixed = 0,
    seed = 123
  )

  breakdown <- attr(res, "timing_breakdown")
  testthat::expect_type(breakdown, "list")
  testthat::expect_true("measured_serial_task_range_sec" %in% names(breakdown))
  testthat::expect_true("measured_serial_baseline_range_sec" %in% names(breakdown))
  testthat::expect_true("speedup_range" %in% names(breakdown))
  testthat::expect_length(breakdown$speedup_range, 2L)
  testthat::expect_lte(breakdown$speedup_range[1], breakdown$speedup_range[2])
})
