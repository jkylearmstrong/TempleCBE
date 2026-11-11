# Regression tests for the consolidated findings fixed in 0.8.1 (drafted as 0.7.11). Each test fails on 0.7.10.

test_that("subject_keys() numbers bootstrap copies without building the interaction of its keys", {
  # 300 subjects x 40 intervals with distinct times: stats::ave() over (subject, start, stop)
  # asked for 300 * 12000 * 12000 interaction levels and exhausted memory
  n_sub <- 300L
  n_int <- 40L
  start <- as.numeric(seq_len(n_sub * n_int))
  y <- survival::Surv(start, start + 0.5, rep(0, n_sub * n_int))
  subject <- rep(seq_len(n_sub), each = n_int)
  expect_identical(subject_keys(y, subject), subject)

  # a bootstrap resample repeats whole subjects: the copies get keys of their own
  y2 <- survival::Surv(c(0, 1, 0, 0, 1), c(1, 2, 3, 1, 2), c(0, 1, 0, 0, 1))
  expect_equal(subject_keys(y2, c(1, 1, 2, 1, 1)), c("1#1", "1#1", "2#1", "1#2", "1#2"))
})

test_that("surv_subject_truth() says what to do with recurrent events", {
  y <- survival::Surv(c(0, 1), c(1, 2), c(1, 1))
  expect_error(surv_subject_truth(y, c(1, 1)), "time to first event")
})

test_that("is_dir_link() is FALSE for a path that does not exist", {
  expect_false(is_dir_link(file.path(withr::local_tempdir(), "absent")))
})

test_that("update_deprecated_calls() keeps line endings and can write a backup", {
  dir <- withr::local_tempdir()
  write_raw <- function(path, text) writeBin(charToRaw(text), path)
  crlf <- file.path(dir, "crlf.R")
  lf <- file.path(dir, "lf.R")
  write_raw(crlf, "library(TempleCBE)\r\nx <- cbe_cox_single(d, 'o', 'f')\r\n")
  write_raw(lf, "library(TempleCBE)\nx <- cbe_cox_single(d, 'o', 'f')")
  suppressMessages(update_deprecated_calls(dir, dry_run = FALSE, backup = TRUE))

  read_raw <- function(path) rawToChar(readBin(path, "raw", file.info(path)$size))
  expect_equal(read_raw(crlf), "library(TempleCBE)\r\nx <- cox_single(d, 'o', 'f')\r\n")
  # no final newline before, none after
  expect_equal(read_raw(lf), "library(TempleCBE)\nx <- cox_single(d, 'o', 'f')")
  expect_equal(read_raw(paste0(crlf, ".bak")), "library(TempleCBE)\r\nx <- cbe_cox_single(d, 'o', 'f')\r\n")
  # no temporary file is left next to the sources
  expect_setequal(list.files(dir, all.files = TRUE, no.. = TRUE), c("crlf.R", "crlf.R.bak", "lf.R", "lf.R.bak"))
})

test_that("the checkpointed tuning wrappers validate `shard`", {
  skip_if_not_installed("tune")
  expect_error(tune_grid_checkpointed(NULL, NULL, checkpoint_dir = tempdir(), shard = c(0, 2)), "`shard` must be")
  expect_error(fit_resamples_checkpointed(NULL, NULL, checkpoint_dir = tempdir(), shard = 3), "`shard` must be")
  expect_equal(shard_active_mask(5, validate_shard_spec("2/2")), c(FALSE, TRUE, FALSE, TRUE, FALSE))
  expect_equal(shard_active_mask(3, NULL), rep(TRUE, 3))
})

test_that("a checkpoint that cannot be written or read gives a warning that names the file", {
  missing_dir <- file.path(withr::local_tempdir(), "no", "such", "dir", "ckpt.rds")
  expect_warning(ok <- save_checkpoint_rds(1:3, missing_dir), "could not be written")
  expect_false(ok)

  corrupt <- withr::local_tempfile(fileext = ".rds")
  writeBin(as.raw(1:5), corrupt)
  expect_warning(val <- read_checkpoint_rds(corrupt), "could not be read")
  expect_null(val)
  expect_warning(chk <- read_verified_checkpoint(corrupt, "fp"), "could not be read")
  expect_equal(chk$status, "invalid")

  good <- withr::local_tempfile(fileext = ".rds")
  expect_true(save_checkpoint_rds(list(a = 1), good))
  expect_equal(read_checkpoint_rds(good), list(a = 1))
})

test_that("cox_single() and km_single() report a coefficient that cannot be estimated", {
  set.seed(1)
  d <- data.frame(time = stats::rexp(40), status = stats::rbinom(40, 1, 0.7), x = 1)
  d$outcome <- survival::Surv(d$time, d$status)
  expect_warning(res <- cox_single(d, "outcome", "x"), "could not be estimated")
  expect_match(as.character(res$interpretation), "could not be estimated")
  km <- suppressWarnings(km_single(d, "outcome", "x"))
  expect_true(is.na(km$direction))
  expect_output(print(km), "could not be estimated")
})

test_that("a label attribute of any length does not break cox_single()", {
  expect_equal(var_label_or(structure(1:3, label = c("First", "Second")), "x"), "First")
  expect_equal(var_label_or(structure(1:3, label = character(0)), "x"), "x")
  expect_equal(var_label_or(structure(1:3, label = NA_character_), "x"), "x")
  expect_equal(var_label_or(1:3, "x"), "x")
})

test_that("fit_ibs_mixed_model() handles a zero IBS and a single alpha", {
  df <- data.frame(alpha = rep(c(0.1, 0.5), each = 3), IBS = c(0.1, 0.2, 0, 0.15, 0.12, 0.11))
  expect_warning(m <- fit_ibs_mixed_model(df), "not a positive number")
  expect_true(all(is.finite(m$data$y)))
  expect_error(suppressWarnings(fit_ibs_mixed_model(data.frame(alpha = 0.1, IBS = 0))), "No fit has a positive IBS")

  expect_warning(
    one <- fit_ibs_mixed_model(data.frame(alpha = 0.5, IBS = c(0.1, 0.2, 0.3))),
    "slope .* could not be estimated"
  )
  expect_match(one$equation, "no slope")
})

test_that("tbl_tune_coef() does not return a count as a proportion", {
  df <- data.frame(term = rep(c("a", "b"), each = 4), estimate = c(1, 0, 2, 3, 0, 0, 0, 1))
  expect_message(out <- tbl_tune_coef(df), "no resampling split column")
  expect_equal(out$prop_selected[out$term == "a"], 0.75)
  expect_equal(out$prop_selected[out$term == "b"], 0.25)

  df$split <- rep(1:4, 2)
  expect_no_message(with_split <- tbl_tune_coef(df))
  expect_equal(with_split$prop_selected[with_split$term == "a"], 0.75)
})

test_that("the tuning plots use `label_map` for their titles", {
  df <- data.frame(alpha = rep(c(0.1, 0.5), each = 3), IBS = c(0.1, 0.2, 0.3, 0.15, 0.12, 0.11),
                   lambda = 0.01, split = rep(1:3, 2))
  map <- c(alpha = "Mixing", IBS = "Brier", "log10(IBS)" = "log Brier", lambda = "Penalty")
  p <- suppressMessages(suppressWarnings(plot_tune_alpha_ibs(df, label_map = map)))
  expect_equal(p$labels$x, "Mixing")
  expect_equal(p$labels$y, "log Brier")
  p2 <- plot_tune_lambda_ibs(df, label_map = map)
  expect_equal(p2$labels$x, "Penalty")
  expect_equal(p2$labels$y, "Brier")
  # without a map the defaults stay
  expect_equal(plot_tune_lambda_ibs(df)$labels$y, "Integrated Brier Score (IBS)")
})

test_that("a term string is evaluated only if it is a transformation of columns", {
  data <- data.frame(x = 1:5, k = 3)
  marker <- gsub("\\\\", "/", withr::local_tempfile())
  expect_false(.constant_expr(data, sprintf("file.create('%s') + x", marker)))
  expect_false(file.exists(marker))
  expect_false(.constant_expr(data, "system('echo hi')"))
  expect_false(.constant_expr(data, "base::unlink(x)"))

  expect_false(.constant_expr(data, "log(x)"))
  expect_true(.constant_expr(data, "I(x * 0)"))
  expect_true(.constant_expr(data, "k"))
  expect_true(.constant_expr(data, "base::log(k)"))
})

test_that("a term that warns is evaluated once", {
  data <- data.frame(x = c(-1, 1, 2))
  # log() of a negative number warns; the handler used to evaluate the term again
  expect_no_warning(res <- .constant_expr(data, "log(x)"))
  expect_false(res)
})

test_that("seeded tokens warn that they are not secret, and a short key always warns", {
  expect_warning(generate_pseudonym_token(n = 2, seed = 1), "NOT secret")
  withr::local_options(templecbe_suppress_weak_key_warning = FALSE)
  expect_warning(
    anonymize_pi("Smith", method = "hmac_surname", key = "short", allow_default_key = TRUE),
    "shorter than 16 characters"
  )
})

test_that("a shared surname is an error unless collisions are allowed", {
  withr::local_options(templecbe_suppress_weak_key_warning = TRUE)
  nms <- paste0("Investigator", 1:3)
  testthat::local_mocked_bindings(generate_last_names = function(...) "Shared")
  expect_error(anonymize_pi(nms, method = "hmac_surname", key = "k"), "Surname collision")
  expect_warning(
    out <- anonymize_pi(nms, method = "hmac_surname", key = "k", allow_collisions = TRUE),
    "Surname collision"
  )
  expect_equal(out, rep("Shared", 3))
})

test_that("a backend that was asked for by name and is absent is an error under strict = TRUE", {
  pdf <- withr::local_tempfile(fileext = ".pdf")
  writeLines("%PDF-1.4", pdf)
  dest <- withr::local_tempfile(fileext = ".docx")
  testthat::local_mocked_bindings(
    check_docx_toolchain = function(...) {
      list(python_with_pdf2docx = NULL, python_any = NULL, soffice = "fake_soffice", word_com = FALSE,
           backend = "libreoffice")
    },
    .validate_conversions = function(conversions, strict) {
      data.frame(valid = TRUE, pdf = conversions$src, docx = conversions$dest)
    }
  )
  conv <- data.frame(src = pdf, dest = dest)
  expect_error(convert_pdfs_to_docx(conv, backend = "python"), "\"python\" is not available")
  expect_warning(
    res <- convert_pdfs_to_docx(conv, backend = "python", strict = FALSE),
    "is not available"
  )
  expect_false(res$converted)
})

test_that("remove_dir_tree() removes links as links and leaves their targets", {
  root <- withr::local_tempdir()
  target <- file.path(root, "target")
  dir.create(target)
  writeLines("keep", file.path(target, "keep.txt"))
  tree <- file.path(root, "tree")
  dir.create(file.path(tree, "sub"), recursive = TRUE)
  writeLines("x", file.path(tree, "sub", "a.txt"))
  skip_if(!make_dir_link(target, file.path(tree, "sub", "lnk")), "cannot create a directory link here")
  expect_true(make_dir_link(target, file.path(root, "top_link")))

  expect_equal(remove_dir_tree(c(tree, file.path(root, "top_link"), file.path(root, "absent"))), c(TRUE, TRUE, TRUE))
  expect_false(dir.exists(tree))
  expect_false(file.exists(file.path(root, "top_link")))
  expect_true(file.exists(file.path(target, "keep.txt")))
})

test_that("pipeline_config() returns the previous options and cbe_labels() leaves its option alone", {
  withr::local_options(pipeline.study_name = "Before", templecbe.label_map = NULL)
  # pipeline_config() keeps its settings in the package since 0.8.2, so they are cleared on exit
  pipeline_config(reset = TRUE)
  withr::defer(pipeline_config(reset = TRUE))
  cfg <- pipeline_config(study_name = "After")
  expect_equal(cfg$study_name, "After")
  expect_equal(attr(cfg, "previous")$pipeline.study_name, "Before")
  options(attr(cfg, "previous"))
  expect_equal(getOption("pipeline.study_name"), "Before")

  withr::defer(cbe_labels(reset = TRUE))
  cbe_labels(c(x1 = "FEV1"))
  expect_null(getOption("templecbe.label_map"))
  expect_equal(resolve_cbe_labels("x1"), "FEV1")
  cbe_labels(reset = TRUE)
  expect_equal(resolve_cbe_labels("x1"), "x1")
})

test_that("find_write_calls() does not change the future plan unless workers are asked for", {
  skip_if_not_installed("future")
  skip_if_not_installed("furrr")
  dir <- withr::local_tempdir()
  writeLines("saveRDS(x, 'a.rds')", file.path(dir, "a.R"))
  before <- future::plan()
  res <- find_write_calls(dir)
  expect_identical(class(future::plan()), class(before))
  expect_true(any(res$pattern == "saveRDS"))
})

test_that("installer helpers refuse what is not a package name or an extension source", {
  expect_error(install_package_no_lock("../evil"), "Not valid package name")
  expect_error(install_package_no_lock("--configure-args=x"), "Not valid package name")
  expect_error(install_package_no_lock(character()), "character vector of package names")

  expect_silent(.check_extension_source("jkylearmstrong-temple/quarto_temple_brand"))
  expect_silent(.check_extension_source("owner/repo@v1.2.0"))
  expect_silent(.check_extension_source(withr::local_tempdir()))
  expect_warning(.check_extension_source("https://example.org/ext.zip"), "from the URL")
  expect_error(.check_extension_source("http://example.org/ext.zip"), "not a GitHub source")
  expect_error(.check_extension_source("owner/repo; rm -rf x"), "not a GitHub source")
  expect_error(.check_extension_source(c("a/b", "c/d")), "single string")
})

test_that("glm_single() looks a family name up in stats, not in the caller", {
  set.seed(2)
  d <- data.frame(y = stats::rbinom(60, 1, 0.5), x = stats::rnorm(60))
  binomial <- function(...) stop("the caller's object was used")
  res <- glm_single(d, "y", "x", family = "binomial", normalize = FALSE)
  expect_s3_class(res, "cbe_coef_screen")
  expect_error(glm_single(d, "y", "x", family = "binomail"), "is not a family name")
})

test_that("cbe_cox_table() names the columns a table lacks", {
  x <- structure(list(table = data.frame(Variable = "a", HR = 1.2)), class = "cbe_cox")
  expect_error(cbe_cox_table(x), "missing column\\(s\\): Level, Role")
})

test_that("write_workbook() keeps long sheet names unique within 31 characters", {
  long <- strrep("a", 31)
  x <- stats::setNames(
    list(data.frame(a = 1), data.frame(a = 2), data.frame(a = 3)),
    c(paste0(long, "_first"), paste0(long, "_second"), paste0(toupper(long), "_third"))
  )
  path <- withr::local_tempfile(fileext = ".xlsx")
  write_workbook(x, path, include_metadata = FALSE)
  sheets <- readxl::excel_sheets(path)
  expect_length(sheets, 3L)
  expect_true(all(nchar(sheets) <= 31L))
  expect_equal(anyDuplicated(tolower(sheets)), 0L)
})

test_that("pool_imputations() gives NA, not NaN, for a term with no variance at all", {
  imp <- replicate(3, data.frame(term = c("a", "b"), estimate = c(1, 2), std.error = c(0, 0.5)), simplify = FALSE)
  expect_warning(out <- pool_imputations(imp), "total variance of 0")
  a <- out[out$term == "a", ]
  expect_equal(a$estimate, 1)
  expect_true(all(is.na(c(a$statistic, a$p.value, a$conf.low, a$conf.high))))
  b <- out[out$term == "b", ]
  expect_false(anyNA(c(b$statistic, b$p.value)))
})

test_that("profvis_summary() checks the profile and survives a total of zero", {
  expect_error(
    profvis_summary(list(x = list(message = list(prof = data.frame(label = "f"))))),
    "no column\\(s\\) memalloc, meminc, time, depth"
  )
  prof <- list(x = list(message = list(prof = data.frame(
    label = c("f", "g"), memalloc = 0, meminc = 0, time = 1:2, depth = 1:2
  ))))
  out <- profvis_summary(prof)
  expect_equal(out$memory_by_function$pct, c(0, 0))
  expect_equal(out$memory_increment_by_function$pct, c(0, 0))
})

test_that("pairwise_combos() falls back on the name for an NA label and reports unknown columns", {
  df <- data.frame(A = c("x", "y"), B = c("1", "2"))
  labs <- data.frame(variable = c("A", "B"), label = c(NA, "Bee"))
  out <- pairwise_combos(df, labels_df = labs)
  expect_equal(out$label1, "A")
  expect_equal(out$label2, "Bee")
  expect_warning(pairwise_combos(df, cols = c("A", "B", "Z")), "not in `data`.*Z")
})

test_that("scan_data_io() names the directory that does not exist", {
  expect_error(scan_data_io(file.path(tempdir(), "no_such_dir_0711")), "`code_path` must be an existing directory.*no_such_dir_0711")
})

test_that("render_pipeline() rejects a `workers` that is not a number", {
  skip_if_not_installed("quarto")
  f <- withr::local_tempfile(fileext = ".qmd")
  writeLines(c("---", "title: t", "---", "", "text"), f)
  withr::local_options(templecbe_suppress_renv_warning = TRUE)
  expect_error(
    suppressMessages(render_pipeline(f, workers = "two", renv_guard = FALSE, manifest = FALSE)),
    "`workers` must be NULL or a single number"
  )
})

test_that("simulate_section_data() validates n_subjects and missing_rate", {
  mapping <- demo_cbe_mapping()
  index <- mapping$INDEX[1]
  expect_error(simulate_section_data(mapping, index, n_subjects = 0), "`n_subjects` must be")
  expect_error(simulate_section_data(mapping, index, n_subjects = 2.5), "`n_subjects` must be")
  expect_error(simulate_section_data(mapping, index, missing_rate = 1.5), "`missing_rate` must be")
  expect_error(simulate_section_data(mapping, index, missing_rate = -0.1), "`missing_rate` must be")

  # the caller's random stream is left where it was
  set.seed(42)
  expected <- stats::runif(1)
  set.seed(42)
  simulate_section_data(mapping, index, n_subjects = 3, seed = 7)
  expect_equal(stats::runif(1), expected)
})

test_that("every p-value formatter gives what it gave before", {
  p <- c(0.0004, 0.034, 0.5, NA)
  expect_equal(fmt_p(p), c("p < 0.001", "p = 0.034", "p = 0.500", "—"))
  expect_equal(format_p_value(p), c("<0.001", "0.034", "0.500", NA))
  expect_equal(format_p_value(p, digits = 2, na = ""), c("<0.01", "0.03", "0.50", ""))
  expect_equal(.fmt_p_short(NA), "p = NA")
  expect_equal(.fmt_p_short(0.2), "p = 0.200")
})

test_that("local_wd() changes the directory for the calling function only", {
  here <- getwd()
  dir <- withr::local_tempdir()
  f <- function() {
    local_wd(dir)
    normalizePath(getwd(), winslash = "/")
  }
  expect_equal(f(), normalizePath(dir, winslash = "/"))
  expect_equal(getwd(), here)
})

test_that("diff_deliverables() refuses an archive with an entry outside its folder", {
  skip_if_not_installed("zip")
  work <- withr::local_tempdir()
  dir.create(file.path(work, "inner"))
  writeLines("x", file.path(work, "escape.txt"))
  writeLines("y", file.path(work, "inner", "ok.txt"))
  bad <- file.path(work, "bad.zip")
  good <- file.path(work, "good.zip")
  withr::with_dir(file.path(work, "inner"), {
    made <- tryCatch({
      suppressWarnings(zip::zip(bad, files = c("ok.txt", "../escape.txt"), mode = "mirror"))
      TRUE
    }, error = function(e) FALSE)
    zip::zip(good, files = "ok.txt")
  })
  skip_if(!made || !any(grepl("..", zip::zip_list(bad)$filename, fixed = TRUE)),
          "could not build an archive with a '..' entry")
  expect_error(diff_deliverables(bad, good), "absolute path or a '..' component")
})

test_that(".onLoad() sets the Cox ties default once and keeps a value that is already set", {
  on_load <- get(".onLoad", envir = asNamespace("TempleCBE"))
  withr::local_options(templecbe.cox_ties = NULL)
  on_load(NULL, "TempleCBE")
  expect_equal(getOption("templecbe.cox_ties"), "breslow")
  options(templecbe.cox_ties = "efron")
  on_load(NULL, "TempleCBE")
  expect_equal(getOption("templecbe.cox_ties"), "efron")
})

test_that("build_manual_versioned() writes the versioned manual and the latest copy", {
  skip_if_not_installed("callr")
  skip_if_not_installed("devtools")
  pkg <- withr::local_tempdir()
  writeLines(c("Package: demo", "Version: 1.2.3", "Title: Demo", "Description: Demo.", "License: MIT"),
             file.path(pkg, "DESCRIPTION"))
  out_dir <- file.path(pkg, "out")
  seen <- NULL
  testthat::local_mocked_bindings(
    rcmd = function(cmd, cmdargs, ...) {
      seen <<- c(cmd, cmdargs)
      writeLines("%PDF", sub("^--output=", "", cmdargs[startsWith(cmdargs, "--output=")]))
      list(stdout = "built\n")
    },
    .package = "callr"
  )
  withr::local_envvar(R_RD4PDF = NA)
  expect_message(expect_output(res <- build_manual_versioned(pkg, path = out_dir), "built"), "Manual built")
  expect_equal(basename(res), "demo_1.2.3.pdf")
  expect_true(all(file.exists(file.path(out_dir, c("demo_1.2.3.pdf", "demo_latest.pdf")))))
  expect_equal(seen[1:2], c("Rd2pdf", "--force"))

  unlink(file.path(out_dir, "demo_latest.pdf"))
  suppressMessages(capture.output(build_manual_versioned(pkg, path = out_dir, latest = FALSE)))
  expect_false(file.exists(file.path(out_dir, "demo_latest.pdf")))
})
