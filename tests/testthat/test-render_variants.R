rv_template <- function(env = parent.frame(), ext = "qmd", params = c("use_imputed: false", "label: base")) {
  d <- normalizePath(withr::local_tempdir(.local_envir = env), winslash = "/")
  path <- file.path(d, paste0("analysis.", ext))
  writeLines(c("---", "title: T", "format: html", "params:", paste0("  ", params), "---", "", "Body."), path)
  path
}

# A renderer that writes the file it is asked for and records what it was given
rv_recorder <- function(fail_on = NULL, fail_after_write = FALSE) {
  calls <- new.env()
  calls$log <- list()
  f <- function(input, output_format, output_file, execute_params, quiet, metadata = NULL) {
    calls$log[[length(calls$log) + 1L]] <- list(
      input = input, format = output_format, file = output_file, params = execute_params,
      quiet = quiet, metadata = metadata
    )
    target <- file.path(dirname(input), output_file)
    failing <- !is.null(fail_on) && identical(execute_params$label, fail_on)
    if (!failing || fail_after_write) {
      writeLines(paste(output_format, jsonlite::toJSON(execute_params, auto_unbox = TRUE)), target)
    }
    if (failing) stop("boom", call. = FALSE)
    invisible(NULL)
  }
  list(f = f, calls = calls)
}

test_that("render_variants() plans one rendering per variant and format without rendering", {
  tpl <- rv_template()
  plan <- render_variants(
    tpl, list(original = list(use_imputed = FALSE), imputed = list(use_imputed = TRUE)),
    formats = c("html", "docx"), dry_run = TRUE
  )
  expect_s3_class(plan, "tbl_df")
  expect_named(plan, c("variant", "format", "params", "output", "status", "error"))
  expect_identical(plan$variant, c("original", "original", "imputed", "imputed"))
  expect_identical(plan$format, c("html", "docx", "html", "docx"))
  expect_identical(basename(plan$output), c("analysis_original.html", "analysis_original.docx", "analysis_imputed.html", "analysis_imputed.docx"))
  expect_identical(unique(plan$status), "planned")
  expect_identical(plan$params[[3]], list(use_imputed = TRUE))
  expect_false(file.exists(plan$output[1]))
})

test_that("render_variants() hands each variant's parameters to the renderer and moves the outputs", {
  tpl <- rv_template()
  out_dir <- file.path(dirname(tpl), "reports")
  rec <- rv_recorder()
  res <- render_variants(
    tpl, list(original = list(use_imputed = FALSE), imputed = list(use_imputed = TRUE, label = "imp")),
    output_dir = out_dir, renderer = rec$f
  )
  expect_identical(res$status, c("rendered", "rendered"))
  expect_true(all(file.exists(res$output)))
  expect_identical(dirname(res$output[1]), normalizePath(out_dir, winslash = "/"))
  expect_identical(basename(res$output), c("analysis_original.html", "analysis_imputed.html"))
  # nothing left behind next to the template: only the template itself
  expect_identical(list.files(dirname(tpl), all.files = TRUE, no.. = TRUE), c("analysis.qmd", "reports"))
  expect_identical(rec$calls$log[[2]]$params, list(use_imputed = TRUE, label = "imp"))
  expect_true(rec$calls$log[[1]]$quiet)
  expect_match(readLines(res$output[2]), "\"use_imputed\":true")
})

test_that("render_variants() never touches a file that already exists next to the template", {
  tpl <- rv_template()
  mine <- file.path(dirname(tpl), "analysis_final.html")
  writeLines("hand-made", mine)
  out_dir <- file.path(dirname(tpl), "out")
  res <- render_variants(tpl, list(final = list(label = "x")), output_dir = out_dir, renderer = rv_recorder()$f)
  expect_identical(res$status, "rendered")
  expect_identical(readLines(mine), "hand-made")
  expect_true(file.exists(file.path(out_dir, "analysis_final.html")))
  expect_false(identical(readLines(file.path(out_dir, "analysis_final.html")), "hand-made"))
})

test_that("render_variants() replaces a file of the final name only in output_dir", {
  tpl <- rv_template()
  old <- file.path(dirname(tpl), "analysis_a.html")
  writeLines("old", old)
  res <- render_variants(tpl, list(a = list(label = "x")), renderer = rv_recorder()$f)
  expect_identical(normalizePath(res$output, winslash = "/"), normalizePath(old, winslash = "/"))
  expect_false(identical(readLines(old), "old"))
})

test_that("a stale file at the final name is not reported as a rendering that did not happen", {
  tpl <- rv_template()
  out_dir <- file.path(dirname(tpl), "out")
  dir.create(out_dir)
  writeLines("stale", file.path(out_dir, "analysis_a.html"))
  lazy <- function(input, output_format, output_file, execute_params, quiet, metadata = NULL) invisible(NULL)
  expect_error(
    render_variants(tpl, list(a = list()), output_dir = out_dir, renderer = lazy),
    "did not write"
  )
  expect_identical(readLines(file.path(out_dir, "analysis_a.html")), "stale") # not removed before the renderer succeeded
})

test_that("render_variants() refuses a parameter the template does not declare, before rendering anything", {
  tpl <- rv_template()
  rec <- rv_recorder()
  expect_error(
    render_variants(tpl, list(ok = list(use_imputed = TRUE), typo = list(use_imputd = TRUE)), renderer = rec$f),
    "Variant 'typo'.*use_imputd.*Declared: use_imputed, label"
  )
  expect_length(rec$calls$log, 0L)
  expect_error(
    render_variants(rv_template(params = "x: 1"), list(a = list(y = 1)), renderer = rec$f, dry_run = TRUE),
    "does not declare"
  )
})

test_that("parameters named like YAML booleans (n, y, on) are read as written", {
  tpl <- rv_template(params = c("n: 10", "y: 2", "on: yes", "'quoted': 1", "# comment", "", "label: z"))
  expect_identical(.rv_declared_params(tpl), c("n", "y", "on", "quoted", "label"))
  res <- render_variants(tpl, list(a = list(n = 5, y = 1, on = TRUE)), renderer = rv_recorder()$f)
  expect_identical(res$status, "rendered")
})

test_that(".rv_declared_params() reads nested defaults, no params, inline maps and unreadable headers", {
  d <- normalizePath(withr::local_tempdir(), winslash = "/")
  write <- function(...) { p <- file.path(d, paste0(sample(letters, 8, TRUE), collapse = "")); writeLines(c(...), p); p }
  expect_identical(.rv_declared_params(write("---", "title: T", "params:", "  a:", "    nested: 1", "  b: 2", "format: html", "---")), c("a", "b"))
  expect_identical(.rv_declared_params(write("---", "title: T", "---", "x")), character(0))
  expect_identical(.rv_declared_params(write("---", "params:", "---", "x")), character(0))
  expect_null(.rv_declared_params(write("---", "params: {a: 1}", "---")))
  expect_null(.rv_declared_params(write("no header", "x")))
  expect_null(.rv_declared_params(write("---", "title: unterminated")))
})

test_that("an unreadable header warns that the parameter check was skipped; check_params = FALSE skips it quietly", {
  d <- normalizePath(withr::local_tempdir(), winslash = "/")
  tpl <- file.path(d, "nb.qmd")
  writeLines(c("# no YAML header", "```{python}", "x = 1", "```"), tpl)
  rec <- rv_recorder()
  expect_warning(
    res <- render_variants(tpl, list(a = list(whatever = 1)), renderer = rec$f),
    "could not be read, so the parameters"
  )
  expect_identical(res$status, "rendered")
  expect_no_warning(render_variants(tpl, list(b = list(whatever = 2)), renderer = rec$f, check_params = FALSE))
  # a header that declares none rejects parameters unless the check is off (Jupyter-style templates)
  tpl2 <- rv_template(params = "a: 1")
  writeLines(c("---", "title: T", "---", "x"), tpl2)
  expect_error(render_variants(tpl2, list(v = list(z = 1)), renderer = rec$f), "declares none")
  expect_identical(render_variants(tpl2, list(v = list(z = 1)), renderer = rec$f, check_params = FALSE)$status, "rendered")
})

test_that("render_variants() asks Quarto for self-contained HTML only for html output, unless told not to", {
  tpl <- rv_template()
  rec <- rv_recorder()
  render_variants(tpl, list(a = list()), formats = c("html", "docx"), renderer = rec$f)
  expect_identical(rec$calls$log[[1]]$metadata, list(`embed-resources` = TRUE))
  expect_null(rec$calls$log[[2]]$metadata)
  rec2 <- rv_recorder()
  render_variants(tpl, list(a = list()), self_contained = FALSE, renderer = rec2$f)
  expect_null(rec2$calls$log[[1]]$metadata)
})

test_that("render_variants() stops at the first failure with the results so far, and cleans up", {
  tpl <- rv_template()
  variants <- list(a = list(label = "good"), b = list(label = "bad"), c = list(label = "good"))
  out_dir <- file.path(dirname(tpl), "out")
  rec <- rv_recorder(fail_on = "bad", fail_after_write = TRUE)
  err <- tryCatch(render_variants(tpl, variants, output_dir = out_dir, renderer = rec$f), error = function(e) e)
  expect_s3_class(err, "render_variants_error")
  expect_match(conditionMessage(err), "Rendering variant 'b' \\(html\\) failed: boom")
  expect_identical(err$results$status, c("rendered", "failed", "planned"))
  expect_length(rec$calls$log, 2L)
  # the first output is kept, the partial file of the failed rendering is gone, nothing is left next to the template
  expect_true(file.exists(file.path(out_dir, "analysis_a.html")))
  expect_false(file.exists(file.path(out_dir, "analysis_b.html")))
  expect_identical(list.files(dirname(tpl), all.files = TRUE, no.. = TRUE), c("analysis.qmd", "out"))
})

test_that("render_variants() records failures and goes on when told to", {
  tpl <- rv_template()
  rec <- rv_recorder(fail_on = "bad")
  expect_warning(
    res <- render_variants(tpl, list(a = list(label = "bad"), b = list(label = "good")), renderer = rec$f, stop_on_error = FALSE),
    "1 rendering\\(s\\) failed"
  )
  expect_identical(res$status, c("failed", "rendered"))
  expect_identical(res$error[1], "boom")
  expect_true(is.na(res$error[2]))
})

test_that("render_variants() reports a renderer that wrote nothing", {
  tpl <- rv_template()
  lazy <- function(input, output_format, output_file, execute_params, quiet, metadata = NULL) invisible(NULL)
  expect_error(render_variants(tpl, list(a = list()), renderer = lazy), "did not write")
})

test_that("render_variants() checks its arguments", {
  tpl <- rv_template()
  v <- list(a = list(label = "x"))
  expect_error(render_variants(file.path(dirname(tpl), "none.qmd"), v), "existing .qmd or .Rmd")
  other <- file.path(dirname(tpl), "x.txt")
  writeLines("x", other)
  expect_error(render_variants(other, v), "must be a .qmd or .Rmd")
  expect_error(render_variants(tpl, list()), "named list")
  expect_error(render_variants(tpl, list(list(label = "x"))), "named list")
  expect_error(render_variants(tpl, list(a = list(), A = list())), "must be unique")
  expect_error(render_variants(tpl, list(`a b` = list())), "letters, digits")
  expect_error(render_variants(tpl, list(a = list(1))), "named list of parameters")
  expect_error(render_variants(tpl, v, formats = character()), "formats")
  expect_error(render_variants(tpl, v, formats = "../x"), "Format names may contain only")
  expect_error(render_variants(tpl, v, formats = "a b"), "Format names may contain only")
  expect_error(render_variants(tpl, v, dry_run = NA), "dry_run")
  expect_error(render_variants(tpl, v, stop_on_error = NA), "stop_on_error")
  expect_error(render_variants(tpl, v, self_contained = "yes"), "self_contained")
  expect_error(render_variants(tpl, v, check_params = NULL), "check_params")
  expect_error(render_variants(tpl, v, output_dir = other), "is a file")
  # two formats that write one extension would write one file
  expect_error(render_variants(tpl, v, formats = c("html", "revealjs"), dry_run = TRUE), "same file")
})

test_that("render_variants() works on an .Rmd with the default renderer", {
  skip_if_not_installed("rmarkdown")
  skip_if(!rmarkdown::pandoc_available(), "pandoc is not available")
  tpl <- rv_template(ext = "Rmd", params = c("use_imputed: false"))
  writeLines(c("---", "title: T", "params:", "  use_imputed: false", "---", "", "```{r}", "cat('imputed =', params$use_imputed)", "```"), tpl)
  res <- render_variants(tpl, list(raw = list(use_imputed = FALSE), imp = list(use_imputed = TRUE)))
  expect_identical(res$status, c("rendered", "rendered"))
  expect_match(paste(readLines(res$output[2]), collapse = " "), "imputed = TRUE")
  expect_match(paste(readLines(res$output[1]), collapse = " "), "imputed = FALSE")
  expect_identical(sort(list.files(dirname(tpl))), sort(c("analysis.Rmd", basename(res$output))))
})
