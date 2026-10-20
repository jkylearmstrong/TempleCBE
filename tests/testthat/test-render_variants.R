rv_template <- function(env = parent.frame(), ext = "qmd", params = c("use_imputed: false", "label: base")) {
  d <- withr::local_tempdir(.local_envir = env)
  path <- file.path(d, paste0("analysis.", ext))
  writeLines(c("---", "title: T", "format: html", "params:", paste0("  ", params), "---", "", "Body."), path)
  path
}

# A renderer that writes the file it is asked for and records what it was given
rv_recorder <- function(fail_on = NULL) {
  calls <- new.env()
  calls$log <- list()
  f <- function(input, output_format, output_file, execute_params, quiet) {
    calls$log[[length(calls$log) + 1L]] <- list(
      input = input, format = output_format, file = output_file, params = execute_params, quiet = quiet
    )
    if (!is.null(fail_on) && identical(execute_params$label, fail_on)) stop("boom", call. = FALSE)
    writeLines(paste(output_format, jsonlite::toJSON(execute_params, auto_unbox = TRUE)), file.path(dirname(input), output_file))
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
  # nothing left behind next to the template
  expect_false(file.exists(file.path(dirname(tpl), "analysis_original.html")))
  expect_identical(vapply(rec$calls$log, function(x) x$file, ""), c("analysis_original.html", "analysis_imputed.html"))
  expect_identical(rec$calls$log[[2]]$params, list(use_imputed = TRUE, label = "imp"))
  expect_true(rec$calls$log[[1]]$quiet)
  expect_match(readLines(res$output[2]), "\"use_imputed\":true")
})

test_that("render_variants() leaves outputs in the template's folder by default and replaces old ones", {
  tpl <- rv_template()
  old <- file.path(dirname(tpl), "analysis_a.html")
  writeLines("old", old)
  res <- render_variants(tpl, list(a = list(label = "x")), renderer = rv_recorder()$f)
  expect_identical(normalizePath(res$output, winslash = "/"), normalizePath(old, winslash = "/"))
  expect_false(identical(readLines(old), "old"))
})

test_that("render_variants() refuses a parameter the template does not declare, before rendering anything", {
  skip_if_not_installed("rmarkdown")
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

test_that("render_variants() stops at the first failure, or records failures when told to", {
  tpl <- rv_template()
  variants <- list(a = list(label = "bad"), b = list(label = "good"))
  rec <- rv_recorder(fail_on = "bad")
  expect_error(render_variants(tpl, variants, renderer = rec$f), "Rendering variant 'a' \\(html\\) failed: boom")
  expect_length(rec$calls$log, 1L)

  rec2 <- rv_recorder(fail_on = "bad")
  expect_warning(res <- render_variants(tpl, variants, renderer = rec2$f, stop_on_error = FALSE), "1 rendering\\(s\\) failed")
  expect_identical(res$status, c("failed", "rendered"))
  expect_identical(res$error[1], "boom")
  expect_true(is.na(res$error[2]))
})

test_that("render_variants() reports a renderer that wrote nothing", {
  tpl <- rv_template()
  lazy <- function(input, output_format, output_file, execute_params, quiet) invisible(NULL)
  expect_error(render_variants(tpl, list(a = list()), renderer = lazy), "did not write analysis_a.html")
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
  expect_error(render_variants(tpl, v, formats = c("html", "html"), dry_run = NA), "dry_run")
  expect_error(render_variants(tpl, v, stop_on_error = NA), "stop_on_error")
  # the same variant and format twice cannot clash, but two formats with one extension can
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
})
