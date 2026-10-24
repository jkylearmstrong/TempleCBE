rpp_template <- function(env = parent.frame()) {
  d <- normalizePath(withr::local_tempdir(.local_envir = env), winslash = "/")
  path <- file.path(d, "mixed_models.qmd")
  writeLines(c("---", "title: T", "format: html", "params:", "  impute: false", "  data_suffix: original",
               "---", "", "Body."), path)
  path
}

rpp_renderer <- function() {
  calls <- new.env()
  calls$log <- list()
  f <- function(input, output_format, output_file, execute_params, quiet, metadata = NULL) {
    calls$log[[length(calls$log) + 1L]] <- list(file = output_file, params = execute_params)
    writeLines("x", file.path(dirname(input), output_file))
    invisible(NULL)
  }
  list(f = f, calls = calls)
}

test_that("both parameter sets go to the one template, under the output names", {
  tpl <- rpp_template()
  rec <- rpp_renderer()
  out_dir <- file.path(dirname(tpl), "reports")
  res <- render_parameterized_pair(
    tpl,
    params_original = list(data_suffix = "original", impute = FALSE),
    params_imputed = list(data_suffix = "imputed", impute = TRUE),
    output_names = c("mixed_models_original", "mixed_models_imputed"),
    output_dir = out_dir, renderer = rec$f
  )
  expect_identical(res$variant, c("mixed_models_original", "mixed_models_imputed"))
  expect_identical(unique(res$status), "rendered")
  expect_identical(
    basename(res$output),
    c("mixed_models_mixed_models_original.html", "mixed_models_mixed_models_imputed.html")
  )
  expect_true(all(file.exists(res$output)))
  expect_identical(rec$calls$log[[1]]$params, list(data_suffix = "original", impute = FALSE))
  expect_identical(rec$calls$log[[2]]$params, list(data_suffix = "imputed", impute = TRUE))
})

test_that("the default output names are original and imputed, and formats are passed on", {
  tpl <- rpp_template()
  plan <- render_parameterized_pair(
    tpl, list(impute = FALSE), list(impute = TRUE), formats = c("html", "docx"), dry_run = TRUE
  )
  expect_identical(plan$variant, c("original", "original", "imputed", "imputed"))
  expect_identical(plan$format, c("html", "docx", "html", "docx"))
  expect_identical(unique(plan$status), "planned")
})

test_that("parameter sets that name different parameters are refused before anything renders", {
  tpl <- rpp_template()
  rec <- rpp_renderer()
  expect_error(
    render_parameterized_pair(tpl, list(impute = FALSE, data_suffix = "a"), list(impute = TRUE), renderer = rec$f),
    "Only in params_original: data_suffix"
  )
  expect_error(
    render_parameterized_pair(tpl, list(impute = FALSE), list(impute = TRUE, data_suffix = "b"), renderer = rec$f),
    "Only in params_imputed: data_suffix"
  )
  expect_length(rec$calls$log, 0L)
  # the order of the names does not matter
  expect_no_error(render_parameterized_pair(
    tpl, list(impute = FALSE, data_suffix = "a"), list(data_suffix = "b", impute = TRUE), dry_run = TRUE
  ))
})

test_that("parameters must be named lists", {
  tpl <- rpp_template()
  expect_error(render_parameterized_pair(tpl, "x", list(impute = TRUE)), "`params_original` must be a named list")
  expect_error(render_parameterized_pair(tpl, list(impute = TRUE), list(TRUE)), "`params_imputed` must be a named list")
  expect_error(
    render_parameterized_pair(tpl, list(impute = TRUE, impute = FALSE), list(impute = TRUE, impute = FALSE)),
    "named twice"
  )
})

test_that("output names must be two different names", {
  tpl <- rpp_template()
  p <- list(impute = FALSE)
  expect_error(render_parameterized_pair(tpl, p, p, output_names = "a"), "two different")
  expect_error(render_parameterized_pair(tpl, p, p, output_names = c("a", "a")), "two different")
  expect_error(render_parameterized_pair(tpl, p, p, output_names = c("a", "A")), "two different")
  expect_error(render_parameterized_pair(tpl, p, p, output_names = c("a", "")), "two different")
  expect_error(render_parameterized_pair(tpl, p, p, output_names = c("a", NA)), "two different")
  expect_error(render_parameterized_pair(tpl, p, p, output_names = c("a", "b c")), "`output_names` may contain only")
})

test_that("a parameter the template does not declare is still caught by render_variants()", {
  tpl <- rpp_template()
  expect_error(
    render_parameterized_pair(tpl, list(impute = FALSE, typo = 1), list(impute = TRUE, typo = 2), dry_run = TRUE),
    "does not declare"
  )
})
