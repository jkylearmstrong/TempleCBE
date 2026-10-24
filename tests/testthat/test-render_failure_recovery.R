withr::local_options(templecbe_suppress_renv_warning = TRUE)

make_docs <- function(dir) {
  bad <- file.path(dir, "a_bad.Rmd")
  good <- file.path(dir, "b_good.Rmd")
  writeLines(c("---", "title: 'Bad'", "output: html_document", "---", "", "```{r}", "stop('Fatal document error')", "```"), bad)
  writeLines(c("---", "title: 'Good'", "output: html_document", "---", "", "Fine."), good)
  list(bad = bad, good = good)
}

skip_if_no_rmd <- function() {
  skip_if_not(requireNamespace("rmarkdown", quietly = TRUE) && requireNamespace("jquerylib", quietly = TRUE))
  skip_if(!nzchar(Sys.which("pandoc")) && !rmarkdown::pandoc_available(), "pandoc is not available")
}

test_that("render_pipeline(on_error = 'continue') reports the failed document and still renders the rest", {
  skip_if_no_rmd()
  dir <- withr::local_tempdir()
  docs <- make_docs(dir)
  res <- suppressWarnings(suppressMessages(
    render_pipeline(list(docs$bad, docs$good), formats = "html", engine = "rmarkdown", on_error = "continue",
                    manifest = FALSE, renv_guard = FALSE)
  ))
  expect_equal(nrow(res), 2L)
  expect_false(identical(res$status[res$file == "a_bad.Rmd"], "success"))
  expect_equal(res$status[res$file == "b_good.Rmd"], "success")
  expect_true(file.exists(file.path(dir, "b_good.html")))
  expect_false(file.exists(file.path(dir, "a_bad.html")))
})

test_that("render_pipeline(on_error = 'stop') stops at the first failed document", {
  skip_if_no_rmd()
  dir <- withr::local_tempdir()
  docs <- make_docs(dir)
  expect_error(
    suppressWarnings(suppressMessages(
      render_pipeline(list(docs$bad, docs$good), formats = "html", engine = "rmarkdown", on_error = "stop",
                      manifest = FALSE, renv_guard = FALSE)
    )),
    "Error rendering"
  )
  expect_false(file.exists(file.path(dir, "b_good.html")))
})

test_that("a failing document does not stop the manifest from being written", {
  skip_if_no_rmd()
  skip_if_not_installed("jsonlite")
  dir <- withr::local_tempdir()
  docs <- make_docs(dir)
  suppressWarnings(suppressMessages(
    render_pipeline(list(docs$bad, docs$good), formats = "html", engine = "rmarkdown", on_error = "continue",
                    manifest = TRUE, renv_guard = FALSE)
  ))
  manifest <- file.path(dir, "render_manifest.json")
  expect_true(file.exists(manifest))
  files <- vapply(jsonlite::read_json(manifest)$per_node_wall_time, function(x) x$file, "")
  expect_true(all(c("a_bad.Rmd", "b_good.Rmd") %in% files))
})

test_that("render_pipeline() with a missing document in a list says which one", {
  skip_if_no_rmd()
  dir <- withr::local_tempdir()
  docs <- make_docs(dir)
  expect_error(render_pipeline(list(docs$good, file.path(dir, "nope.Rmd")), formats = "html", engine = "rmarkdown"), "not found")
})
