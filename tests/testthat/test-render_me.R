test_that("render_me errors on a missing path", {
  skip_if_not(requireNamespace("quarto", quietly = TRUE), "quarto package not available")
  expect_error(render_me(file.path(tempdir(), "does-not-exist.qmd")), "not found")
})

test_that("render_me renders a single file and reports timing/status", {
  skip_if_not(requireNamespace("quarto", quietly = TRUE), "quarto package not available")

  tmp_dir <- file.path(tempdir(), "test_render_me_single")
  dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  qmd_file <- file.path(tmp_dir, "minimal.qmd")
  writeLines(c(
    "---",
    "title: \"Minimal Test\"",
    "format: gfm",
    "---",
    "",
    "## Hello World"
  ), qmd_file)

  res <- render_me(qmd_file, formats = "gfm")

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$file[1], "minimal.qmd")
  expect_equal(res$output[1], "gfm")
  expect_equal(res$status[1], "success")
  expect_true(file.exists(file.path(tmp_dir, "minimal.md")))
})

test_that("render_me renders every matching file in a directory", {
  skip_if_not(requireNamespace("quarto", quietly = TRUE), "quarto package not available")

  tmp_dir <- file.path(tempdir(), "test_render_me_dir")
  dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  for (nm in c("a", "b")) {
    writeLines(c(
      "---",
      "title: \"Test\"",
      "format: gfm",
      "---",
      "",
      paste0("## ", nm)
    ), file.path(tmp_dir, paste0(nm, ".qmd")))
  }

  res <- render_me(tmp_dir, formats = "gfm")

  expect_equal(nrow(res), 2)
  expect_setequal(res$file, c("a.qmd", "b.qmd"))
  expect_true(all(res$status == "success"))
})

test_that("render_me renders .Rmd files using rmarkdown or quarto", {
  skip_if_not(requireNamespace("rmarkdown", quietly = TRUE), "rmarkdown package not available")

  tmp_dir <- file.path(tempdir(), "test_render_me_rmd")
  dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  rmd_file <- file.path(tmp_dir, "report.Rmd")
  writeLines(c(
    "---",
    "title: \"Test Rmd\"",
    "output: html_document",
    "---",
    "",
    "## Hello from R Markdown"
  ), rmd_file)

  res <- render_me(rmd_file, formats = "html", engine = "rmarkdown")

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$file[1], "report.Rmd")
  expect_equal(res$output[1], "html")
  expect_equal(res$status[1], "success")
  expect_true(file.exists(file.path(tmp_dir, "report.html")))
})
