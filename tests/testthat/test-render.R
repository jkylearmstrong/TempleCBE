test_that("render errors on a missing path", {
  skip_if_not(requireNamespace("quarto", quietly = TRUE), "quarto package not available")
  expect_error(render(file.path(tempdir(), "does-not-exist.qmd")), "not found")
  expect_error(render_me(file.path(tempdir(), "does-not-exist.qmd")), "not found")
})

test_that("render renders a single file and reports timing/status", {
  skip_if_not(requireNamespace("quarto", quietly = TRUE), "quarto package not available")

  tmp_dir <- file.path(tempdir(), "test_render_single")
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

  res <- render(qmd_file, formats = "gfm")

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$file[1], "minimal.qmd")
  expect_equal(res$output[1], "gfm")
  expect_equal(res$status[1], "success")
  expect_true(file.exists(file.path(tmp_dir, "minimal.md")))

  # Backwards compatibility alias render_me
  res_alias <- render_me(qmd_file, formats = "gfm")
  expect_equal(res_alias$status[1], "success")
})

test_that("render renders every matching file in a directory", {
  skip_if_not(requireNamespace("quarto", quietly = TRUE), "quarto package not available")

  tmp_dir <- file.path(tempdir(), "test_render_dir")
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

  res <- render(tmp_dir, formats = "gfm")

  expect_equal(nrow(res), 2)
  expect_setequal(res$file, c("a.qmd", "b.qmd"))
  expect_true(all(res$status == "success"))
})

test_that("render accepts a list of file paths", {
  skip_if_not(requireNamespace("quarto", quietly = TRUE), "quarto package not available")

  tmp_dir <- file.path(tempdir(), "test_render_list")
  dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  files <- c()
  for (nm in c("doc1", "doc2")) {
    f <- file.path(tmp_dir, paste0(nm, ".qmd"))
    writeLines(c(
      "---",
      "title: \"Test Doc\"",
      "format: gfm",
      "---",
      "",
      paste0("Content ", nm)
    ), f)
    files <- c(files, f)
  }

  # Pass as a list
  res <- render(as.list(files), formats = "gfm")
  expect_equal(nrow(res), 2)
  expect_setequal(res$file, c("doc1.qmd", "doc2.qmd"))
  expect_true(all(res$status == "success"))
})

test_that("render renders from computeGraph FileOutputs and pipeline lists", {
  skip_if_not(requireNamespace("quarto", quietly = TRUE), "quarto package not available")

  tmp_dir <- file.path(tempdir(), "test_render_computegraph")
  dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  qmd_file <- file.path(tmp_dir, "cg_report.qmd")
  writeLines(c(
    "---",
    "title: \"Compute Graph Report\"",
    "format: gfm",
    "---",
    "",
    "Pipeline content"
  ), qmd_file)

  # Create FileOutputs object specifying output_format = "gfm"
  cg_obj <- create_qmd_renderer(
    name = "cg_report",
    path = qmd_file,
    output_format = "gfm"
  )

  # Passing a single FileOutputs object
  res1 <- render(cg_obj)
  expect_equal(nrow(res1), 1)
  expect_equal(res1$output[1], "gfm")
  expect_equal(res1$status[1], "success")

  # Passing a full pipeline list containing non-renderable raw data and renderable FileOutputs
  raw_data <- FilePath(name = "raw", path = file.path(tmp_dir, "raw.csv"), renders = FALSE)
  pipeline <- list(raw_data, cg_obj)

  res2 <- render(pipeline)
  expect_equal(nrow(res2), 1)
  expect_equal(res2$file[1], "cg_report.qmd")
  expect_equal(res2$status[1], "success")
})

test_that("render extracts formats from document YAML when formats = 'yaml'", {
  skip_if_not(requireNamespace("quarto", quietly = TRUE), "quarto package not available")

  tmp_dir <- file.path(tempdir(), "test_render_yaml")
  dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  qmd_file <- file.path(tmp_dir, "yaml_doc.qmd")
  writeLines(c(
    "---",
    "title: \"YAML Formats Test\"",
    "format: gfm",
    "---",
    "",
    "Testing yaml formats extraction"
  ), qmd_file)

  expect_equal(extract_yaml_formats(qmd_file), "gfm")

  res <- render(qmd_file, formats = "yaml")
  expect_equal(nrow(res), 1)
  expect_equal(res$output[1], "gfm")
  expect_equal(res$status[1], "success")
})

test_that("computeGraph pipeline_config(default_formats) overrides defaults", {
  skip_if_not(requireNamespace("quarto", quietly = TRUE), "quarto package not available")

  tmp_dir <- file.path(tempdir(), "test_render_options")
  dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  qmd_file <- file.path(tmp_dir, "opt_doc.qmd")
  writeLines(c(
    "---",
    "title: \"Options Test\"",
    "format: gfm",
    "---",
    "",
    "Testing option overrides"
  ), qmd_file)

  old_opt <- getOption("pipeline.default_formats")
  on.exit(options(pipeline.default_formats = old_opt), add = TRUE)

  # Set default formats via pipeline_config
  pipeline_config(default_formats = "gfm")
  expect_equal(getOption("pipeline.default_formats"), "gfm")

  # Calling render() without formats argument should use pipeline default "gfm"
  res <- render(qmd_file)
  expect_equal(res$output[1], "gfm")
  expect_equal(res$status[1], "success")
})

test_that("render forwards dots (...) to backend engines", {
  skip_if_not(requireNamespace("quarto", quietly = TRUE), "quarto package not available")

  tmp_dir <- file.path(tempdir(), "test_render_dots")
  dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  qmd_file <- file.path(tmp_dir, "dots_doc.qmd")
  writeLines(c(
    "---",
    "title: \"Dots Test\"",
    "format: gfm",
    "---",
    "",
    "Testing dots"
  ), qmd_file)

  # Passing quiet = TRUE via ...
  res <- render(qmd_file, formats = "gfm", quiet = TRUE)
  expect_equal(res$status[1], "success")
})

test_that("render renders .Rmd files using rmarkdown or quarto", {
  skip_if_not(requireNamespace("rmarkdown", quietly = TRUE), "rmarkdown package not available")

  tmp_dir <- file.path(tempdir(), "test_render_rmd")
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

  res <- render(rmd_file, formats = "html", engine = "rmarkdown")

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$file[1], "report.Rmd")
  expect_equal(res$output[1], "html")
  expect_equal(res$status[1], "success")
  expect_true(file.exists(file.path(tmp_dir, "report.html")))
})
