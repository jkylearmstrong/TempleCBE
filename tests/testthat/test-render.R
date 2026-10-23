withr::local_options(templecbe_suppress_renv_warning = TRUE)

test_that("render errors on a missing path", {
  skip_if_not(requireNamespace("quarto", quietly = TRUE), "quarto package not available")
  expect_error(render_pipeline(file.path(tempdir(), "does-not-exist.qmd")), "not found")
  expect_error(render_pipeline(file.path(tempdir(), "does-not-exist.qmd")), "not found")
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

  res <- render_pipeline(qmd_file, formats = "gfm")

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$file[1], "minimal.qmd")
  expect_equal(res$output[1], "gfm")
  expect_equal(res$status[1], "success")
  expect_true(file.exists(file.path(tmp_dir, "minimal.md")))

  # Backwards compatibility alias render_me
  res_alias <- render_pipeline(qmd_file, formats = "gfm")
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

  res <- render_pipeline(tmp_dir, formats = "gfm")

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
  res <- render_pipeline(as.list(files), formats = "gfm")
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
  res1 <- render_pipeline(cg_obj)
  expect_equal(nrow(res1), 1)
  expect_equal(res1$output[1], "gfm")
  expect_equal(res1$status[1], "success")

  # Passing a full pipeline list containing non-renderable raw data and renderable FileOutputs
  file.create(file.path(tmp_dir, "raw.csv"))
  raw_data <- FilePath(name = "raw", path = file.path(tmp_dir, "raw.csv"), renders = FALSE)
  pipeline <- list(raw_data, cg_obj)

  res2 <- render_pipeline(pipeline, only_stale = FALSE)
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

  res <- render_pipeline(qmd_file, formats = "yaml")
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
  res <- render_pipeline(qmd_file)
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
  res <- render_pipeline(qmd_file, formats = "gfm", quiet = TRUE)
  expect_equal(res$status[1], "success")
})

test_that("render renders .Rmd files using rmarkdown or quarto", {
  skip_if_not(requireNamespace("rmarkdown", quietly = TRUE) && requireNamespace("jquerylib", quietly = TRUE), "rmarkdown and jquerylib packages not available")

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

  res <- render_pipeline(rmd_file, formats = "html", engine = "rmarkdown")

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$file[1], "report.Rmd")
  expect_equal(res$output[1], "html")
  expect_equal(res$status[1], "success")
  expect_true(file.exists(file.path(tmp_dir, "report.html")))
})

test_that("render supports dry_run = TRUE without modifying disk", {
  tmp_dir <- file.path(tempdir(), "test_render_dry_run")
  dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  qmd_file <- file.path(tmp_dir, "dry.qmd")
  writeLines(c("---", "title: 'Dry'", "---"), qmd_file)

  out <- capture.output({
    res <- render_pipeline(qmd_file, formats = "gfm", dry_run = TRUE)
  })

  expect_s3_class(res, "data.frame")
  expect_equal(res$status[1], "dry_run")
  expect_true(any(grepl("dry.qmd", out)))
  expect_false(file.exists(file.path(tmp_dir, "dry.md")))
})

test_that("render cache = 'refresh' wipes document cache and files directories", {
  tmp_dir <- file.path(tempdir(), "test_render_cache_refresh")
  dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  qmd_file <- file.path(tmp_dir, "cached.qmd")
  writeLines(c("---", "title: 'Cached'", "---"), qmd_file)

  cache_dir <- file.path(tmp_dir, "cached_cache")
  files_dir <- file.path(tmp_dir, "cached_files")
  dir.create(cache_dir, showWarnings = FALSE)
  dir.create(files_dir, showWarnings = FALSE)
  file.create(file.path(cache_dir, "dummy.txt"))

  expect_true(dir.exists(cache_dir))
  expect_true(dir.exists(files_dir))

  # Render with dry_run or execution
  render_pipeline(qmd_file, formats = "gfm", cache = "refresh", dry_run = TRUE)
  # When dry_run is TRUE, cache wiping happens during execution:
  expect_true(dir.exists(cache_dir)) # dry_run didn't wipe

  # Now simulate render_single call by running render on dummy Rmd
  if (requireNamespace("rmarkdown", quietly = TRUE) && requireNamespace("jquerylib", quietly = TRUE)) {
    rmd_file <- file.path(tmp_dir, "dummy_rmd.Rmd")
    writeLines(c("---", "title: 'Rmd Cache'", "output: html_document", "---"), rmd_file)
    r_cache <- file.path(tmp_dir, "dummy_rmd_cache")
    r_files <- file.path(tmp_dir, "dummy_rmd_files")
    dir.create(r_cache, showWarnings = FALSE)
    dir.create(r_files, showWarnings = FALSE)

    render_pipeline(rmd_file, formats = "html", engine = "rmarkdown", cache = "refresh")
    expect_false(dir.exists(r_cache))
    expect_false(dir.exists(r_files))
  }
})

test_that("render resume skips completed targets", {
  skip_if_not(requireNamespace("rmarkdown", quietly = TRUE) && requireNamespace("jquerylib", quietly = TRUE))

  tmp_dir <- file.path(tempdir(), "test_render_resume")
  dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  rmd1 <- file.path(tmp_dir, "r1.Rmd")
  rmd2 <- file.path(tmp_dir, "r2.Rmd")
  writeLines(c("---", "title: '1'", "output: html_document", "---"), rmd1)
  writeLines(c("---", "title: '2'", "output: html_document", "---"), rmd2)

  prev_res <- data.frame(
    file = "r1.Rmd",
    output = "html",
    duration = 1.23,
    status = "success",
    stringsAsFactors = FALSE
  )

  msgs <- capture_messages({
    res <- render_pipeline(list(rmd1, rmd2), formats = "html", engine = "rmarkdown", resume = prev_res)
  })

  expect_true(any(grepl("Resuming: skipping already completed 'r1.Rmd'", msgs)))
  expect_equal(nrow(res), 2)
  expect_equal(res$duration[res$file == "r1.Rmd"], 1.23)
  expect_equal(res$status, c("success", "success"))
})

test_that("render on_error = 'stop' halts immediately on failure", {
  tmp_dir <- file.path(tempdir(), "test_render_error_stop")
  dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  bad_file <- file.path(tmp_dir, "bad.qmd")
  # Syntax error in R code block that fails during rendering
  writeLines(c(
    "---",
    "title: 'Bad'",
    "format: gfm",
    "---",
    "",
    "```{r}",
    "stop('Fatal document error')",
    "```"
  ), bad_file)

  skip_if_not(requireNamespace("quarto", quietly = TRUE))

  expect_error(
    render_pipeline(bad_file, formats = "gfm", on_error = "stop"),
    "Error rendering"
  )
})

test_that("render writes render_manifest.json beside outputs", {
  skip_if_not(requireNamespace("rmarkdown", quietly = TRUE) && requireNamespace("jquerylib", quietly = TRUE))
  skip_if_not(requireNamespace("jsonlite", quietly = TRUE))

  tmp_dir <- file.path(tempdir(), "test_render_manifest")
  dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  rmd_file <- file.path(tmp_dir, "doc.Rmd")
  writeLines(c("---", "title: 'Manifest Test'", "output: html_document", "---"), rmd_file)

  res <- render_pipeline(rmd_file, formats = "html", engine = "rmarkdown", manifest = TRUE, renv_guard = FALSE)

  manifest_file <- file.path(tmp_dir, "render_manifest.json")
  expect_true(file.exists(manifest_file))

  manifest_data <- jsonlite::read_json(manifest_file)
  expect_true(!is.null(manifest_data$templecbe_version))
  expect_true(!is.null(manifest_data$timestamp))
  expect_true(!is.null(manifest_data$future_plan))
  expect_true(!is.null(manifest_data$per_node_wall_time))
  expect_equal(manifest_data$per_node_wall_time[[1]]$file, "doc.Rmd")
})

test_that("check_renv_guard warns when renv.lock exists but renv is inactive", {
  tmp_dir <- file.path(tempdir(), "test_renv_guard")
  dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  lock_file <- file.path(tmp_dir, "renv.lock")
  writeLines(c("{\"Packages\": {}}"), lock_file)

  # renv is genuinely active when the suite runs from a project with an
  # activated renv (the Docker image, a local checkout): state "inactive" here.
  testthat::local_mocked_bindings(renv_project_active = function() FALSE)
  withr::local_envvar(list(RENV_PROJECT = ""))
  withr::local_options(templecbe_suppress_renv_warning = FALSE)
  expect_warning(
    check_renv_guard(project_root = tmp_dir),
    "renv is inactive"
  )
})


