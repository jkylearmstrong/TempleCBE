test_that("FilePath S4 class validates arguments and slots", {
  # Valid construction
  f1 <- FilePath("raw_data", "data/raw.csv", stage = "01_raw")
  expect_s4_class(f1, "FilePath")
  expect_equal(f1@name, "raw_data")
  expect_equal(f1@path, "data/raw.csv")
  expect_false(f1@renders)
  expect_equal(f1@stage, "01_raw")

  # Argument validation
  expect_error(FilePath("", "data/raw.csv"), "single non-empty character string")
  expect_error(FilePath("raw", ""), "single non-empty character string")
  expect_error(FilePath(123, "data/raw.csv"), "single non-empty character string")
  expect_error(FilePath("raw", 123), "single non-empty character string")
})

test_that("FileUses S4 class validates dependencies and detects self-dependencies", {
  f_raw <- FilePath("raw_data", "data/raw.csv")
  
  # Valid FileUses
  f_script <- FileUses("process_script", "scripts/clean.R", dependencies = list(f_raw))
  expect_s4_class(f_script, "FileUses")
  expect_length(f_script@dependencies, 1)

  # Self-dependency error
  expect_error(
    FileUses("clean_data", "data/clean.csv", dependencies = list(FilePath("clean_data", "data/clean.csv"))),
    "cannot depend on itself"
  )

  # Non-FilePath dependency error
  expect_error(
    FileUses("clean_data", "data/clean.csv", dependencies = list("not_a_filepath_object")),
    "does not inherit from FilePath"
  )
})

test_that("FileOutputs S4 class validates output list and detects self-outputs", {
  f_raw <- FilePath("raw_data", "data/raw.csv")
  f_out <- FilePath("derived_data", "data/clean.csv")

  # Valid FileOutputs
  f_proc <- FileOutputs(
    name = "clean_script",
    path = "scripts/clean.R",
    dependencies = list(f_raw),
    output = list(f_out),
    renders = FALSE
  )
  expect_s4_class(f_proc, "FileOutputs")
  expect_length(f_proc@output, 1)

  # Self-output error
  expect_error(
    FileOutputs("clean_script", "scripts/clean.R", output = list(FilePath("clean_script", "scripts/clean.R"))),
    "cannot output itself"
  )

  # Non-FilePath output error
  expect_error(
    FileOutputs("clean_script", "scripts/clean.R", output = list("string_output")),
    "does not inherit from FilePath"
  )
})

test_that("pipeline_config sets and gets global configuration options", {
  # pipeline_config() cannot unset an option (NULL means "leave it alone"), so a
  # restore through it would leave default_formats set for every later test file.
  # Start from unset and let withr put back exactly what was there.
  withr::local_options(
    pipeline.study_name = NULL,
    pipeline.stage_labels = NULL,
    pipeline.stage_colors = NULL,
    pipeline.default_formats = NULL
  )
  expect_equal(pipeline_config()$study_name, "Computational Pipeline")
  expect_null(pipeline_config()$default_formats)

  cfg <- pipeline_config(
    study_name = "Temple Oncology",
    stage_labels = c("01_raw" = "Raw Data", "02_clean" = "Clean Data"),
    stage_colors = c("01_raw" = "#FF0000", "02_clean" = "#00FF00"),
    default_formats = c("pdf", "html")
  )

  expect_equal(cfg$study_name, "Temple Oncology")
  expect_equal(cfg$stage_labels[["01_raw"]], "Raw Data")
  expect_equal(cfg$default_formats, c("pdf", "html"))
})

test_that("get_render_plan validates inputs, duplicate names, and circular dependencies", {
  # Empty input
  expect_equal(get_render_plan(list()), list())

  # Invalid input types
  expect_error(
    get_render_plan(list("not_a_filepath")),
    "must inherit from FilePath"
  )

  # Duplicate node names
  f1 <- FilePath("dup_name", "path1.csv")
  f2 <- FilePath("dup_name", "path2.csv")
  expect_error(
    get_render_plan(list(f1, f2)),
    "Duplicate pipeline object name"
  )

  # Circular dependency detection (Cycle: A -> B -> A)
  node_a_ref <- FilePath("node_b", "b.R")
  node_b_ref <- FilePath("node_a", "a.R")

  node_a <- FileOutputs("node_a", "a.R", dependencies = list(node_a_ref), renders = TRUE)
  node_b <- FileOutputs("node_b", "b.R", dependencies = list(node_b_ref), renders = TRUE)

  # a.R and b.R are not real files, which get_render_plan() reports before it finds the cycle
  expect_warning(
    expect_error(
      get_render_plan(list(node_a, node_b)),
      "Circular dependency detected in computational pipeline graph"
    ),
    "not found on disk"
  )
})

test_that("get_render_plan determines correct topological order and staleness propagation", {
  tmp_dir <- withr::local_tempdir()

  raw_file <- file.path(tmp_dir, "raw.csv")
  out_csv <- file.path(tmp_dir, "clean.csv")
  qmd_file <- file.path(tmp_dir, "report.qmd")
  report_pdf <- file.path(tmp_dir, "report.pdf")

  # Fixed modification times, ten seconds apart. Sleeping between writes would
  # make the result depend on the clock and on the timestamp resolution of the
  # file system (FAT keeps 2 seconds, some network drives 1 second).
  t0 <- as.POSIXct("2020-01-01 00:00:00", tz = "UTC")
  stamp <- function(path, seconds) stopifnot(Sys.setFileTime(path, t0 + seconds))

  writeLines("raw data", raw_file)
  stamp(raw_file, 0)
  writeLines("clean data", out_csv)
  stamp(out_csv, 10)
  writeLines("qmd content", qmd_file)
  stamp(qmd_file, 20)
  writeLines("pdf output", report_pdf)
  stamp(report_pdf, 30)

  f_raw <- FilePath("raw_data", raw_file)
  f_clean <- FilePath("clean_csv", out_csv)
  f_pdf <- FilePath("report_pdf", report_pdf)

  qmd_node <- FileOutputs(
    name = "report_qmd",
    path = qmd_file,
    dependencies = list(f_clean),
    output = list(f_pdf),
    renders = TRUE
  )

  all_objs <- list(f_raw, f_clean, qmd_node, f_pdf)

  # 1. When everything is up-to-date, render plan should be empty
  plan_clean <- get_render_plan(all_objs)
  expect_equal(length(plan_clean), 0)

  # 2. Modify raw data (making clean.csv and report.qmd stale)
  writeLines("new raw data content", raw_file)
  stamp(raw_file, 40)

  # Re-instantiate with updated mtime
  f_raw_new <- FilePath("raw_data", raw_file)
  # qmd_node outputs report_pdf, but report_pdf is now older than raw_file if qmd depends on it
  qmd_node_stale <- FileOutputs(
    name = "report_qmd",
    path = qmd_file,
    dependencies = list(f_raw_new),
    output = list(f_pdf),
    renders = TRUE
  )

  plan_stale <- get_render_plan(list(f_raw_new, qmd_node_stale, f_pdf))
  expect_equal(length(plan_stale), 1)
  expect_equal(names(plan_stale), "report_qmd")
})

test_that("as_pipeline_graph and pipeline_summary generate graph representations", {
  f1 <- FilePath("input_data", "data/raw.csv", stage = "01_raw", artifact_role = "raw_data")
  f2_out <- FilePath("output_csv", "data/clean.csv", stage = "02_clean", artifact_role = "derived_data")
  f2 <- FileOutputs(
    name = "cleaner_script",
    path = "scripts/clean.R",
    dependencies = list(f1),
    output = list(f2_out),
    stage = "02_clean",
    artifact_role = "helper_script",
    renders = FALSE
  )

  pipe_list <- list(f1, f2, f2_out)
  tg <- as_pipeline_graph(pipe_list)

  expect_s3_class(tg, "tbl_graph")
  expect_equal(igraph::vcount(tg), 3)
  expect_equal(igraph::ecount(tg), 2)

  # pipeline_summary returns stage summary invisibly
  summ <- pipeline_summary(tg)
  expect_s3_class(summ, "tbl_df")
  expect_true("stage" %in% names(summ))
})
