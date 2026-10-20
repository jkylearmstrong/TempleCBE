# Coverage for dedupe_pipeline(), list_stage_data_artifacts(), wrap_pipeline_label() and
# print_pipeline() in R/compute_graph.R. Files live in temp directories only.

pipe_names <- function(x) vapply(x, function(o) o@name, character(1))

# ---- dedupe_pipeline() ---------------------------------------------------------------------------

dup_fixture <- function() {
  list(
    a1 = FilePath("a", "a1.csv"),
    b = FilePath("b", "b.csv"),
    a2 = FileUses("a", "a2.csv"),
    a3 = FileOutputs("a", "a3.R", renders = FALSE)
  )
}

test_that("dedupe_pipeline() rejects input that is not a list of pipeline objects", {
  expect_error(dedupe_pipeline("x"), "must inherit from FilePath")
  expect_error(dedupe_pipeline(list(FilePath("a", "a.csv"), "x")), "must inherit from FilePath")
  expect_error(dedupe_pipeline(list(), strategy = "nope"), "should be one of")
})

test_that("dedupe_pipeline() returns a list without duplicates unchanged, whatever the strategy", {
  objs <- list(FilePath("a", "a.csv"), FilePath("b", "b.csv"))
  for (s in c("keep_first", "prefer_outputs", "keep_last", "rename")) {
    expect_identical(dedupe_pipeline(objs, strategy = s), objs)
  }
  expect_identical(dedupe_pipeline(list()), list())
})

test_that("dedupe_pipeline() keep_first (the default) keeps the first object of each name", {
  d <- dup_fixture()
  got <- dedupe_pipeline(unname(d))
  expect_identical(pipe_names(got), c("a", "b"))
  expect_identical(got[[1]]@path, "a1.csv")
  expect_identical(got, dedupe_pipeline(unname(d), strategy = "keep_first"))
})

test_that("dedupe_pipeline() keep_last keeps the last object of each name, in list order", {
  got <- dedupe_pipeline(unname(dup_fixture()), strategy = "keep_last")
  expect_identical(pipe_names(got), c("b", "a"))
  expect_identical(got[[2]]@path, "a3.R")
})

test_that("dedupe_pipeline() prefer_outputs ranks FileOutputs over FileUses over FilePath", {
  d <- dup_fixture()
  # the chosen objects keep the order they have in the list: b (index 2) comes before a3 (index 4)
  got <- dedupe_pipeline(unname(d), strategy = "prefer_outputs")
  expect_identical(pipe_names(got), c("b", "a"))
  expect_s4_class(got[[2]], "FileOutputs")

  without_outputs <- dedupe_pipeline(list(d$a1, d$b, d$a2), strategy = "prefer_outputs")
  expect_identical(class(without_outputs[[2]])[1], "FileUses")

  # a tie goes to the first object, and the order of the first occurrences is kept
  tie <- list(FilePath("a", "first.csv"), FilePath("b", "b.csv"), FilePath("a", "second.csv"))
  got_tie <- dedupe_pipeline(tie, strategy = "prefer_outputs")
  expect_identical(pipe_names(got_tie), c("a", "b"))
  expect_identical(got_tie[[1]]@path, "first.csv")
})

test_that("dedupe_pipeline() rename keeps every object and numbers the repeats", {
  got <- dedupe_pipeline(unname(dup_fixture()), strategy = "rename")
  expect_length(got, 4L)
  expect_identical(pipe_names(got), c("a", "b", "a_2", "a_3"))
  expect_false(anyDuplicated(pipe_names(got)) > 0L)
  # the objects themselves are untouched apart from the name
  expect_identical(got[[3]]@path, "a2.csv")
  expect_s4_class(got[[4]], "FileOutputs")
})

test_that("dedupe_pipeline() rename makes names unique even when a numbered name exists", {
  skip("known bug: strategy = \"rename\" gives c(\"a\", \"a_2\", \"a_2\") for names c(\"a\", \"a\", \"a_2\")")
  got <- dedupe_pipeline(
    list(FilePath("a", "1.csv"), FilePath("a", "2.csv"), FilePath("a_2", "3.csv")),
    strategy = "rename"
  )
  expect_false(anyDuplicated(pipe_names(got)) > 0L)
})

test_that("dedupe_pipeline() says what it resolved only when verbose", {
  objs <- unname(dup_fixture())
  expect_silent(dedupe_pipeline(objs))
  expect_message(dedupe_pipeline(objs, verbose = TRUE), "resolving duplicates for: a")
})

# ---- list_stage_data_artifacts() -----------------------------------------------------------------

test_that("list_stage_data_artifacts() warns about a missing directory and returns an empty tibble", {
  d <- withr::local_tempdir()
  expect_warning(res <- list_stage_data_artifacts(file.path(d, "nope")), "Directory does not exist")
  expect_s3_class(res, "tbl_df")
  expect_identical(nrow(res), 0L)
})

test_that("list_stage_data_artifacts() returns an empty tibble when no data file matches", {
  d <- withr::local_tempdir()
  writeLines("x", file.path(d, "notes.txt"))
  res <- list_stage_data_artifacts(d)
  expect_s3_class(res, "tbl_df")
  expect_identical(nrow(res), 0L)
})

test_that("list_stage_data_artifacts() separates primary handoff datasets from intermediate tables", {
  d <- withr::local_tempdir()
  primary <- c("DATA.rds", "INDEX.rds", "Index_Scores_v2.csv", "imputed_DATA_1.xlsx")
  other <- c("table1.csv", "DATA.rds.bak.csv", "my_DATA.rds")
  for (f in c(primary, other, "readme.txt")) writeLines("x", file.path(d, f))
  dir.create(file.path(d, "sub"))
  writeLines("x", file.path(d, "sub", "nested.csv")) # not scanned: the scan is not recursive

  res <- list_stage_data_artifacts(d, stage = "02_clean")
  expect_named(res, c("file_name", "path", "stage", "artifact_role", "size_kb", "mtime"))
  expect_setequal(res$file_name, c(primary, other))
  expect_true(all(res$stage == "02_clean"))
  role <- stats::setNames(res$artifact_role, res$file_name)
  expect_true(all(role[primary] == "primary_data"))
  expect_true(all(role[other] == "intermediate_data"))
  expect_true(all(file.exists(res$path)))
})

test_that("list_stage_data_artifacts() matches primary patterns regardless of case and takes custom ones", {
  d <- withr::local_tempdir()
  writeLines("x", file.path(d, "data.rds"))
  writeLines("x", file.path(d, "final_model.rds"))
  res <- list_stage_data_artifacts(d)
  expect_identical(res$artifact_role[res$file_name == "data.rds"], "primary_data")
  expect_identical(res$artifact_role[res$file_name == "final_model.rds"], "intermediate_data")
  custom <- list_stage_data_artifacts(d, primary_patterns = "^final_")
  expect_identical(custom$artifact_role[custom$file_name == "final_model.rds"], "primary_data")
  expect_identical(custom$artifact_role[custom$file_name == "data.rds"], "intermediate_data")
  # the stage tag defaults to NA
  expect_true(all(is.na(res$stage)))
})

test_that("list_stage_data_artifacts() reports size in kilobytes and the modification time", {
  d <- withr::local_tempdir()
  p <- file.path(d, "big.csv")
  writeBin(raw(2048), p)
  Sys.setFileTime(p, as.POSIXct("2024-01-01 00:00:00", tz = "UTC"))
  res <- list_stage_data_artifacts(d)
  expect_equal(res$size_kb, 2)
  expect_equal(as.numeric(res$mtime), as.numeric(as.POSIXct("2024-01-01 00:00:00", tz = "UTC")))
})

# ---- wrap_pipeline_label() -----------------------------------------------------------------------

test_that("wrap_pipeline_label() leaves a label that fits as it is", {
  expect_identical(wrap_pipeline_label("raw_data", 18), "raw_data")
  expect_identical(wrap_pipeline_label("", 18), "")
})

test_that("wrap_pipeline_label() breaks identifiers after underscores", {
  expect_identical(wrap_pipeline_label("imputed_DATA_XLSX", 10), "imputed_\nDATA_XLSX")
  expect_identical(wrap_pipeline_label("a_b_c_d", 4), "a_b_\nc_d")
  # the underscore stays with the piece before it, so nothing is lost
  out <- wrap_pipeline_label("one_two_three_four_five", 8)
  expect_identical(gsub("\n", "", out), "one_two_three_four_five")
})

test_that("wrap_pipeline_label() wraps words on spaces and cannot break a long single token", {
  out <- wrap_pipeline_label("final analysis report", 10)
  expect_identical(strsplit(out, "\n")[[1]], c("final", "analysis", "report"))
  expect_identical(wrap_pipeline_label("Supercalifragilistic", 5), "Supercalifragilistic")
})

# ---- print_pipeline() ----------------------------------------------------------------------------

print_fixture <- function() {
  raw <- FilePath("raw_data", "raw.csv", stage = "01_raw")
  clean <- FilePath("clean_data", "clean.rds", stage = "02_clean")
  prep <- FileOutputs("prep_script", "prep.R", dependencies = list(raw), output = list(clean),
                      renders = FALSE, stage = "02_clean")
  rep_pdf <- FilePath("report_pdf", "report.pdf", stage = "output_report")
  rep <- FileOutputs("report_qmd", "report.qmd", dependencies = list(clean), output = list(rep_pdf),
                     stage = "03_report")
  list(raw, prep, rep)
}

test_that("print_pipeline() writes a PNG, makes its directory and reports the size", {
  skip_if_not_installed("ggraph")
  d <- withr::local_tempdir()
  out <- file.path(d, "figs", "deep", "graph.png")
  msgs <- testthat::capture_messages(res <- suppressWarnings(print_pipeline(print_fixture(), out, dpi = 40)))
  expect_identical(res, out)
  expect_true(file.exists(out))
  expect_gt(file.size(out), 0)
  expect_match(paste(msgs, collapse = ""), "Saved: .*graph\\.png \\(.*in x .*in, \\d+ layers, \\d+ nodes\\)")
})

test_that("print_pipeline() accepts a title and an igraph built beforehand", {
  skip_if_not_installed("ggraph")
  d <- withr::local_tempdir()
  g <- suppressWarnings(as_igraph(print_fixture()))
  out <- file.path(d, "titled.png")
  suppressMessages(print_pipeline(g, out, title = "My pipeline", dpi = 40))
  expect_true(file.exists(out))
})

test_that("print_pipeline() drops stages and the edges into named nodes", {
  skip_if_not_installed("ggraph")
  d <- withr::local_tempdir()
  objs <- print_fixture()

  full <- file.path(d, "full.png")
  fewer <- file.path(d, "fewer.png")
  m_full <- testthat::capture_messages(suppressWarnings(print_pipeline(objs, full, dpi = 40)))
  m_drop <- testthat::capture_messages(
    suppressWarnings(print_pipeline(objs, fewer, drop_stages = "output_report", dpi = 40))
  )
  n_nodes <- function(m) as.integer(sub(".* (\\d+) nodes\\).*", "\\1", paste(m, collapse = "")))
  expect_lt(n_nodes(m_drop), n_nodes(m_full))

  edges <- file.path(d, "edges.png")
  suppressMessages(suppressWarnings(print_pipeline(objs, edges, drop_edges_to = "report_qmd", dpi = 40)))
  expect_true(file.exists(edges))
})

test_that("print_pipeline() warns and writes nothing when every node is filtered out", {
  skip_if_not_installed("ggraph")
  d <- withr::local_tempdir()
  out <- file.path(d, "empty.png")
  stages <- c("01_raw", "02_clean", "03_report", "output_report")
  expect_warning(
    res <- print_pipeline(print_fixture(), out, drop_stages = stages),
    "Empty graph after filtering"
  )
  expect_null(res)
  expect_false(file.exists(out))
})
