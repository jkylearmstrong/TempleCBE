# Regression tests for the pre-0.5.0 review of R/compute_graph.R (findings R-CG-01 to R-CG-12).
# Each test fails on the code before the fix and passes after it.
#
# R-CG-02 is only partly closed: the graph's identity is still the node name, and a path that
# appears under two names, or a name used for two paths, is reported with a warning.

# A file with a fixed modification time. Sleeping between writes would make the result depend on
# the clock and on the timestamp resolution of the file system (FAT keeps 2 seconds), so every
# file gets an exact time, `seconds` after a fixed origin. Always create the file BEFORE the
# FilePath object that reads its mtime.
cg_origin <- as.POSIXct("2020-01-01 00:00:00", tz = "UTC")
cg_file <- function(dir, name, seconds) {
  path <- file.path(dir, name)
  writeLines("x", path)
  stopifnot(Sys.setFileTime(path, cg_origin + seconds))
  path
}

# The same, for a file with some content (a document whose YAML header is read).
cg_text_file <- function(dir, name, lines, seconds) {
  path <- file.path(dir, name)
  writeLines(lines, path)
  stopifnot(Sys.setFileTime(path, cg_origin + seconds))
  path
}

# get_render_plan() narrates its progress with messages; most tests only want the result.
quiet_plan <- function(objects) suppressMessages(get_render_plan(objects))

# Two small independent chains (stages s1 and s2) for the graph-export tests.
cg_two_chains <- function(dir) {
  a <- FilePath("a", cg_file(dir, "a.csv", 1), stage = "s1")
  rb <- FileOutputs(
    "rb", cg_file(dir, "b.qmd", 1),
    dependencies = list(a),
    output = list(FilePath("b_out", file.path(dir, "b.pdf"), stage = "s1")),
    stage = "s1"
  )
  c1 <- FilePath("c1", cg_file(dir, "c1.csv", 1), stage = "s2")
  rd <- FileOutputs(
    "rd", cg_file(dir, "d.qmd", 1),
    dependencies = list(c1),
    output = list(FilePath("d_out", file.path(dir, "d.pdf"), stage = "s2")),
    stage = "s2"
  )
  list(a, rb, c1, rd)
}

# Saving an interactive widget needs visNetwork and htmlwidgets, and a self-contained page needs
# pandoc. None of them is a hard dependency of the package.
skip_if_no_widget_stack <- function() {
  testthat::skip_if_not_installed("visNetwork")
  testthat::skip_if_not_installed("htmlwidgets")
  testthat::skip_if_not_installed("rmarkdown")
  testthat::skip_if_not_installed("jsonlite")
  testthat::skip_if_not(
    rmarkdown::pandoc_available(),
    "pandoc is needed to save a self-contained widget"
  )
}

# Number of nodes in a saved visNetwork page (the widget data is embedded as JSON).
cg_html_node_count <- function(file) {
  html <- paste(readLines(file, warn = FALSE), collapse = "\n")
  block <- regmatches(html, regexpr("(?s)<script type=\"application/json\"[^>]*>.*?</script>", html, perl = TRUE))
  json <- sub("</script>$", "", sub("^<script[^>]*>", "", block))
  length(jsonlite::fromJSON(json)$x$nodes$id)
}


# R-CG-01 ----------------------------------------------------------------------------------

test_that("R-CG-01: staleness propagates through a producer that does not render", {
  dir <- withr::local_tempdir()
  # raw.csv (edited last) -> prep.R (renders = FALSE) -> derived.rds -> report.qmd -> report.pdf
  # The report's own inputs look fresh when compared one hop at a time: report.pdf is newer than
  # derived.rds. Only derived.rds is out of date, because raw.csv changed after it was built.
  f_raw <- FilePath("raw", cg_file(dir, "raw.csv", 1000))
  f_rds <- FilePath("derived", cg_file(dir, "derived.rds", 100))
  f_pdf <- FilePath("report_pdf", cg_file(dir, "report.pdf", 200))
  prep <- FileOutputs(
    "prep", cg_file(dir, "prep.R", 10),
    dependencies = list(f_raw), output = list(f_rds), renders = FALSE
  )
  report <- FileOutputs(
    "report", cg_file(dir, "report.qmd", 20),
    dependencies = list(f_rds), output = list(f_pdf)
  )
  objs <- list(f_raw, prep, f_rds, report, f_pdf)

  # the plan names the report, and says which producer has to be run by hand first
  msgs <- testthat::capture_messages(plan <- get_render_plan(objs))
  expect_equal(names(plan), "report")
  expect_match(paste(msgs, collapse = "\n"), "not rendered here: prep", fixed = TRUE)
  expect_false(any(grepl("All files are up-to-date", msgs, fixed = TRUE)))

  # the graph and the DOT source flag the report's output too, not only derived.rds
  g <- as_igraph(objs)
  stale <- stats::setNames(igraph::V(g)$stale, igraph::V(g)$name)
  expect_true(stale[["derived"]])
  expect_true(stale[["report_pdf"]])
  expect_false(stale[["raw"]])
  expect_match(visualize_pipeline(objs, extract_graph_code = TRUE), "report_pdf (STALE)", fixed = TRUE)

  # once prep.R has been re-run and the report re-rendered, nothing is stale any more
  f_rds2 <- FilePath("derived", cg_file(dir, "derived.rds", 1100))
  f_pdf2 <- FilePath("report_pdf", cg_file(dir, "report.pdf", 1200))
  prep2 <- FileOutputs(
    "prep", cg_file(dir, "prep.R", 10),
    dependencies = list(f_raw), output = list(f_rds2), renders = FALSE
  )
  report2 <- FileOutputs(
    "report", cg_file(dir, "report.qmd", 20),
    dependencies = list(f_rds2), output = list(f_pdf2)
  )
  objs2 <- list(f_raw, prep2, f_rds2, report2, f_pdf2)
  expect_equal(quiet_plan(objs2), list())
  expect_false(any(igraph::V(as_igraph(objs2))$stale))
})

test_that("R-CG-01: staleness propagates through a helper script that reads newer inputs", {
  dir <- withr::local_tempdir()
  # raw.csv (edited last) -> helper.qmd (a FileUses object: reads, produces nothing)
  #   -> report.qmd -> report.pdf
  f_raw <- FilePath("raw", cg_file(dir, "raw.csv", 1000))
  helper <- FileUses("helper", cg_file(dir, "helper.qmd", 10), dependencies = list(f_raw))
  f_pdf <- FilePath("report_pdf", cg_file(dir, "report.pdf", 200))
  report <- FileOutputs(
    "report", cg_file(dir, "report.qmd", 20),
    dependencies = list(helper), output = list(f_pdf)
  )
  objs <- list(f_raw, helper, report, f_pdf)

  expect_equal(names(quiet_plan(objs)), "report")
  g <- as_igraph(objs)
  expect_true(igraph::V(g)$stale[igraph::V(g)$name == "report_pdf"])

  # control: the same helper with older inputs leaves the report alone
  f_raw_old <- FilePath("raw", cg_file(dir, "raw.csv", 5))
  helper_old <- FileUses("helper", cg_file(dir, "helper.qmd", 10), dependencies = list(f_raw_old))
  report_old <- FileOutputs(
    "report", cg_file(dir, "report.qmd", 20),
    dependencies = list(helper_old), output = list(f_pdf)
  )
  expect_equal(quiet_plan(list(f_raw_old, helper_old, report_old, f_pdf)), list())
})

test_that("R-CG-01: every report that depends on a stale helper is planned", {
  dir <- withr::local_tempdir()
  f_raw <- FilePath("raw", cg_file(dir, "raw.csv", 1000))
  helper <- FileUses("helper", cg_file(dir, "helper.R", 10), dependencies = list(f_raw))
  pdf_a <- FilePath("pdf_a", cg_file(dir, "a.pdf", 200))
  pdf_b <- FilePath("pdf_b", cg_file(dir, "b.pdf", 200))
  pdf_c <- FilePath("pdf_c", cg_file(dir, "c.pdf", 200))
  report_a <- FileOutputs("report_a", cg_file(dir, "a.qmd", 20), dependencies = list(helper), output = list(pdf_a))
  report_b <- FileOutputs("report_b", cg_file(dir, "b.qmd", 20), dependencies = list(helper), output = list(pdf_b))
  # a report that does not read the helper is not dragged into the plan
  report_c <- FileOutputs("report_c", cg_file(dir, "c.qmd", 20), dependencies = list(), output = list(pdf_c))

  plan <- quiet_plan(list(f_raw, helper, report_a, pdf_a, report_b, pdf_b, report_c, pdf_c))
  expect_equal(sort(names(plan)), c("report_a", "report_b"))
})

test_that("R-CG-01: staleness crosses several non-rendering producers", {
  dir <- withr::local_tempdir()
  # raw.csv (newest) -> prep1.R -> d1.rds -> prep2.R -> d2.rds -> report.qmd -> report.pdf
  # Only d1.rds is older than something it was built from. d2.rds and report.pdf are each newer
  # than their direct inputs, so the report is out of date only through two hops.
  f_raw <- FilePath("raw", cg_file(dir, "raw.csv", 1000))
  d1 <- FilePath("d1", cg_file(dir, "d1.rds", 100))
  d2 <- FilePath("d2", cg_file(dir, "d2.rds", 150))
  f_pdf <- FilePath("report_pdf", cg_file(dir, "report.pdf", 200))
  prep1 <- FileOutputs("prep1", cg_file(dir, "prep1.R", 10), dependencies = list(f_raw), output = list(d1), renders = FALSE)
  prep2 <- FileOutputs("prep2", cg_file(dir, "prep2.R", 10), dependencies = list(d1), output = list(d2), renders = FALSE)
  report <- FileOutputs("report", cg_file(dir, "report.qmd", 20), dependencies = list(d2), output = list(f_pdf))
  objs <- list(f_raw, prep1, d1, prep2, d2, report, f_pdf)

  msgs <- testthat::capture_messages(plan <- get_render_plan(objs))
  expect_equal(names(plan), "report")
  expect_match(paste(msgs, collapse = "\n"), "not rendered here: prep1, prep2", fixed = TRUE)

  g <- as_igraph(objs)
  stale <- stats::setNames(igraph::V(g)$stale, igraph::V(g)$name)
  expect_true(stale[["d2"]])
  expect_true(stale[["report_pdf"]])
})


test_that("R-CG-01: a stale producer with no report downstream is not called up to date", {
  dir <- withr::local_tempdir()
  # raw.csv -> prep.R (does not render) -> derived.rds; nothing reads derived.rds. The objects read
  # their mtime when they are built, so the files are written first, `derived_seconds` after the
  # origin, and the objects built afterwards.
  build <- function(derived_seconds) {
    raw <- FilePath("raw", cg_file(dir, "raw.csv", 1000))
    derived <- FilePath("derived", cg_file(dir, "derived.rds", derived_seconds))
    prep <- FileOutputs(
      "prep", cg_file(dir, "prep.R", 10),
      dependencies = list(raw), output = list(derived), renders = FALSE
    )
    list(raw, prep, derived)
  }
  plan_messages <- function(objects) paste(testthat::capture_messages(get_render_plan(objects)), collapse = "")

  # derived.rds is older than its input: the producer is stale, but there is no report to render
  expect_equal(quiet_plan(build(100)), list())
  msgs <- plan_messages(build(100))
  expect_match(msgs, "Out of date, but not rendered here: prep", fixed = TRUE)
  expect_match(msgs, "Nothing to render: no report reads the outputs", fixed = TRUE)
  expect_false(grepl("All files are up-to-date", msgs, fixed = TRUE))

  # derived.rds newer than its input: the same call is quiet about producers and says so
  msgs_fresh <- plan_messages(build(2000))
  expect_match(msgs_fresh, "All files are up-to-date", fixed = TRUE)
  expect_false(grepl("Out of date", msgs_fresh, fixed = TRUE))
})


# R-CG-03 ----------------------------------------------------------------------------------

test_that("R-CG-03: a declared input that is not on disk is reported", {
  dir <- withr::local_tempdir()
  f_pdf <- FilePath("report_pdf", cg_file(dir, "report.pdf", 500))
  gone <- FilePath("input", file.path(dir, "does_not_exist.csv"))
  report <- FileOutputs(
    "report", cg_file(dir, "report.qmd", 10),
    dependencies = list(gone), output = list(f_pdf)
  )
  # a report whose output has not been built yet: its producer will create the file, so a missing
  # output is expected and must not be counted among the files that are "not found"
  builder <- FileOutputs(
    "builder", cg_file(dir, "builder.qmd", 10),
    output = list(FilePath("not_built", file.path(dir, "not_built_yet.pdf")))
  )

  expect_warning(
    plan <- quiet_plan(list(gone, report, f_pdf, builder)),
    "^1 declared file\\(s\\) were not found on disk.*treated as unchanged: input$"
  )
  # a file that is not there is left out of the comparison, as before: the report stays fresh,
  # while the report with the unbuilt output is planned
  expect_equal(names(plan), "builder")
})

test_that("R-CG-03: a mistyped report path is reported", {
  dir <- withr::local_tempdir()
  f_pdf <- FilePath("report_pdf", cg_file(dir, "report.pdf", 500))
  report <- FileOutputs("report", file.path(dir, "TYPO.qmd"), output = list(f_pdf))

  expect_warning(
    plan <- quiet_plan(list(report, f_pdf)),
    "not found on disk.*: report$"
  )
  expect_equal(plan, list())
})

test_that("R-CG-03: relative paths built from the wrong working directory are reported", {
  project <- withr::local_tempdir()
  cg_file(project, "raw.csv", 0)
  cg_file(project, "report.qmd", 10)
  cg_file(project, "report.pdf", 20)
  build <- function() {
    raw <- FilePath("raw", "raw.csv")
    pdf <- FilePath("report_pdf", "report.pdf")
    list(raw, FileOutputs("report", "report.qmd", dependencies = list(raw), output = list(pdf)), pdf)
  }
  from_project <- withr::with_dir(project, build())
  from_elsewhere <- withr::with_dir(withr::local_tempdir(), build())

  expect_no_warning(plan_here <- quiet_plan(from_project))
  expect_equal(plan_here, list())
  expect_warning(
    quiet_plan(from_elsewhere),
    "2 declared file\\(s\\) were not found on disk.*: raw, report$"
  )
})

test_that("R-CG-03: several missing files are reported in one warning", {
  dir <- withr::local_tempdir()
  f_pdf <- FilePath("report_pdf", cg_file(dir, "report.pdf", 500))
  gone_a <- FilePath("gone_a", file.path(dir, "a.csv"))
  gone_b <- FilePath("gone_b", file.path(dir, "b.csv"))
  report <- FileOutputs(
    "report", cg_file(dir, "report.qmd", 10),
    dependencies = list(gone_a, gone_b), output = list(f_pdf)
  )

  warns <- testthat::capture_warnings(quiet_plan(list(gone_a, gone_b, report, f_pdf)))
  expect_length(warns, 1L)
  expect_match(warns, "2 declared file(s) were not found on disk", fixed = TRUE)
  expect_match(warns, "gone_a, gone_b", fixed = TRUE)
})

test_that("R-CG-03: a missing file declared only inside `dependencies` is reported", {
  # the missing input is not passed to get_render_plan() as an object of its own, only named by
  # the report that reads it, which is how a mistyped data path usually looks
  dir <- withr::local_tempdir()
  f_pdf <- FilePath("report_pdf", cg_file(dir, "report.pdf", 500))
  gone <- FilePath("input", file.path(dir, "does_not_exist.csv"))
  report <- FileOutputs(
    "report", cg_file(dir, "report.qmd", 10),
    dependencies = list(gone), output = list(f_pdf)
  )

  warns <- testthat::capture_warnings(plan <- quiet_plan(list(report, f_pdf)))
  expect_length(warns, 1L)
  expect_match(warns, "1 declared file(s) were not found on disk", fixed = TRUE)
  expect_match(warns, "treated as unchanged: input", fixed = TRUE)
  expect_equal(plan, list())

  # listed both ways, it is still one file and one warning
  warns_both <- testthat::capture_warnings(quiet_plan(list(gone, report, f_pdf)))
  expect_match(warns_both, "1 declared file(s) were not found on disk", fixed = TRUE)
})


# R-CG-04 ----------------------------------------------------------------------------------

test_that("R-CG-04: get_render_plan() accepts a single report that is not wrapped in a list", {
  dir <- withr::local_tempdir()
  report <- FileOutputs(
    "report", cg_file(dir, "report.qmd", 10),
    output = list(FilePath("report_pdf", file.path(dir, "missing.pdf")))
  )

  plan <- quiet_plan(report)
  expect_equal(names(plan), "report")
  expect_equal(plan, quiet_plan(list(report)))

  # a single file that is not a report is a valid, empty plan, not an error
  expect_equal(quiet_plan(FilePath("raw", cg_file(dir, "raw.csv", 0))), list())
})


# R-CG-05 ----------------------------------------------------------------------------------

test_that("R-CG-05: export_interactive_pipeline() keeps an existing <stem>_files folder", {
  skip_if_no_widget_stack()
  dir <- withr::local_tempdir()
  pipe <- cg_two_chains(dir)

  # a folder of that name next to the target belongs to someone else (a rendered document's
  # support files, say)
  mine <- file.path(dir, "pipeline_files")
  dir.create(mine)
  writeLines("keep me", file.path(mine, "precious.txt"))

  out <- file.path(dir, "pipeline.html")
  suppressMessages(export_interactive_pipeline(pipe, file = out))
  expect_true(file.exists(out))
  expect_true(dir.exists(mine))
  expect_equal(readLines(file.path(mine, "precious.txt")), "keep me")

  # a folder the export itself creates is still cleaned up
  fresh <- file.path(dir, "fresh", "other.html")
  suppressMessages(export_interactive_pipeline(pipe, file = fresh))
  expect_true(file.exists(fresh))
  expect_false(dir.exists(file.path(dir, "fresh", "other_files")))
})


# R-CG-06 ----------------------------------------------------------------------------------

test_that("R-CG-06: export_subgraph() errors on an unknown node or stage", {
  dir <- withr::local_tempdir()
  pipe <- cg_two_chains(dir)
  out <- file.path(dir, "sub.html")

  # before the fix a misspelling silently exported the whole graph
  expect_error(
    export_subgraph(pipe, focal_node = "rbb", file = out),
    "`focal_node` 'rbb' is not a node of the graph",
    fixed = TRUE
  )
  expect_error(
    export_subgraph(pipe, stage = "no_such_stage", file = out),
    "`stage` 'no_such_stage' matches no node; the stages are: s1, s2.",
    fixed = TRUE
  )
  # the same for a graph that is passed in already built
  expect_error(
    export_subgraph(as_igraph(pipe), focal_node = "rbb", file = out),
    "is not a node of the graph"
  )
  expect_false(file.exists(out))

  # a known node still exports just its neighbourhood, and a known stage just that stage
  skip_if_no_widget_stack()
  focal <- file.path(dir, "focal.html")
  suppressMessages(export_subgraph(pipe, focal_node = "rb", file = focal))
  expect_equal(cg_html_node_count(focal), 3L) # a, rb, b_out
  by_stage <- file.path(dir, "stage.html")
  suppressMessages(export_subgraph(pipe, stage = "s2", file = by_stage))
  expect_equal(cg_html_node_count(by_stage), 3L) # c1, rd, d_out
})

test_that("R-CG-06: export_subgraph(stage =) hands only that stage's nodes to the exporter", {
  # The stage filter used to be written as V(g)[V(g)$stage == stage]. Inside `[` igraph evaluates
  # with the vertex attributes in scope, so `stage` meant the attribute and every vertex matched:
  # a known stage exported the whole graph. Mocking the exporter keeps this test free of the
  # widget stack.
  dir <- withr::local_tempdir()
  pipe <- cg_two_chains(dir)

  handed_over <- NULL
  testthat::with_mocked_bindings(
    export_interactive_pipeline = function(all_objects, ...) {
      handed_over <<- all_objects
      invisible(NULL)
    },
    suppressMessages(export_subgraph(pipe, stage = "s2", file = file.path(dir, "x.html")))
  )
  expect_setequal(igraph::V(handed_over)$name, c("c1", "rd", "d_out"))
  expect_true(all(igraph::V(handed_over)$stage == "s2"))
})


# R-CG-07 ----------------------------------------------------------------------------------

test_that("R-CG-07: as_tbl_graph() still handles lists that are not pipelines", {
  # an adjacency list is one of the inputs tidygraph's own list method accepts
  adj <- list(a = c("b", "c"), b = "c", c = character())
  tg <- tidygraph::as_tbl_graph(adj)
  own <- utils::getFromNamespace("as_tbl_graph.list", "tidygraph")(adj)

  expect_s3_class(tg, "tbl_graph")
  expect_equal(igraph::vcount(tg), 3L)
  expect_equal(igraph::ecount(tg), 3L)
  expect_equal(igraph::V(tg)$name, igraph::V(own)$name)
  expect_equal(igraph::as_edgelist(tg), igraph::as_edgelist(own))

  # a list of pipeline objects still goes to the pipeline conversion
  dir <- withr::local_tempdir()
  pipe <- cg_two_chains(dir)
  tg_pipe <- tidygraph::as_tbl_graph(pipe)
  expect_s3_class(tg_pipe, "tbl_graph")
  expect_equal(igraph::vcount(tg_pipe), igraph::vcount(as_igraph(pipe)))
  expect_true(all(c("stage", "stale") %in% igraph::vertex_attr_names(tg_pipe)))
})


# R-CG-09 ----------------------------------------------------------------------------------

test_that("R-CG-09: collapse_by_stage() marks a stage stale when any one of its files is", {
  dir <- withr::local_tempdir()
  # stage s1 holds, in this node order: a fresh input, the report, a missing (so stale) output,
  # and another fresh file. The stale one is neither the first nor the last node of the stage.
  raw <- FilePath("raw", cg_file(dir, "raw.csv", 1), stage = "s1")
  stale_out <- FilePath("out_rds", file.path(dir, "missing.rds"), stage = "s1")
  report <- FileOutputs(
    "report", cg_file(dir, "report.qmd", 1),
    dependencies = list(raw), output = list(stale_out), stage = "s1"
  )
  extra <- FilePath("extra", cg_file(dir, "extra.csv", 1), stage = "s1")
  # stage s2 holds only a fresh file
  ext <- FilePath("ext", cg_file(dir, "ext.csv", 1), stage = "s2")

  g <- as_igraph(list(raw, report, stale_out, extra, ext))
  node_stale <- stats::setNames(igraph::V(g)$stale, igraph::V(g)$name)
  expect_equal(unname(node_stale[c("raw", "report", "out_rds", "extra", "ext")]), c(FALSE, FALSE, TRUE, FALSE, FALSE))

  cc <- collapse_by_stage(g)
  stage_stale <- stats::setNames(igraph::V(cc)$stale, igraph::V(cc)$name)
  stage_color <- stats::setNames(igraph::V(cc)$color, igraph::V(cc)$name)
  expect_equal(igraph::vcount(cc), 2L)
  expect_true(stage_stale[["s1"]])
  expect_false(stage_stale[["s2"]])
  # the stale stage is drawn in the stale colour of as_igraph(), the fresh one is not
  expect_equal(stage_color[["s1"]], "#FADBD8")
  expect_false(identical(stage_color[["s2"]], "#FADBD8"))
})


# R-CG-10 ----------------------------------------------------------------------------------

test_that("R-CG-10: create_qmd_renderer() rejects an NA, empty or blank name or path like the constructors", {
  msg_name <- "`name` must be a single non-empty character string."
  msg_path <- "`path` must be a single non-empty character string."
  # the constructor's own answer, which create_qmd_renderer() has to give as well
  expect_error(FileOutputs(NA_character_, "a.qmd"), msg_name, fixed = TRUE)
  expect_error(FileOutputs("n", NA_character_), msg_path, fixed = TRUE)

  # before the fix an NA name became "NA PDF" and an NA path "NA.pdf", and the object was built
  for (bad in list(NA_character_, "", "  ")) {
    expect_error(create_qmd_renderer(bad, "a.qmd", output_format = "pdf"), msg_name, fixed = TRUE)
    expect_error(create_qmd_renderer("n", bad, output_format = "pdf"), msg_path, fixed = TRUE)
  }
  expect_error(create_qmd_renderer(c("a", "b"), "a.qmd"), msg_name, fixed = TRUE)
  expect_error(create_qmd_renderer("n", c("a.qmd", "b.qmd")), msg_path, fixed = TRUE)
  expect_error(create_qmd_renderer(path = "a.qmd"), msg_name, fixed = TRUE)
  expect_error(create_qmd_renderer("n"), msg_path, fixed = TRUE)
  # the path is checked before it is opened to read the YAML formats
  expect_error(create_qmd_renderer("n", NA_character_, output_format = "yaml"), msg_path, fixed = TRUE)

  # a good call is unchanged: the outputs are named after the report and the format
  r <- create_qmd_renderer("r", "a/report.Rmd", output_format = c("pdf", "docx"), file_stage = "s1")
  expect_s4_class(r, "FileOutputs")
  expect_equal(vapply(r@output, function(o) o@name, ""), c("r PDF", "r DOCX"))
  expect_equal(vapply(r@output, function(o) o@path, ""), c("a/report.pdf", "a/report.docx"))
  expect_equal(vapply(r@output, function(o) o@artifact_role, ""), rep("deliverable_report", 2))
  expect_equal(r@stage, "s1")
  expect_true(r@renders)
})


# R-CG-11 ----------------------------------------------------------------------------------

test_that("R-CG-11: create_qmd_renderer() writes the extension a format really produces", {
  file_of <- function(fmt) basename(create_qmd_renderer("r", "a/report.qmd", output_format = fmt)@output[[1]]@path)
  expected <- c(
    html = "html", revealjs = "html", ioslides = "html", slidy = "html", html_document = "html",
    pdf = "pdf", beamer = "pdf", typst = "pdf", pdf_document = "pdf", beamer_presentation = "pdf",
    gfm = "md", github_document = "md", md = "md", md_document = "md",
    docx = "docx", word_document = "docx",
    pptx = "pptx", powerpoint_presentation = "pptx",
    odt = "odt", rtf = "rtf", epub = "epub"
  )
  for (fmt in names(expected)) {
    expect_equal(file_of(fmt), paste0("report.", expected[[fmt]]), info = fmt)
  }
  # a format added by an extension is named <extension>-<base format> and writes the base's file
  expect_equal(file_of("temple-html"), "report.html")
  expect_equal(file_of("titlepage-pdf"), "report.pdf")
  # the output node is still named after the format, not the extension
  r <- create_qmd_renderer("slides", "a/slides.qmd", output_format = c("revealjs", "pdf"))
  expect_equal(vapply(r@output, function(o) o@name, ""), c("slides REVEALJS", "slides PDF"))
  expect_equal(vapply(r@output, function(o) o@path, ""), c("a/slides.html", "a/slides.pdf"))
})

test_that("R-CG-11: output_format = 'yaml' finds the file the document renders to, so it is not always stale", {
  skip_if_not_installed("rmarkdown")
  dir <- withr::local_tempdir()
  # A Quarto slide deck (`format: revealjs`) is written as slides.html, and an R Markdown
  # `output: github_document` as notes.md. Both outputs are newer than their sources.
  slides <- cg_text_file(dir, "slides.qmd", c("---", "title: t", "format: revealjs", "---", "", "x"), 10)
  notes <- cg_text_file(dir, "notes.Rmd", c("---", "title: t", "output: github_document", "---", "", "x"), 10)
  cg_file(dir, "slides.html", 500)
  cg_file(dir, "notes.md", 500)

  r_slides <- create_qmd_renderer("slides", slides, output_format = "yaml")
  r_notes <- create_qmd_renderer("notes", notes, output_format = "yaml")
  expect_equal(basename(r_slides@output[[1]]@path), "slides.html")
  expect_equal(basename(r_notes@output[[1]]@path), "notes.md")

  # before the fix the paths were slides.revealjs and notes.gfm, which never exist
  objs <- list(r_slides, r_notes)
  expect_equal(quiet_plan(objs), list())
  expect_false(any(igraph::V(as_igraph(objs))$stale))
  expect_false(grepl("STALE", visualize_pipeline(objs, extract_graph_code = TRUE), fixed = TRUE))
})

test_that("R-CG-11: a format with no known extension keeps its name as the extension and warns that it is a guess", {
  expect_warning(
    r <- create_qmd_renderer("r", "a/report.qmd", output_format = c("pdf", "frobnicate")),
    "'frobnicate'.*is a guess"
  )
  # the old behaviour is kept
  expect_equal(vapply(r@output, function(o) basename(o@path), ""), c("report.pdf", "report.frobnicate"))
  # known formats, and extension formats of a known base, are quiet
  expect_no_warning(
    create_qmd_renderer("r", "a/report.qmd", output_format = c("pdf", "revealjs", "gfm", "temple-html"))
  )
})

test_that("R-CG-11: output_format_extensions() and create_qmd_renderer() share one mapping", {
  fmts <- c("revealjs", "ioslides", "beamer", "typst", "gfm", "github_document", "word_document", "temple-html")
  # (not tools::file_ext(): it does not read an extension that contains an underscore)
  per_format <- vapply(
    fmts,
    function(fmt) sub("^report[.]", "", basename(create_qmd_renderer("r", "a/report.qmd", output_format = fmt)@output[[1]]@path)),
    ""
  )
  expect_equal(unname(per_format), c("html", "html", "pdf", "pdf", "md", "md", "docx", "html"))
  expect_equal(
    unname(vapply(fmts, output_format_extensions, "")),
    unname(per_format)
  )
})

test_that("R-CG-11: the format of an output file is read back from its name, so render() asks for revealjs, not html", {
  r <- create_qmd_renderer("s", "a/s.qmd", output_format = c("revealjs", "gfm", "pdf", "temple-html", "docx"))
  expect_equal(
    vapply(r@output, .output_format, ""),
    c("revealjs", "gfm", "pdf", "temple-html", "docx")
  )
  # an output that was not made by create_qmd_renderer() is its extension
  expect_equal(.output_format(FilePath("Report", "x.PDF")), "pdf")
  expect_equal(.output_format(FilePath("Final gfm", "final.md", artifact_role = "deliverable_report")), "gfm")
  expect_equal(.output_format(FilePath("Final gfm", "final.md")), "md")
  # a name that ends in some other format's name does not override the extension
  expect_equal(.output_format(FilePath("A revealjs", "a.pdf", artifact_role = "deliverable_report")), "pdf")
  expect_equal(.output_format(FilePath("Report", "report", artifact_role = "deliverable_report")), "")

  # render() takes the formats it renders from these outputs
  skip_if_not_installed("quarto")
  asked <- character()
  res <- testthat::with_mocked_bindings(
    suppressMessages(render(r)),
    quarto_render = function(input, output_format, ...) {
      asked <<- c(asked, output_format)
      invisible(NULL)
    },
    .package = "quarto"
  )
  expect_equal(asked, c("revealjs", "gfm", "pdf", "temple-html", "docx"))
  expect_equal(res$output, asked)
})


# R-CG-12 ----------------------------------------------------------------------------------

test_that("R-CG-12: every graph and summary rejects two top-level objects with one name, as the plan does", {
  dir <- withr::local_tempdir()
  same <- list(
    FileOutputs("Rpt", cg_file(dir, "a.qmd", 1)),
    FileOutputs("Rpt", cg_file(dir, "b.qmd", 1)),
    FilePath("Other", cg_file(dir, "o1.csv", 1)),
    FilePath("Other", cg_file(dir, "o2.csv", 1))
  )
  msg <- "Duplicate pipeline object name(s) detected: Rpt, Other"
  # get_render_plan() has always said so; before the fix nothing else did, and the first report
  # simply vanished from the graph
  expect_error(get_render_plan(same), msg, fixed = TRUE)
  expect_error(as_igraph(same), msg, fixed = TRUE)
  expect_error(as_pipeline_graph(same), msg, fixed = TRUE)
  expect_error(pipeline_summary(same), msg, fixed = TRUE)
  expect_error(visualize_pipeline(same, extract_graph_code = TRUE), msg, fixed = TRUE)
  expect_error(export_subgraph(same, focal_node = "Rpt", file = file.path(dir, "x.html")), msg, fixed = TRUE)

  # a name at the top level and again as a bare copy inside a `dependencies` list is the normal
  # way to wire a pipeline, and stays accepted without a warning
  raw <- FilePath("raw", cg_file(dir, "raw.csv", 1))
  out <- FilePath("report_pdf", cg_file(dir, "report.pdf", 500))
  report <- FileOutputs("report", cg_file(dir, "report.qmd", 10), dependencies = list(raw), output = list(out))
  summary_report <- FileOutputs(
    "summary", cg_file(dir, "summary.qmd", 10),
    dependencies = list(report, out), output = list(FilePath("summary_pdf", cg_file(dir, "summary.pdf", 500)))
  )
  objs <- list(raw, report, out, summary_report)
  expect_no_warning(g <- as_igraph(objs))
  expect_equal(igraph::vcount(g), 5L)
  expect_no_warning(plan <- quiet_plan(objs))
  expect_equal(plan, list())
  expect_no_warning(visualize_pipeline(objs, extract_graph_code = TRUE))
})

test_that("R-CG-12: two producers of one output file warn, naming the output and both producers", {
  dir <- withr::local_tempdir()
  shared <- FilePath("shared_out", file.path(dir, "shared.pdf"))
  p1 <- FileOutputs("P1", cg_file(dir, "p1.qmd", 1), output = list(shared))
  p2 <- FileOutputs("P2", cg_file(dir, "p2.qmd", 1), output = list(shared))
  objs <- list(p1, p2, shared)

  w <- testthat::capture_warnings(g <- as_igraph(objs))
  expect_length(w, 1L)
  expect_match(w, "'shared_out'.*P1.*P2")
  expect_match(w, "'P1' decides", fixed = TRUE) # what the code does about it: the first one counts
  # a warning, not an error: the graph is built, with both producers feeding the output
  expect_setequal(igraph::as_ids(igraph::neighbors(g, "shared_out", mode = "in")), c("P1", "P2"))

  # the plan, the summary and the DOT source say the same, once each
  w_plan <- testthat::capture_warnings(quiet_plan(objs))
  expect_length(w_plan, 1L)
  expect_match(w_plan, "'shared_out'.*P1.*P2")
  w_summary <- testthat::capture_warnings(utils::capture.output(pipeline_summary(objs)))
  expect_length(w_summary, 1L)
  expect_match(w_summary, "'shared_out'.*P1.*P2")
  w_dot <- testthat::capture_warnings(visualize_pipeline(objs, extract_graph_code = TRUE))
  expect_length(w_dot, 1L)
  expect_match(w_dot, "'shared_out'.*P1.*P2")

  # a third producer is named too, and two shared outputs are both listed
  other <- FilePath("other_out", file.path(dir, "other.pdf"))
  p3 <- FileOutputs("P3", cg_file(dir, "p3.qmd", 1), output = list(shared, other))
  p4 <- FileOutputs("P4", cg_file(dir, "p4.qmd", 1), output = list(other))
  w_many <- testthat::capture_warnings(as_igraph(list(p1, p2, p3, p4)))
  expect_length(w_many, 1L)
  expect_match(w_many, "'shared_out'.*P1.*P2.*P3")
  expect_match(w_many, "'other_out'.*P3.*P4")

  # outputs with different names are quiet, and so is one producer that lists an output twice
  p5 <- FileOutputs("P5", cg_file(dir, "p5.qmd", 1), output = list(FilePath("out5", file.path(dir, "5.pdf"))))
  expect_no_warning(as_igraph(list(p1, p5)))
  p6 <- FileOutputs("P6", cg_file(dir, "p6.qmd", 1), output = list(other, other))
  expect_no_warning(as_igraph(list(p6)))
})


# R-CG-02 ----------------------------------------------------------------------------------

test_that("R-CG-02: one file declared under two names is reported, once", {
  dir <- withr::local_tempdir()
  # A writes a.rds as "a_rds"; B reads the same file as "a_data_alias". They are two nodes, so
  # when A is re-rendered nothing says that B has to follow.
  raw <- FilePath("raw", cg_file(dir, "raw.csv", 1000))
  a_path <- cg_file(dir, "a.rds", 100)
  a_rds <- FilePath("a_rds", a_path)
  a_pdf <- FilePath("a_pdf", cg_file(dir, "A.pdf", 120))
  b_pdf <- FilePath("b_pdf", cg_file(dir, "B.pdf", 200))
  A <- FileOutputs("A", cg_file(dir, "A.qmd", 10), dependencies = list(raw), output = list(a_pdf, a_rds))
  B <- FileOutputs("B", cg_file(dir, "B.qmd", 10), dependencies = list(FilePath("a_data_alias", a_path)), output = list(b_pdf))
  objs <- list(raw, A, B, a_rds, a_pdf, b_pdf)

  w <- testthat::capture_warnings(g <- as_igraph(objs))
  expect_length(w, 1L)
  expect_match(w, "same file under more than one name", fixed = TRUE)
  expect_match(w, "'a_rds', 'a_data_alias'", fixed = TRUE)
  expect_match(w, "a.rds", fixed = TRUE)
  expect_match(w, "identity is the name", fixed = TRUE)
  # identity stays the name: the file is two nodes
  expect_true(all(c("a_rds", "a_data_alias") %in% igraph::V(g)$name))

  w_plan <- testthat::capture_warnings(quiet_plan(objs))
  expect_length(w_plan, 1L)
  expect_match(w_plan, "'a_rds', 'a_data_alias'", fixed = TRUE)

  # control: a file under ONE name is no conflict, however its path is spelt and however often
  # it is repeated (at the top level, and as a copy in several reports' dependencies)
  csv <- cg_file(dir, "input.csv", 1)
  dir.create(file.path(dir, "sub"))
  spellings <- c(csv, file.path(dir, ".", "input.csv"), file.path(dir, "sub", "..", "input.csv"))
  reports <- lapply(seq_along(spellings), function(i) {
    FileOutputs(
      paste0("r", i), cg_file(dir, paste0("r", i, ".qmd"), 10),
      dependencies = list(FilePath("input", spellings[i])),
      output = list(FilePath(paste0("o", i), file.path(dir, paste0("o", i, ".pdf"))))
    )
  })
  one_name <- c(list(FilePath("input", csv)), reports)
  expect_no_warning(as_igraph(one_name))
  expect_no_warning(quiet_plan(one_name))
})

test_that("R-CG-02: one name used for two different files is reported, once", {
  dir <- withr::local_tempdir()
  one <- cg_file(dir, "one.csv", 1)
  two <- cg_file(dir, "two.csv", 1)
  r1 <- FileOutputs(
    "r1", cg_file(dir, "r1.qmd", 10),
    dependencies = list(FilePath("x", one)), output = list(FilePath("r1_pdf", file.path(dir, "r1.pdf")))
  )
  r2 <- FileOutputs(
    "r2", cg_file(dir, "r2.qmd", 10),
    dependencies = list(FilePath("x", two)), output = list(FilePath("r2_pdf", file.path(dir, "r2.pdf")))
  )

  w <- testthat::capture_warnings(g <- as_igraph(list(r1, r2)))
  expect_length(w, 1L)
  expect_match(w, "name used for more than one file", fixed = TRUE)
  expect_match(w, "'x'", fixed = TRUE)
  expect_match(w, "one.csv", fixed = TRUE)
  expect_match(w, "two.csv", fixed = TRUE)
  # identity stays the name: one node
  expect_equal(sum(igraph::V(g)$name == "x"), 1L)

  w_plan <- testthat::capture_warnings(quiet_plan(list(r1, r2)))
  expect_length(w_plan, 1L)
  expect_match(w_plan, "'x'", fixed = TRUE)
})

test_that("R-CG-02: on Windows two spellings that differ only in case are the same file", {
  skip_if_not(.Platform$OS.type == "windows", "file names are case-insensitive on Windows only")
  dir <- withr::local_tempdir()
  r <- FileOutputs(
    "r", cg_file(dir, "r.qmd", 10),
    dependencies = list(FilePath("upper", file.path(dir, "NEW.csv")), FilePath("lower", file.path(dir, "new.csv")))
  )
  expect_warning(as_igraph(list(r)), "'upper', 'lower'", fixed = TRUE)
})


# R-CG-08 ----------------------------------------------------------------------------------

# Two pipelines that share the node "shared" (a data file). `consumer` reads it through a bare
# copy, so in graph `a` its stage, role and description are the placeholders "Other",
# "unspecified" and "". `producer` writes it, describes it, and is newer than the file, so in
# graph `b` the node is stale.
cg_shared_pipelines <- function(dir) {
  shared <- cg_file(dir, "shared.rds", 100)
  consumer <- FileOutputs(
    "consumer", cg_file(dir, "consumer.qmd", 10),
    dependencies = list(FilePath("shared", shared)),
    output = list(FilePath("consumer_pdf", cg_file(dir, "consumer.pdf", 500))),
    stage = "reports"
  )
  producer <- FileOutputs(
    "producer", cg_file(dir, "producer.qmd", 400),
    output = list(FilePath(
      "shared", shared,
      stage = "data", artifact_role = "derived_data", description = "the shared dataset"
    )),
    stage = "data"
  )
  list(consumer = consumer, producer = producer, a = as_igraph(list(consumer)), b = as_igraph(list(producer)))
}

# One node of a graph, as a one-row data frame.
cg_node <- function(g, name) {
  nodes <- igraph::as_data_frame(g, "vertices")
  row <- nodes[nodes$name == name, , drop = FALSE]
  rownames(row) <- NULL
  row
}

cg_node_columns <- c(
  "stage", "artifact_role", "path", "description", "renders", "stale", "color", "shape", "mtime", "title"
)

test_that("R-CG-08: join_pipelines() keeps the real attributes of a shared node whichever graph comes first", {
  dir <- withr::local_tempdir()
  p <- cg_shared_pipelines(dir)
  ab <- join_pipelines(p$a, p$b)
  ba <- join_pipelines(p$b, p$a)

  # before the fix join_pipelines(a, b) gave "Other" / "unspecified" / "" / FALSE, and only the
  # other argument order gave the real values
  for (joined in list(ab, ba)) {
    node <- cg_node(joined, "shared")
    expect_equal(node$stage, "data")
    expect_equal(node$artifact_role, "derived_data")
    expect_equal(node$description, "the shared dataset")
    expect_true(node$stale)
    expect_equal(node$path, cg_node(p$a, "shared")$path)
  }
  # the node is the same whichever graph is first, and is the producer's own view of it,
  # colour, shape and tooltip included
  expect_equal(cg_node(ab, "shared")[cg_node_columns], cg_node(ba, "shared")[cg_node_columns])
  expect_equal(cg_node(ab, "shared")[cg_node_columns], cg_node(p$b, "shared")[cg_node_columns])

  # nodes that only one graph has are untouched, on either side of the join (including the
  # placeholders of a node that has no details at all)
  for (joined in list(ab, ba)) {
    for (only_a in c("consumer", "consumer_pdf")) {
      expect_equal(cg_node(joined, only_a)[cg_node_columns], cg_node(p$a, only_a)[cg_node_columns], info = only_a)
    }
    expect_equal(cg_node(joined, "producer")[cg_node_columns], cg_node(p$b, "producer")[cg_node_columns])
    expect_setequal(igraph::V(joined)$name, c("consumer", "shared", "consumer_pdf", "producer"))
    # and the merged graph still has a label for every node (the two copies were left as label.x/label.y)
    expect_equal(igraph::V(joined)$label, igraph::V(joined)$name)
  }

  # lists of objects are joined the same way
  expect_equal(
    cg_node(join_pipelines(list(p$consumer), list(p$producer)), "shared")[cg_node_columns],
    cg_node(ab, "shared")[cg_node_columns]
  )

  # when both graphs hold a real value, the first graph's value is kept
  csv <- cg_file(dir, "in.csv", 1)
  detail_cols <- c("stage", "artifact_role", "description")
  one <- as_igraph(list(FilePath("in", csv, stage = "s1", artifact_role = "raw_data", description = "first")))
  two <- as_igraph(list(FilePath("in", csv, stage = "s2", artifact_role = "reference", description = "second")))
  expect_equal(cg_node(join_pipelines(one, two), "in")[detail_cols], cg_node(one, "in")[detail_cols])
  expect_equal(cg_node(join_pipelines(two, one), "in")[detail_cols], cg_node(two, "in")[detail_cols])
})

test_that("R-CG-08: a shared node is stale if it is stale in either graph, whichever side has the details", {
  dir <- withr::local_tempdir()
  shared <- cg_file(dir, "shared.rds", 100)
  # `maker` is newer than shared.rds, so its output is stale, but says nothing else about it ...
  maker_new <- FileOutputs("maker", cg_file(dir, "maker.qmd", 400), output = list(FilePath("shared", shared)))
  stale_side <- as_igraph(list(maker_new))
  # ... while `reader` describes the file, and a bare copy of a file is never stale
  reader <- FileOutputs(
    "reader", cg_file(dir, "reader.qmd", 10),
    dependencies = list(FilePath(
      "shared", shared,
      stage = "data", artifact_role = "derived_data", description = "the shared dataset"
    )),
    output = list(FilePath("reader_pdf", cg_file(dir, "reader.pdf", 500)))
  )
  detailed_side <- as_igraph(list(reader))
  expect_true(cg_node(stale_side, "shared")$stale)
  expect_false(cg_node(detailed_side, "shared")$stale)

  for (joined in list(join_pipelines(stale_side, detailed_side), join_pipelines(detailed_side, stale_side))) {
    node <- cg_node(joined, "shared")
    expect_true(node$stale)
    expect_equal(node$stage, "data")
    expect_equal(node$description, "the shared dataset")
    # drawn as stale, in the colour and with the tooltip as_igraph() gives a stale node
    expect_equal(node$color, "#FADBD8")
    expect_match(node$title, "STALE (Needs Re-render)", fixed = TRUE)
    expect_false(grepl("Up-to-date", node$title, fixed = TRUE))
  }

  # fresh in both graphs: not stale, not drawn as stale
  maker_old <- FileOutputs("maker", cg_file(dir, "maker.qmd", 10), output = list(FilePath("shared", shared)))
  fresh_side <- as_igraph(list(maker_old))
  for (joined in list(join_pipelines(fresh_side, detailed_side), join_pipelines(detailed_side, fresh_side))) {
    node <- cg_node(joined, "shared")
    expect_false(node$stale)
    expect_false(identical(node$color, "#FADBD8"))
    expect_match(node$title, "Up-to-date", fixed = TRUE)
  }
})

test_that("R-CG-08: join_pipelines() does not duplicate the edges two graphs share", {
  dir <- withr::local_tempdir()
  p <- cg_shared_pipelines(dir)

  # a graph joined with itself is the same graph (before the fix: 4 edges for 2)
  expect_equal(igraph::ecount(join_pipelines(p$a, p$a)), igraph::ecount(p$a))
  expect_equal(igraph::vcount(join_pipelines(p$a, p$a)), igraph::vcount(p$a))
  # graphs with no edge in common keep all of them
  ab <- join_pipelines(p$a, p$b)
  expect_equal(igraph::ecount(ab), igraph::ecount(p$a) + igraph::ecount(p$b))
  # overlapping graphs: the whole pipeline in one graph, joined with each half, is that pipeline
  whole <- as_igraph(list(p$consumer, p$producer))
  for (joined in list(join_pipelines(ab, whole), join_pipelines(whole, ab), join_pipelines(p$a, whole))) {
    expect_equal(igraph::ecount(joined), igraph::ecount(whole))
    expect_equal(igraph::vcount(joined), igraph::vcount(whole))
    expect_false(anyDuplicated(igraph::as_edgelist(joined)) > 0)
  }
  # the edges keep their attributes
  expect_setequal(igraph::edge_attr(ab, "type"), c("dependency", "output"))
  expect_true(all(nzchar(igraph::edge_attr(ab, "title"))))
})
