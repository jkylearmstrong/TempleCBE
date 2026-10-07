test_that("output_format_extensions maps format names to their real file extensions", {
  expect_equal(output_format_extensions("html"), "html")
  expect_equal(output_format_extensions(c("html", "pdf", "docx")), "html|pdf|docx")
  # Regression test: formats whose extension doesn't match their name.
  expect_equal(output_format_extensions("revealjs"), "html")
  expect_equal(output_format_extensions("beamer"), "pdf")
  # Unknown format names pass through as their own extension.
  expect_equal(output_format_extensions("pptx"), "pptx")
  expect_match(output_format_extensions("all"), "pptx")
})

test_that("zip_render includes outputs for formats beyond html/pdf/docx", {
  # Regression test: a prior bug hardcoded the output glob to
  # html|pdf|docx regardless of the `formats` argument, so any other
  # requested format's output silently never made it into the zip.
  skip_if_not(requireNamespace("quarto", quietly = TRUE), "quarto package not available")

  tmp_dir <- file.path(tempdir(), "test_zip_render_gfm")
  dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  qmd_file <- file.path(tmp_dir, "minimal.qmd")
  writeLines(c(
    "---",
    "title: \"Minimal Test\"",
    "format: gfm",
    "---",
    "",
    "## Hello World",
    "This is a minimal test for zip_render."
  ), qmd_file)

  res <- zip_render(
    input = qmd_file,
    formats = "gfm",
    build_dir = file.path(tmp_dir, "build"),
    copy_back_dir = tmp_dir,
    verbose = FALSE
  )

  expect_length(res$outputs, 1)
  expect_match(res$outputs[1], "minimal\\.md$")
  expect_true(file.exists(res$outputs[1]))
})

test_that("zip_render works correctly for HTML output", {
  skip_if_not(requireNamespace("quarto", quietly = TRUE), "quarto package not available")

  # Create a temporary directory for the test
  tmp_dir <- file.path(tempdir(), "test_zip_render")
  dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  # Write a minimal QMD file
  qmd_file <- file.path(tmp_dir, "minimal.qmd")
  writeLines(c(
    "---",
    "title: \"Minimal Test\"",
    "format: html",
    "---",
    "",
    "## Hello World",
    "This is a minimal test for zip_render."
  ), qmd_file)

  # Run zip_render
  res <- zip_render(
    input = qmd_file,
    formats = "html",
    build_dir = file.path(tmp_dir, "build"),
    copy_back_dir = tmp_dir,
    verbose = FALSE
  )

  # Verify output list fields
  expect_type(res, "list")
  expect_true(file.exists(res$zip))
  expect_equal(basename(res$zip), "minimal.zip")

  # Verify that the outputs include the html file
  expect_length(res$outputs, 1)
  expect_match(res$outputs[1], "minimal\\.html$")
  expect_true(file.exists(res$outputs[1]))

  # Verify files inside the zip
  zip_files <- zip::zip_list(res$zip)$filename
  expect_true(any(grepl("minimal\\.html$", zip_files)))
  expect_true(any(grepl("minimal\\.qmd$", zip_files)))
})

test_that("output_format_extensions resolves extension formats to their base format", {
  expect_equal(output_format_extensions("titlepage-pdf"), "pdf")
  expect_equal(output_format_extensions(c("temple-html", "temple-pdf", "temple-typst")), "html|pdf")
  expect_equal(output_format_extensions("temple-revealjs"), "html")
  expect_equal(output_format_extensions("my-custom"), "my-custom")
})

test_that("zip_render renders extension formats and zips the extension with its paths", {
  # Regression test: extension formats (e.g. temple-pdf) weren't matched to
  # their output file, and _quarto.yml/_extensions weren't copied to the
  # build directory, so the format couldn't even be found there.
  skip_if_not(requireNamespace("quarto", quietly = TRUE), "quarto package not available")
  skip_if(is.null(quarto::quarto_path()), "Quarto CLI not available")
  skip_if_not_installed("zip")

  tmp_dir <- withr::local_tempdir("test_zip_render_ext")
  ext_dir <- file.path(tmp_dir, "_extensions", "mini")
  dir.create(ext_dir, recursive = TRUE)
  writeLines(c("title: Mini", "version: 1.0.0", "contributes:",
               "  formats:", "    html:", "      toc: true"),
             file.path(ext_dir, "_extension.yml"))
  writeLines(c("project:", "  type: default"), file.path(tmp_dir, "_quarto.yml"))

  qmd_file <- file.path(tmp_dir, "minimal.qmd")
  writeLines(c("---", "title: \"Minimal Test\"", "---", "", "## Hello World"), qmd_file)

  res <- zip_render(
    input = qmd_file,
    formats = "mini-html",
    build_dir = withr::local_tempdir("test_zip_render_ext_build"),
    copy_back_dir = tmp_dir,
    verbose = FALSE
  )

  expect_length(res$outputs, 1)
  expect_match(res$outputs[1], "minimal\\.html$")
  expect_match(paste(readLines(res$outputs[1], warn = FALSE), collapse = "\n"), "id=\"TOC\"")

  zip_files <- zip::zip_list(res$zip)$filename
  expect_true("_extensions/mini/_extension.yml" %in% zip_files)
  expect_true("_quarto.yml" %in% zip_files)
})

# quarto::quarto_render() stand-in that "renders" <stem>.html next to its input
# (the build directory is the working directory when zip_render() calls it), so
# these tests need the quarto package but neither the Quarto CLI nor a render.
local_fake_quarto <- function(extra_outputs = character(0), writes_output = TRUE, .env = parent.frame()) {
  testthat::local_mocked_bindings(
    quarto_render = function(input, output_format = NULL, execute_dir = NULL, ...) {
      if (writes_output) writeLines("<html>rendered</html>", sub("\\.qmd$", ".html", input, ignore.case = TRUE))
      for (f in extra_outputs) writeLines("rendered", f)
      invisible(NULL)
    },
    .package = "quarto",
    .env = .env
  )
}

# A folder with `<stem>.qmd` in it, which becomes the working directory.
local_qmd_folder <- function(stem = "plain", .env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = .env)
  writeLines(c("---", "title: x", "---", "text"), file.path(dir, paste0(stem, ".qmd")))
  withr::local_dir(dir, .local_envir = .env)
  dir
}

test_that("a relative copy_back_dir is relative to the caller's folder, not to the build folder", {
  skip_if_not_installed("quarto")
  skip_if_not_installed("zip")
  dir <- local_qmd_folder()
  local_fake_quarto()
  old_wd <- getwd()

  res <- zip_render("plain.qmd", formats = "html", detect = "none", verbose = FALSE, copy_back_dir = "deliverables")

  # The zip used to be written to <build dir>/deliverables while the call
  # reported "Created zip: deliverables/plain.zip".
  expect_true(file.exists(file.path(dir, "deliverables", "plain.zip")))
  expect_equal(normalizePath(res$zip), normalizePath(file.path(dir, "deliverables", "plain.zip")))
  # The path that is returned (and printed) does not depend on the working directory.
  expect_match(res$zip, "^([A-Za-z]:)?/")
  expect_false(dir.exists(file.path(res$build_dir, "deliverables")))
  expect_equal(getwd(), old_wd)
  expect_setequal(zip::zip_list(res$zip)$filename, c("plain.html", "plain.qmd"))
})

test_that("a relative build_dir works, and both can be relative and not exist yet", {
  skip_if_not_installed("quarto")
  skip_if_not_installed("zip")
  dir <- local_qmd_folder()
  local_fake_quarto()

  # This used to stop at the zip step: "Cannot open zip file `rel_build/plain.zip` for writing".
  res <- zip_render("plain.qmd", formats = "html", detect = "none", verbose = FALSE, build_dir = "rel_build")
  expect_true(file.exists(file.path(dir, "rel_build", "plain.html")))
  expect_true(file.exists(file.path(dir, "plain.zip")))   # the default: next to the input
  expect_equal(normalizePath(res$build_dir), normalizePath(file.path(dir, "rel_build")))

  res2 <- zip_render("plain.qmd", formats = "html", detect = "none", verbose = FALSE,
                     build_dir = file.path("work", "b"), copy_back_dir = file.path("out", "zips"))
  expect_true(file.exists(file.path(dir, "out", "zips", "plain.zip")))
  expect_true(file.exists(file.path(dir, "work", "b", "plain.html")))
})

test_that("a relative input in another folder works with relative directories", {
  skip_if_not_installed("quarto")
  skip_if_not_installed("zip")
  root <- withr::local_tempdir()
  dir.create(file.path(root, "src"))
  writeLines(c("---", "title: x", "---", "text"), file.path(root, "src", "plain.qmd"))
  withr::local_dir(root)
  local_fake_quarto()

  res <- zip_render("src/plain.qmd", formats = "html", detect = "none", verbose = FALSE, copy_back_dir = "out")
  expect_true(file.exists(file.path(root, "out", "plain.zip")))
  expect_false(file.exists(file.path(root, "src", "plain.zip")))
})

test_that("outputs are found whatever characters the file name holds", {
  skip_if_not_installed("quarto")
  skip_if_not_installed("zip")

  # "(final)", "[1", "+" and "." are all regular expression syntax. The first
  # left its outputs out of the zip with no word, the second made list.files()
  # fail after the render had finished.
  for (stem in c("Results (final)", "a[1", "x+y", "dots.in.name", "two words")) {
    root <- withr::local_tempdir()
    writeLines(c("---", "title: x", "---", "text"), file.path(root, paste0(stem, ".qmd")))
    withr::local_dir(root)
    local_fake_quarto()

    res <- zip_render(file.path(root, paste0(stem, ".qmd")), formats = "html", detect = "none", verbose = FALSE,
                      build_dir = file.path(root, "build"), copy_back_dir = file.path(root, "out"))
    expect_equal(basename(res$outputs), paste0(stem, ".html"), info = stem)
    expect_setequal(zip::zip_list(res$zip)$filename, paste0(stem, c(".html", ".qmd")))
  }
})

test_that("an output whose name differs only in case is still found", {
  skip_if_not_installed("quarto")
  skip_if_not_installed("zip")
  dir <- local_qmd_folder("report")
  testthat::local_mocked_bindings(
    quarto_render = function(input, output_format = NULL, execute_dir = NULL, ...) writeLines("x", "REPORT.HTML"),
    .package = "quarto"
  )
  res <- zip_render("report.qmd", formats = "html", detect = "none", verbose = FALSE, copy_back_dir = "out")
  expect_equal(basename(res$outputs), "REPORT.HTML")
  expect_true("REPORT.HTML" %in% zip::zip_list(res$zip)$filename)
})

test_that("a render that leaves no output where it is looked for is reported", {
  skip_if_not_installed("quarto")
  skip_if_not_installed("zip")
  dir <- local_qmd_folder()
  # An `output-dir` in _quarto.yml puts the output in a sub-folder of the build folder.
  testthat::local_mocked_bindings(
    quarto_render = function(input, output_format = NULL, execute_dir = NULL, ...) {
      dir.create("_output")
      writeLines("<html>rendered</html>", file.path("_output", "plain.html"))
    },
    .package = "quarto"
  )

  # An explicit build folder: the default one is named after the second, so an
  # earlier test's plain.html could be sitting in it.
  expect_warning(
    res <- zip_render("plain.qmd", formats = c("html", "pdf"), detect = "none", verbose = FALSE,
                      build_dir = "fresh_build", copy_back_dir = "out"),
    "No rendered output \\(plain.html, plain.pdf\\) was found"
  )
  expect_length(res$outputs, 0L)
  # The call still finishes: the zip holds the source.
  expect_equal(zip::zip_list(res$zip)$filename, "plain.qmd")
})
