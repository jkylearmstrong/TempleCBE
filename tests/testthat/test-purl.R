# Every R Markdown and Quarto document the package ships (report templates,
# picker skeletons, vignettes, README) must purl into R code that parses.
# Purled scripts go to a temporary directory, never into the source tree.

purl_source_root <- function() {
  root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = FALSE)
  desc <- file.path(root, "DESCRIPTION")
  if (file.exists(desc) && identical(unname(read.dcf(desc, "Package")[1, 1]), "TempleCBE")) {
    root
  } else {
    # R CMD check runs tests against the installed package
    system.file(package = "TempleCBE")
  }
}

purl_documents <- function(root) {
  files <- list.files(root, pattern = "\\.(Rmd|qmd)$", recursive = TRUE, ignore.case = TRUE)
  files <- files[!grepl("^(renv|docs|tests|\\.Rproj\\.user)/|(^|/)_extensions/", files)]
  file.path(root, files)
}

test_that("every shipped R Markdown and Quarto document purls to parseable R code", {
  skip_if_not_installed("knitr")

  docs <- purl_documents(purl_source_root())
  expect_gt(length(docs), 0)
  # Both formats must stay covered, or purl support for one could lapse unnoticed
  expect_true(any(grepl("\\.qmd$", docs, ignore.case = TRUE)))
  expect_true(any(grepl("\\.Rmd$", docs, ignore.case = TRUE)))

  sibling_scripts <- sub("\\.(Rmd|qmd)$", ".R", docs, ignore.case = TRUE)
  existed_before <- file.exists(sibling_scripts)

  out_dir <- withr::local_tempdir()
  for (i in seq_along(docs)) {
    doc <- docs[[i]]
    out <- file.path(out_dir, sprintf("%02d_%s.R", i, basename(doc)))

    purled <- tryCatch(knitr::purl(doc, output = out, quiet = TRUE), error = identity)
    expect_false(inherits(purled, "error"), info = paste("purl failed:", doc))
    if (inherits(purled, "error")) next

    parsed <- tryCatch(parse(out, keep.source = FALSE), error = identity)
    expect_false(
      inherits(parsed, "error"),
      info = paste("purled code does not parse:", doc, if (inherits(parsed, "error")) conditionMessage(parsed))
    )
  }

  # purl() defaults to writing next to its input; the explicit output must prevent that
  expect_identical(file.exists(sibling_scripts), existed_before)
})
