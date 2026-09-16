# The R Markdown template picker reads inst/rmarkdown/templates; create_report()
# reads inst/templates. The skeleton files are copies and must stay identical.
skeleton_sources <- list(
  "temple-report" = c(
    skeleton.Rmd = "temple.qmd",
    bib.bib = "bib.bib"
  ),
  "t-test-example" = c(
    skeleton.Rmd = "t_test_example.Rmd",
    t_test_child.Rmd = "t_test_child.Rmd",
    bib.bib = "bib.bib",
    "grateful-refs.bib" = "grateful-refs.bib"
  )
)

test_that("each picker template has a name, description, and skeleton", {
  for (template in names(skeleton_sources)) {
    dir <- system.file("rmarkdown", "templates", template, package = "TempleCBE")
    expect_true(nzchar(dir), info = template)

    yaml <- readLines(file.path(dir, "template.yaml"))
    expect_true(any(grepl("^name: \\S", yaml)), info = template)
    expect_true(any(grepl("^description:", yaml)), info = template)
    expect_true(file.exists(file.path(dir, "skeleton", "skeleton.Rmd")), info = template)
  }
})

test_that("picker skeleton files match their inst/templates sources", {
  for (template in names(skeleton_sources)) {
    sources <- skeleton_sources[[template]]
    for (skeleton_file in names(sources)) {
      copy <- system.file("rmarkdown", "templates", template, "skeleton", skeleton_file, package = "TempleCBE")
      source <- system.file("templates", sources[[skeleton_file]], package = "TempleCBE")
      expect_identical(
        unname(tools::md5sum(copy)),
        unname(tools::md5sum(source)),
        info = paste0(template, "/skeleton/", skeleton_file, " vs inst/templates/", sources[[skeleton_file]])
      )
    }
  }
})
