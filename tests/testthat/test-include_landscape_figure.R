test_that("LaTeX output wraps the figure in a landscape environment", {
  out <- include_landscape_figure("fig/a.png", caption = "Cap", label = "fig-a", format = "latex")
  expect_s3_class(out, "knit_asis")
  txt <- as.character(out)
  expect_match(txt, "\\begin{landscape}", fixed = TRUE)
  expect_match(txt, "\\end{landscape}", fixed = TRUE)
  expect_match(txt, "![Cap](fig/a.png){#fig-a width=\"100%\"}", fixed = TRUE)
  b <- as.character(include_landscape_figure("a.png", format = "latex", environment = "blandscape"))
  expect_match(b, "\\blandscape", fixed = TRUE)
  expect_match(b, "\\elandscape", fixed = TRUE)
})

test_that("HTML, Word and other output carry no LaTeX", {
  for (f in c("html", "docx", "other")) {
    txt <- as.character(include_landscape_figure("a.png", caption = "C", width = "8in", format = f))
    expect_false(grepl("\\", txt, fixed = TRUE))
    expect_match(txt, "![C](a.png){width=\"8in\"}", fixed = TRUE)
  }
})

test_that("the format is detected from knitr when not given", {
  skip_if_not_installed("knitr")
  local_mocked_bindings(is_latex_output = function() TRUE, .package = "knitr")
  expect_match(as.character(include_landscape_figure("a.png")), "begin{landscape}", fixed = TRUE)
  local_mocked_bindings(is_latex_output = function() FALSE, is_html_output = function(...) TRUE, .package = "knitr")
  expect_false(grepl("landscape", as.character(include_landscape_figure("a.png")), fixed = TRUE))
})

test_that("include_landscape_figure() validates its arguments", {
  expect_error(include_landscape_figure(c("a", "b")), "one file path")
  expect_error(include_landscape_figure("a.png", label = "1 bad"), "label")
  expect_error(include_landscape_figure("a.png", width = "wide"), "width")
  expect_error(include_landscape_figure("a.png", caption = 1), "caption")
  expect_error(include_landscape_figure("a.png", format = "pdf"))
})
