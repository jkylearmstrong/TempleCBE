#' Scaffold a New Report From a Template
#'
#' Copies a bundled report template (and its supporting bibliography/title
#' files) into \code{location}.
#'
#' The \code{"temple"} template renders with the
#' \href{https://github.com/jkylearmstrong-temple/quarto_temple_brand}{quarto_temple_brand}
#' extension's \code{temple-html}, \code{temple-pdf}, and \code{temple-typst}
#' formats, so it needs that extension installed beside it (see
#' \code{\link{use_temple_brand}}). It uses no \code{title.tex} or child
#' document, and is Quarto-only.
#'
#' @param location Directory to create the report in (default \code{getwd()}).
#' @param template_name One of \code{"t_test_example"} (default), \code{"example"}, or \code{"temple"}.
#' @param child Logical (default \code{TRUE}); also copy the child-document template.
#' @param type One of \code{".qmd"} (default) or \code{".Rmd"}.
#' @param include_bib Logical (default \code{TRUE}); also copy the \code{.bib} file.
#' @param include_tex Logical (default \code{TRUE}); also copy the title \code{.tex} file.
#' @param install_brand Logical (default \code{FALSE}); for the \code{"temple"}
#'   template, also run \code{\link{use_temple_brand}(location)}, which
#'   downloads the extension.
#' @return A list indicating whether each file was created.
#' @export
#' @examples
#' \dontrun{
#' create_report(here::here("analysis"))
#' create_report(here::here("analysis"), template_name = "temple", install_brand = TRUE)
#' }
create_report <- function(location = getwd(), template_name = "t_test_example",
                           child = TRUE, type = ".qmd", include_bib = TRUE, include_tex = TRUE,
                           install_brand = FALSE) {
  template_list <- c("t_test_example", "example", "temple")
  if (!(template_name %in% template_list)) {
    warning("`template_name` should be one of: ", paste(template_list, collapse = ", "), ". Using t_test_example.")
    template_name <- "t_test_example"
  }

  type_list <- c(".qmd", ".Rmd")
  if (!(type %in% type_list)) {
    warning("`type` should be one of: ", paste(type_list, collapse = ", "), ". Using .qmd.")
    type <- ".qmd"
  }

  is_temple <- template_name == "temple"
  if (is_temple && type != ".qmd") {
    warning("The `temple` template is Quarto-only. Using .qmd.")
    type <- ".qmd"
  }

  template_path <- system.file("templates", paste0(template_name, type), package = "TempleCBE")
  child_path <- system.file("templates", paste0("t_test_child", type), package = "TempleCBE")
  bib_path <- system.file("templates", "bib.bib", package = "TempleCBE")
  title_path <- system.file("templates", "title.tex", package = "TempleCBE")

  new_report_path <- file.path(location, paste0(template_name, type))
  new_child_path <- file.path(location, paste0("t_test_child", type))
  new_bib_path <- file.path(location, "bib.bib")
  new_title_path <- file.path(location, "title.tex")

  created <- list(
    template_created = file.copy(template_path, new_report_path),
    bib_created = if (include_bib) file.copy(bib_path, new_bib_path) else FALSE,
    title_tex_created = if (include_tex && !is_temple) file.copy(title_path, new_title_path) else FALSE,
    child_created = if (child && !is_temple) file.copy(child_path, new_child_path) else FALSE
  )

  if (is_temple) {
    if (isTRUE(install_brand)) {
      use_temple_brand(location)
    } else if (is.na(temple_extension_dir(location))) {
      message("The temple template renders with the quarto_temple_brand extension; ",
              "install it with use_temple_brand(\"", location, "\").")
    }
  }

  created
}
