#' Scaffold a New Report From a Template
#'
#' Copies a bundled report template (and its supporting bibliography/title
#' files) into \code{location}.
#'
#' The \code{"temple"} template renders with the
#' \href{https://github.com/jkylearmstrong-temple/quarto_temple_brand}{quarto_temple_brand}
#' extension's \code{temple-html}, \code{temple-pdf}, and \code{temple-typst}
#' formats, so it needs that extension installed somewhere Quarto can find it
#' from \code{location} (see \code{\link{use_temple_brand}}): either at an
#' ancestor project root shared by every report, or, with \code{install_brand
#' = TRUE}, in \code{location} itself for a one-off report. It uses no
#' \code{title.tex} or child document, and is Quarto-only.
#'
#' @param location Directory to create the report in (default \code{getwd()}).
#' @param template_name One of \code{"t_test_example"} (default), \code{"example"}, or \code{"temple"}.
#' @param child Logical (default \code{TRUE}); also copy the child-document template.
#' @param type One of \code{".qmd"} (default) or \code{".Rmd"}.
#' @param include_bib Logical (default \code{TRUE}); also copy the \code{.bib} file.
#' @param include_tex Logical (default \code{TRUE}); also copy the title \code{.tex} file.
#' @param install_brand Logical (default \code{FALSE}); for the \code{"temple"}
#'   template, also run \code{\link{use_temple_brand}(location)}, which
#'   downloads the extension into \code{location} itself. Leave this
#'   \code{FALSE} when the extension is already installed at the project
#'   root (see \code{\link{use_temple_brand}}); it doesn't need reinstalling
#'   per report.
#' @param filename Base name (no extension) for the report file, default
#'   \code{NULL} uses \code{template_name}. The report is always written to
#'   \code{location}, so calling \code{create_report()} twice for the same
#'   \code{location} with the same \code{template_name} (or the same
#'   \code{filename}) overwrites the first report; pass a distinct
#'   \code{filename} for each report that shares a \code{location}, e.g.
#'   \code{create_report("analysis", filename = "analysis1")} and
#'   \code{create_report("analysis", filename = "analysis2")}.
#' @return A list indicating whether each file was created.
#' @export
#' @examples
#' \dontrun{
#' create_report(here::here("analysis"))
#'
#' # A project with several "temple" reports: install the extension once at
#' # the project root, then scaffold each report without install_brand.
#' use_temple_brand(here::here())
#' create_report(here::here("analysis"), template_name = "temple")
#' create_report(here::here("reports"), template_name = "temple", filename = "q3")
#'
#' # A single one-off "temple" report instead installs beside itself.
#' create_report(here::here("analysis"), template_name = "temple", install_brand = TRUE)
#'
#' # Two reports sharing one location need distinct `filename`s, or the
#' # second call overwrites the first report file:
#' create_report(here::here("analysis"), template_name = "temple", filename = "analysis1")
#' create_report(here::here("analysis"), template_name = "temple", filename = "analysis2")
#' }
create_report <- function(location = getwd(), template_name = "t_test_example",
                           child = TRUE, type = ".qmd", include_bib = TRUE, include_tex = TRUE,
                           install_brand = FALSE, filename = NULL) {
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

  # Validate destination location
  if (!is.character(location) || length(location) != 1L || !nzchar(trimws(location))) {
    stop("`location` must be a single non-empty directory path.", call. = FALSE)
  }
  if (!dir.exists(location)) {
    dir.create(location, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(location)) {
    stop("Destination directory '", location, "' could not be created or accessed.", call. = FALSE)
  }
  if (file.access(location, 2) != 0) {
    stop("Destination directory '", location, "' is not writable (permission denied).", call. = FALSE)
  }

  if (!is.null(filename)) {
    if (!is.character(filename) || length(filename) != 1L || !nzchar(trimws(filename))) {
      stop("`filename` must be a single non-empty string, or NULL to use `template_name`.", call. = FALSE)
    }
    # Strip an extension the caller may have included, so it isn't doubled.
    filename <- sub("\\.(qmd|Rmd)$", "", trimws(filename), ignore.case = TRUE)
  }
  report_name <- if (is.null(filename)) template_name else filename

  template_path <- system.file("templates", paste0(template_name, type), package = "TempleCBE")
  child_path <- system.file("templates", paste0("t_test_child", type), package = "TempleCBE")
  bib_path <- system.file("templates", "bib.bib", package = "TempleCBE")
  title_path <- system.file("templates", "title.tex", package = "TempleCBE")

  new_report_path <- file.path(location, paste0(report_name, type))
  if (file.exists(new_report_path)) {
    warning(
      "Overwriting existing report file '", new_report_path, "'. ",
      "Pass a different `filename` to create a separate report in this `location` instead.",
      call. = FALSE
    )
  }
  new_child_path <- file.path(location, paste0("t_test_child", type))
  new_bib_path <- file.path(location, "bib.bib")
  new_title_path <- file.path(location, "title.tex")

  # Helper to validate source template files (existence & readability)
  check_template_src <- function(path, desc) {
    if (!nzchar(path) || !file.exists(path)) {
      stop(
        "Template file '", desc, "' does not exist. ",
        "The package templates directory appears corrupted or missing required files.",
        call. = FALSE
      )
    }
    if (file.access(path, 4) != 0) {
      stop("Template file '", path, "' is not readable (permission denied).", call. = FALSE)
    }
  }

  # Helper to validate destination file writeability
  check_target_dst <- function(path) {
    if (file.exists(path) && file.access(path, 2) != 0) {
      stop("Destination file '", path, "' already exists and is not writable (permission denied).", call. = FALSE)
    }
  }

  # Pre-flight validate all files that will be copied
  check_template_src(template_path, paste0(template_name, type))
  check_target_dst(new_report_path)

  if (include_bib) {
    check_template_src(bib_path, "bib.bib")
    check_target_dst(new_bib_path)
  }

  if (include_tex && !is_temple) {
    check_template_src(title_path, "title.tex")
    check_target_dst(new_title_path)
  }

  if (child && !is_temple) {
    check_template_src(child_path, paste0("t_test_child", type))
    check_target_dst(new_child_path)
  }

  created <- list(
    template_created = file.copy(template_path, new_report_path, overwrite = TRUE),
    bib_created = if (include_bib) file.copy(bib_path, new_bib_path, overwrite = TRUE) else FALSE,
    title_tex_created = if (include_tex && !is_temple) file.copy(title_path, new_title_path, overwrite = TRUE) else FALSE,
    child_created = if (child && !is_temple) file.copy(child_path, new_child_path, overwrite = TRUE) else FALSE
  )

  if (is_temple) {
    if (isTRUE(install_brand)) {
      use_temple_brand(location, check_root = FALSE)
    } else if (is.na(temple_extension_dir_upward(location))) {
      message("The temple template renders with the quarto_temple_brand extension; ",
              "install it once at the project root with use_temple_brand(here::here()), ",
              "or beside this report with use_temple_brand(\"", location, "\").")
    }
  }

  created
}
