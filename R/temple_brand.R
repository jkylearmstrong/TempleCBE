# Temple University brand colors. Mirrors the `color.palette` block of the
# quarto_temple_brand extension's brand.yml, a copy of which ships in
# inst/brand/brand.yml (tests check the two agree).
temple_hex <- c(
  # Primary
  cherry            = "#a41e35",
  white             = "#ffffff",
  black             = "#000000",
  # Secondary
  `clear-skies`     = "#deefec",
  `book-nook`       = "#fff2e8",
  # Accents: formal
  `academic-gold`   = "#ad7422",
  `diamond-acres`   = "#9e9597",
  `founders-garden` = "#772762",
  `night-owl`       = "#005a70",
  # Accents: casual
  `owls-eye`        = "#f3aa00",
  `conwell-blue`    = "#12d0ff",
  `upward-momentum` = "#1fceb6",
  `cherry-blossom`  = "#fe649f"
)

temple_palettes <- list(
  main       = c("cherry", "night-owl", "owls-eye", "founders-garden",
                 "upward-momentum", "diamond-acres", "black"),
  diverging  = c("night-owl", "white", "cherry"),
  sequential = c("book-nook", "cherry")
)

#' Temple University Brand Colors
#'
#' Hex codes for the Temple University palette used by the
#' \href{https://github.com/jkylearmstrong-temple/quarto_temple_brand}{quarto_temple_brand}
#' Quarto extension, so R graphics match branded reports. The palette follows
#' Temple's current brand
#' (\url{https://liberalarts.temple.edu/marcom/logos-and-brand}): primary
#' cherry, white, and black; secondary Clear Skies and Book Nook; formal
#' accents Academic Gold, Diamond Acres, Founder's Garden, and Night Owl; and
#' casual accents Owl's Eye, Conwell Blue, Upward Momentum, and Cherry Blossom.
#'
#' @param ... Optional color names (e.g. \code{"cherry"}, \code{"night-owl"}).
#'   With none, every color is returned.
#' @return A named character vector of hex codes.
#' @seealso \code{\link{temple_pal}}, \code{\link{scale_colour_temple}},
#'   \code{\link{theme_temple}}
#' @export
#' @examples
#' temple_colors()
#' temple_colors("cherry", "night-owl")
temple_colors <- function(...) {
  cols <- c(...)
  if (is.null(cols)) return(temple_hex)
  unknown <- setdiff(cols, names(temple_hex))
  if (length(unknown)) {
    stop("Unknown Temple color(s): ", paste(unknown, collapse = ", "),
         ". Available: ", paste(names(temple_hex), collapse = ", "), call. = FALSE)
  }
  temple_hex[cols]
}

#' Temple Color Palettes
#'
#' Returns a palette function that generates \code{n} Temple brand colors.
#'
#' \describe{
#'   \item{\code{"main"}}{Qualitative: cherry, Night Owl, Owl's Eye,
#'     Founder's Garden, Upward Momentum, Diamond Acres, black. At most 7
#'     colors.}
#'   \item{\code{"diverging"}}{Night Owl through white to cherry, for values
#'     centered at zero (correlations, loadings, differences).}
#'   \item{\code{"sequential"}}{Book Nook to cherry.}
#' }
#'
#' @param palette One of \code{"main"}, \code{"diverging"}, or \code{"sequential"}.
#' @param reverse Logical; reverse the color order.
#' @return A function taking \code{n} and returning \code{n} hex codes.
#' @export
#' @examples
#' temple_pal()(3)
#' temple_pal("diverging")(5)
temple_pal <- function(palette = c("main", "diverging", "sequential"), reverse = FALSE) {
  palette <- match.arg(palette)
  cols <- unname(temple_hex[temple_palettes[[palette]]])
  if (isTRUE(reverse)) cols <- rev(cols)

  if (palette == "main") {
    function(n) {
      if (n > length(cols)) {
        stop("The Temple \"main\" palette has ", length(cols), " colors; ", n,
             " were requested.", call. = FALSE)
      }
      cols[seq_len(n)]
    }
  } else {
    grDevices::colorRampPalette(cols)
  }
}

#' Temple Color and Fill Scales for ggplot2
#'
#' @inheritParams temple_pal
#' @param discrete Logical; a discrete scale (default for \code{"main"}) or a
#'   continuous gradient.
#' @param midpoint For continuous \code{"diverging"} scales, the data value
#'   mapped to white (default 0).
#' @param ... Passed to \code{\link[ggplot2]{discrete_scale}},
#'   \code{\link[ggplot2]{scale_colour_gradient2}}, or
#'   \code{\link[ggplot2]{scale_colour_gradientn}}.
#' @return A ggplot2 scale.
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
#'   geom_point() +
#'   scale_colour_temple()
#'
#' ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
#'   geom_raster() +
#'   scale_fill_temple("sequential", discrete = FALSE)
scale_colour_temple <- function(palette = c("main", "diverging", "sequential"),
                                discrete = NULL, reverse = FALSE, midpoint = 0, ...) {
  temple_scale("colour", match.arg(palette), discrete, reverse, midpoint, ...)
}

#' @rdname scale_colour_temple
#' @export
scale_color_temple <- scale_colour_temple

#' @rdname scale_colour_temple
#' @export
scale_fill_temple <- function(palette = c("main", "diverging", "sequential"),
                              discrete = NULL, reverse = FALSE, midpoint = 0, ...) {
  temple_scale("fill", match.arg(palette), discrete, reverse, midpoint, ...)
}

temple_scale <- function(aesthetic, palette, discrete, reverse, midpoint, ...) {
  if (is.null(discrete)) discrete <- palette == "main"

  if (isTRUE(discrete)) {
    return(ggplot2::discrete_scale(aesthetic, palette = temple_pal(palette, reverse), ...))
  }

  cols <- unname(temple_hex[temple_palettes[[palette]]])
  if (isTRUE(reverse)) cols <- rev(cols)

  if (palette == "diverging") {
    ggplot2::continuous_scale(
      aesthetic,
      palette = scales::div_gradient_pal(cols[1], cols[2], cols[3]),
      rescaler = function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
        scales::rescale_mid(x, to, from, midpoint)
      },
      ...
    )
  } else {
    ggplot2::continuous_scale(aesthetic, palette = scales::gradient_n_pal(cols), ...)
  }
}

#' Temple ggplot2 Theme
#'
#' A minimal theme with Temple cherry titles and facet strips, matching
#' reports rendered with the quarto_temple_brand extension.
#'
#' @param base_size Base font size in points.
#' @param base_family Base font family. The brand's body typeface is
#'   \code{"Faustina"} (headings use \code{"Roboto"}); the default \code{""}
#'   uses the device font,
#'   because naming a font that isn't installed makes devices warn.
#' @return A ggplot2 theme.
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point(colour = temple_colors("cherry")) +
#'   facet_wrap(~cyl) +
#'   labs(title = "Weight vs. mileage") +
#'   theme_temple()
theme_temple <- function(base_size = 11, base_family = "") {
  cherry <- unname(temple_hex["cherry"])
  black <- unname(temple_hex["black"])

  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      text = ggplot2::element_text(colour = black),
      plot.title = ggplot2::element_text(colour = cherry, face = "bold", size = ggplot2::rel(1.2)),
      plot.title.position = "plot",
      strip.background = ggplot2::element_rect(fill = cherry, colour = NA),
      strip.text = ggplot2::element_text(colour = "white", face = "bold"),
      panel.grid.minor = ggplot2::element_blank()
    )
}

#' Path to the Bundled Temple brand.yml
#'
#' A copy of the quarto_temple_brand extension's \code{brand.yml}, for tools
#' that read brand files directly, such as \code{quarto::theme_brand_ggplot2()}
#' or \code{bslib::bs_theme(brand = )}.
#'
#' @return Path to \code{brand.yml} inside the installed package.
#' @export
#' @examples
#' temple_brand_path()
temple_brand_path <- function() {
  system.file("brand", "brand.yml", package = "TempleCBE", mustWork = TRUE)
}

#' Install the Temple Brand Quarto Extension Into a Project
#'
#' Installs \href{https://github.com/jkylearmstrong-temple/quarto_temple_brand}{quarto_temple_brand}
#' with \code{quarto::quarto_add_extension()}, creating a minimal
#' \code{_quarto.yml} first if there isn't one (Quarto only applies a brand
#' extension inside a project). Documents in \code{path} can then use
#' \code{format: temple-html}, \code{temple-pdf} (LaTeX title page),
#' \code{temple-typst}, or \code{temple-revealjs}, and every format picks up
#' the brand colors, fonts, and logo.
#'
#' \strong{Install once, at the project root.} Quarto resolves a project's
#' \code{_extensions} from any subfolder of that project, so a single install
#' at the root (where \code{_quarto.yml} lives) covers every report under it.
#' Calling \code{use_temple_brand()} again for each report's own subfolder
#' instead creates a separate \code{_extensions} copy per report, which can
#' drift out of sync as the extension is updated. See
#' \code{\link{create_report}}'s \code{install_brand} argument for the
#' one-report shortcut, which installs into that report's own folder and is
#' fine when there's only ever going to be one.
#'
#' @param path Project directory (created if missing). Defaults to the
#'   working directory.
#' @param extension Extension source passed to \code{quarto add}: a GitHub
#'   \code{org/repo}, a URL, or a local path.
#' @param quiet Logical; suppress Quarto's output.
#' @param check_root Logical; warn when \code{path} isn't the current
#'   project's root as found by \code{\link[here]{here}} (i.e. the nearest
#'   ancestor with a \code{.Rproj}, \code{.git}, or similar marker). Set to
#'   \code{FALSE} to install into a report's own subfolder without the
#'   warning, e.g. for a one-off report that won't grow siblings.
#' @return Invisibly, a list with \code{path}, \code{quarto_yml},
#'   \code{created_quarto_yml}, and \code{extension_dir}.
#' @seealso \code{\link{create_report}} with \code{template_name = "temple"}.
#' @export
#' @examples
#' \dontrun{
#' # Install once at the project root; every report below it shares the
#' # extension automatically.
#' use_temple_brand(here::here())
#' create_report(here::here("analysis"), template_name = "temple")
#' create_report(here::here("reports"), template_name = "temple", filename = "q3")
#'
#' # A single, one-off report: install alongside it and skip the check.
#' use_temple_brand("analysis", check_root = FALSE)
#' create_report("analysis", template_name = "temple")
#' }
use_temple_brand <- function(path = ".",
                             extension = "jkylearmstrong-temple/quarto_temple_brand",
                             quiet = FALSE,
                             check_root = TRUE) {
  if (!requireNamespace("quarto", quietly = TRUE)) {
    stop("Package 'quarto' is required by use_temple_brand(). Install it with install.packages(\"quarto\").", call. = FALSE)
  }
  if (!requireNamespace("withr", quietly = TRUE)) {
    stop("Package 'withr' is required by use_temple_brand().", call. = FALSE)
  }

  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)

  if (isTRUE(check_root)) {
    project_root <- tryCatch(
      normalizePath(here::here(), winslash = "/", mustWork = FALSE),
      error = function(e) NA_character_
    )
    if (!is.na(project_root) && !identical(path, project_root)) {
      warning(
        "Installing quarto_temple_brand into '", path, "', not the project root ",
        "('", project_root, "', per here::here()). If another report also calls ",
        "use_temple_brand() or create_report(..., install_brand = TRUE), each gets ",
        "its own '_extensions' copy instead of sharing one. Prefer ",
        "use_temple_brand(here::here()) once at the root, then create_report(location, ",
        "template_name = \"temple\") (install_brand = FALSE, the default) for each ",
        "report. Pass check_root = FALSE to install here anyway without this warning.",
        call. = FALSE
      )
    }
  }

  quarto_yml <- file.path(path, c("_quarto.yml", "_quarto.yaml"))
  created <- !any(file.exists(quarto_yml))
  quarto_yml <- if (created) quarto_yml[1] else quarto_yml[file.exists(quarto_yml)][1]
  if (created) writeLines(c("project:", "  type: default"), quarto_yml)

  withr::with_dir(path, quarto::quarto_add_extension(extension, no_prompt = TRUE, quiet = quiet))

  extension_dir <- temple_extension_dir(path)
  if (is.na(extension_dir)) {
    stop("Quarto finished, but no 'temple' extension was found under ",
         file.path(path, "_extensions"), ".", call. = FALSE)
  }

  invisible(list(path = path, quarto_yml = quarto_yml,
                 created_quarto_yml = created, extension_dir = extension_dir))
}

# Directory of the installed temple extension (_extensions/temple or
# _extensions/<org>/temple), or NA if absent.
temple_extension_dir <- function(path) {
  manifests <- list.files(file.path(path, "_extensions"), pattern = "^_extension\\.ya?ml$",
                          recursive = TRUE, full.names = TRUE)
  dirs <- dirname(manifests)[basename(dirname(manifests)) == "temple"]
  if (length(dirs)) dirs[[1]] else NA_character_
}

# Like temple_extension_dir(), but also checks ancestor directories up to
# (and including) the enclosing project root, mirroring how Quarto itself
# resolves a project's _extensions from any subfolder. Used to avoid telling
# users to (re)install when the extension already lives at the project root.
temple_extension_dir_upward <- function(path) {
  path <- tryCatch(normalizePath(path, winslash = "/", mustWork = FALSE), error = function(e) path)
  repeat {
    found <- temple_extension_dir(path)
    if (!is.na(found)) return(found)
    is_project_root <- any(file.exists(file.path(path, c("_quarto.yml", "_quarto.yaml"))))
    parent <- dirname(path)
    if (is_project_root || identical(parent, path)) return(NA_character_)
    path <- parent
  }
}
