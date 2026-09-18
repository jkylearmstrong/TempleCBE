#' Build a Versioned PDF Reference Manual
#'
#' Renders the package's Rd documentation to a PDF manual (via \code{R CMD
#' Rd2pdf}) named with the current package version, so successive builds
#' never collide or overwrite each other. Writes into
#' \code{pkgdown/assets/manual/} by default: pkgdown copies
#' \code{pkgdown/assets/*} verbatim into \code{docs/} on every
#' \code{pkgdown::build_site()}, which is what makes a navbar link to the
#' manual resolve on the published site. (\code{inst/manual/} would not be
#' copied there, and would also ship with every installed copy of the
#' package.)
#'
#' @param pkg Path to the package, passed to \code{\link[devtools]{as.package}}.
#' @param path Directory to write the PDF into. Defaults to
#'   \code{pkgdown/assets/manual/} under the package root.
#' @param latest Logical (default \code{TRUE}); if \code{TRUE}, also writes a
#'   stable \code{<package>_latest.pdf} copy alongside the versioned file, so
#'   a permanent link (e.g. in a pkgdown navbar) doesn't need to change on
#'   every version bump.
#' @return The path to the versioned PDF, invisibly.
#' @export
#' @examples
#' \dontrun{
#' build_manual_versioned()
#' }
build_manual_versioned <- function(pkg = ".", path = NULL, latest = TRUE) {
  rlang::check_installed("callr", reason = "to build the PDF manual via R CMD Rd2pdf.")

  pkg <- devtools::as.package(pkg)

  path <- if (is.null(path)) fs::path(pkg$path, "pkgdown", "assets", "manual") else path
  fs::dir_create(path)

  name <- paste0(pkg$package, "_", pkg$version, ".pdf")
  out_file <- fs::path(path, name)

  # R_RD4PDF controls hyperlink/TeX style (e.g. "times,inconsolata,hyper").
  # Warn rather than force it, since overriding here could silently change
  # output for other callers relying on the same environment variable.
  rd_style <- Sys.getenv("R_RD4PDF", "")
  if (nzchar(rd_style) && !grepl("hyper", rd_style, fixed = TRUE)) {
    warning("Hyperlinks appear disabled (R_RD4PDF='", rd_style, "').")
  }

  result <- tryCatch(
    callr::rcmd(
      "Rd2pdf",
      cmdargs = c("--force", paste0("--output=", out_file), pkg$path),
      fail_on_status = TRUE,
      # Merge stderr into stdout: works around a Windows callr/processx bug
      # where a failed subprocess's e$stdout comes back empty.
      stderr = "2>&1",
      spinner = FALSE
    ),
    error = function(e) {
      cat(e$stdout)
      stop("Failed to build manual: ", conditionMessage(e), call. = FALSE)
    }
  )
  cat(result$stdout)

  if (isTRUE(latest)) {
    fs::file_copy(
      out_file,
      fs::path(path, paste0(pkg$package, "_latest.pdf")),
      overwrite = TRUE
    )
  }

  message("✅ Manual built: ", out_file)
  invisible(out_file)
}
