#' Locate a SAS Executable
#'
#' Checks, in order: `getOption("templecbe.sas")`, the `SAS_EXE` environment
#' variable, the `SASROOT` environment variable (`sas.exe`), `sas` on the `PATH`,
#' and default SAS install locations (`SASHome/SASFoundation/<version>` under
#' `C:/Program Files`, `C:/Program Files (x86)`, `C:`, `D:/Program Files`, or
#' `D:` on Windows; under `/usr/local/SASHome`, `/opt/sas/SASHome`, or
#' `/opt/SASHome` elsewhere), preferring the highest installed version.
#'
#' @return The path to the SAS executable, or `NULL` if none is found.
#' @seealso [run_sas_script()], [cbe_sas_macro_path()], [cbe_sas_macro_dir()]
#' @export
#' @examples
#' find_sas()
find_sas <- function() {
  sasroot_exe <- if (nzchar(Sys.getenv("SASROOT", ""))) {
    file.path(Sys.getenv("SASROOT"), if (.Platform$OS.type == "windows") "sas.exe" else "sas")
  } else {
    ""
  }
  sashome_env <- Sys.getenv("SASHOME", "")

  configured <- c(
    getOption("templecbe.sas", ""),
    Sys.getenv("SAS_EXE", ""),
    sasroot_exe,
    unname(Sys.which("sas"))
  )
  configured <- configured[nzchar(configured) & file.exists(configured)]
  if (length(configured) > 0) {
    return(configured[1])
  }

  pattern <- if (.Platform$OS.type == "windows") {
    search_roots <- c(
      "C:/Program Files",
      "C:/Program Files (x86)",
      "C:",
      "D:/Program Files",
      "D:/Program Files (x86)",
      "D:"
    )
    if (nzchar(sashome_env)) {
      search_roots <- c(sashome_env, search_roots)
    }
    c(
      file.path(search_roots, "SASHome", "SASFoundation", "*", "sas.exe"),
      file.path(search_roots, "SASFoundation", "*", "sas.exe")
    )
  } else {
    search_roots <- c("/usr/local/SASHome", "/opt/sas/SASHome", "/opt/SASHome")
    if (nzchar(sashome_env)) {
      search_roots <- c(sashome_env, search_roots)
    }
    file.path(search_roots, "SASFoundation", "*", "sas")
  }
  found <- Sys.glob(pattern)
  found <- found[file.exists(found)]
  if (length(found) == 0) {
    return(NULL)
  }
  # SAS version folders look like "9.4" or "9.4m8"; "m" marks a maintenance release.
  versions <- numeric_version(gsub("m", ".", basename(dirname(found))), strict = FALSE)
  found[order(versions, decreasing = TRUE, na.last = TRUE)][1]
}

#' Run a SAS Program in Batch Mode
#'
#' Runs a `.sas` file with the SAS executable, writing its log and listing to
#' separate folders next to the program (created if needed).
#'
#' @param path Path to the `.sas` program.
#' @param sas_path Path to the SAS executable. Defaults to [find_sas()].
#' @param log_dir Folder for the `.log` file. Defaults to `logs/` beside
#'   `path`.
#' @param list_dir Folder for the `.lst` listing. Defaults to `list/` beside
#'   `path`.
#' @return The exit status of the SAS process (0 on success, 1 for warnings, 2
#'   for errors).
#' @seealso [find_sas()], [cbe_sas_macro_path()]
#' @export
#' @examples
#' \dontrun{
#' programs <- list.files("validation", pattern = "\\.sas$", full.names = TRUE)
#' vapply(programs, run_sas_script, integer(1))
#' }
run_sas_script <- function(path,
                           sas_path = find_sas(),
                           log_dir = file.path(dirname(path), "logs"),
                           list_dir = file.path(dirname(path), "list")) {
  if (!is.character(path) || length(path) != 1 || !file.exists(path)) {
    stop("`path` must be the path of an existing .sas file.", call. = FALSE)
  }
  if (is.null(sas_path) || !nzchar(sas_path)) {
    stop(
      "No SAS executable found. Pass `sas_path`, set options(templecbe.sas = ...), ",
      "or set the SAS_EXE environment variable.",
      call. = FALSE
    )
  }
  dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(list_dir, recursive = TRUE, showWarnings = FALSE)
  res <- safe_system2(sas_path, sas_args(path, log_dir, list_dir), check = FALSE, log_failures = FALSE)
  status <- attr(res, "status")
  exit_code <- if (is.null(status)) {
    if (is.numeric(res) && length(res) == 1L) as.integer(res) else 0L
  } else {
    as.integer(status)
  }
  stem <- sub("\\.sas$", "", basename(path), ignore.case = TRUE)
  log_file <- file.path(log_dir, paste0(stem, ".log"))
  # SAS's own diagnostics normally land in `log_file`, but a failure before
  # SAS even starts writing it (license failure, bad -log path, disk full)
  # leaves nothing to review there; fall back to whatever was captured on
  # stdout/stderr so the failure isn't completely silent.
  fallback_output <- if (exit_code != 0L && !file.exists(log_file) && is.character(res) && length(res) > 0L) {
    paste0("\nNo log file was written. Captured output:\n", paste(res, collapse = "\n"))
  } else {
    ""
  }
  if (exit_code >= 2L) {
    stop(
      sprintf("SAS process failed with exit code %d (errors encountered). Review log at: %s%s", exit_code, log_file, fallback_output),
      call. = FALSE
    )
  }
  if (exit_code == 1L) {
    warning(
      sprintf("SAS process completed with warnings (exit code 1). Review log at: %s%s", log_file, fallback_output),
      call. = FALSE
    )
  }
  invisible(exit_code)
}

#' Command-Line Arguments for a Batch SAS Run
#'
#' @keywords internal
#' @noRd
sas_args <- function(path, log_dir, list_dir) {
  stem <- sub("\\.sas$", "", basename(path), ignore.case = TRUE)
  c(
    shQuote(path),
    "-log", shQuote(file.path(log_dir, paste0(stem, ".log"))),
    "-print", shQuote(file.path(list_dir, paste0(stem, ".lst")))
  )
}

#' Directory of TempleCBE SAS Macros
#'
#' Returns the file system path to the directory containing TempleCBE's
#' bundled SAS macros (`cbe_brier_score.sas`, `cbe_cox_phreg.sas`,
#' `cbe_counting_process.sas`, and `cbe_macros.sas`).
#'
#' @return Absolute path to the SAS macro directory.
#' @seealso [cbe_sas_macro_path()], [run_sas_script()], [find_sas()]
#' @export
#' @examples
#' try(cbe_sas_macro_dir())
cbe_sas_macro_dir <- function() {
  dir <- locate_package_path("sas")
  if (is.null(dir)) {
    stop("Could not locate TempleCBE SAS macro directory.", call. = FALSE)
  }
  dir
}

#' Path to a TempleCBE SAS Macro File
#'
#' Returns the absolute path to an installed TempleCBE SAS macro or benchmark script.
#'
#' @param macro Filename of the SAS macro or script (default: `"cbe_macros.sas"`).
#'   Available files include `"cbe_macros.sas"`, `"cbe_brier_score.sas"`,
#'   `"cbe_cox_phreg.sas"`, `"cbe_counting_process.sas"`, `"coxtvc.sas"`,
#'   `"cpdata.sas"`, `"benchmark_brier_lung.sas"`, and `"example_85_7.sas"`.
#' @return Absolute path to the requested `.sas` file.
#' @seealso [cbe_sas_macro_dir()], [run_sas_script()], [find_sas()]
#' @export
#' @examples
#' try(cbe_sas_macro_path("cbe_macros.sas"))
cbe_sas_macro_path <- function(macro = "cbe_macros.sas") {
  if (!is.character(macro) || length(macro) != 1 || !nzchar(macro)) {
    stop("`macro` must be a single non-empty character string.", call. = FALSE)
  }
  if (!grepl("\\.sas$", macro, ignore.case = TRUE)) {
    macro <- paste0(macro, ".sas")
  }
  dir <- cbe_sas_macro_dir()
  target <- file.path(dir, macro)
  if (!file.exists(target)) {
    available <- list.files(dir, pattern = "\\.sas$", ignore.case = TRUE)
    stop(
      "SAS macro '", macro, "' was not found in ", dir, ".\n",
      "Available macros: ", paste(available, collapse = ", "),
      call. = FALSE
    )
  }
  normalizePath(target, winslash = "/", mustWork = TRUE)
}
