#' Locate a SAS Executable
#'
#' Checks, in order: `getOption("templecbe.sas")`, the `SAS_EXE` environment
#' variable, `sas` on the `PATH`, and the default SAS 9 install locations
#' (`SASHome/SASFoundation/<version>` under `C:/Program Files` or
#' `C:/Program Files (x86)` on Windows; under `/usr/local/SASHome`,
#' `/opt/sas/SASHome`, or `/opt/SASHome` elsewhere), preferring the highest
#' installed version.
#'
#' @return The path to the SAS executable, or `NULL` if none is found.
#' @seealso [run_sas_script()]
#' @export
#' @examples
#' find_sas()
find_sas <- function() {
  configured <- c(getOption("templecbe.sas", ""), Sys.getenv("SAS_EXE", ""), unname(Sys.which("sas")))
  configured <- configured[nzchar(configured) & file.exists(configured)]
  if (length(configured) > 0) {
    return(configured[1])
  }

  pattern <- if (.Platform$OS.type == "windows") {
    file.path(c("C:/Program Files", "C:/Program Files (x86)"), "SASHome", "SASFoundation", "*", "sas.exe")
  } else {
    file.path(c("/usr/local/SASHome", "/opt/sas/SASHome", "/opt/SASHome"), "SASFoundation", "*", "sas")
  }
  found <- Sys.glob(pattern)
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
#' @seealso [find_sas()]
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
  system2(sas_path, sas_args(path, log_dir, list_dir))
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
