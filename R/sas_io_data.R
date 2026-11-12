#' Rossi Recidivism Dataset
#'
#' Loads the classic Rossi recidivism study dataset used in biostatistical benchmarks
#' for survival models with time-varying coefficients and covariates (e.g., JSS Vol 61, Code 01).
#' The study tracks 432 convicts released from Maryland state prisons over a 52-week follow-up period.
#'
#' @param format Either `"csv"` (default, reading `inst/extdata/rossi.csv`) or
#'   `"sas"` (reading the native `inst/extdata/recid.sas7bdat` via [haven::read_sas()]).
#' @return A tibble with 432 rows containing recidivism follow-up data:
#'   \describe{
#'     \item{week}{Follow-up time until arrest or censoring (1--52 weeks).}
#'     \item{arrest}{Event indicator (1 = arrested, 0 = censored/did not re-offend).}
#'     \item{fin}{Financial aid treatment (1 = received financial aid, 0 = control).}
#'     \item{age}{Age at release from prison in years.}
#'     \item{race}{Race indicator (1 = Black, 0 = other).}
#'     \item{wexp}{Prior full-time work experience (1 = yes, 0 = no).}
#'     \item{mar}{Marital status (1 = married, 0 = unmarried).}
#'     \item{paro}{Release on parole (1 = yes, 0 = no).}
#'     \item{prio}{Number of prior convictions.}
#'     \item{educ}{Education level coded as 2 (grades 2-5) through 6 (some college).}
#'     \item{emp1--emp52}{Weekly employment status indicator across the 52 weeks.}
#'   }
#' @source Rossi, P. H., Berk, R. A., & Lenihan, K. J. (1980). *Money, work, and crime:
#'   Some experimental results*. Academic Press.
#' @seealso [sas_macro_path()], [run_sas_script()], [read_sas_data()], [write_sas_data()],
#'   [sas_parity_boundary()]
#' @export
#' @examples
#' df <- rossi_data()
#' head(df[, 1:10])
rossi_data <- function(format = c("csv", "sas")) {
  format <- match.arg(format)
  if (format == "sas") {
    rlang::check_installed("haven", reason = "to read native .sas7bdat files.")
    path <- locate_package_path("extdata", "recid.sas7bdat")
    if (is.null(path)) {
      stop("Could not locate recid.sas7bdat.", call. = FALSE)
    }
    return(tibble::as_tibble(haven::read_sas(path)))
  }

  path <- locate_package_path("extdata", "rossi.csv")
  if (is.null(path)) {
    stop("Could not locate rossi.csv.", call. = FALSE)
  }
  tibble::as_tibble(utils::read.csv(path, stringsAsFactors = FALSE))
}

#' Read or Write a SAS Data Set
#'
#' General R-to-SAS data-exchange wrappers. Before them, the only SAS reading
#' in the package was [rossi_data(format = "sas")] for the one bundled file,
#' and the R-to-SAS vignette used a bare `haven::write_sas()` call with no
#' TempleCBE function around it. These wrappers add the checks that bare
#' `haven` calls do not: the file must exist and have a SAS extension when
#' reading, the data must be a data frame and the target extension must be a
#' SAS format when writing, and SAS date/time variables are converted on the
#' way in (see Details).
#'
#' @param path Path to a `.sas7bdat` or `.xpt` file (`.csv` is also accepted
#'   by [read_sas_data()], as the plain-text interchange fallback). For
#'   [write_sas_data()], the extension decides the format when `format =
#'   "auto"`.
#' @param data A data frame to write.
#' @param format `"auto"` (default: infer from the extension), `"sas7bdat"`,
#'   `"xpt"` (SAS Version 5 transport, the FDA submission format), or, for
#'   reading only, `"csv"`.
#' @param ... Passed to [haven::read_sas()] / [haven::read_xpt()] when reading
#'   (for example `col_select`), or to [haven::write_sas()] /
#'   [haven::write_xpt()] when writing.
#'
#' @details
#' `haven` preserves SAS variable labels, formats and date/time classes as R
#' attributes. [read_sas_data()] additionally converts SAS date variables
#' (class `Date` already, from `haven`) and leaves SAS datetimes as
#' `POSIXct`; it does not strip labels. SAS missing values (`.`, `.A`--`.Z`)
#' arrive as `NA`; SAS special missing values are not distinguished on the
#' way in, matching the parity boundary in [sas_parity_boundary()] - see the
#' `PROC COMPARE` row there.
#'
#' SAS variable names are at most 32 characters; [write_sas_data()] stops
#' when a column name is longer, rather than letting `haven` truncate it
#' silently and collide two variables. Names that are not valid SAS names
#' (starting with a digit, containing spaces or symbols) are likewise an
#' error: rename first, for example with [reconcile_column_dictionary()].
#'
#' Note that `haven` 2.5.2 deprecated writing `.sas7bdat` files
#' ([haven::write_sas()]) in favour of transport files, so a `.sas7bdat`
#' write warns (with TempleCBE's own message, once per call, rather than
#' haven's once-in-a-while lifecycle warning); the write itself still works.
#' Prefer `.xpt` for new exports (with its 8-character name limits, checked
#' above).
#'
#' @return [read_sas_data()]: a tibble. [write_sas_data()]: invisibly, the
#'   normalized path written.
#' @seealso [rossi_data()], [sas_macro_path()], [run_sas_script()],
#'   [sas_parity_boundary()], [reconcile_column_dictionary()]
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("haven", quietly = TRUE)) {
#'   p <- file.path(tempdir(), "rossi.xpt")
#'   write_sas_data(rossi_data(), p)
#'   back <- read_sas_data(p)
#'   nrow(back)
#' }
#' }
read_sas_data <- function(path, format = c("auto", "sas7bdat", "xpt", "csv"), ...) {
  format <- match.arg(format)
  if (!is.character(path) || length(path) != 1L || !nzchar(path) || !file.exists(path)) {
    stop("`path` must be the path of an existing file.", call. = FALSE)
  }
  ext <- tolower(tools::file_ext(path))
  if (identical(format, "auto")) {
    format <- switch(ext,
      sas7bdat = "sas7bdat",
      xpt = "xpt",
      csv = "csv",
      stop("Cannot infer the format from extension '.", ext, "': use `format`.", call. = FALSE)
    )
  }
  if (identical(format, "csv")) {
    return(tibble::as_tibble(utils::read.csv(path, stringsAsFactors = FALSE, ...)))
  }
  rlang::check_installed("haven", reason = "to read SAS data sets with `read_sas_data()`.")
  out <- if (identical(format, "sas7bdat")) {
    haven::read_sas(path, ...)
  } else {
    haven::read_xpt(path, ...)
  }
  tibble::as_tibble(out)
}

#' @rdname read_sas_data
#' @export
write_sas_data <- function(data, path, format = c("auto", "sas7bdat", "xpt"), ...) {
  format <- match.arg(format)
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  if (!is.character(path) || length(path) != 1L || !nzchar(path)) {
    stop("`path` must be a single non-empty file path.", call. = FALSE)
  }
  ext <- tolower(tools::file_ext(path))
  if (identical(format, "auto")) {
    format <- switch(ext,
      sas7bdat = "sas7bdat",
      xpt = "xpt",
      stop("Cannot infer the format from extension '.", ext, "': use `format`.", call. = FALSE)
    )
  }
  if (nrow(data) == 0L) {
    warning("`data` has 0 rows; writing an empty SAS data set.", call. = FALSE)
  }
  long_names <- names(data)[nchar(names(data)) > 32L]
  if (length(long_names) > 0L) {
    stop(
      "SAS variable names are at most 32 characters; too long: ",
      paste(long_names, collapse = ", "), ". Rename them first.", call. = FALSE
    )
  }
  if (identical(format, "xpt")) {
    # SAS Version 5 transport files limit variable names to 8 characters;
    # haven::write_xpt() would error on longer names without naming them.
    long_xpt <- names(data)[nchar(names(data)) > 8L]
    if (length(long_xpt) > 0L) {
      stop(
        "SAS transport (.xpt) variable names are at most 8 characters; too long: ",
        paste(long_xpt, collapse = ", "), ". Rename them first.", call. = FALSE
      )
    }
    # The transport member (data set) name is the file stem, also at most
    # 8 characters; haven::write_xpt() errors on a longer stem as "`name`".
    stem <- tools::file_path_sans_ext(basename(path))
    if (nchar(stem) > 8L) {
      stop(
        "SAS transport (.xpt) file stem '", stem, "' is longer than 8 characters; ",
        "rename the file (the stem is the transport member name).", call. = FALSE
      )
    }
  }
  bad_names <- names(data)[!grepl("^[A-Za-z_][A-Za-z0-9_]*$", names(data))]
  if (length(bad_names) > 0L) {
    stop(
      "Not valid SAS variable names: ", paste(bad_names, collapse = ", "),
      ". Rename them first.", call. = FALSE
    )
  }
  rlang::check_installed("haven", reason = "to write SAS data sets with `write_sas_data()`.")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (identical(format, "sas7bdat")) {
    warning(
      "Writing .sas7bdat files is deprecated in haven (>= 2.5.2) and may stop working; ",
      "prefer a transport file: `write_sas_data(data, \"file.xpt\")`.",
      call. = FALSE
    )
    # haven's own lifecycle warning is muffled: the warning above replaces it
    withCallingHandlers(
      haven::write_sas(data, path, ...),
      lifecycle_warning_deprecated = function(w) invokeRestart("muffleWarning")
    )
  } else {
    haven::write_xpt(data, path, version = 5, ...)
  }
  invisible(normalizePath(path, winslash = "/", mustWork = FALSE))
}

#' The SAS Numerical-Parity Boundary
#'
#' Documents, as data, which SAS procedures TempleCBE claims numerical parity
#' with, which R function is the counterpart, and what is explicitly out of
#' scope. The README frames "SAS numerical parity" as a pillar, but the
#' procedures in scope are only the ones listed here; everything else is a
#' follow-up (see [compare_km_lifetest()] for the first increment beyond
#' them, PROC LIFETEST).
#'
#' @return A tibble with one row per SAS procedure (or parity topic) and
#'   columns `proc` (SAS procedure or topic), `r_counterpart`, `status`
#'   (`"parity"`, `"partial"` or `"not_started"`), `evidence` (where the
#'   agreement was checked) and `notes`.
#' @seealso [compare_df()], [compare_km_lifetest()], [compare_recurrent_phreg()],
#'   [read_sas_data()], [write_sas_data()]
#' @export
#' @examples
#' sas_parity_boundary()
sas_parity_boundary <- function() {
  tibble::tibble(
    proc = c(
      "PROC PHREG", "PROC COMPARE", "PROC PRINCOMP", "Zero-cell Fisher",
      "Missing codes", "PROC LIFETEST", "PROC PHREG (recurrent)", "PROC LOGISTIC", "PROC MIXED / GLIMMIX",
      "PROC FREQ", "PROC MEANS"
    ),
    r_counterpart = c(
      "cox_multi() / coxnet()", "compare_df()", "run_pca()",
      "exact2x2()", "add_missing() / compare_df()", "km_single() / survfit() via compare_km_lifetest()",
      "recurrent_cox() via compare_recurrent_phreg()",
      "glm_single()", "lmm_single() / piecewise_lmm()", "tbl_contingency_2x2()",
      "summary_table()"
    ),
    status = c(
      "parity", "parity", "parity", "parity", "partial",
      "parity", "parity", "not_started", "not_started", "not_started", "not_started"
    ),
    evidence = c(
      "inst/sas/benchmark_brier_lung.sas, benchmark_brier_lung_full.sas, example_85_7.sas; tests/testthat/test-sas_parity_reference.R",
      "inst/sas/benchmark_proc_compare.sas; tests/testthat/test-sas_parity_reference.R",
      "inst/sas/benchmark_princomp_iris.sas; tests/testthat/test-sas_parity_reference.R",
      "cbe_exact2x2 tests",
      "compare_df() documents the difference: SAS special missing values are not part of the benchmark",
      "inst/sas/benchmark_lifetest_km.sas; run once on SAS 9.4 (TS1M8), 11 Nov 2026 - listing inst/sas/list/benchmark_lifetest_km.lst, log inst/sas/logs/benchmark_lifetest_km.log; Tier-1 test tests/testthat/test-gaps-0811-w2-data.R",
      "inst/sas/benchmark_recurrent_phreg.sas; run once on SAS 9.4 (TS1M8), 12 Nov 2026 - listing inst/sas/list/benchmark_recurrent_phreg.lst, log inst/sas/logs/benchmark_recurrent_phreg.log; Tier-1 test tests/testthat/test-round2-0911-sas.R",
      "Follow-up work; no benchmark program yet",
      "Follow-up work; no benchmark program yet",
      "Follow-up work; no benchmark program yet",
      "Follow-up work; no benchmark program yet"
    ),
    notes = c(
      "Breslow ties; SAS GCONV=1E-8 vs R convergence leaves estimates ~5 decimals apart (see helper-sas-reference.R).",
      "METHOD=ABSOLUTE only; sign of diff, type-mismatch handling, trailing blanks and length/format/label checks differ - see ?compare_df.",
      "PRINCOMP on iris; agreement on eigenvalues / loadings as benchmarked.",
      "Zero-cell Fisher exact handling matches SAS.",
      "SAS special missing values (.A-.Z) are not distinguished on read; see ?read_sas_data.",
      "First increment beyond the original boundary (B5): KM estimates, median survival and the log-rank test. R quantities agree with the stored SAS 9.4 listing (see ?compare_km_lifetest).",
      "Recurrent-admission baseline (round 2): Andersen-Gill and PWP gap-time fits on survival::cgd with COVS(AGGREGATE) robust SEs. R quantities agree with the stored SAS 9.4 listing (Efron ties, robust and model-based SEs); see ?compare_recurrent_phreg.",
      "glm_single() has no SAS benchmark; LOGISTIC parity (estimates, odds ratios, Wald tests) is follow-up work.",
      "Mixed-model parity (REML estimates, covariance parameters) is follow-up work.",
      "FREQ parity (chi-square, exact tests beyond zero-cell Fisher) is follow-up work.",
      "MEANS parity (descriptive statistics) is follow-up work."
    )
  )
}
