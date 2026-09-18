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
#' @seealso [cbe_sas_macro_path()], [run_sas_script()]
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
