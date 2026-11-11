#' TempleCBE: Biostatistics, Clinical Data Science, and Modeling Utilities
#'
#' Utilities of Temple University's Center for Biostatistics and Epidemiology
#' (CBE): statistical tests and report tables, data-quality and missingness
#' displays, recipe steps for \pkg{tidymodels}, penalized Cox models with
#' nested cross-validation and survival metrics, checkpointed long-running
#' computations, and report rendering and delivery.
#'
#' @section Naming:
#' Functions that begin with `cbe_` are presentation and formatting functions
#' in the Temple CBE style (themes, palettes, scales, plots, report tables,
#' number formatters). Every other function is named for what it does, for
#' example [nested_survival_run()], [cox_single()], [checkpoint_map()] and
#' [describe_columns()]. Some S3 classes keep a `cbe_` prefix from earlier
#' versions; these are class names, not functions, and are kept so that saved
#' objects and `inherits()` checks continue to work.
#'
#' @section Where to start:
#' * Survival modeling: [coxnet()], [cv_coxnet()], [nested_cv_coxnet()],
#'   [nested_survival_run()].
#' * Long computations that resume after an interruption: [checkpoint_map()].
#' * Reports: [theme_cbe()], [cbe_cox_table()], [bundle_deliverables()].
#' * Calls to functions renamed in earlier versions: [find_deprecated_calls()].
#'
#' The vignettes (`browseVignettes("TempleCBE")`) and the website
#' <https://jkylearmstrong.github.io/TempleCBE/> have worked examples.
#'
#' @keywords internal
"_PACKAGE"
