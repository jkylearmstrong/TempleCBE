#' Render One Template Twice: the Original and the Imputed Analysis
#'
#' Renders a single parameterized report for two parameter sets, typically the analysis of
#' the original data and of the imputed data, so that the two reports come from one template
#' instead of from two copies that drift apart (a data-cleaning fix made in one and forgotten
#' in the other). It is [render_variants()] for exactly two variants, with two guards:
#' * the two parameter sets must name the **same parameters**. A parameter set in one and not
#'   in the other is nearly always a forgotten difference, so it is an error before anything is
#'   rendered. (Set it in both, to its default if it should not differ. [render_variants()]
#'   itself allows variants that set different parameters.)
#' * `output_names` must be two different, non-empty names.
#'
#' The template must declare the parameters in its YAML header under `params:`; that is
#' checked by [render_variants()]. Quarto passes `execute_params` to documents that use the
#' Knitr engine and read `params$<name>`; a template whose parameters live elsewhere (a Jupyter
#' notebook's `parameters` cell, say) needs `check_params = FALSE`, passed on in `...`.
#'
#' The outputs are named `<template>_<output_names[1]>.<ext>` and
#' `<template>_<output_names[2]>.<ext>`, written to `output_dir` (the template's folder by
#' default). Your own files are not touched; see [render_variants()] for how the renderings are
#' staged and which files are replaced.
#'
#' @param input Path to the `.qmd` or `.Rmd` template.
#' @param params_original,params_imputed Named lists of parameters for the two renderings.
#'   They must name the same parameters.
#' @param output_names Two different names for the two renderings, used in the output file
#'   names; letters, digits, `_`, `.` and `-`. Default `c("original", "imputed")`.
#' @param formats Output formats for each rendering, e.g. `c("html", "docx")`.
#' @param output_dir Folder for the outputs.
#' @param ... Further arguments for [render_variants()] (`stop_on_error`, `dry_run`,
#'   `self_contained`, `check_params`, `renderer`, `quiet`).
#' @return The tibble of [render_variants()]: one row per rendering and format.
#' @seealso [render_variants()], [render_pipeline()]
#' @export
#' @examples
#' \dontrun{
#' render_parameterized_pair(
#'   "analysis/mixed_models.qmd",
#'   params_original = list(data_suffix = "original", impute = FALSE),
#'   params_imputed  = list(data_suffix = "imputed",  impute = TRUE),
#'   output_names    = c("original", "imputed"),   # -> mixed_models_original.html, mixed_models_imputed.html
#'   formats         = c("html", "docx"),
#'   output_dir      = "reports"
#' )
#' }
render_parameterized_pair <- function(input,
                                      params_original,
                                      params_imputed,
                                      output_names = c("original", "imputed"),
                                      formats = "html",
                                      output_dir = NULL,
                                      ...) {
  for (arg in c("params_original", "params_imputed")) {
    val <- get(arg)
    if (!is.list(val) || (length(val) > 0L && (is.null(names(val)) || any(!nzchar(names(val)))))) {
      stop("`", arg, "` must be a named list of parameters.", call. = FALSE)
    }
  }
  names_orig <- sort(names(params_original))
  names_imp <- sort(names(params_imputed))
  if (!identical(names_orig, names_imp)) {
    only_orig <- setdiff(names_orig, names_imp)
    only_imp <- setdiff(names_imp, names_orig)
    stop(
      "`params_original` and `params_imputed` must name the same parameters.\n",
      if (length(only_orig)) paste0("- Only in params_original: ", paste(only_orig, collapse = ", "), "\n") else "",
      if (length(only_imp)) paste0("- Only in params_imputed: ", paste(only_imp, collapse = ", "), "\n") else "",
      "Set it in both (to its default in the one where it should not differ).",
      call. = FALSE
    )
  }
  if (anyDuplicated(names(params_original)) || anyDuplicated(names(params_imputed))) {
    stop("A parameter is named twice in `params_original` or `params_imputed`.", call. = FALSE)
  }
  if (!is.character(output_names) || length(output_names) != 2L || anyNA(output_names) ||
      !all(nzchar(output_names)) || identical(tolower(output_names[1L]), tolower(output_names[2L]))) {
    stop("`output_names` must be two different, non-empty names.", call. = FALSE)
  }

  if (!all(grepl("^[A-Za-z0-9][A-Za-z0-9_.-]*$", output_names))) {
    stop("`output_names` may contain only letters, digits, `_`, `.` and `-`, and must start with a letter or digit.",
         call. = FALSE)
  }

  variants <- stats::setNames(list(params_original, params_imputed), output_names)
  render_variants(input = input, variants = variants, formats = formats, output_dir = output_dir, ...)
}
