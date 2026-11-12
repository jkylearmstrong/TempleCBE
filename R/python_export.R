#' Export a Fitted Model, Survival Matrix, or Data Dictionary to Python
#'
#' Writes a fitted TempleCBE model (or a data frame / survival matrix / data
#' dictionary) to a directory of plain files a Python session can read with no
#' R runtime: CSV tables, a JSON manifest and model metadata, and a small
#' Python loader script (`<name>_load.py`) that reads them back with the
#' standard library, or with `pandas` when it is installed. This is the
#' package function behind README gap area 4 ("Python & Reticulate
#' Cross-Language ML"): before it, the only Python-facing exports were
#' [find_python()] (a locator for the PDF converter), `adapter_tidymodels_survex()`
#' and the DALEX summarizers, and the reticulate vignette showed only raw user
#' code calling Python from R.
#'
#' @section What is exported:
#' * A data frame or matrix: `<name>_data.csv`.
#' * A `coxnet_model` ([coxnet()]): `coefficients.csv` (the [generics::tidy()]
#'   coefficients at `penalty`), `model.json` (penalty, mixture, predictor
#'   names, outcome type), and, when `new_data` and `eval_time` are given,
#'   `survival.csv` (one row per subject, one column per evaluation time).
#' * A `cv_coxnet` object: coefficients at `"lambda.min"`, `metrics.csv`
#'   (the cross-validated metrics, when present), and `model.json`.
#' * A `joint_model`: the [generics::tidy()] coefficients and `model.json`
#'   (component model classes); a survival matrix is added when `new_data`
#'   and `eval_time` are given, from the `.pred_survival` list-column of
#'   [predict.joint_model()].
#' * A named list: each data-frame element as `<name>_<element>.csv` and the
#'   whole list as `<name>.json`.
#' * A data dictionary passed as `dictionary` (a data frame, for example the
#'   `matched` table of [reconcile_column_dictionary()]): `dictionary.csv`
#'   and `dictionary.json`.
#'
#' Every export also writes `manifest.json` (package and R versions, object
#' class, file list, creation time) and `<name>_load.py`. The loader exposes
#' `load_coefficients()`, `load_survival()`, `load_dictionary()` and
#' `load_manifest()`, each returning `pandas` DataFrames when `pandas` is
#' importable and plain lists of dicts otherwise, and `load_data()` only when
#' a data file was actually exported (a data frame or matrix object): model
#' exports write no `<name>_data.csv`, so their loaders do not define it
#' instead of raising `FileNotFoundError`.
#'
#' @section Reticulate pickle format:
#' With `format = "pickle"`, the same payload is additionally saved as
#' `<name>.pkl` with [reticulate::py_save_object()], which needs the
#' \pkg{reticulate} package (a `Suggests` dependency, not installed everywhere).
#' The plain files are always written, so a pickle failure never loses the
#' export; when `reticulate` is absent the function stops with a clear message
#' instead of writing a partial pickle. A pickle is Python-version sensitive
#' and opaque to other tools, so prefer the default `"files"` format for
#' artifacts that must be inspected, versioned, or read outside Python.
#'
#' @param object The object to export: a data frame, a matrix, a `coxnet_model`,
#'   a `cv_coxnet`, a `joint_model`, or a named list.
#' @param path Directory the export is written to. Created if needed.
#' @param name File-name stem for the artifacts (default `"templecbe_export"`).
#'   Must be a single string without a path separator.
#' @param eval_time,new_data Passed to [predict()] for `coxnet_model` /
#'   `cv_coxnet` / `joint_model` objects to also export a survival matrix.
#'   Both must be given together; otherwise no survival matrix is written.
#'   `eval_time` must be finite, non-negative, sorted and free of duplicates.
#' @param penalty Penalty passed to [generics::tidy()] / [predict()] for
#'   `coxnet_model` objects. Defaults to the model's own penalty.
#' @param dictionary Optional data frame (a column dictionary) to export
#'   alongside `object`, as `dictionary.csv` and `dictionary.json`.
#' @param format `"files"` (default) or `"pickle"` (additionally save a
#'   `reticulate` pickle; see the Reticulate pickle format section).
#' @param overwrite If `FALSE` (default), refuse to write into a directory
#'   that already holds an export with the same `name`.
#' @return Invisibly, a list of class `python_export` with `path`, `name`,
#'   `object_class`, `format` and the `files` written (absolute paths).
#' @seealso [find_python()], [reconcile_column_dictionary()],
#'   [adapter_tidymodels_survex()]
#' @param x The object to print (the result of the function).
#' @param ... Not used.
#' @export
#' @examples
#' \donttest{
#' out <- export_to_python(head(mtcars), path = tempfile("pyexp_"), name = "demo")
#' out$files
#' # The loader is plain Python; read it back without reticulate:
#' cat(readLines(file.path(out$path, "demo_load.py")), sep = "\n")
#' }
export_to_python <- function(object, path, name = "templecbe_export", eval_time = NULL,
                             new_data = NULL, penalty = NULL, dictionary = NULL,
                             format = c("files", "pickle"), overwrite = FALSE) {
  format <- match.arg(format)
  if (identical(format, "pickle")) {
    # Check before writing anything: a failed pickle must not leave a
    # directory of plain files behind with a manifest claiming pickle format.
    rlang::check_installed("reticulate", reason = "to save a Python pickle with `export_to_python()`.")
  }
  if (!is.character(path) || length(path) != 1L || !nzchar(path)) {
    stop("`path` must be a single non-empty directory path.", call. = FALSE)
  }
  if (!is.character(name) || length(name) != 1L || !nzchar(name) ||
      grepl("[/\\\\]", name)) {
    stop("`name` must be a single file-name stem without a path separator.", call. = FALSE)
  }
  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    stop("`overwrite` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.null(dictionary) && !is.data.frame(dictionary)) {
    stop("`dictionary` must be a data frame.", call. = FALSE)
  }
  # Probe the dictionary before anything is written, as for `object`: a
  # list-column would otherwise fail mid-export and leave partial files.
  if (!is.null(dictionary)) {
    .check_csv_writable(dictionary, "`dictionary`")
  }
  if (xor(is.null(eval_time), is.null(new_data))) {
    stop("`eval_time` and `new_data` must be given together to export a survival matrix.", call. = FALSE)
  }
  if (!is.null(eval_time) &&
      (!is.numeric(eval_time) || length(eval_time) < 1L ||
       any(!is.finite(eval_time)) || any(eval_time < 0))) {
    stop("`eval_time` must be one or more finite, non-negative numbers.", call. = FALSE)
  }
  # predict() rejects unsorted or duplicated evaluation times; fail here
  # rather than shipping an export that silently lacks survival.csv.
  if (!is.null(eval_time) && (anyDuplicated(eval_time) || is.unsorted(eval_time))) {
    stop("`eval_time` must be sorted in increasing order with no duplicates.", call. = FALSE)
  }
  # A classed list (a fitted model object such as `fine_gray_model`,
  # `cox_time_varying` or `joint_readmission_death`) is a list, so without
  # this guard it would fall through to the named-list branch and fail
  # inside jsonlite with an S3-method error instead of the documented
  # supported-class error. Only a plain list may take the list branch.
  if (!is.data.frame(object) && !is.matrix(object) &&
      !inherits(object, c("coxnet_model", "cv_coxnet", "joint_model")) &&
      !(is.list(object) && identical(class(object), "list"))) {
    stop(
      "`object` must be a data frame, matrix, `coxnet_model`, `cv_coxnet`, ",
      "`joint_model`, or named list; got class '", class(object)[1], "'.", call. = FALSE
    )
  }

  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  stem <- file.path(path, name)
  existing <- paste0(stem, c("_data.csv", "_load.py", ".json", ".pkl", "_coefficients.csv"))
  if (!isTRUE(overwrite) && any(file.exists(existing))) {
    stop(
      "An export named '", name, "' already exists in ", path, ". ",
      "Pass `overwrite = TRUE` or choose another `name`.", call. = FALSE
    )
  }

  files <- character()
  write_csv <- function(df, file) {
    utils::write.csv(as.data.frame(df), file, row.names = FALSE, na = "")
    files <<- c(files, normalizePath(file, winslash = "/", mustWork = FALSE))
    invisible(file)
  }
  write_json <- function(x, file) {
    jsonlite::write_json(x, file, auto_unbox = TRUE, pretty = TRUE, na = "null")
    files <<- c(files, normalizePath(file, winslash = "/", mustWork = FALSE))
    invisible(file)
  }

  object_class <- class(object)[1]
  payload <- list()

  if (is.data.frame(object) || is.matrix(object)) {
    .check_csv_writable(object, "`object`")
    write_csv(object, paste0(stem, "_data.csv"))
    payload$data <- as.data.frame(object)
  } else if (inherits(object, "coxnet_model")) {
    coefs <- generics::tidy(object, penalty = penalty)
    write_csv(coefs, paste0(stem, "_coefficients.csv"))
    payload$coefficients <- as.data.frame(coefs)
    meta <- list(
      kind = "coxnet_model",
      penalty = if (is.null(penalty)) object$penalty else penalty,
      mixture = object$mixture,
      predictors = colnames(object$x),
      n_rows = nrow(object$x),
      outcome_type = attr(object$y, "type")
    )
    write_json(meta, paste0(stem, "_model.json"))
    payload$model <- meta
    surv <- .python_export_survival(object, new_data, eval_time, penalty = penalty)
    if (!is.null(surv)) {
      write_csv(surv, paste0(stem, "_survival.csv"))
      payload$survival <- as.data.frame(surv)
    }
  } else if (inherits(object, "cv_coxnet")) {
    coefs <- generics::tidy(object, penalty = "lambda.min")
    write_csv(coefs, paste0(stem, "_coefficients.csv"))
    payload$coefficients <- as.data.frame(coefs)
    meta <- list(
      kind = "cv_coxnet",
      penalty = "lambda.min",
      mixture = object$mixture,
      eval_time = object$eval_time,
      predictors = colnames(object$fit$x)
    )
    write_json(meta, paste0(stem, "_model.json"))
    payload$model <- meta
    if (!is.null(object$metrics) && is.data.frame(object$metrics)) {
      write_csv(object$metrics, paste0(stem, "_metrics.csv"))
      payload$metrics <- as.data.frame(object$metrics)
    }
    surv <- .python_export_survival(object, new_data, eval_time)
    if (!is.null(surv)) {
      write_csv(surv, paste0(stem, "_survival.csv"))
      payload$survival <- as.data.frame(surv)
    }
  } else if (inherits(object, "joint_model")) {
    coefs <- generics::tidy(object)
    write_csv(coefs, paste0(stem, "_coefficients.csv"))
    payload$coefficients <- as.data.frame(coefs)
    meta <- list(
      kind = "joint_model",
      coxnet_model = class(object$coxnet_model)[1],
      status_model = class(object$status_model)[1],
      time_model = class(object$time_model)[1]
    )
    write_json(meta, paste0(stem, "_model.json"))
    payload$model <- meta
    surv <- .python_export_survival(object, new_data, eval_time)
    if (!is.null(surv)) {
      write_csv(surv, paste0(stem, "_survival.csv"))
      payload$survival <- as.data.frame(surv)
    }
  } else if (is.list(object)) {
    if (is.null(names(object)) || any(!nzchar(names(object)))) {
      stop("A list `object` must be named to export to Python.", call. = FALSE)
    }
    # The names become file stems (`<name>_<element>.csv`), so each must be
    # unique and free of path separators or a mid-export failure/overwrite
    # follows.
    if (anyNA(names(object)) || anyDuplicated(names(object)) ||
        any(grepl("[/\\\\]", names(object)))) {
      stop("The names of a list `object` must be unique, not missing, and free of ",
           "path separators ('/' or '\\').", call. = FALSE)
    }
    # `<name>_dictionary.csv` is the dictionary argument's file: an element of
    # that name would be overwritten by it and listed twice in the manifest.
    if (!is.null(dictionary) && "dictionary" %in% names(object)) {
      stop("A list `object` cannot have an element named 'dictionary' when ",
           "`dictionary` is given: both would be written to the same file.", call. = FALSE)
    }
    for (nm in names(object)) {
      if (is.data.frame(object[[nm]])) {
        .check_csv_writable(object[[nm]], paste0("element `", nm, "`"))
      }
    }
    # Serialise before writing anything: an element jsonlite cannot handle
    # (for example a fitted model object) must fail here, not after the
    # data-frame elements have already been written with no manifest.
    probe <- tryCatch(
      {
        jsonlite::toJSON(object, auto_unbox = TRUE, na = "null")
        TRUE
      },
      error = function(e) e
    )
    if (inherits(probe, "error")) {
      stop(
        "A list `object` must contain only values that can be written as JSON ",
        "(data frames, atomic vectors, plain lists); an element could not be ",
        "serialised: ", conditionMessage(probe), call. = FALSE
      )
    }
    for (nm in names(object)) {
      if (is.data.frame(object[[nm]])) {
        write_csv(object[[nm]], paste0(stem, "_", nm, ".csv"))
      }
    }
    write_json(object, paste0(stem, ".json"))
    payload <- object
  } else {
    stop(
      "`object` must be a data frame, matrix, `coxnet_model`, `cv_coxnet`, ",
      "`joint_model`, or named list; got class '", object_class, "'.", call. = FALSE
    )
  }

  if (!is.null(dictionary)) {
    write_csv(dictionary, paste0(stem, "_dictionary.csv"))
    write_json(dictionary, paste0(stem, "_dictionary.json"))
    payload$dictionary <- as.data.frame(dictionary)
  }

  loader_file <- paste0(stem, "_load.py")
  # Only a data-frame / matrix export writes a data file, so only its loader
  # defines load_data(): a model export's loader must not offer a loader for
  # a file that does not exist.
  has_data <- is.data.frame(object) || is.matrix(object)
  writeLines(.python_loader_text(name, has_data = has_data), loader_file)
  files <- c(files, normalizePath(loader_file, winslash = "/", mustWork = FALSE))

  if (identical(format, "pickle")) {
    pkl <- paste0(stem, ".pkl")
    reticulate::py_save_object(payload, pkl)
    files <- c(files, normalizePath(pkl, winslash = "/", mustWork = FALSE))
  }

  manifest <- list(
    package = "TempleCBE",
    package_version = as.character(utils::packageVersion("TempleCBE")),
    r_version = as.character(getRversion()),
    created_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    name = name,
    object_class = object_class,
    format = format,
    # the manifest itself is part of the export: list it too, so the
    # manifest's inventory agrees with the returned `files`
    files = c(basename(files), paste0(name, "_manifest.json"))
  )
  write_json(manifest, paste0(stem, "_manifest.json"))

  out <- list(path = path, name = name, object_class = object_class, format = format, files = files)
  class(out) <- c("python_export", "list")
  invisible(out)
}

#' @rdname export_to_python
#' @export
print.python_export <- function(x, ...) {
  cat("<python_export> ", x$object_class, " -> ", x$path, "\n", sep = "")
  cat("  files:\n")
  for (f in x$files) cat("   - ", basename(f), "\n", sep = "")
  invisible(x)
}

# Trial-write a data frame as CSV to a temporary file so an unwritable
# element (for example a list-column) fails with a clear message before any
# export file exists, instead of leaving a partial CSV behind.
.check_csv_writable <- function(df, label) {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  res <- tryCatch(
    {
      utils::write.csv(as.data.frame(df), tmp, row.names = FALSE, na = "")
      TRUE
    },
    error = function(e) e
  )
  if (inherits(res, "error")) {
    stop("Cannot export ", label, " as CSV (for example a list-column): ",
         conditionMessage(res), call. = FALSE)
  }
  invisible(TRUE)
}

# Predict a survival matrix and flatten the list-column of per-subject tibbles
# that predict.coxnet_model()/predict.cv_coxnet() return into one wide data
# frame (one row per subject, one column per evaluation time). Returns NULL
# (with a warning) when prediction fails or returns an unrecognised shape, so
# a model export still succeeds without its survival matrix.
.python_export_survival <- function(object, new_data, eval_time, penalty = NULL) {
  if (is.null(eval_time) || is.null(new_data)) {
    return(NULL)
  }
  pred <- tryCatch(
    if (inherits(object, "coxnet_model")) {
      stats::predict(object, new_data, type = "survival", eval_time = eval_time, penalty = penalty)
    } else {
      stats::predict(object, new_data, type = "survival", eval_time = eval_time)
    },
    error = function(e) e
  )
  if (inherits(pred, "error")) {
    warning("Survival matrix was not exported: predict() failed: ", conditionMessage(pred), call. = FALSE)
    return(NULL)
  }
  # joint_model predict() returns a list with a $surv element; the coxnet
  # predict methods return a tibble with a .pred list-column, and
  # predict.joint_model(type = "survival") returns a tibble with a
  # .pred_survival list-column of the same per-subject curves.
  if (is.list(pred) && !is.data.frame(pred) && !is.null(pred$surv)) {
    pred <- pred$surv
  }
  pred_col <- if (is.data.frame(pred) && ".pred" %in% names(pred) && is.list(pred$.pred)) {
    ".pred"
  } else if (is.data.frame(pred) && ".pred_survival" %in% names(pred) && is.list(pred$.pred_survival)) {
    ".pred_survival"
  } else {
    NA_character_
  }
  if (!is.na(pred_col)) {
    wide <- do.call(rbind, lapply(pred[[pred_col]], function(d) {
      stats::setNames(as.numeric(d$.pred_survival), paste0("t_", d$.eval_time))
    }))
    out <- data.frame(.row = seq_len(nrow(pred)), wide, check.names = FALSE)
    return(tibble::as_tibble(out))
  }
  warning("Survival matrix was not exported: predict() returned an unrecognised shape.", call. = FALSE)
  NULL
}

# The Python loader written next to every export. Standard library only, with
# pandas used when importable; it reads the files beside itself, so the
# exported directory is self-contained and movable. `load_data()` is only
# defined when a data file was exported (`has_data = TRUE`).
.python_loader_text <- function(name, has_data = TRUE) {
  load_data <- if (isTRUE(has_data)) {
    paste0(
      "def load_data():\n",
      '    """The exported data frame (<stem>_data.csv)."""\n',
      '    return _read_csv(f"{_STEM}_data.csv")\n',
      "\n",
      "\n"
    )
  } else {
    ""
  }
  paste0(
    '"""Loader for the TempleCBE Python export "', name, '".\n',
    "\n",
    "Reads the CSV/JSON artifacts written by TempleCBE::export_to_python()\n",
    "from the directory this file lives in. pandas is used when available;\n",
    "otherwise each loader returns a plain list of dicts.\n",
    '"""\n',
    "\n",
    "import csv\n",
    "import json\n",
    "from pathlib import Path\n",
    "\n",
    "_HERE = Path(__file__).resolve().parent\n",
    "_STEM = ", jsonlite::toJSON(name, auto_unbox = TRUE), "\n",
    "\n",
    "try:\n",
    "    import pandas as _pd\n",
    "except ImportError:  # pragma: no cover - pandas is optional\n",
    "    _pd = None\n",
    "\n",
    "\n",
    "def _read_csv(filename):\n",
    "    path = _HERE / filename\n",
    "    if _pd is not None:\n",
    "        return _pd.read_csv(path)\n",
    "    with path.open(newline=\"\") as fh:\n",
    "        return list(csv.DictReader(fh))\n",
    "\n",
    "\n",
    "def _read_json(filename):\n",
    "    with (_HERE / filename).open() as fh:\n",
    "        return json.load(fh)\n",
    "\n",
    "\n",
    load_data,
    "def load_coefficients():\n",
    '    """Fitted-model coefficients (<stem>_coefficients.csv)."""\n',
    '    return _read_csv(f"{_STEM}_coefficients.csv")\n',
    "\n",
    "\n",
    "def load_survival():\n",
    '    """Survival probabilities (<stem>_survival.csv)."""\n',
    '    return _read_csv(f"{_STEM}_survival.csv")\n',
    "\n",
    "\n",
    "def load_dictionary():\n",
    '    """The exported data dictionary (<stem>_dictionary.csv)."""\n',
    '    return _read_csv(f"{_STEM}_dictionary.csv")\n',
    "\n",
    "\n",
    "def load_manifest():\n",
    '    """Export metadata (<stem>_manifest.json)."""\n',
    '    return _read_json(f"{_STEM}_manifest.json")\n'
  )
}
