#' Extract Output Formats From Document YAML Front Matter
#'
#' Inspects the YAML header of a Quarto (\code{.qmd}) or R Markdown (\code{.Rmd})
#' file and returns all declared output formats (e.g. \code{"html"}, \code{"pdf"},
#' \code{"docx"}, \code{"gfm"}).
#'
#' @param file_path Path to the \code{.qmd} or \code{.Rmd} document.
#' @return Character vector of lowercase format names, or empty character vector if none found.
#' @export
extract_yaml_formats <- function(file_path) {
  if (!file.exists(file_path)) return(character())
  fmts <- character()

  yaml_data <- tryCatch({
    if (requireNamespace("rmarkdown", quietly = TRUE)) {
      rmarkdown::yaml_front_matter(file_path)
    } else {
      NULL
    }
  }, error = function(e) NULL)

  if (!is.null(yaml_data)) {
    # 1. Quarto 'format'
    if (!is.null(yaml_data$format)) {
      if (is.character(yaml_data$format)) {
        fmts <- c(fmts, yaml_data$format)
      } else if (is.list(yaml_data$format)) {
        fmts <- c(fmts, names(yaml_data$format))
      }
    }
    # 2. R Markdown 'output'
    if (!is.null(yaml_data$output)) {
      raw_outs <- if (is.character(yaml_data$output)) {
        yaml_data$output
      } else if (is.list(yaml_data$output)) {
        names(yaml_data$output)
      } else {
        character()
      }
      for (ro in raw_outs) {
        clean <- sub("^rmarkdown::", "", ro)
        fmt_name <- switch(clean,
          "pdf_document"    = "pdf",
          "word_document"   = "docx",
          "html_document"   = "html",
          "html_vignette"   = "html",
          "github_document" = "gfm",
          "md_document"     = "md",
          "rtf_document"    = "rtf",
          "odt_document"    = "odt",
          sub("_document$", "", clean)
        )
        fmts <- c(fmts, fmt_name)
      }
    }
  }

  unique(tolower(fmts[nzchar(fmts)]))
}

# TRUE when an renv project is active in this session (kept apart so tests can
# state "renv is inactive" without depending on how the test run was started).
renv_project_active <- function() {
  if (!requireNamespace("renv", quietly = TRUE)) return(FALSE)
  proj <- tryCatch(renv::project(), error = function(e) NULL)
  !is.null(proj) && nzchar(proj)
}

#' Guard Against Inactive renv or Lockfile Disagreements
#'
#' Checks whether an `renv.lock` exists in the project root or ancestors and
#' warns if `renv` is inactive (which commonly occurs when Quarto renders from a
#' subfolder, skipping the project-root `.Rprofile`), or if any currently loaded
#' package versions disagree with the lockfile definitions.
#'
#' @param project_root Optional project root directory. Defaults to [here::here()]
#'   if available, or the current working directory.
#' @return Logical `TRUE` if clean or not using renv, `FALSE` if a warning was emitted.
#' @export
check_renv_guard <- function(project_root = NULL) {
  lock_path <- NULL
  candidates <- c("renv.lock", file.path("..", "renv.lock"), file.path("..", "..", "renv.lock"))
  if (!is.null(project_root)) {
    candidates <- c(file.path(project_root, "renv.lock"), candidates)
  } else if (requireNamespace("here", quietly = TRUE)) {
    candidates <- c(file.path(here::here(), "renv.lock"), candidates)
  }
  for (cand in candidates) {
    if (file.exists(cand)) {
      lock_path <- normalizePath(cand, winslash = "/")
      break
    }
  }

  if (is.null(lock_path)) return(invisible(TRUE))

  renv_active <- renv_project_active()
  if (!renv_active && nzchar(Sys.getenv("RENV_PROJECT"))) {
    renv_active <- TRUE
  }

  if (!renv_active) {
    if (!isTRUE(getOption("templecbe_suppress_renv_warning", FALSE))) {
      warning(
        "renv is inactive even though renv.lock exists at '", lock_path, "'. ",
        "Quarto rendering from a subfolder skips the root .Rprofile. Activate renv or set RENV_PROJECT.",
        call. = FALSE
      )
    }
    return(invisible(FALSE))
  }

  if (requireNamespace("jsonlite", quietly = TRUE)) {
    tryCatch({
      lock_data <- jsonlite::read_json(lock_path)
      if (!is.null(lock_data$Packages)) {
        disagreements <- character()
        loaded_pkgs <- loadedNamespaces()
        for (pkg_name in intersect(names(lock_data$Packages), loaded_pkgs)) {
          expected_ver <- lock_data$Packages[[pkg_name]]$Version
          actual_ver <- as.character(utils::packageVersion(pkg_name))
          if (!is.null(expected_ver) && expected_ver != actual_ver) {
            disagreements <- c(disagreements, paste0(pkg_name, " (lock: ", expected_ver, ", loaded: ", actual_ver, ")"))
          }
        }
        if (length(disagreements) > 0) {
          warning(
            "renv.lock disagrees with currently loaded package version(s): ",
            paste(disagreements, collapse = ", "),
            call. = FALSE
          )
          return(invisible(FALSE))
        }
      }
    }, error = function(e) NULL)
  }

  invisible(TRUE)
}

#' Write a Run Manifest JSON Beside Render Outputs
#'
#' Records environment metadata, git commit hash, package versions, random number
#' generator seed, execution future plan, and per-node wall times beside render outputs.
#'
#' @param output_dir Directory to write `render_manifest.json` into.
#' @param targets_result Data frame of render timing and status from [render()].
#' @param seed Optional integer RNG seed used for the run.
#' @param seed_scheme Task seed generation scheme identifier (defaults to
#'   `current_seed_scheme()`, `"v0.5.6"`).
#' @return The absolute path to the written `render_manifest.json` file.
#' @export
write_run_manifest <- function(output_dir, targets_result, seed = NULL, seed_scheme = current_seed_scheme()) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    warning("Package 'jsonlite' is required to write run manifest. Install it with install.packages(\"jsonlite\").", call. = FALSE)
    return(invisible(NULL))
  }

  git_sha <- tryCatch({
    res <- suppressWarnings(system2("git", c("rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE))
    status <- attr(res, "status")
    if ((is.null(status) || status == 0) && length(res) > 0 && nzchar(res[1])) {
      trimws(res[1])
    } else {
      NA_character_
    }
  }, error = function(e) NA_character_)

  plan_name <- tryCatch({
    if (requireNamespace("future", quietly = TRUE)) {
      as.character(class(future::plan())[1])
    } else "sequential"
  }, error = function(e) "unknown")

  key_pkgs <- c("TempleCBE", "survival", "glmnet", "recipes", "rsample", "yardstick", "quarto", "rmarkdown", "renv")
  pkg_versions <- list()
  for (p in key_pkgs) {
    if (requireNamespace(p, quietly = TRUE)) {
      pkg_versions[[p]] <- as.character(utils::packageVersion(p))
    }
  }

  clean_timing <- if (is.data.frame(targets_result)) {
    as.data.frame(targets_result)
  } else list()

  manifest <- list(
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    templecbe_version = templecbe_version(),
    git_sha = git_sha,
    future_plan = plan_name,
    seed = if (!is.null(seed)) seed else if (exists(".Random.seed", envir = .GlobalEnv)) .GlobalEnv$.Random.seed[1:min(2, length(.GlobalEnv$.Random.seed))] else NA,
    seed_scheme = as.character(seed_scheme),
    package_versions = pkg_versions,
    per_node_wall_time = clean_timing
  )

  manifest_path <- file.path(output_dir, "render_manifest.json")
  jsonlite::write_json(manifest, manifest_path, pretty = TRUE, auto_unbox = TRUE)
  invisible(manifest_path)
}

#' Render Pipeline Documents and Reports
#'
#' Dispatches Quarto or R Markdown compilation across single documents, directories,
#' or computational pipeline graphs, with support for staleness detection, range filtering,
#' cache invalidation, error recovery, run manifests, and renv guards.
#'
#' @param path A file path (character), directory, list of files, or list of pipeline
#'   objects (`FilePath`, `FileUses`, `FileOutputs`).
#' @param formats Character vector of output formats (e.g. `c("pdf", "docx", "html")`).
#' @param pattern File pattern regex matching documents to render.
#' @param engine Rendering engine: `"auto"` (default), `"quarto"`, or `"rmarkdown"`.
#' @param workers Number of parallel workers for concurrent rendering via `future`/`furrr`.
#'   Defaults to `1` (sequential). Set to `> 1` to initialize multisession workers, or `NULL`
#'   to use an existing `future::plan()` without altering it.
#' @param only_stale Logical. When `TRUE` (default), only out-of-date documents are rendered.
#'   When `FALSE`, performs a full graph rerun.
#' @param from Optional starting node name or index in the topological plan.
#' @param to Optional ending node name or index in the topological plan.
#' @param dry_run Logical. When `TRUE`, prints the ordered render plan without executing.
#' @param rerun Rerun strategy shorthand: `"none"` (default, uses `only_stale` and `cache` settings)
#'   or `"full"` (forces full graph execution with fresh caches, setting `only_stale = FALSE`
#'   and `cache = "refresh"`).
#' @param skip_downstream Logical. When `TRUE` (default), if an upstream node fails during
#'   pipeline rendering (with `on_error = "continue"`), all dependent downstream reports are
#'   skipped and marked with status `"skipped_upstream_failure"` to prevent compiling on stale data.
#' @param cache Cache handling: `"keep"` (default), `"refresh"` (wipes `_cache` and `_files`),
#'   or `"stale"`.
#' @param on_error Behavior upon error: `"continue"` (default) or `"stop"`.
#' @param resume Optional data frame of previous render results to resume from.
#' @param manifest Logical. When \code{TRUE} (default), writes a \code{render_manifest.json}
#'   beside rendered outputs containing TempleCBE version, git commit hash, package versions,
#'   RNG seed, `seed_scheme`, future plan, and per-document wall time.
#' @param renv_guard Logical. When \code{TRUE} (default), verifies that \pkg{renv} is active
#'   if an \code{renv.lock} exists, and warns of any package version discrepancies.
#' @param seed Optional integer seed for reproducible document execution.
#' @param skip_downstream Logical (default `TRUE`). Skip downstream targets when an upstream target fails.
#' @param rerun One of `"none"` (default) or `"full"` (forces `only_stale = FALSE` and `cache = "refresh"`).
#' @param stale_by Staleness strategy: `"mtime"` (default; compares file modification
#'   times) or `"hash"` (compares SHA-256 content hashes of declared inputs and
#'   outputs stored in `pipeline_hashes.json`).
#' @param hash_file Optional path to `pipeline_hashes.json` when `stale_by = "hash"`.
#' @param on_complete Optional callback `function(result)` invoked when `render_pipeline()`
#'   finishes or errors (defaults to `getOption("templecbe.on_complete", NULL)`).
#' @param ... Additional arguments passed to rendering engines.
#' @return A data frame of rendered files, output formats, durations, and status.
#' @export
render_pipeline <- function(path,
                   formats = getOption("pipeline.default_formats", c("pdf", "docx")),
                   pattern = "\\.(qmd|Rmd|rmd)$",
                   engine = c("auto", "quarto", "rmarkdown"),
                   workers = 1,
                   only_stale = TRUE,
                   from = NULL,
                   to = NULL,
                   dry_run = FALSE,
                   cache = c("keep", "refresh", "stale"),
                   on_error = c("continue", "stop"),
                   resume = NULL,
                   manifest = TRUE,
                   renv_guard = TRUE,
                   seed = NULL,
                   skip_downstream = TRUE,
                   rerun = c("none", "full"),
                   stale_by = c("mtime", "hash"),
                   hash_file = NULL,
                   on_complete = getOption("templecbe.on_complete", NULL),
                   ...) {
  formats_explicit <- !missing(formats)
  dots <- list(...)
  run_with_on_complete(
    fn_name = "render",
    on_complete = on_complete,
    meta = list(seed = seed, dry_run = dry_run),
    expr = {
  rerun <- match.arg(rerun)
  stale_by <- match.arg(stale_by)
  if (rerun == "full") {
    only_stale <- FALSE
    cache <- "refresh"
  }
  if (isTRUE(renv_guard)) {
    check_renv_guard()
  }
  if (!is.null(seed)) {
    local_seed(seed)
  }
  engine <- match.arg(engine)
  cache <- match.arg(cache)
  on_error <- match.arg(on_error)

  # 1. Resolve pipeline graph if path is a list of pipeline objects or a single FileOutputs/FilePath
  is_pipeline_obj <- function(x) is(x, "FilePath") || is(x, "FileOutputs") || is(x, "FileUses")
  pipeline_graph <- NULL
  orig_pipeline_objects <- NULL
  if (is.list(path) && length(path) > 0L && all(vapply(path, is_pipeline_obj, logical(1)))) {
    path <- refresh_pipeline(path)
    orig_pipeline_objects <- path
    pipeline_graph <- tryCatch(as_igraph(path), error = function(e) NULL)
    plan <- get_render_plan(
      path,
      only_stale = only_stale,
      from = from,
      to = to,
      dry_run = FALSE,
      stale_by = stale_by,
      hash_file = hash_file
    )
    path <- plan
  } else if (is_pipeline_obj(path)) {
    orig_pipeline_objects <- refresh_pipeline(list(path))
    if (stale_by == "hash" && isTRUE(only_stale)) {
      plan <- get_render_plan(
        orig_pipeline_objects,
        only_stale = only_stale,
        from = from,
        to = to,
        dry_run = FALSE,
        stale_by = stale_by,
        hash_file = hash_file
      )
      path <- plan
    }
  } else if (!is.null(from) || !is.null(to)) {
    if (is.list(path) && length(path) > 0L) {
      names_vec <- if (!is.null(names(path)) && any(nzchar(names(path)))) {
        names(path)
      } else {
        vapply(path, function(x) if (is(x, "FileOutputs")) x@name else basename(as.character(x)), character(1))
      }
      start_idx <- 1L
      end_idx <- length(path)
      if (!is.null(from)) {
        start_idx <- if (is.numeric(from)) as.integer(from) else match(as.character(from), names_vec)
        if (is.na(start_idx) || start_idx < 1L || start_idx > length(path)) stop("'from' not found in path list: ", from, call. = FALSE)
      }
      if (!is.null(to)) {
        end_idx <- if (is.numeric(to)) as.integer(to) else match(as.character(to), names_vec)
        if (is.na(end_idx) || end_idx < 1L || end_idx > length(path)) stop("'to' not found in path list: ", to, call. = FALSE)
      }
      if (start_idx > end_idx) stop("'from' occurs after 'to'.", call. = FALSE)
      path <- path[start_idx:end_idx]
    }
  }

  targets <- list()
  doc_exts <- c("pdf", "docx", "html", "gfm", "md", "rtf", "odt", "pptx", "epub")

  add_target <- function(fpath, fmts = NULL, derive_docx = FALSE, node_name = NULL, dependencies = character(), outputs = character()) {
    if (is.null(fpath) || !nzchar(fpath)) return()

    resolved_fmts <- fmts
    if (is.null(resolved_fmts)) {
      if (is.null(formats) || identical(formats, "yaml") || identical(formats, "auto")) {
        yaml_f <- extract_yaml_formats(fpath)
        resolved_fmts <- if (length(yaml_f) > 0) yaml_f else c("pdf", "docx")
      } else if ("yaml" %in% formats) {
        yaml_f <- extract_yaml_formats(fpath)
        resolved_fmts <- unique(c(yaml_f, setdiff(formats, "yaml")))
      } else {
        resolved_fmts <- formats
      }
    } else if (identical(resolved_fmts, "yaml") || identical(resolved_fmts, "auto")) {
      yaml_f <- extract_yaml_formats(fpath)
      resolved_fmts <- if (length(yaml_f) > 0) yaml_f else formats
    }

    targets[[length(targets) + 1L]] <<- list(
      name = if (!is.null(node_name) && nzchar(node_name)) node_name else basename(fpath),
      path = fpath,
      formats = resolved_fmts,
      derive_docx = derive_docx,
      dependencies = dependencies,
      outputs = outputs
    )
  }

  process_item <- function(item, item_name = NULL) {
    if (is(item, "FileOutputs")) {
      if (.hasSlot(item, "renders") && !isTRUE(item@renders)) return()

      ctrls_data <- produces_data(item)

      # Extract deliverable report formats from item@output
      report_outs <- Filter(function(o) {
        if (.hasSlot(o, "artifact_role") && length(o@artifact_role) > 0 && o@artifact_role == "deliverable_report") return(TRUE)
        ext <- tolower(tools::file_ext(o@path))
        ext %in% doc_exts
      }, item@output)

      # the format an output stands for is not always its extension (revealjs writes .html)
      custom_fmts <- if (length(report_outs) > 0) {
        unique(vapply(report_outs, .output_format, character(1)))
      } else NULL

      effective_fmts <- if (formats_explicit) formats else custom_fmts
      if (is.null(effective_fmts)) effective_fmts <- c("pdf", "docx")

      derive_docx <- FALSE
      if (ctrls_data) {
        if ("docx" %in% effective_fmts) {
          derive_docx <- TRUE
          effective_fmts <- unique(c("pdf", setdiff(effective_fmts, "docx")))
        }
      }

      dep_names <- character()
      dep_paths <- character()
      if (length(item@dependencies) > 0) {
        dep_names <- vapply(item@dependencies, function(d) d@name, character(1))
        dep_paths <- vapply(item@dependencies, function(d) d@path, character(1))
      }
      out_names <- character()
      out_paths <- character()
      if (length(item@output) > 0) {
        out_names <- vapply(item@output, function(o) o@name, character(1))
        out_paths <- vapply(item@output, function(o) o@path, character(1))
      }

      add_target(
        item@path,
        effective_fmts,
        derive_docx = derive_docx,
        node_name = item@name,
        dependencies = unique(c(dep_names, dep_paths)),
        outputs = unique(c(out_names, out_paths))
      )

    } else if (is(item, "FilePath")) {
      if (.hasSlot(item, "renders") && isTRUE(item@renders)) {
        add_target(item@path, if (formats_explicit) formats else NULL, node_name = item@name)
      }
    } else if (is.list(item) && "path" %in% names(item)) {
      item_f <- if ("formats" %in% names(item)) item$formats else if ("format" %in% names(item)) item$format else NULL
      effective_fmts <- if (!is.null(item_f)) item_f else formats
      add_target(
        item$path,
        effective_fmts,
        node_name = if ("name" %in% names(item)) item$name else NULL,
        dependencies = if ("dependencies" %in% names(item)) item$dependencies else character(),
        outputs = if ("output" %in% names(item)) item$output else character()
      )

    } else if (is.character(item)) {
      if (!is.null(item_name) && (file.exists(item_name) || grepl(pattern, item_name))) {
        add_target(item_name, item)
      } else {
        for (p in item) {
          if (dir.exists(p)) {
            matched <- list.files(p, pattern = pattern, full.names = TRUE)
            for (m in matched) add_target(normalizePath(m, winslash = "/"), if (formats_explicit) formats else NULL)
          } else if (file.exists(p)) {
            add_target(normalizePath(p, winslash = "/"), if (formats_explicit) formats else NULL)
          } else {
            matched <- list.files(dirname(p), pattern = utils::glob2rx(basename(p)), full.names = TRUE)
            if (length(matched) > 0) {
              for (m in matched) add_target(normalizePath(m, winslash = "/"), if (formats_explicit) formats else NULL)
            } else {
              stop("File or directory not found: ", p, call. = FALSE)
            }
          }
        }
      }
    }
  }

  if (is(path, "FileOutputs") || is(path, "FilePath")) {
    process_item(path)
  } else if (is.list(path)) {
    for (i in seq_along(path)) {
      nm <- if (!is.null(names(path)) && nzchar(names(path)[i])) names(path)[i] else NULL
      process_item(path[[i]], nm)
    }
  } else if (is.character(path)) {
    process_item(path)
  } else {
    stop("Unsupported 'path' argument type: ", class(path)[1], call. = FALSE)
  }

  if (length(targets) == 0) {
    if (isTRUE(dry_run)) {
      message("Render plan is empty.")
    } else {
      message("No files found to render matching pattern: ", pattern)
    }
    return(invisible(data.frame(file = character(), output = character(), duration = numeric(), status = character(), stringsAsFactors = FALSE)))
  }

  if (isTRUE(dry_run)) {
    cat("Render plan (dry run):\n")
    for (i in seq_along(targets)) {
      tgt <- targets[[i]]
      tgt_fmts <- if (isTRUE(tgt$derive_docx)) c(tgt$formats, "docx(derived)") else tgt$formats
      cat(sprintf("  [%d] %s -> %s\n", i, basename(tgt$path), paste(toupper(tgt_fmts), collapse = ", ")))
    }
    df <- data.frame(
      file = vapply(targets, function(t) basename(t$path), character(1)),
      output = vapply(targets, function(t) paste(if (isTRUE(t$derive_docx)) c(t$formats, "docx") else t$formats, collapse = ", "), character(1)),
      duration = 0,
      status = "dry_run",
      stringsAsFactors = FALSE
    )
    return(invisible(df))
  }

  # Check engine availability
  has_quarto <- requireNamespace("quarto", quietly = TRUE)
  has_rmarkdown <- requireNamespace("rmarkdown", quietly = TRUE)

  target_paths <- vapply(targets, function(t) t$path, character(1))
  has_qmd <- any(tolower(tools::file_ext(target_paths)) == "qmd")

  if (has_qmd && !has_quarto) {
    stop("Package 'quarto' is required to render .qmd files. Install it with install.packages(\"quarto\").",
         call. = FALSE)
  }

  if (engine == "quarto" && !has_quarto) {
    stop("Package 'quarto' is required when engine = 'quarto'. Install it with install.packages(\"quarto\").",
         call. = FALSE)
  }

  if (engine == "rmarkdown" && !has_rmarkdown) {
    stop("Package 'rmarkdown' is required when engine = 'rmarkdown'. Install it with install.packages(\"rmarkdown\").",
         call. = FALSE)
  }

  if (!has_quarto && !has_rmarkdown) {
    stop("Either 'quarto' or 'rmarkdown' package is required by render_pipeline().", call. = FALSE)
  }

  # Helper to map format shorthand for rmarkdown
  map_rmd_format <- function(fmt) {
    switch(tolower(fmt),
      "pdf"  = "pdf_document",
      "docx" = "word_document",
      "word" = "word_document",
      "html" = "html_document",
      "gfm"  = "github_document",
      "md"   = "md_document",
      "rtf"  = "rtf_document",
      "odt"  = "odt_document",
      fmt
    )
  }

  # 2. Worker helper to render all formats for a single target
  render_single <- function(file_path, file_formats, derive_docx = FALSE) {
    ext <- tolower(tools::file_ext(file_path))

    # Handle cache wiping
    stem <- tools::file_path_sans_ext(file_path)
    doc_dir <- dirname(file_path)
    doc_base <- basename(stem)
    cache_dir <- file.path(doc_dir, paste0(doc_base, "_cache"))
    files_dir <- file.path(doc_dir, paste0(doc_base, "_files"))

    wipe_cache <- switch(cache,
      "refresh" = TRUE,
      "stale"   = TRUE,
      "keep"    = FALSE
    )
    if (wipe_cache) {
      if (dir.exists(cache_dir)) unlink(cache_dir, recursive = TRUE)
      if (dir.exists(files_dir)) unlink(files_dir, recursive = TRUE)
    }

    rows <- purrr::map_dfr(file_formats, function(fmt) {
      # Check resume
      if (!is.null(resume) && is.data.frame(resume)) {
        m <- resume$file == basename(file_path) & tolower(resume$output) == tolower(fmt) & resume$status == "success"
        if (any(m)) {
          message("Resuming: skipping already completed '", basename(file_path), "' to ", toupper(fmt))
          dur <- if ("duration" %in% names(resume)) resume$duration[which(m)[1]] else 0
          return(data.frame(
            file = basename(file_path),
            output = fmt,
            duration = dur,
            status = "success",
            stringsAsFactors = FALSE
          ))
        }
      }

      message("Rendering '", basename(file_path), "' to ", toupper(fmt), "...")
      tic <- Sys.time()

      res <- tryCatch({
        if (ext == "qmd") {
          q_formals <- names(formals(quarto::quarto_render))
          q_args <- list(input = file_path, output_format = fmt)
          q_dots <- dots
          if ("params" %in% names(q_dots) && !"execute_params" %in% names(q_dots)) {
            q_dots$execute_params <- q_dots$params
            q_dots$params <- NULL
          }
          valid_q_dots <- q_dots[names(q_dots) %in% q_formals]
          do.call(quarto::quarto_render, c(q_args, valid_q_dots))
        } else if (engine == "rmarkdown" || (!has_quarto && has_rmarkdown)) {
          rmd_fmt <- map_rmd_format(fmt)
          rmd_formals <- names(formals(rmarkdown::render))
          rmd_args <- list(input = file_path, output_format = rmd_fmt)
          rmd_dots <- dots
          if ("execute_params" %in% names(rmd_dots) && !"params" %in% names(rmd_dots)) {
            rmd_dots$params <- rmd_dots$execute_params
            rmd_dots$execute_params <- NULL
          }
          if (!"quiet" %in% names(rmd_dots)) {
            rmd_dots$quiet <- TRUE
          }
          valid_rmd_dots <- rmd_dots[names(rmd_dots) %in% rmd_formals]
          do.call(rmarkdown::render, c(rmd_args, valid_rmd_dots))
        } else {
          # engine is "auto" or "quarto" on .Rmd
          tryCatch({
            q_formals <- names(formals(quarto::quarto_render))
            q_args <- list(input = file_path, output_format = fmt)
            q_dots <- dots
            if ("params" %in% names(q_dots) && !"execute_params" %in% names(q_dots)) {
              q_dots$execute_params <- q_dots$params
              q_dots$params <- NULL
            }
            valid_q_dots <- q_dots[names(q_dots) %in% q_formals]
            do.call(quarto::quarto_render, c(q_args, valid_q_dots))
          }, error = function(e) {
            if (has_rmarkdown) {
              rmd_fmt <- map_rmd_format(fmt)
              rmd_formals <- names(formals(rmarkdown::render))
              rmd_args <- list(input = file_path, output_format = rmd_fmt)
              rmd_dots <- dots
              if ("execute_params" %in% names(rmd_dots) && !"params" %in% names(rmd_dots)) {
                rmd_dots$params <- rmd_dots$execute_params
                rmd_dots$execute_params <- NULL
              }
              if (!"quiet" %in% names(rmd_dots)) {
                rmd_dots$quiet <- TRUE
              }
              valid_rmd_dots <- rmd_dots[names(rmd_dots) %in% rmd_formals]
              do.call(rmarkdown::render, c(rmd_args, valid_rmd_dots))
            } else {
              stop(e)
            }
          })
        }
        "success"
      }, error = function(e) {
        if (on_error == "stop") {
          stop("Error rendering '", basename(file_path), "' to ", fmt, ": ", conditionMessage(e), call. = FALSE)
        }
        warning("Error rendering '", basename(file_path), "' to ", fmt, ": ", conditionMessage(e), call. = FALSE)
        conditionMessage(e)
      })

      dur <- as.numeric(Sys.time() - tic, units = "secs")

      data.frame(
        file     = basename(file_path),
        output   = fmt,
        duration = round(dur, 2),
        status   = res,
        stringsAsFactors = FALSE
      )
    })

    if (isTRUE(derive_docx)) {
      docx_row <- if (!is.null(resume) && is.data.frame(resume) &&
                      any(resume$file == basename(file_path) & tolower(resume$output) == "docx" & resume$status == "success")) {
        message("Resuming: skipping already completed '", basename(file_path), "' to DOCX")
        m_idx <- which(resume$file == basename(file_path) & tolower(resume$output) == "docx" & resume$status == "success")[1]
        data.frame(
          file = basename(file_path),
          output = "docx",
          duration = if ("duration" %in% names(resume)) resume$duration[m_idx] else 0,
          status = "success",
          stringsAsFactors = FALSE
        )
      } else {
        message("Deriving DOCX from PDF for '", basename(file_path), "'...")
        tic_docx <- Sys.time()
        src_pdf <- sub("\\.(qmd|Rmd|rmd)$", ".pdf", file_path, ignore.case = TRUE)
        dest_docx <- sub("\\.(qmd|Rmd|rmd)$", ".docx", file_path, ignore.case = TRUE)
        conv_status <- tryCatch({
          if (!file.exists(src_pdf)) stop("Cannot derive DOCX: source PDF not found at ", src_pdf, call. = FALSE)
          conv <- convert_pdf_to_docx(src_pdf, dest = dest_docx)
          if (identical(conv, FALSE) || !file.exists(dest_docx)) {
            stop("convert_pdf_to_docx failed to produce ", dest_docx, call. = FALSE)
          }
          "success"
        }, error = function(e) {
          if (on_error == "stop") stop(e)
          warning("Error deriving DOCX from '", basename(src_pdf), "': ", conditionMessage(e), call. = FALSE)
          conditionMessage(e)
        })
        dur_docx <- as.numeric(Sys.time() - tic_docx, units = "secs")
        data.frame(
          file = basename(file_path),
          output = "docx",
          duration = round(dur_docx, 2),
          status = conv_status,
          stringsAsFactors = FALSE
        )
      }
      rows <- dplyr::bind_rows(rows, docx_row)
    }

    rows
  }

  # 3. Execution: Topological waves across targets.
  # Independent branches within each wave can render in parallel when explicitly requested.
  has_future <- requireNamespace("future", quietly = TRUE) && requireNamespace("furrr", quietly = TRUE)

  # Fleet rule: never touch future::plan() inside functions unless explicitly asked (workers > 1)
  touch_plan <- !is.null(workers) && is.numeric(workers) && as.integer(workers) > 1L

  n_workers <- if (is.null(workers)) {
    if (has_future && !inherits(future::plan(), "sequential")) {
      2L # Parallel-capable under external plan
    } else {
      1L
    }
  } else {
    max(1L, as.integer(workers))
  }

  can_parallelize <- has_future && length(targets) > 1 && n_workers > 1L

  if (can_parallelize && touch_plan) {
    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)
    future::plan(future::multisession, workers = n_workers)
  }

  # Build dependency mapping between targets
  n_targets <- length(targets)
  target_deps <- vector("list", n_targets)
  for (i in seq_len(n_targets)) {
    tgt_i <- targets[[i]]
    deps_i <- integer()
    for (j in seq_len(n_targets)) {
      if (i == j) next
      tgt_j <- targets[[j]]
      j_identifiers <- unique(c(tgt_j$name, tgt_j$path, tgt_j$outputs))

      # 1. Direct dependency match
      dep_match <- any(tgt_i$dependencies %in% j_identifiers)

      # 2. Graph reachability from tgt_j to tgt_i
      graph_match <- FALSE
      if (!dep_match && !is.null(pipeline_graph) && tgt_j$name %in% igraph::V(pipeline_graph)$name) {
        reachable_from_j <- names(igraph::subcomponent(pipeline_graph, tgt_j$name, mode = "out"))
        if (tgt_i$name %in% reachable_from_j || any(tgt_i$dependencies %in% reachable_from_j)) {
          graph_match <- TRUE
        }
      }

      if (dep_match || graph_match) {
        deps_i <- c(deps_i, j)
      }
    }
    target_deps[[i]] <- unique(deps_i)
  }

  # Partition targets into topological waves (generations)
  remaining <- seq_len(n_targets)
  waves <- list()
  while (length(remaining) > 0) {
    ready <- remaining[vapply(remaining, function(idx) {
      !any(target_deps[[idx]] %in% remaining)
    }, logical(1))]
    if (length(ready) == 0) {
      ready <- remaining[1]
    }
    waves[[length(waves) + 1L]] <- ready
    remaining <- setdiff(remaining, ready)
  }

  times_list <- list()
  failed_nodes <- character()

  for (wave_indices in waves) {
    wave_targets <- targets[wave_indices]

    # Check for upstream failures among targets in this wave
    skipped_flags <- vapply(wave_targets, function(tgt) {
      if (!isTRUE(skip_downstream) || length(failed_nodes) == 0) return(FALSE)
      reachable <- character()
      if (!is.null(pipeline_graph)) {
        for (fn in failed_nodes) {
          if (fn %in% igraph::V(pipeline_graph)$name) {
            reachable <- unique(c(reachable, setdiff(names(igraph::subcomponent(pipeline_graph, fn, mode = "out")), fn)))
          }
        }
      }
      tgt$name %in% reachable ||
        any(tgt$dependencies %in% failed_nodes) ||
        any(tgt$dependencies %in% reachable)
    }, logical(1))

    # Mark skipped targets
    if (any(skipped_flags)) {
      for (tgt in wave_targets[skipped_flags]) {
        message("Skipping '", basename(tgt$path), "' due to upstream failure.")
        skip_fmts <- if (isTRUE(tgt$derive_docx)) unique(c(tgt$formats, "docx")) else tgt$formats
        skip_df <- data.frame(
          file = basename(tgt$path),
          output = skip_fmts,
          duration = 0,
          status = "skipped_upstream_failure",
          stringsAsFactors = FALSE
        )
        times_list[[length(times_list) + 1L]] <- skip_df
        failed_nodes <- unique(c(failed_nodes, tgt$name, tgt$path, tgt$outputs))
      }
    }

    # Execute runnable targets in parallel across workers if available
    runnable_targets <- wave_targets[!skipped_flags]
    if (length(runnable_targets) > 0) {
      wave_results <- if (length(runnable_targets) > 1 && can_parallelize && n_workers > 1L) {
        furrr::future_map(
          runnable_targets,
          function(tgt) render_single(tgt$path, tgt$formats, isTRUE(tgt$derive_docx)),
          .options = furrr::furrr_options(seed = TRUE)
        )
      } else {
        lapply(runnable_targets, function(tgt) render_single(tgt$path, tgt$formats, isTRUE(tgt$derive_docx)))
      }

      for (k in seq_along(runnable_targets)) {
        tgt <- runnable_targets[[k]]
        tgt_res <- wave_results[[k]]
        times_list[[length(times_list) + 1L]] <- tgt_res
        if (any(tgt_res$status != "success")) {
          failed_nodes <- unique(c(failed_nodes, tgt$name, tgt$path, tgt$outputs))
        }
      }
    }
  }

  times <- dplyr::bind_rows(times_list)

  if ((stale_by == "hash" || !is.null(hash_file)) && !isTRUE(dry_run)) {
    objs_for_hash <- orig_pipeline_objects
    if (is.null(objs_for_hash) && length(targets) > 0L) {
      objs_for_hash <- lapply(targets, function(t) {
        FileOutputs(name = t$name, path = t$path, renders = TRUE)
      })
    }
    if (!is.null(objs_for_hash) && length(objs_for_hash) > 0L) {
      hf_written <- tryCatch(
        record_pipeline_hashes(refresh_pipeline(objs_for_hash), hash_file = hash_file),
        error = function(e) NULL
      )
      if (!is.null(hf_written)) {
        attr(times, "hash_file") <- hf_written
      }
    }
  }

  if (isTRUE(manifest) && length(targets) > 0 && !isTRUE(dry_run)) {
    out_dirs <- unique(vapply(targets, function(t) dirname(t$path), character(1)))
    manifest_paths <- character()
    for (od in out_dirs) {
      if (dir.exists(od)) {
        mf <- tryCatch(write_run_manifest(od, times, seed = seed), error = function(e) NULL)
        if (!is.null(mf)) manifest_paths <- c(manifest_paths, mf)
      }
    }
    if (length(manifest_paths) > 0) {
      attr(times, "manifest_path") <- manifest_paths[1]
    }
  }

  times
    }
  )
}
