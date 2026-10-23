#' Reconcile a Data Frame's Column Names With a Column Dictionary
#'
#' Maps the columns of `data` onto a dictionary of source names (for example the
#' upper-case or mixed-case variable names of a SAS export) and target names (the
#' standardized R names), and reports whether the mapping is complete and one to one.
#' It is meant for the check that comes before a rename: it does not guess, and it does not
#' rename anything that is ambiguous.
#'
#' Each column of `data` is matched to the dictionary's source names in three tiers, and the
#' first tier that finds anything decides:
#' 1. **Exact**: `col == source`.
#' 2. **Case-insensitive**: `tolower(col) == tolower(source)`.
#' 3. **Normalized**: both cleaned by `janitor::make_clean_names()` (the cleaning that
#'    [clean_names_keep_labels()] applies: symbols and spaces become `_`, lower case, repeated
#'    `_` collapsed), one name at a time, so that two names that clean to the same string are
#'    seen as a collision and not renamed apart.
#'
#' A match is a **conflict**, and is not used, when
#' * a column matches more than one dictionary entry in its tier (`"ambiguous_column"`);
#' * a dictionary entry is matched by more than one column (`"shared_source"`), for
#'   instance `Bruise_Area` and `bruise_area` both present;
#' * two matched entries have the same target (`"shared_target"`), or the dictionary lists a
#'   source twice with different targets (`"duplicate_source"`);
#' * the target is already the name of another column of `data` that is not itself being
#'   renamed (`"target_in_data"`).
#'
#' @param data A data frame.
#' @param dictionary A data frame with one row per variable.
#' @param source_col,target_col Names of the columns of `dictionary` that hold the source
#'   names (as they appear in `data`) and the target names.
#' @param required Character vector of target names that must be mapped for the result to
#'   be valid. `NULL` (the default) requires every target in the dictionary.
#' @param rename When is `$data` filled in with the renamed data frame? `"if_valid"`
#'   (the default), `"always"` (the unambiguous matches are renamed, whatever else is wrong)
#'   or `"never"`.
#' @return An object of class `cbe_column_reconciliation`, a list with
#'   * `data`: the data frame with the matched columns renamed, or `NULL` (see `rename`);
#'   * `matched`: tibble of the confirmed matches (`raw_name`, `clean_name`, `target_name`,
#'     `match_tier`, the tier being `"exact"`, `"case_insensitive"` or `"normalized"`);
#'   * `unmapped_data`: columns of `data` that matched nothing;
#'   * `unmapped_dict`: source names of the dictionary that matched no column;
#'   * `conflicts`: tibble (`type`, `raw_name`, `source_name`, `target_name`, `detail`);
#'   * `is_valid`: `TRUE` if there is no conflict and every required target is mapped.
#' @seealso [clean_names_keep_labels()], [compare_df()]
#' @export
#' @examples
#' dat <- data.frame(Bruise_Area = 1:3, CCS_CB_caud_ceph = 4:6, extra = 7:9)
#' dict <- data.frame(
#'   source = c("bruise_area", "CCS CB caud ceph", "age"),
#'   target = c("bruise_area", "ccs_cb_caud_ceph", "age")
#' )
#' res <- reconcile_column_dictionary(dat, dict)
#' res$matched
#' res$unmapped_data
#' res$unmapped_dict
#' res$is_valid
reconcile_column_dictionary <- function(data,
                                        dictionary,
                                        source_col = "source",
                                        target_col = "target",
                                        required = NULL,
                                        rename = c("if_valid", "always", "never")) {
  rename <- match.arg(rename)
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  if (!is.data.frame(dictionary)) {
    stop("`dictionary` must be a data frame.", call. = FALSE)
  }
  for (arg in c("source_col", "target_col")) {
    val <- get(arg)
    if (!is.character(val) || length(val) != 1L || is.na(val) || !val %in% names(dictionary)) {
      stop("`", arg, "` must name a column of `dictionary`.", call. = FALSE)
    }
  }
  src <- as.character(dictionary[[source_col]])
  tgt <- as.character(dictionary[[target_col]])
  if (anyNA(src) || anyNA(tgt) || any(!nzchar(src)) || any(!nzchar(tgt))) {
    stop("`dictionary` has missing or empty source or target names.", call. = FALSE)
  }
  if (!is.null(required)) {
    if (!is.character(required) || anyNA(required)) {
      stop("`required` must be NULL or a character vector of target names.", call. = FALSE)
    }
    unknown <- setdiff(required, tgt)
    if (length(unknown)) {
      stop("`required` names targets that are not in the dictionary: ",
           paste(unknown, collapse = ", "), call. = FALSE)
    }
  }

  cols <- names(data)
  clean1 <- function(x) vapply(x, function(s) janitor::make_clean_names(s), character(1), USE.NAMES = FALSE)
  src_lower <- tolower(src)
  src_clean <- clean1(src)

  conflicts <- list()
  add_conflict <- function(type, raw = NA_character_, source = NA_character_, target = NA_character_, detail = "") {
    conflicts[[length(conflicts) + 1L]] <<- tibble::tibble(
      type = type, raw_name = raw, source_name = source, target_name = target, detail = detail
    )
  }

  # the dictionary may list one source twice, with the same target (harmless) or with two
  dup_src <- unique(src[duplicated(src)])
  for (s in dup_src) {
    t <- unique(tgt[src == s])
    if (length(t) > 1L) {
      add_conflict("duplicate_source", source = s, target = paste(t, collapse = " | "),
                   detail = "the dictionary lists this source with different targets")
    }
  }
  bad_src <- if (length(dup_src)) dup_src[vapply(dup_src, function(s) length(unique(tgt[src == s])) > 1L, logical(1))] else character()

  # tier by tier: the first tier with any match decides for a column
  hit <- lapply(cols, function(col) {
    tiers <- list(
      exact = which(src == col),
      case_insensitive = which(src_lower == tolower(col)),
      normalized = which(src_clean == janitor::make_clean_names(col))
    )
    for (nm in names(tiers)) {
      idx <- tiers[[nm]]
      if (length(idx)) {
        # a source listed twice with the same target is one entry
        entries <- unique(src[idx])
        return(list(tier = nm, entries = entries))
      }
    }
    NULL
  })
  names(hit) <- cols

  matched_rows <- list()
  ambiguous <- character()
  for (col in cols) {
    h <- hit[[col]]
    if (is.null(h)) next
    if (length(h$entries) > 1L) {
      add_conflict("ambiguous_column", raw = col, source = paste(h$entries, collapse = " | "),
                   detail = paste0("matches ", length(h$entries), " dictionary entries at the ", h$tier, " tier"))
      ambiguous <- c(ambiguous, col)
      next
    }
    if (h$entries %in% bad_src) next
    matched_rows[[length(matched_rows) + 1L]] <- tibble::tibble(
      raw_name = col,
      clean_name = janitor::make_clean_names(col),
      source_name = h$entries,
      target_name = unique(tgt[src == h$entries]),
      match_tier = h$tier
    )
  }
  matched <- if (length(matched_rows)) {
    dplyr::bind_rows(matched_rows)
  } else {
    tibble::tibble(raw_name = character(), clean_name = character(), source_name = character(),
                   target_name = character(), match_tier = character())
  }

  # one dictionary entry claimed by several columns
  shared_src <- unique(matched$source_name[duplicated(matched$source_name)])
  for (s in shared_src) {
    rows <- matched[matched$source_name == s, ]
    add_conflict("shared_source", raw = paste(rows$raw_name, collapse = " | "), source = s,
                 target = rows$target_name[1L], detail = "several columns match this dictionary entry")
  }
  matched <- matched[!matched$source_name %in% shared_src, ]

  # two entries with one target
  shared_tgt <- unique(matched$target_name[duplicated(matched$target_name)])
  for (t in shared_tgt) {
    rows <- matched[matched$target_name == t, ]
    add_conflict("shared_target", raw = paste(rows$raw_name, collapse = " | "),
                 source = paste(rows$source_name, collapse = " | "), target = t,
                 detail = "several columns would get this target name")
  }
  matched <- matched[!matched$target_name %in% shared_tgt, ]

  # the target is the name of a column that stays (and is not renamed to something else)
  stays <- setdiff(cols, matched$raw_name)
  clash <- matched$target_name %in% stays & matched$target_name != matched$raw_name
  for (i in which(clash)) {
    add_conflict("target_in_data", raw = matched$raw_name[i], source = matched$source_name[i],
                 target = matched$target_name[i], detail = "another column of the data already has this name")
  }
  matched <- matched[!clash, ]

  conflicted_sources <- unique(c(
    bad_src,
    unlist(lapply(conflicts, function(x) if (x$type %in% c("shared_source", "shared_target", "target_in_data", "ambiguous_column")) strsplit(x$source_name, " | ", fixed = TRUE)[[1L]] else NULL))
  ))
  conflict_df <- if (length(conflicts)) {
    dplyr::bind_rows(conflicts)
  } else {
    tibble::tibble(type = character(), raw_name = character(), source_name = character(),
                   target_name = character(), detail = character())
  }

  raw_in_conflict <- unique(unlist(strsplit(stats::na.omit(conflict_df$raw_name), " | ", fixed = TRUE)))
  unmapped_data <- setdiff(cols, c(matched$raw_name, raw_in_conflict))
  unmapped_dict <- setdiff(unique(src), c(matched$source_name, conflicted_sources))

  need <- if (is.null(required)) unique(tgt) else required
  missing_required <- setdiff(need, matched$target_name)
  is_valid <- nrow(conflict_df) == 0L && length(missing_required) == 0L

  renamed <- NULL
  if (rename == "always" || (rename == "if_valid" && is_valid)) {
    renamed <- data
    idx <- match(matched$raw_name, names(renamed))
    names(renamed)[idx] <- matched$target_name
  }

  structure(
    list(
      data = renamed,
      matched = matched[, c("raw_name", "clean_name", "target_name", "match_tier")],
      unmapped_data = unmapped_data,
      unmapped_dict = unmapped_dict,
      conflicts = conflict_df,
      is_valid = is_valid,
      missing_required = missing_required
    ),
    class = "cbe_column_reconciliation"
  )
}

#' @export
print.cbe_column_reconciliation <- function(x, ...) {
  cat("<cbe_column_reconciliation> ", if (x$is_valid) "valid" else "NOT valid", "\n", sep = "")
  cat("  matched:       ", nrow(x$matched), "\n", sep = "")
  cat("  unmapped data: ", length(x$unmapped_data), if (length(x$unmapped_data)) paste0(" (", paste(utils::head(x$unmapped_data, 5L), collapse = ", "), if (length(x$unmapped_data) > 5L) ", ..." else "", ")") else "", "\n", sep = "")
  cat("  unmapped dict: ", length(x$unmapped_dict), if (length(x$unmapped_dict)) paste0(" (", paste(utils::head(x$unmapped_dict, 5L), collapse = ", "), if (length(x$unmapped_dict) > 5L) ", ..." else "", ")") else "", "\n", sep = "")
  cat("  conflicts:     ", nrow(x$conflicts), "\n", sep = "")
  if (length(x$missing_required)) {
    cat("  required but not mapped: ", paste(x$missing_required, collapse = ", "), "\n", sep = "")
  }
  invisible(x)
}
