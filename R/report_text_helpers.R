# Report text helpers: label dictionary lookups, the abbreviation glossary, reference-level
# contrast labels, model-table clean-up and Excel column letters.

# Normalises a label dictionary to term / short / full / unit / domain (character, NA when absent).
.label_dict <- function(dict) {
  if (is.character(dict) && !is.null(names(dict))) {
    return(tibble::tibble(term = names(dict), short = unname(dict), full = unname(dict),
                          unit = NA_character_, domain = NA_character_))
  }
  if (!is.data.frame(dict) || ncol(dict) < 2L) {
    stop("`dict` must be a named character vector or a data frame with a term column and at least one of ",
         "`short`, `full`.", call. = FALSE)
  }
  nms <- tolower(names(dict))
  pick <- function(choices) {
    i <- match(TRUE, nms %in% choices, nomatch = NA_integer_)
    if (is.na(i)) rep(NA_character_, nrow(dict)) else trimws(as.character(dict[[i]]))
  }
  term_i <- match(TRUE, nms %in% c("term", "variable", "var", "name", "column", "id"), nomatch = 1L)
  short <- pick(c("short", "short_label", "label", "abbreviation", "abbrev", "short_name"))
  full <- pick(c("full", "full_label", "long", "description", "definition", "full_name"))
  if (all(is.na(short)) && all(is.na(full))) {
    stop("`dict` has neither a short-label nor a full-label column (`short`, `full`).", call. = FALSE)
  }
  blank <- function(v) ifelse(!is.na(v) & !nzchar(v), NA_character_, v)
  out <- tibble::tibble(term = as.character(dict[[term_i]]), short = blank(short), full = blank(full),
                        unit = blank(pick(c("unit", "units"))), domain = blank(pick(c("domain", "group", "category"))))
  out <- out[!is.na(out$term), , drop = FALSE]
  if (anyDuplicated(out$term)) {
    stop("`dict` has duplicated terms: ", paste(unique(out$term[duplicated(out$term)]), collapse = ", "), ".",
         call. = FALSE)
  }
  out
}

# labels of `x` in the requested form; NA where the dictionary has no entry
.dict_lookup <- function(x, dict, form) {
  d <- .label_dict(dict)
  i <- match(x, d$term)
  short <- d$short[i]
  full <- d$full[i]
  unit <- d$unit[i]
  if (form == "short") return(ifelse(is.na(short), full, short))
  lab <- ifelse(is.na(full), short, full)
  if (form == "full") return(lab)
  # A label already ending in the unit — "(mmHg)" or the bare " mmHg" —
  # must not get the unit appended a second time.
  has_unit <- !is.na(lab) & !is.na(unit) &
    !endsWith(lab, paste0("(", unit, ")")) &
    !endsWith(lab, paste0(" ", unit)) &
    lab != unit
  ifelse(has_unit, paste0(lab, " (", unit, ")"), lab)
}

#' Abbreviation Glossary From a Label Dictionary
#'
#' Turns the label dictionary used by [pretty_name()] into an abbreviation reference table for a
#' report appendix: abbreviation (short label), definition (full label), unit and domain.
#'
#' @param dict A data frame with a term column (`term`, `variable`, `name`, ...), a short-label column
#'   (`short`, `abbreviation`, `label`) and a full-label column (`full`, `definition`, `description`),
#'   and optionally `unit` and `domain` columns. Column names are matched without regard to case.
#' @param domains Optional character vector of domains to keep, in the order given. Needs a `domain`
#'   column.
#' @param only_abbreviations If `TRUE` (default), entries whose short and full labels are the same,
#'   or that lack one of them, are left out: they are not abbreviations.
#' @param include_term If `TRUE`, the raw column name is kept as the first column. Default `FALSE`.
#' @return A tibble with `Abbreviation`, `Definition`, and `Unit` / `Domain` when the dictionary has
#'   them, one row per distinct abbreviation and definition, sorted by domain (in `domains` order, or
#'   alphabetically) and then by abbreviation without regard to case.
#' @seealso [pretty_name()], [tbl_label_map()]
#' @export
#' @examples
#' dict <- data.frame(
#'   term = c("liv_rl", "thb_osm3", "age"),
#'   short = c("LIV (%RL)", "tHb", "Age"),
#'   full = c("Lung injury volume", "Total hemoglobin", "Age"),
#'   unit = c("% right lung", "g/dL", "years"),
#'   domain = c("Imaging", "Blood", "Demographics")
#' )
#' tbl_abbreviations_glossary(dict)
#' pretty_name(c("liv_rl", "age", "not_in_dict"), dict = dict, form = "unit")
tbl_abbreviations_glossary <- function(dict, domains = NULL, only_abbreviations = TRUE, include_term = FALSE) {
  d <- .label_dict(dict)
  if (!is.null(domains)) {
    if (all(is.na(d$domain))) stop("`domains` was given but `dict` has no `domain` column.", call. = FALSE)
    gone <- setdiff(domains, d$domain)
    if (length(gone)) stop("`domains` not in `dict`: ", paste(gone, collapse = ", "), ".", call. = FALSE)
    d <- d[!is.na(d$domain) & d$domain %in% domains, , drop = FALSE]
  }
  if (isTRUE(only_abbreviations)) {
    d <- d[!is.na(d$short) & !is.na(d$full) & d$short != d$full, , drop = FALSE]
  }
  dom_rank <- if (!is.null(domains)) match(d$domain, domains) else rank(tolower(d$domain), na.last = TRUE, ties.method = "min")
  d <- d[order(dom_rank, tolower(d$short), tolower(d$full)), , drop = FALSE]
  out <- tibble::tibble(Term = d$term, Abbreviation = d$short, Definition = d$full, Unit = d$unit, Domain = d$domain)
  if (!isTRUE(include_term)) {
    out$Term <- NULL
    out <- unique(out)
  }
  if (all(is.na(out$Unit))) out$Unit <- NULL else out$Unit[is.na(out$Unit)] <- ""
  if (all(is.na(out$Domain))) out$Domain <- NULL else out$Domain[is.na(out$Domain)] <- ""
  out
}

#' Label Model Terms of Categorical Predictors With Their Reference Level
#'
#' Rewrites dummy-coded model terms such as `head_shapeHemis` (the naming of [stats::model.matrix()])
#' or `head_shape_Hemis` (the naming of `recipes::step_dummy()`) as
#' `"Head Shape: Hemis (relative to Flathead)"`, so a table or forest plot says what each
#' coefficient is compared with. Terms that are not contrasts of a known factor are returned
#' unchanged, or with their own label from `labels`.
#'
#' @section How terms are matched:
#' For every factor, character or logical column of `data` (or entry of `reference`), the candidate
#' term names are the variable name followed by each non-reference level, joined by nothing or by
#' `_` or `.`, with the level either as it is or passed through [make.names()]. A term must equal
#' a candidate exactly, so `age` is never mistaken for a level of `ag`. The reference is the first
#' level of a factor, the first value in sort order of a character column, and `FALSE` for a
#' logical. This is correct for treatment contrasts (the default in R and in `step_dummy()`); it
#' does not describe sum, Helmert or polynomial contrasts. Interaction terms (`a:b`) are labelled
#' part by part and joined by `" x "`.
#'
#' @param terms Character vector of model term names.
#' @param data Optional data frame the model was fitted to; its categorical columns give the
#'   variables, levels and references.
#' @param reference Optional named list giving the levels of each variable with the reference
#'   first, e.g. `list(head_shape = c("Flathead", "Hemis"))`. Used instead of, or in addition to,
#'   `data` (an entry here wins over the column of the same name).
#' @param labels Optional named character vector of display names for variables,
#'   `c(head_shape = "Head Shape")`. Without it the variable's `label` attribute is used, then
#'   [pretty_name()].
#' @param template Text with the placeholders `\{variable\}`, `\{level\}` and `\{reference\}`. Default
#'   `"\{variable\}: \{level\} (relative to \{reference\})"`.
#' @return A character vector as long as `terms`.
#' @seealso [pretty_name()], [relevel_factor()]
#' @export
#' @examples
#' d <- data.frame(head_shape = factor(c("Flathead", "Hemis", "Round")), age = c(40, 50, 60))
#' format_contrast_labels(c("head_shapeHemis", "head_shape_Round", "age"), data = d)
#' format_contrast_labels("orientation_Spine", reference = list(orientation = c("Chestwall", "Spine")))
format_contrast_labels <- function(terms, data = NULL, reference = NULL, labels = NULL,
                                   template = "{variable}: {level} (relative to {reference})") {
  if (is.factor(terms)) terms <- as.character(terms)
  if (!is.character(terms)) stop("`terms` must be a character vector.", call. = FALSE)
  if (is.null(data) && is.null(reference)) stop("Give `data` or `reference`.", call. = FALSE)
  if (!is.null(data) && !is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  if (!is.null(reference) && (!is.list(reference) || is.null(names(reference)) || any(!nzchar(names(reference))))) {
    stop("`reference` must be a named list of level vectors (reference first).", call. = FALSE)
  }
  if (!is.null(labels) && (!is.character(labels) || is.null(names(labels)))) {
    stop("`labels` must be a named character vector.", call. = FALSE)
  }
  if (!is.character(template) || length(template) != 1L) stop("`template` must be one string.", call. = FALSE)

  lev <- list()
  var_lab <- character()
  if (!is.null(data)) {
    for (v in names(data)) {
      col <- data[[v]]
      l <- if (is.factor(col)) levels(col) else if (is.character(col)) sort(unique(col[!is.na(col)])) else if (is.logical(col)) c("FALSE", "TRUE")
      if (length(l) >= 2L) lev[[v]] <- l
      a <- attr(col, "label", exact = TRUE)
      if (is.character(a) && length(a) == 1L && nzchar(a)) var_lab[v] <- a
    }
  }
  for (v in names(reference)) {
    l <- as.character(reference[[v]])
    if (length(l) < 2L) stop("`reference$", v, "` needs the reference level and at least one other level.", call. = FALSE)
    lev[[v]] <- l
  }
  name_of <- function(v) {
    if (!is.null(labels) && v %in% names(labels)) labels[[v]] else if (v %in% names(var_lab)) var_lab[[v]] else pretty_name(v)
  }

  # candidate term name -> finished label; a name claimed by two variables is ambiguous and left alone
  map <- character()
  clash <- character()
  for (v in names(lev)) {
    l <- lev[[v]]
    for (k in l[-1L]) {
      cand <- unique(c(paste0(v, k), paste0(v, "_", k), paste0(v, ".", k),
                       paste0(v, make.names(k)), paste0(v, "_", make.names(k)), paste0(v, ".", make.names(k))))
      lab <- gsub("{reference}", l[1L], gsub("{level}", k, gsub("{variable}", name_of(v), template, fixed = TRUE),
                                             fixed = TRUE), fixed = TRUE)
      clash <- c(clash, intersect(cand, names(map)[map[intersect(cand, names(map))] != lab]))
      map[cand] <- lab
    }
  }
  map <- map[setdiff(names(map), clash)]

  one <- function(term) {
    if (is.na(term)) return(NA_character_)
    if (term %in% names(map)) return(map[[term]])
    if (!is.null(labels) && term %in% names(labels)) return(labels[[term]])
    term
  }
  vapply(terms, function(term) {
    if (is.na(term)) return(NA_character_)
    if (term %in% names(map) || !grepl(":", term, fixed = TRUE)) return(one(term))
    paste(vapply(strsplit(term, ":", fixed = TRUE)[[1L]], one, character(1)), collapse = " x ")
  }, character(1), USE.NAMES = FALSE)
}

.unescape_pipes <- function(v) gsub("&#0*124;?|&#[xX]0*7[cC];?|&0*124;?|&vert;|&verbar;", "|", v)

# "time40.4573170731707" / "time = 40.4573170731707" -> "time = 40.5"
.round_conditioning <- function(v, digits, min_decimals) {
  pat <- paste0("([A-Za-z][A-Za-z0-9_.]*?)(\\s*=\\s*|\\s?)(-?[0-9]+\\.[0-9]{", min_decimals, ",})")
  hit <- !is.na(v) & grepl(pat, v, perl = TRUE)
  v[hit] <- vapply(v[hit], function(s) {
    m <- gregexpr(pat, s, perl = TRUE)
    regmatches(s, m) <- lapply(regmatches(s, m), function(parts) {
      vapply(parts, function(part) {
        g <- regmatches(part, regexec(pat, part, perl = TRUE))[[1L]]
        paste0(g[2L], " = ", formatC(round(as.numeric(g[4L]), digits), format = "f", digits = digits))
      }, character(1))
    })
    s
  }, character(1))
  v
}

#' Clean a Mixed-Model, emmeans or ANOVA Table for Word and PDF Output
#'
#' Fixes rendering artefacts that recur when `broom.mixed`, `emmeans` and `anova` tables are sent
#' to Word or PDF:
#'
#' * HTML-escaped pipes in random-effect terms (`(1 &#124; subject_id)`, `&124;`, `&vert;`) become `|`.
#' * Unrounded conditioning values written into labels and notes (`time40.4573170731707`,
#'   `time = 40.4573170731707`) become `time = 40.5`.
#' * The degrees-of-freedom columns `NumDF` and `DenDF` are renamed `Num DF` and `Den DF` (and
#'   `t.ratio`, `z.ratio`, `F.ratio` to `t ratio`, `z ratio`, `F ratio`), with a footnote defining them.
#' * Coefficients dropped for rank deficiency get an explanatory footnote.
#'
#' Text is changed in the column names, in character and factor columns, and in the `mesg`
#' attribute that `emmeans` summaries carry. Numeric columns are not touched.
#'
#' @section Rounding rule:
#' A number is rounded only when it has at least `min_decimals` decimals and directly follows a
#' name (letters, digits, `_`, `.`, starting with a letter), with or without `=`. A name that ends in
#' digits and is glued to its value without `=` cannot be told apart from the value
#' (`x12.34567` is read as `x = 12.3`); write such labels with `=`.
#'
#' @param x A data frame (tibble, `summary_emm`, `anova` table), or a character vector.
#' @param digits Decimals kept for conditioning values (default 1).
#' @param min_decimals Smallest number of decimals that marks a value as unrounded (default 4).
#' @param model Optional fitted model (`lm`, `glm`, `merMod`) to look for coefficients dropped
#'   because of rank deficiency.
#' @param dropped Optional character vector of dropped coefficient names, instead of `model`.
#' @param rename_df If `TRUE` (default), rename the degrees-of-freedom and ratio columns.
#' @return `x` with the fixes applied. For a data frame, `attr(, "footnotes")` is a character vector
#'   of footnotes to print under the table (empty when none apply).
#' @seealso [format_contrast_labels()], [fit_lmer_safely()]
#' @export
#' @examples
#' tab <- data.frame(
#'   term = c("(Intercept)", "sd__(Intercept) (1 &#124; subject_id)"),
#'   contrast = c("time40.4573170731707", "group A - group B"),
#'   NumDF = c(1, 2), DenDF = c(38.2, 40.1)
#' )
#' out <- sanitize_model_table(tab)
#' out
#' attr(out, "footnotes")
sanitize_model_table <- function(x, digits = 1, min_decimals = 4, model = NULL, dropped = NULL, rename_df = TRUE) {
  if (!is.numeric(digits) || length(digits) != 1L || digits < 0) stop("`digits` must be a non-negative number.", call. = FALSE)
  if (!is.numeric(min_decimals) || length(min_decimals) != 1L || min_decimals < 1) {
    stop("`min_decimals` must be a positive number.", call. = FALSE)
  }
  digits <- as.integer(digits)
  min_decimals <- as.integer(min_decimals)
  fix <- function(v) .round_conditioning(.unescape_pipes(v), digits, min_decimals)
  if (is.character(x) || is.factor(x)) {
    if (is.factor(x)) {
      levels(x) <- fix(levels(x))
      return(x)
    }
    return(fix(x))
  }
  if (!is.data.frame(x)) stop("`x` must be a data frame or a character vector.", call. = FALSE)
  if (!is.null(dropped) && !is.character(dropped)) stop("`dropped` must be a character vector.", call. = FALSE)

  for (j in seq_along(x)) {
    if (is.character(x[[j]])) {
      x[[j]] <- fix(x[[j]])
    } else if (is.factor(x[[j]])) {
      lv <- fix(levels(x[[j]]))
      # two levels can become one after rounding
      x[[j]] <- factor(lv[as.integer(x[[j]])], levels = unique(lv))
    }
  }
  nm <- fix(names(x))
  notes <- character()
  if (isTRUE(rename_df)) {
    ren <- c(NumDF = "Num DF", DenDF = "Den DF", t.ratio = "t ratio", z.ratio = "z ratio", F.ratio = "F ratio")
    hit <- nm %in% names(ren)
    if (any(nm %in% c("NumDF", "DenDF"))) {
      notes <- c(notes, paste0("Num DF: numerator degrees of freedom of the F test (number of parameters tested). ",
                               "Den DF: denominator degrees of freedom (estimated for a mixed model)."))
    }
    nm[hit] <- unname(ren[nm[hit]])
  }
  names(x) <- nm
  if (!is.null(rownames(x)) && is.character(attr(x, "row.names"))) rownames(x) <- make.unique(fix(rownames(x)))
  mesg <- attr(x, "mesg", exact = TRUE)
  if (is.character(mesg)) attr(x, "mesg") <- fix(mesg)

  if (is.null(dropped) && !is.null(model)) {
    dropped <- if (inherits(model, "merMod")) {
      if (!requireNamespace("lme4", quietly = TRUE)) stop("Package \"lme4\" is required for a mixed `model`.", call. = FALSE)
      xm <- lme4::getME(model, "X")
      cd <- attr(xm, "col.dropped")
      if (is.null(cd)) character() else names(cd)
    } else {
      cf <- tryCatch(stats::coef(model), error = function(e) NULL)
      if (is.numeric(cf)) names(cf)[is.na(cf)] else character()
    }
  }
  if (length(dropped)) {
    notes <- c(notes, paste0("Not estimable and dropped from the model because of rank deficiency (the term is an ",
                             "exact linear combination of other terms): ", paste(fix(dropped), collapse = ", "), "."))
  }
  attr(x, "footnotes") <- notes
  x
}

#' Excel Column Letters
#'
#' `excel_col_letters()` converts column positions to Excel column letters (`1` is `"A"`, `26` is
#' `"Z"`, `27` is `"AA"`, `703` is `"AAA"`). `rename_to_excel()` names the columns of a data frame by
#' their position letters, for sheets with multi-row headers that are easier to address by position
#' (`df$C`) until proper names are assigned.
#'
#' @param n Positive whole numbers (column positions). `NA` gives `NA`.
#' @return `excel_col_letters()`: a character vector as long as `n`. `rename_to_excel()`: `df` with its
#'   columns renamed `A`, `B`, ...; the original names are kept in `attr(, "original_names")`.
#' @seealso [read_positional_excel_headers()], [excel_colnames()]
#' @export
#' @examples
#' excel_col_letters(c(1, 26, 27, 52, 703))
#' names(rename_to_excel(data.frame(id = 1, `Blood gas` = 2, check.names = FALSE)))
excel_col_letters <- function(n) {
  if (!is.numeric(n)) stop("`n` must be numeric.", call. = FALSE)
  ok <- !is.na(n)
  if (any(n[ok] < 1 | n[ok] != round(n[ok]))) stop("`n` must be positive whole numbers.", call. = FALSE)
  vapply(n, function(k) {
    if (is.na(k)) return(NA_character_)
    out <- character()
    while (k > 0) {
      r <- (k - 1) %% 26
      out <- c(LETTERS[r + 1], out)
      k <- (k - 1) %/% 26
    }
    paste(out, collapse = "")
  }, character(1))
}

#' @rdname excel_col_letters
#' @param df A data frame.
#' @export
rename_to_excel <- function(df) {
  if (!is.data.frame(df)) stop("`df` must be a data frame.", call. = FALSE)
  attr(df, "original_names") <- names(df)
  names(df) <- excel_col_letters(seq_along(df))
  df
}
