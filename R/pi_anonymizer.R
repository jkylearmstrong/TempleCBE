#' PI Name Generator and Anonymizer
#'
#' Provides functions to procedurally generate synthetic last names,
#' generate pseudonym tokens (sequential or HMAC-based), and anonymize
#' investigator names (supporting both single scalar and vectorized inputs).
#'
#' @name pi_anonymizer
NULL

#' Default Secrets Path for PI Mapping
#'
#' Locates or creates the user-scoped directory for confidential PI mapping files.
#' Uses base R's `tools::R_user_dir("TempleCBE", which = "data")` to avoid storing
#' sensitive identifier mappings inside the repository tree.
#'
#' @return File path to `pi_mapping.json` inside the user's data directory.
#' @noRd
default_secrets_path <- function() {
  dir <- tools::R_user_dir("TempleCBE", which = "data")
  validate_secrets_dir(dir)
  file.path(dir, "pi_mapping.json")
}

# helper: find repo root by locating .git
.find_repo_root <- function(path = ".") {
  p <- tryCatch(normalizePath(path, winslash = "/", mustWork = FALSE), error = function(e) NULL)
  if (is.null(p)) return(NULL)
  repeat {
    git_marker <- file.path(p, ".git")
    # In a normal checkout `.git` is a directory; in a git worktree it's a file.
    if (dir.exists(git_marker) || file.exists(git_marker)) return(p)
    parent <- dirname(p)
    if (identical(parent, p)) return(NULL)
    p <- parent
  }
}

#' Procedural Synthetic Last Name Generator
#'
#' Generates realistic, pronounceable synthetic surnames using phonetic
#' onsets, vowels, and common surname endings (e.g., `-berg`, `-ton`, `-man`,
#' `-wood`, `-ford`). Supports strict exclusion lists (e.g. to guarantee real
#' investigator surnames are never generated) and reproducible seed-based sampling
#' without mutating the global RNG state.
#'
#' @param n Integer, number of unique names to generate (default 10).
#' @param seed Optional integer, random seed for reproducibility.
#' @param exclude Character vector of surnames (case-insensitive) that must
#'   not be generated.
#' @param max_tries Integer, maximum attempts multiplier before throwing an error
#'   if distinct names cannot be generated.
#'
#' @return A character vector of length `n` containing synthetic surnames.
#' @export
#'
#' @examples
#' # Generate 5 random last names
#' generate_last_names(5)
#'
#' # Reproducible generation with exclusions
#' generate_last_names(3, seed = 1, exclude = c("Madison", "Davison"))
generate_last_names <- function(n = 10,
                                seed = NULL,
                                exclude = character(),
                                max_tries = 100L) {
  if (!is.numeric(n) || length(n) != 1 || is.na(n) || n < 0 || n != round(n)) {
    stop("`n` must be a single whole number, 0 or more.", call. = FALSE)
  }
  n <- as.integer(n)
  if (n == 0L) return(character(0))

  onsets  <- c("b", "c", "d", "f", "g", "h", "j", "k", "l", "m", "n", "p", "r", "s", "t", "v", "w", "z",
               "br", "cr", "dr", "fr", "gr", "kr", "pr", "tr", "vr", "st", "str", "cl", "gl", "pl")
  vowels  <- c("a", "e", "i", "o", "u", "ai", "ea", "ie", "oa", "ou")
  endings <- c("n", "r", "s", "t", "ll", "son", "sen", "ton", "man", "berg", "wood", "well", "ford", "land")

  make_name <- function() {
    k <- sample(2:3, 1)
    stem <- paste0(sample(onsets, k, replace = TRUE), sample(vowels, k, replace = TRUE), collapse = "")
    name <- paste0(stem, sample(endings, 1))
    paste0(toupper(substring(name, 1, 1)), substring(name, 2))
  }

  draw <- function() {
    taken <- tolower(exclude)
    out <- character()
    tries <- 0L
    while (length(out) < n) {
      tries <- tries + 1L
      if (tries > max_tries * n) {
        stop("Couldn't generate ", n, " distinct names; lower `n` or shorten `exclude`.", call. = FALSE)
      }
      candidate <- make_name()
      if (!tolower(candidate) %in% taken) {
        out <- c(out, candidate)
        taken <- c(taken, tolower(candidate))
      }
    }
    out
  }

  if (is.null(seed)) {
    draw()
  } else if (requireNamespace("withr", quietly = TRUE)) {
    withr::with_seed(seed, draw())
  } else {
    old_seed <- if (exists(".Random.seed", envir = .GlobalEnv)) get(".Random.seed", envir = .GlobalEnv) else NULL
    on.exit({
      if (is.null(old_seed)) {
        if (exists(".Random.seed", envir = .GlobalEnv)) rm(".Random.seed", envir = .GlobalEnv)
      } else {
        assign(".Random.seed", old_seed, envir = .GlobalEnv)
      }
    }, add = TRUE)
    set.seed(seed)
    draw()
  }
}

#' Generate Pseudonym Tokens (Cryptographic Hash)
#'
#' Generates secure cryptographic hash pseudonym tokens. When `name` is supplied,
#' computes deterministic keyed HMAC-SHA256 tokens. When `name` is `NULL`, generates
#' random cryptographic hash tokens (or reproducible hash tokens when `seed` is provided).
#' Supports custom prefix, configurable character length (e.g. for short tokens), and
#' custom renaming/formatting functions.
#'
#' @param name Optional character vector of names to tokenize. If provided, computes
#'   a deterministic HMAC-SHA256 token for each non-NA name.
#' @param n Integer, number of tokens to generate when `name` is `NULL` (default 1).
#' @param prefix Character string, prefix prepended to each token (default `"PI_"`).
#' @param key Character string, secret key / pepper for HMAC tokenization. Defaults to
#'   the environment variable `Sys.getenv("TEMPLECBE_SECRET_KEY", unset = "")`.
#' @param n_chars Integer, number of hex characters from the hash digest to include (default 16).
#'   Can be shortened (e.g. 4 or 8) to produce compact/short tokens.
#' @param rename_fn Optional function to transform or rename tokens (e.g., `tolower`, `toupper`,
#'   or a custom function such as `function(tok) paste0("INV-", substr(tok, 4, 8))`).
#' @param seed Optional integer, random seed for reproducible random token generation when `name` is `NULL`.
#'
#' @return Character vector of pseudonym tokens.
#' @export
#'
#' @examples
#' # Random cryptographic hash tokens
#' generate_pseudonym_token(n = 3)
#'
#' # Short hash tokens (4 hex chars)
#' generate_pseudonym_token(n = 3, n_chars = 4)
#'
#' # Custom renaming function
#' generate_pseudonym_token(n = 2, rename_fn = tolower)
#'
#' # Stateless deterministic token with key
#' generate_pseudonym_token("Franklin", key = "study_salt")
#'
#' # Vectorized tokenization
#' generate_pseudonym_token(c("Smith", "Jones"), key = "study_salt")
generate_pseudonym_token <- function(name = NULL,
                                     n = 1,
                                     prefix = "PI_",
                                     key = Sys.getenv("TEMPLECBE_SECRET_KEY", unset = ""),
                                     n_chars = 16,
                                     rename_fn = NULL,
                                     seed = NULL) {
  if (!is.null(rename_fn) && !is.function(rename_fn)) {
    stop("`rename_fn` must be a function or NULL.")
  }
  if (!is.null(n_chars)) {
    n_chars <- as.integer(n_chars)
    if (length(n_chars) != 1 || is.na(n_chars) || n_chars < 1) {
      stop("`n_chars` must be a positive integer or NULL for full hash.")
    }
  }

  slice_hash <- function(h) {
    if (is.null(n_chars)) h else substr(h, 1, min(nchar(h), n_chars))
  }

  if (!is.null(name)) {
    if (!is.character(name)) {
      stop("`name` must be a character vector.")
    }
    effective_key <- if (nzchar(key)) key else "temple_cbe_default_salt"
    tokens <- vapply(name, function(nm) {
      if (is.na(nm)) return(NA_character_)
      h <- as.character(openssl::sha256(nm, key = effective_key))
      paste0(prefix, slice_hash(h))
    }, character(1), USE.NAMES = FALSE)
  } else {
    if (!is.numeric(n) || length(n) != 1 || n < 1) {
      stop("`n` must be a positive integer.")
    }
    n <- as.integer(n)
    if (!is.null(seed)) {
      tokens <- vapply(seq_len(n), function(i) {
        raw_hash <- as.character(openssl::sha256(paste0("templecbe_seed_", seed, "_", i)))
        paste0(prefix, slice_hash(raw_hash))
      }, character(1), USE.NAMES = FALSE)
    } else {
      # Use true OpenSSL CSPRNG source (32 bytes = 256 bits of entropy)
      tokens <- vapply(seq_len(n), function(i) {
        raw_hash <- as.character(openssl::sha256(openssl::rand_bytes(32)))
        paste0(prefix, slice_hash(raw_hash))
      }, character(1), USE.NAMES = FALSE)
    }
  }

  if (!is.null(rename_fn)) {
    tokens <- rename_fn(tokens)
  }

  if (is.null(name) && n == 1) tokens[1] else tokens
}

#' Generate Random PI Names or Tokens
#'
#' Generates random synthetic principal investigator (PI) names or cryptographic
#' pseudonym tokens (`PI_...`).
#'
#' @param n Integer, number of names to generate (default 1).
#' @param format Character, either `"synthetic"` (realistic surnames) or `"token"`
#'   (cryptographic hash tokens).
#' @param prefix Character, prefix when format is `"token"` (default `"PI_"`).
#' @param n_chars Integer, number of hex characters from the hash digest when `format = "token"` (default 16).
#' @param rename_fn Optional function to rename or format generated tokens/names.
#' @param seed Optional integer, random seed for reproducibility.
#'
#' @return A character vector of length `n` (or a single character string if `n = 1`).
#' @export
#'
#' @examples
#' generate_pi_names(1)
#' generate_pi_names(3, format = "token")
#' generate_pi_names(3, format = "token", n_chars = 8)
generate_pi_names <- function(n = 1,
                              format = c("synthetic", "token"),
                              prefix = "PI_",
                              n_chars = 16,
                              rename_fn = NULL,
                              seed = NULL) {
  format <- match.arg(format)
  if (format == "token") {
    return(generate_pseudonym_token(name = NULL, n = n, prefix = prefix, n_chars = n_chars, rename_fn = rename_fn, seed = seed))
  }
  res <- generate_last_names(n = n, seed = seed)
  if (!is.null(rename_fn)) {
    res <- rename_fn(res)
  }
  res
}

#' Anonymize Investigator Names (Vectorized)
#'
#' Anonymizes investigator names into pseudonyms. Supports both single scalar
#' names and character vectors (e.g. data frame columns like `df$investigator`).
#' Offers both stateless keyed-HMAC hashing and stateful mapping table persistence.
#'
#' Methods:
#' \itemize{
#'   \item `"hmac_token"` (default): Stateless deterministic token (e.g., `PI_5c1ebfd9`) computed via
#'     `openssl::sha256(name, key = key)`. No mapping file needed on disk.
#'   \item `"hmac_surname"`: Stateless deterministic procedural surname generated via
#'     keyed HMAC seed and [generate_last_names()]. Consistent across runs without disk state.
#'   \item `"token"`: Stateful cryptographic tokens persisted to a user-level JSON mapping file
#'     (`tools::R_user_dir("TempleCBE", "data")/pi_mapping.json`). Explicitly refuses to write into
#'     the Git repository tree to prevent accidental commits of PHI.
#'   \item `"synthetic"`: Random procedural surnames via [generate_last_names()].
#' }
#'
#' @param name Character vector of investigator names or surnames to mask.
#' @param method Character string specifying anonymization approach: `"hmac_token"`,
#'   `"hmac_surname"`, `"token"`, or `"synthetic"`.
#' @param key Character string, secret key / pepper for HMAC methods. Defaults to
#'   `Sys.getenv("TEMPLECBE_SECRET_KEY", unset = "")`.
#' @param secrets_path Path to confidential mapping file when `method = "token"`.
#'   Defaults to `tools::R_user_dir("TempleCBE", which = "data")/pi_mapping.json`.
#' @param exclude Character vector of names that must never be generated (passed to
#'   [generate_last_names()] when method is `"hmac_surname"` or `"synthetic"`).
#' @param prefix Character string, prefix for tokens (default `"PI_"`).
#' @param n_chars Integer, number of hex characters for tokens (default 8). Can be shortened for short names.
#' @param rename_fn Optional function to rename or format generated tokens/pseudonyms.
#' @param seed Optional integer, random seed when method is `"synthetic"`.
#'
#' @return Character vector of anonymized tokens or pseudonyms, matching the length of `name`.
#' @export
#'
#' @examples
#' # Vectorized token mapping (default method = "hmac_token")
#' anonymize_pi(c("Franklin", "Taylor", "Patel"), key = "study_salt")
#'
#' # Short tokens (4 characters)
#' anonymize_pi(c("Franklin", "Taylor"), n_chars = 4, key = "study_salt")
#'
#' # Custom renaming function
#' anonymize_pi(c("Franklin", "Taylor"), rename_fn = tolower, key = "study_salt")
#'
#' # Vectorized realistic pseudonyms
#' anonymize_pi(c("Franklin", "Taylor"), method = "hmac_surname", key = "study_salt")
anonymize_pi <- function(name,
                         method = c("hmac_token", "hmac_surname", "token", "synthetic"),
                         key = Sys.getenv("TEMPLECBE_SECRET_KEY", unset = ""),
                         secrets_path = NULL,
                         exclude = character(),
                         prefix = "PI_",
                         n_chars = 16,
                         rename_fn = NULL,
                         seed = NULL) {
  if (missing(name) || !is.character(name) || length(name) == 0) {
    stop("`name` must be a character vector with at least one element.")
  }
  method <- match.arg(method)

  is_na <- is.na(name)
  out <- character(length(name))
  if (all(is_na)) {
    return(rep(NA_character_, length(name)))
  }
  valid_indices <- which(!is_na)
  valid_names <- name[valid_indices]

  if (method == "hmac_token") {
    res <- generate_pseudonym_token(name = valid_names, prefix = prefix, key = key, n_chars = n_chars, rename_fn = rename_fn)
    out[valid_indices] <- res
    out[is_na] <- NA_character_
    return(out)
  }

  if (method == "hmac_surname") {
    effective_key <- if (nzchar(key)) key else "temple_cbe_default_salt"
    res <- vapply(valid_names, function(nm) {
      h <- openssl::sha256(nm, key = effective_key)
      s <- strtoi(substr(as.character(h), 1, 7), 16L)
      generate_last_names(n = 1, seed = s, exclude = c(nm, exclude))
    }, character(1), USE.NAMES = FALSE)
    if (!is.null(rename_fn)) {
      res <- rename_fn(res)
    }
    out[valid_indices] <- res
    out[is_na] <- NA_character_
    return(out)
  }

  if (method == "synthetic") {
    res <- generate_last_names(n = length(valid_names), seed = seed, exclude = c(valid_names, exclude))
    if (!is.null(rename_fn)) {
      res <- rename_fn(res)
    }
    out[valid_indices] <- res
    out[is_na] <- NA_character_
    return(out)
  }

  # Default: method == "token" (persistent JSON mapping table)
  if (is.null(secrets_path)) {
    secrets_path <- default_secrets_path()
  } else {
    parent_dir <- dirname(secrets_path)
    validate_secrets_dir(parent_dir)
  }
  # Normalize through the parent directory: normalizePath() cannot canonicalize a
  # file that doesn't exist yet, which left Windows 8.3 short names and macOS
  # /var -> /private/var unresolved and let the repository check below miss.
  secrets_path <- tryCatch(
    file.path(normalizePath(dirname(secrets_path), winslash = "/", mustWork = FALSE), basename(secrets_path)),
    error = function(e) secrets_path
  )

  # Refuse to write into the repository tree to avoid accidentally committing PHI.
  # Check the provided `secrets_path` first so this guard works even when the
  # current working directory is outside the repository (e.g., R CMD check tempdirs).
  repo_root <- .find_repo_root(secrets_path)
  if (is.null(repo_root)) {
    repo_root <- .find_repo_root(".")
  }
  if (!is.null(repo_root)) {
    repo_root_norm <- normalizePath(repo_root, winslash = "/", mustWork = FALSE)
    if (startsWith(secrets_path, repo_root_norm)) {
      stop("Refusing to write mapping into repository path (", secrets_path, "). Pass an explicit secrets_path outside the repository to override.")
    }
  }

  lock_path <- paste0(secrets_path, ".lock")

  res <- with_file_lock(lock_path, {
    # Ensure file exists
    if (!file.exists(secrets_path)) {
      mapping_data <- list(mappings = list())
      json_str <- if (requireNamespace("jsonlite", quietly = TRUE)) {
        jsonlite::toJSON(mapping_data, pretty = TRUE, auto_unbox = TRUE)
      } else {
        '{"mappings": {}}'
      }
      atomic_write_file(json_str, secrets_path)
    }

    if (requireNamespace("jsonlite", quietly = TRUE)) {
      mapping_data <- jsonlite::fromJSON(secrets_path)
      mapping <- if (is.list(mapping_data$mappings)) mapping_data$mappings else list()

      # Track modifications
      modified <- FALSE
      unique_names <- unique(valid_names)

      for (nm in unique_names) {
        if (!nm %in% names(mapping)) {
          token <- generate_pseudonym_token(name = nm, prefix = prefix, key = key, n_chars = n_chars, rename_fn = rename_fn)
          # Collision prevention across existing mappings
          existing_tokens <- as.character(unlist(mapping, use.names = FALSE))
          coll_counter <- 1L
          while (token %in% existing_tokens) {
            coll_counter <- coll_counter + 1L
            token <- generate_pseudonym_token(
              name = paste0(nm, "_coll_", coll_counter),
              prefix = prefix,
              key = key,
              n_chars = n_chars,
              rename_fn = rename_fn
            )
          }
          mapping[[nm]] <- token
          modified <- TRUE
        }
      }

      if (modified) {
        mapping_data$mappings <- mapping
        json_str <- jsonlite::toJSON(mapping_data, pretty = TRUE, auto_unbox = TRUE)
        atomic_write_file(json_str, secrets_path)
      }

      vapply(valid_names, function(nm) as.character(mapping[[nm]]), character(1), USE.NAMES = FALSE)
    } else {
      lines <- readLines(secrets_path, warn = FALSE)
      vapply(valid_names, function(nm) {
        match_line <- grep(paste0('"', nm, '"\\s*:'), lines, value = TRUE)
        if (length(match_line) > 0) {
          sub('.*:\\s*"([^"]+)".*', '\\1', match_line[1])
        } else {
          generate_pseudonym_token(name = nm, prefix = prefix, key = key, n_chars = n_chars, rename_fn = rename_fn)
        }
      }, character(1), USE.NAMES = FALSE)
    }
  })
  out[valid_indices] <- res
  out[is_na] <- NA_character_
  return(out)
}
