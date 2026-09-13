#' PI Name Generator and Anonymizer
#'
#' Provides functions in R to generate random synthetic PI names or anonymized
#' tokens, and to map real PI names to confidential tokens stored in secrets/pi_mapping.json.

#' Generate Random PI Names or Tokens
#'
#' @param n Integer, number of names to generate (default 1).
#' @param format Character, either "synthetic" (realistic surnames) or "token" (PI_i, PI_j...).
#' @param prefix Character, prefix when format is "token" (default "PI_").
#' @param seed Optional integer, random seed for reproducibility.
#' @return Character vector of length n (or scalar if n = 1).
#' @export
generate_pi_names <- function(n = 1,
                              format = c("synthetic", "token"),
                              prefix = "PI_",
                              seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  format <- match.arg(format)
  
  synthetic_names <- c(
    "Khan", "Armstrong", "Zhao", "Smith", "Patel", "Garcia", "Kim",
    "Chen", "Williams", "Johnson", "Brown", "Miller", "Davis",
    "Wilson", "Martinez", "Anderson", "Taylor", "Thomas", "Hernandez",
    "Moore", "Martin", "Jackson", "Thompson", "White", "Lopez",
    "Lee", "Gonzalez", "Harris", "Clark", "Lewis", "Robinson", "Walker"
  )
  
  if (format == "token") {
    # Generate cryptographic hash tokens: PI_...
    tokens <- vapply(seq_len(n), function(i) {
      h <- as.character(openssl::sha256(openssl::rand_bytes(16)))
      paste0(prefix, substr(h, 1, 8))
    }, character(1))
    return(if (n == 1) tokens[1] else tokens)
  }
  
  sampled <- sample(synthetic_names, size = n, replace = TRUE)
  if (n == 1) sampled[1] else sampled
}

#' Default secrets path (user data directory, not the repo)
default_secrets_path <- function() {
  if (requireNamespace("rappdirs", quietly = TRUE)) {
    dir <- rappdirs::user_data_dir("TempleCBE")
  } else {
    dir <- file.path(Sys.getenv("HOME", unset = tempdir()), ".TempleCBE")
  }
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  file.path(dir, "pi_mapping.json")
}

# helper: find repo root by locating .git
.find_repo_root <- function(path = ".") {
  p <- tryCatch(normalizePath(path, winslash = "/", mustWork = FALSE), error = function(e) NULL)
  if (is.null(p)) return(NULL)
  repeat {
    if (dir.exists(file.path(p, ".git"))) return(p)
    parent <- dirname(p)
    if (identical(parent, p)) return(NULL)
    p <- parent
  }
}

#' Retrieve or Map Anonymized Token for a PI Name
#'
#' @param name Real PI name to mask.
#' @param secrets_path Path to confidential mapping file (default "secrets/pi_mapping.json").
#' @return Anonymized string token.
#' @export
anonymize_pi <- function(name, secrets_path = NULL) {
  # If no path provided, use a user-scoped defaults directory (not the repo)
  if (is.null(secrets_path)) secrets_path <- default_secrets_path()
  secrets_path <- tryCatch(normalizePath(secrets_path, winslash = "/", mustWork = FALSE), error = function(e) secrets_path)

  # Refuse to write into the repository tree to avoid accidentally committing PHI
  repo_root <- .find_repo_root(".")
  if (!is.null(repo_root)) {
    repo_root_norm <- normalizePath(repo_root, winslash = "/", mustWork = FALSE)
    if (startsWith(secrets_path, repo_root_norm)) {
      stop("Refusing to write mapping into repository path (", secrets_path, "). Pass an explicit secrets_path outside the repository to override.")
    }
  }

  # Ensure file exists (create an empty mapping if needed)
  if (!file.exists(secrets_path)) {
    mapping_data <- list(mappings = list())
    if (requireNamespace("jsonlite", quietly = TRUE)) {
      jsonlite::write_json(mapping_data, secrets_path, pretty = TRUE, auto_unbox = TRUE)
    } else {
      writeLines('{"mappings": {}}', con = secrets_path)
    }
  }
  
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    mapping_data <- jsonlite::fromJSON(secrets_path)
    mapping <- mapping_data$mappings
    
    if (name %in% names(mapping)) {
      return(mapping[[name]])
    }
    
    # Auto-assign cryptographic hash token
    h <- as.character(openssl::sha256(name))
    token <- paste0("PI_", substr(h, 1, 8))
    mapping[[name]] <- token
    mapping_data$mappings <- mapping
    jsonlite::write_json(mapping_data, secrets_path, pretty = TRUE, auto_unbox = TRUE)
    return(token)
  } else {
    lines <- readLines(secrets_path, warn = FALSE)
    match_line <- grep(paste0('"', name, '"\\s*:'), lines, value = TRUE)
    if (length(match_line) > 0) {
      val <- sub('.*:\\s*"([^"]+)".*', '\\1', match_line[1])
      return(val)
    }
    h <- as.character(openssl::sha256(name))
    return(paste0("PI_", substr(h, 1, 8)))
  }
}
