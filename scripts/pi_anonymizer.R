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
    # Generate cryptographic hash tokens: PI_... with 256 bits of CSPRNG entropy
    tokens <- vapply(seq_len(n), function(i) {
      h <- as.character(openssl::sha256(openssl::rand_bytes(32)))
      paste0(prefix, substr(h, 1, 16))
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
  if (exists("validate_secrets_dir", mode = "function")) {
    validate_secrets_dir(dir)
  } else {
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
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
  if (is.null(secrets_path)) {
    secrets_path <- default_secrets_path()
  } else {
    parent_dir <- dirname(secrets_path)
    if (exists("validate_secrets_dir", mode = "function")) {
      validate_secrets_dir(parent_dir)
    }
  }
  secrets_path <- tryCatch(normalizePath(secrets_path, winslash = "/", mustWork = FALSE), error = function(e) secrets_path)

  # Refuse to write into the repository tree to avoid accidentally committing PHI
  repo_root <- .find_repo_root(".")
  if (!is.null(repo_root)) {
    repo_root_norm <- normalizePath(repo_root, winslash = "/", mustWork = FALSE)
    if (startsWith(secrets_path, repo_root_norm)) {
      stop("Refusing to write mapping into repository path (", secrets_path, "). Pass an explicit secrets_path outside the repository to override.")
    }
  }

  write_atomic <- function(content, target) {
    if (exists("atomic_write_file", mode = "function")) {
      atomic_write_file(content, target)
    } else {
      tmp <- tempfile(pattern = ".tmp_", tmpdir = dirname(target))
      writeLines(content, con = tmp)
      file.rename(tmp, target)
    }
  }

  lock_dir <- paste0(secrets_path, ".lock")
  acquire_lock <- function(lpath, timeout = 10) {
    t0 <- Sys.time()
    while (!dir.create(lpath, showWarnings = FALSE)) {
      if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > timeout) {
        stop("Failed to acquire advisory lock on ", lpath)
      }
      Sys.sleep(0.05)
    }
  }

  acquire_lock(lock_dir)
  on.exit(if (dir.exists(lock_dir)) unlink(lock_dir, recursive = TRUE, force = TRUE), add = TRUE)

  # Ensure file exists (create an empty mapping if needed)
  if (!file.exists(secrets_path)) {
    mapping_data <- list(mappings = list())
    if (requireNamespace("jsonlite", quietly = TRUE)) {
      write_atomic(jsonlite::toJSON(mapping_data, pretty = TRUE, auto_unbox = TRUE), secrets_path)
    } else {
      write_atomic('{"mappings": {}}', secrets_path)
    }
  }
  
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    mapping_data <- jsonlite::fromJSON(secrets_path)
    mapping <- if (is.list(mapping_data$mappings)) mapping_data$mappings else list()
    
    if (name %in% names(mapping)) {
      return(mapping[[name]])
    }
    
    # Auto-assign cryptographic hash token with 16 hex chars
    h <- as.character(openssl::sha256(openssl::rand_bytes(32)))
    token <- paste0("PI_", substr(h, 1, 16))
    existing <- as.character(unlist(mapping, use.names = FALSE))
    counter <- 1L
    while (token %in% existing) {
      counter <- counter + 1L
      token <- paste0("PI_", substr(as.character(openssl::sha256(paste0(name, "_", counter))), 1, 16))
    }
    mapping[[name]] <- token
    mapping_data$mappings <- mapping
    write_atomic(jsonlite::toJSON(mapping_data, pretty = TRUE, auto_unbox = TRUE), secrets_path)
    return(token)
  } else {
    lines <- readLines(secrets_path, warn = FALSE)
    match_line <- grep(paste0('"', name, '"\\s*:'), lines, value = TRUE)
    if (length(match_line) > 0) {
      val <- sub('.*:\\s*"([^"]+)".*', '\\1', match_line[1])
      return(val)
    }
    h <- as.character(openssl::sha256(openssl::rand_bytes(32)))
    return(paste0("PI_", substr(h, 1, 16)))
  }
}
