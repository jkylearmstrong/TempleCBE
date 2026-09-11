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
    # Generate token sequence: PI_i, PI_j, PI_k...
    letters_seq <- letters[9:26] # 'i' through 'z'
    if (n <= length(letters_seq)) {
      tokens <- paste0(prefix, letters_seq[seq_len(n)])
    } else {
      tokens <- paste0(prefix, seq_len(n))
    }
    return(if (n == 1) tokens[1] else tokens)
  }
  
  sampled <- sample(synthetic_names, size = n, replace = TRUE)
  if (n == 1) sampled[1] else sampled
}

#' Retrieve or Map Anonymized Token for a PI Name
#'
#' @param name Real PI name to mask.
#' @param secrets_path Path to confidential mapping file (default "secrets/pi_mapping.json").
#' @return Anonymized string token.
#' @export
anonymize_pi <- function(name, secrets_path = "secrets/pi_mapping.json") {
  if (!file.exists(secrets_path)) {
    warning("Mapping file not found at: ", secrets_path, ". Returning default prefix.")
    return(paste0("PI_", name))
  }
  
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    mapping_data <- jsonlite::fromJSON(secrets_path)
    mapping <- mapping_data$mappings
    
    if (name %in% names(mapping)) {
      return(mapping[[name]])
    }
    
    # Auto-assign next token
    idx <- length(mapping) + 1
    letters_seq <- letters[9:26]
    token <- if (idx <= length(letters_seq)) paste0("PI_", letters_seq[idx]) else paste0("PI_", idx)
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
    return(paste0("PI_", name))
  }
}
