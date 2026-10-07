#' Investigator Name Pseudonymization Helpers
#'
#' Helpers that generate synthetic surnames and pseudonym tokens, and replace
#' investigator (PI) names with them in reports and shared output. They accept
#' a single name or a character vector, such as a data frame column.
#'
#' @section Scope and limits:
#' These functions pseudonymize \strong{investigator names only}. They do not
#' find, remove, or alter dates, record or subject numbers, locations, free
#' text, or any other identifier, and they are not a general de-identification
#' tool. Replacing investigator names does not by itself make a dataset safe
#' to share.
#'
#' Pseudonyms are not guaranteed to be unique: two different names can map to
#' the same surname (\code{method = "hmac_surname"}), and short tokens (a small
#' \code{n_chars}) collide more often.
#'
#' @section Secret key:
#' The deterministic methods (\code{"hmac_token"} and \code{"hmac_surname"})
#' are only as private as their key. Anyone who knows the key, or who can
#' guess it, can recompute every pseudonym from a list of candidate names. The
#' functions therefore stop unless a key is supplied, either through the
#' \code{key} argument or, preferably, the \code{TEMPLECBE_SECRET_KEY}
#' environment variable. Keep the key out of scripts and repositories: put it
#' in your user \file{.Renviron} (open it with \code{usethis::edit_r_environ()})
#' and restart R:
#'
#' \preformatted{TEMPLECBE_SECRET_KEY=paste-a-long-random-string-here}
#'
#' A random key can be made with
#' \code{paste(as.character(openssl::rand_bytes(32)), collapse = "")}. Use the
#' same key every time to get the same pseudonyms; a lost key cannot be
#' recovered, and a changed key changes every pseudonym.
#'
#' For a quick, non-private run you can pass \code{allow_default_key = TRUE},
#' which falls back to a public built-in key and warns that the output is NOT
#' secret. Methods \code{"token"} (random tokens kept in a private mapping
#' file) and \code{"synthetic"} need no key.
#'
#' @seealso \code{\link{anonymize_pi}}, \code{\link{generate_pseudonym_token}},
#'   \code{\link{generate_pi_names}}, \code{\link{generate_last_names}}
#' @name pi_anonymizer
NULL

# The public constant that earlier versions used silently when no key was set.
# Tokens made with it can be recomputed by anyone, so it is only reachable
# through the explicit `allow_default_key = TRUE` opt-out. Do not change the
# value: it keeps output made with the opt-out identical to earlier releases.
.default_hmac_key <- "temple_cbe_default_salt"

.cbe_census_surnames <- c(
  "Smith", "Johnson", "Williams", "Brown", "Jones", "Garcia", "Miller", "Davis", "Rodriguez", "Martinez",
  "Hernandez", "Lopez", "Gonzalez", "Wilson", "Anderson", "Thomas", "Taylor", "Moore", "Jackson", "Martin",
  "Lee", "Perez", "Thompson", "White", "Harris", "Sanchez", "Clark", "Ramirez", "Lewis", "Robinson",
  "Walker", "Young", "Allen", "King", "Wright", "Scott", "Torres", "Nguyen", "Hill", "Flores",
  "Green", "Adams", "Nelson", "Baker", "Hall", "Rivera", "Campbell", "Mitchell", "Carter", "Roberts",
  "Gomez", "Phillips", "Evans", "Turner", "Diaz", "Parker", "Cruz", "Edwards", "Collins", "Reyes",
  "Stewart", "Morris", "Morales", "Murphy", "Cook", "Rogers", "Gutierrez", "Ortiz", "Morgan", "Cooper",
  "Peterson", "Bailey", "Reed", "Kelly", "Howard", "Ramos", "Kim", "Cox", "Ward", "Richardson",
  "Watson", "Brooks", "Chavez", "Wood", "James", "Bennett", "Gray", "Mendoza", "Ruiz", "Hughes",
  "Price", "Alvarez", "Castillo", "Sanders", "Patel", "Myers", "Long", "Ross", "Foster", "Jimenez",
  "Powell", "Jenkins", "Perry", "Russell", "Sullivan", "Bell", "Coleman", "Butler", "Henderson", "Barnes",
  "Gonzales", "Fisher", "Vasquez", "Simmons", "Romero", "Jordan", "Patterson", "Alexander", "Hamilton", "Graham",
  "Reynolds", "Griffin", "Wallace", "Moreno", "West", "Cole", "Hayes", "Bryant", "Herrera", "Gibson",
  "Ellis", "Tran", "Medina", "Aguilar", "Stevens", "Murray", "Ford", "Castro", "Marshall", "Owens",
  "Harrison", "Fernandez", "Mcdonald", "Woods", "Washington", "Kennedy", "Wells", "Vargas", "Henry", "Chen",
  "Freeman", "Webb", "Tucker", "Guzman", "Burns", "Crawford", "Olson", "Simpson", "Porter", "Hunter",
  "Gordon", "Mendez", "Silva", "Shaw", "Snyder", "Mason", "Dixon", "Munoz", "Hunt", "Hicks",
  "Holmes", "Palmer", "Wagner", "Black", "Robertson", "Boyd", "Rose", "Stone", "Salazar", "Fox",
  "Warren", "Mills", "Meyer", "Rice", "Schmidt", "Garza", "Daniels", "Ferguson", "Nichols", "Stephens",
  "Soto", "Weaver", "Ryan", "Gardner", "Payne", "Grant", "Dunn", "Kelley", "Spencer", "Hawkins",
  "Arnold", "Pierce", "Vazquez", "Hansen", "Peters", "Santos", "Hart", "Bradley", "Knight", "Elliott",
  "Cunningham", "Duncan", "Armstrong", "Hudson", "Carroll", "Lane", "Riley", "Andrews", "Alvarado", "Ray",
  "Delgado", "Berry", "Perkins", "Hoffman", "Johnston", "Matthews", "Pena", "Richards", "Contreras", "Willis",
  "Carpenter", "Lawrence", "Sandoval", "Guerrero", "George", "Chapman", "Rios", "Estrada", "Ortega", "Watkins",
  "Greene", "Nunez", "Wheeler", "Valdez", "Harper", "Burke", "Larson", "Santiago", "Maldonado", "Morrison",
  "Khan", "Zhao"
)

.cbe_first_names <- c(
  "James", "John", "Robert", "Michael", "William", "David", "Richard", "Joseph", "Thomas", "Charles",
  "Christopher", "Daniel", "Matthew", "Anthony", "Mark", "Donald", "Steven", "Paul", "Andrew", "Joshua",
  "Kenneth", "Kevin", "Brian", "George", "Edward", "Ronald", "Timothy", "Jason", "Jeffrey", "Ryan",
  "Jacob", "Gary", "Nicholas", "Eric", "Jonathan", "Stephen", "Larry", "Justin", "Scott", "Brandon",
  "Benjamin", "Samuel", "Gregory", "Alexander", "Frank", "Patrick", "Raymond", "Jack", "Dennis", "Jerry",
  "Tyler", "Aaron", "Jose", "Adam", "Henry", "Nathan", "Douglas", "Zachary", "Peter", "Kyle",
  "Walter", "Ethan", "Jeremy", "Harold", "Keith", "Christian", "Roger", "Noah", "Gerald", "Carl",
  "Terry", "Sean", "Austin", "Arthur", "Lawrence", "Jesse", "Dylan", "Bryan", "Joe", "Jordan",
  "Billy", "Bruce", "Albert", "Willie", "Gabriel", "Logan", "Alan", "Juan", "Wayne", "Roy",
  "Ralph", "Randy", "Eugene", "Vincent", "Russell", "Elijah", "Louis", "Bobby", "Philip", "Johnny",
  "Bradley", "Lucas", "Oliver", "Mason", "Liam", "Caleb", "Isaac", "Nathaniel", "Julian", "Adrian",
  "Leo", "Theodore", "Eli", "Miles", "Amir", "Tariq", "Carlos", "Mateo", "Diego", "Ravi",
  "Arjun", "Kenji", "Min", "Wei", "Marcus",
  "Mary", "Patricia", "Jennifer", "Linda", "Elizabeth", "Barbara", "Susan", "Jessica", "Sarah", "Karen",
  "Nancy", "Margaret", "Lisa", "Betty", "Dorothy", "Sandra", "Ashley", "Kimberly", "Donna", "Emily",
  "Michelle", "Carol", "Amanda", "Melissa", "Deborah", "Stephanie", "Rebecca", "Sharon", "Laura", "Cynthia",
  "Kathleen", "Amy", "Shirley", "Angela", "Helen", "Anna", "Brenda", "Pamela", "Nicole", "Emma",
  "Samantha", "Katherine", "Christine", "Debra", "Rachel", "Catherine", "Carolyn", "Janet", "Ruth", "Maria",
  "Heather", "Diane", "Virginia", "Julie", "Joyce", "Victoria", "Olivia", "Kelly", "Christina", "Lauren",
  "Joan", "Evelyn", "Judith", "Megan", "Cheryl", "Andrea", "Hannah", "Martha", "Jacqueline", "Frances",
  "Gloria", "Ann", "Teresa", "Kathryn", "Sara", "Janice", "Jean", "Alice", "Madison", "Doris",
  "Abigail", "Julia", "Judy", "Grace", "Denise", "Amber", "Marilyn", "Beverly", "Danielle", "Theresa",
  "Sophia", "Marie", "Diana", "Brittany", "Natalie", "Isabella", "Charlotte", "Chloe", "Harper", "Ella",
  "Avery", "Sofia", "Camila", "Aria", "Scarlett", "Riley", "Layla", "Zoey", "Nora", "Lily",
  "Eleanor", "Lillian", "Addison", "Aubrey", "Ellie", "Stella", "Zoe", "Leah", "Hazel", "Violet",
  "Aurora", "Savannah", "Audrey", "Brooklyn", "Bella", "Claire", "Skylar", "Priya", "Mei", "Fatima", "Elena"
)

# Decide which key the deterministic (HMAC) methods use. This is the ONE place
# that sets the policy for a missing key:
#   * current policy: stop, unless the caller passed allow_default_key = TRUE,
#     in which case warn that the output is not secret and use the public key;
#   * "warn only" alternative: turn the stop() below into warning() and drop
#     the allow_default_key argument;
#   * "always require a key" alternative: delete the allow_default_key branch.
# `hint` is extra advice appended to the error for callers that have a keyless
# alternative (anonymize_pi() has method = "token").
resolve_hmac_key <- function(key, allow_default_key = FALSE, hint = "") {
  if (!is.character(key) || length(key) != 1L || is.na(key)) {
    stop("`key` must be a single, non-missing string.", call. = FALSE)
  }
  if (!is.logical(allow_default_key) || length(allow_default_key) != 1L || is.na(allow_default_key)) {
    stop("`allow_default_key` must be TRUE or FALSE.", call. = FALSE)
  }
  if (nzchar(key)) {
    if (nchar(trimws(key)) < 16L && !isTRUE(allow_default_key) && !isTRUE(getOption("templecbe_suppress_weak_key_warning", FALSE))) {
      warning(
        "The secret key is shorter than 16 characters or whitespace-only, which provides weak pseudonym privacy. ",
        "Use a long random string (at least 16 characters, e.g. 32 bytes) for TEMPLECBE_SECRET_KEY.",
        call. = FALSE
      )
    }
    return(key)
  }
  if (!allow_default_key) {
    stop(
      "No secret key was supplied, so the pseudonyms would not be private: ",
      "anyone could recompute them from a list of names.\n",
      "Set the TEMPLECBE_SECRET_KEY environment variable, for example by adding the line ",
      "TEMPLECBE_SECRET_KEY=<a long random string> to your user ~/.Renviron and restarting R, ",
      "or pass `key =`. See ?pi_anonymizer.\n",
      "To knowingly use the public built-in key instead (the output is NOT secret), ",
      "set `allow_default_key = TRUE`.", hint,
      call. = FALSE
    )
  }
  warning(
    "Using the public built-in default key: these pseudonyms are NOT secret, ",
    "and anyone can recompute them from a list of names. ",
    "Set TEMPLECBE_SECRET_KEY (see ?pi_anonymizer) for pseudonyms that are private.",
    call. = FALSE
  )
  .default_hmac_key
}

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

# helper: canonical spelling of a path that may not exist yet.
# normalizePath() leaves a path that does not exist exactly as written, so a symlink,
# junction, 8.3 short name or macOS /var -> /private/var in the part that does exist
# stays unresolved. Resolve the deepest existing ancestor and put the missing tail
# back, so a walk up from the result reaches the real parent directories.
.canonical_path <- function(path) {
  as_given <- cur <- normalizePath(path, winslash = "/", mustWork = FALSE)
  tail <- character()
  while (!file.exists(cur)) {
    parent <- dirname(cur)
    # not even the root exists (an unmapped drive, an unreachable share): nothing to resolve
    if (identical(parent, cur)) return(as_given)
    tail <- c(basename(cur), tail)
    cur <- parent
  }
  cur <- normalizePath(cur, winslash = "/", mustWork = FALSE)
  if (length(tail)) file.path(sub("/$", "", cur), paste(tail, collapse = "/")) else cur
}

# helper: find repo root by locating .git
.find_repo_root <- function(path = ".") {
  p <- tryCatch(.canonical_path(path), error = function(e) NULL)
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
    withr::with_seed(
      seed,
      draw(),
      .rng_kind = "Mersenne-Twister",
      .rng_normal_kind = "Inversion",
      .rng_sample_kind = "Rejection"
    )
  } else {
    old_rng <- RNGkind()
    old_seed <- if (exists(".Random.seed", envir = .GlobalEnv)) get(".Random.seed", envir = .GlobalEnv) else NULL
    on.exit({
      suppressWarnings(RNGkind(kind = old_rng[1], normal.kind = old_rng[2], sample.kind = old_rng[3]))
      if (is.null(old_seed)) {
        if (exists(".Random.seed", envir = .GlobalEnv)) rm(".Random.seed", envir = .GlobalEnv)
      } else {
        assign(".Random.seed", old_seed, envir = .GlobalEnv)
      }
    }, add = TRUE)
    suppressWarnings(RNGkind(kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection"))
    set.seed(seed)
    draw()
  }
}

#' Generate Pseudonym Tokens (Cryptographic Hash)
#'
#' Generates cryptographic hash pseudonym tokens. When \code{name} is supplied,
#' computes deterministic keyed HMAC-SHA256 tokens, which require a secret key
#' (see \code{\link{pi_anonymizer}}). When \code{name} is \code{NULL}, generates
#' random tokens (or reproducible, \emph{non-secret} tokens when \code{seed} is
#' provided). Supports a custom prefix, a configurable length (e.g. for short
#' tokens), and custom renaming/formatting functions.
#'
#' Tokens replace investigator names only; nothing else in a dataset is
#' de-identified (see the scope notes in \code{\link{pi_anonymizer}}).
#'
#' @param name Optional character vector of names to tokenize. If provided, computes
#'   a deterministic HMAC-SHA256 token for each non-NA name; this needs a secret key.
#' @param n Integer, number of tokens to generate when \code{name} is \code{NULL} (default 1).
#' @param prefix Character string, prefix prepended to each token (default \code{"PI_"}).
#' @param key Character string, the secret key for HMAC tokenization, used only when
#'   \code{name} is supplied. Defaults to the environment variable
#'   \code{TEMPLECBE_SECRET_KEY}. An empty key counts as no key: the call stops unless
#'   \code{allow_default_key = TRUE}. Prefer the environment variable over writing the key
#'   into a script.
#' @param n_chars Integer, number of hex characters from the hash digest to include (default 16).
#'   Can be shortened (e.g. 4 or 8) to produce compact tokens, at the cost of more
#'   collisions between different names.
#' @param rename_fn Optional function to transform or rename tokens (e.g., \code{tolower},
#'   \code{toupper}, or a custom function such as
#'   \code{function(tok) paste0("INV-", substr(tok, 4, 8))}).
#' @param seed Optional integer, random seed for reproducible random token generation when
#'   \code{name} is \code{NULL}. Seeded tokens are derived from the seed alone, so anyone who
#'   knows it can regenerate them; do not use them where secrecy matters.
#' @param allow_default_key Logical, default \code{FALSE}. If \code{TRUE} and no key is
#'   available, use the package's public built-in key instead of stopping, and warn that
#'   the tokens are NOT secret. This only reproduces output made without a key by earlier
#'   versions; do not use it for data you intend to protect.
#' @param allow_collisions Logical, default \code{FALSE}. If \code{TRUE}, suppresses errors
#'   when \code{n_chars < 6} or when distinct input names map to identical pseudonym tokens.
#'
#' @return Character vector of pseudonym tokens.
#' @seealso \code{\link{pi_anonymizer}} for scope and key handling.
#' @export
#'
#' @examples
#' # Random tokens need no key
#' generate_pseudonym_token(n = 3)
#'
#' # Short tokens (8 hex chars)
#' generate_pseudonym_token(n = 3, n_chars = 8)
#'
#' # Custom renaming function
#' generate_pseudonym_token(n = 2, rename_fn = tolower)
#'
#' # Deterministic tokens need a secret key. In real use keep it in the
#' # TEMPLECBE_SECRET_KEY environment variable (see ?pi_anonymizer), never in a
#' # script you commit. Here a throwaway value is set for this example only.
#' if (requireNamespace("withr", quietly = TRUE)) {
#'   withr::with_envvar(c(TEMPLECBE_SECRET_KEY = "example-only-not-a-real-key"), {
#'     print(generate_pseudonym_token("Franklin"))
#'     print(generate_pseudonym_token(c("Smith", "Jones")))
#'   })
#' }
generate_pseudonym_token <- function(name = NULL,
                                     n = 1,
                                     prefix = "PI_",
                                     key = Sys.getenv("TEMPLECBE_SECRET_KEY", unset = ""),
                                     n_chars = 16,
                                     rename_fn = NULL,
                                     seed = NULL,
                                     allow_default_key = FALSE,
                                     allow_collisions = FALSE) {
  if (!is.null(rename_fn) && !is.function(rename_fn)) {
    stop("`rename_fn` must be a function or NULL.")
  }
  if (!is.null(n_chars)) {
    n_chars <- as.integer(n_chars)
    if (length(n_chars) != 1 || is.na(n_chars) || n_chars < 1) {
      stop("`n_chars` must be a positive integer or NULL for full hash.")
    }
    if (n_chars < 6L && !isTRUE(allow_collisions)) {
      stop(
        "`n_chars` must be at least 6 to prevent frequent pseudonym collisions (got ", n_chars, "). ",
        "Pass `allow_collisions = TRUE` to override.",
        call. = FALSE
      )
    }
  }

  slice_hash <- function(h) {
    if (is.null(n_chars)) h else substr(h, 1, min(nchar(h), n_chars))
  }

  if (!is.null(name)) {
    if (!is.character(name)) {
      stop("`name` must be a character vector.")
    }
    name <- enc2utf8(as.character(name))
    effective_key <- resolve_hmac_key(key, allow_default_key)

    valid_mask <- !is.na(name) & nzchar(trimws(name))
    u_names <- unique(name[valid_mask])
    if (length(u_names) > 1L) {
      n_eff <- if (is.null(n_chars)) 64L else min(64L, as.integer(n_chars))
      log_space <- n_eff * log(16)
      log_pairs <- log(length(u_names)) + log(length(u_names) - 1) - log(2)
      p_collision <- -expm1(-exp(log_pairs - log_space))
      if (p_collision > 0.01 && !isTRUE(allow_collisions)) {
        warning(
          "With ", length(u_names), " distinct names and n_chars = ", n_eff,
          ", the estimated probability of at least one pseudonym collision is ",
          round(p_collision * 100, 1), "%. ",
          "Increase `n_chars` or use `method = 'token'` for guaranteed unique pseudonyms. ",
          "Pass `allow_collisions = TRUE` to suppress this warning.",
          call. = FALSE
        )
      }
    }

    tokens <- vapply(name, function(nm) {
      if (is.na(nm) || !nzchar(trimws(nm))) return(NA_character_)
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

  if (!is.null(name) && length(u_names) > 1L) {
    u_tokens <- tokens[valid_mask]
    names_by_token <- split(name[valid_mask], u_tokens)
    collided <- names_by_token[lengths(lapply(names_by_token, unique)) > 1L]
    if (length(collided) > 0L) {
      n_collided_names <- sum(lengths(lapply(collided, unique)))
      msg <- paste0(
        "Pseudonym collision detected: ", length(u_names), " distinct names produced ",
        length(unique(u_tokens)), " pseudonyms (", length(collided), " shared token(s) across ",
        n_collided_names, " names). Increase `n_chars` or pass `allow_collisions = TRUE`."
      )
      if (!isTRUE(allow_collisions)) {
        stop(msg, call. = FALSE)
      } else {
        warning(msg, call. = FALSE)
      }
    }
  }

  if (is.null(name) && n == 1) tokens[1] else tokens
}

#' Generate Random PI Names or Tokens
#'
#' Generates random synthetic principal investigator (PI) names, full names, or
#' cryptographic pseudonym tokens (`PI_...`).
#'
#' @param n Integer, number of names to generate (default 1).
#' @param format Character, either `"synthetic"` (realistic surnames), `"full_name"`
#'   (synthetic first and last name), or `"token"` (cryptographic hash tokens).
#'   Aliases `"surname"` and `"full"` are also accepted.
#' @param prefix Character, prefix when format is `"token"` (default `"PI_"`).
#' @param n_chars Integer, number of hex characters from the hash digest when `format = "token"` (default 16).
#' @param rename_fn Optional function to rename or format generated tokens/names.
#' @param seed Optional integer, random seed for reproducibility.
#' @param exclude Character vector of surnames or names (case-insensitive) to omit.
#' @param allow_collisions Logical, default \code{FALSE}. If \code{TRUE}, allows short tokens with \code{n_chars < 6}.
#'
#' @return A character vector of length `n` (or a single character string if `n = 1`).
#' @export
#'
#' @examples
#' generate_pi_names(1)
#' generate_pi_names(3, format = "full_name")
#' generate_pi_names(3, format = "token")
#' generate_pi_names(3, format = "token", n_chars = 8)
generate_pi_names <- function(n = 1,
                              format = c("synthetic", "token", "full_name"),
                              prefix = "PI_",
                              n_chars = 16,
                              rename_fn = NULL,
                              seed = NULL,
                              exclude = character(),
                              allow_collisions = FALSE) {
  if (!is.numeric(n) || length(n) != 1L || is.na(n) || n < 1L || n != round(n)) {
    stop("`n` must be a positive integer.", call. = FALSE)
  }
  n <- as.integer(n)

  format_arg <- if (is.character(format) && length(format) >= 1L) tolower(trimws(format[1L])) else "synthetic"
  if (format_arg == "surname") format_arg <- "synthetic"
  if (format_arg == "full") format_arg <- "full_name"
  if (!format_arg %in% c("synthetic", "token", "full_name")) {
    stop("`format` must be 'synthetic', 'token', or 'full_name'.", call. = FALSE)
  }

  if (format_arg == "token") {
    return(generate_pseudonym_token(
      name = NULL,
      n = n,
      prefix = prefix,
      n_chars = n_chars,
      rename_fn = rename_fn,
      seed = seed,
      allow_collisions = allow_collisions
    ))
  }

  sample_names <- function() {
    if (format_arg == "full_name") {
      firsts <- .cbe_first_names[!tolower(.cbe_first_names) %in% tolower(exclude)]
      lasts <- .cbe_census_surnames[!tolower(.cbe_census_surnames) %in% tolower(exclude)]
      if (length(firsts) == 0L) firsts <- "Investigator"
      if (length(lasts) == 0L) lasts <- as.character(seq_len(n))
      total_combos <- length(firsts) * length(lasts)
      if (n <= total_combos) {
        idx <- sample.int(total_combos, size = n, replace = FALSE) - 1L
        fi <- (idx %/% length(lasts)) + 1L
        li <- (idx %% length(lasts)) + 1L
        res <- paste(firsts[fi], lasts[li])
      } else {
        res <- paste(sample(firsts, size = n, replace = TRUE), sample(lasts, size = n, replace = TRUE))
      }
    } else {
      pool <- .cbe_census_surnames[!tolower(.cbe_census_surnames) %in% tolower(exclude)]
      if (length(pool) == 0L) {
        pool <- paste0("Investigator_", seq_len(n))
      }
      if (n <= length(pool)) {
        res <- sample(pool, size = n, replace = FALSE)
      } else {
        # Fall back to procedural generator when n exceeds the pool size
        res <- generate_last_names(n = n, exclude = exclude)
      }
    }
    if (!is.null(rename_fn)) {
      res <- rename_fn(res)
    }
    if (n == 1L) res[1L] else res
  }

  if (is.null(seed)) {
    sample_names()
  } else if (requireNamespace("withr", quietly = TRUE)) {
    withr::with_seed(seed, sample_names())
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
    sample_names()
  }
}

#' Pseudonymize Investigator Names (Vectorized)
#'
#' Replaces investigator names with pseudonyms. Supports both single scalar
#' names and character vectors (e.g. data frame columns like \code{df$investigator}).
#' Offers stateless keyed-HMAC hashing as well as a stateful mapping table.
#'
#' \strong{Scope.} Only investigator names are replaced. Dates, record or subject
#' numbers, free text, and other identifiers are left untouched, so this is not a
#' general de-identification tool; see \code{\link{pi_anonymizer}} for the limits.
#'
#' \strong{Key required for the deterministic methods.} \code{"hmac_token"} and
#' \code{"hmac_surname"} stop with an error unless a secret key is available, from
#' the \code{key} argument or the \code{TEMPLECBE_SECRET_KEY} environment variable.
#' Without a secret key anyone can recompute the pseudonyms from a list of names.
#' Set the variable in your user \file{.Renviron} rather than writing the key into a
#' script that may be committed. Passing \code{allow_default_key = TRUE} restores the
#' earlier fallback to a public built-in key, with a warning that the output is NOT
#' secret. The check happens before the data is looked at, so an all-\code{NA} input
#' does not skip it.
#'
#' Methods:
#' \itemize{
#'   \item \code{"hmac_token"} (default): Stateless deterministic token (e.g.,
#'     \code{PI_5c1ebfd9a0b3c4d2}) computed via \code{openssl::sha256(name, key = key)}.
#'     Needs a key. No mapping file is written.
#'   \item \code{"hmac_surname"}: Stateless deterministic procedural surname generated via
#'     a keyed HMAC seed and \code{\link{generate_last_names}}. Needs a key. Consistent
#'     across runs without disk state; different names can share a surname.
#'   \item \code{"token"}: Stateful tokens persisted to a user-level JSON mapping file
#'     (\code{tools::R_user_dir("TempleCBE", "data")/pi_mapping.json}), so a name keeps its
#'     token between calls. Works without a key: new names then get random tokens, which
#'     exist only in the mapping file. If a key is set, tokens are the keyed HMAC of the
#'     name instead. The mapping file lists real names next to their tokens and is
#'     confidential. The function refuses to create or write it inside a Git repository
#'     tree, to prevent accidental commits.
#'   \item \code{"synthetic"}: Random procedural surnames via
#'     \code{\link{generate_last_names}}. Needs no key; every element gets a new random
#'     surname, so repeated names are not mapped consistently.
#' }
#'
#' @param name Character vector of investigator names or surnames to mask.
#' @param method Character string specifying the approach: \code{"hmac_token"},
#'   \code{"hmac_surname"}, \code{"token"}, or \code{"synthetic"}.
#' @param key Character string, the secret key for the HMAC methods (and for keyed tokens
#'   with \code{method = "token"}). Defaults to the environment variable
#'   \code{TEMPLECBE_SECRET_KEY}. An empty key counts as no key. Prefer the environment
#'   variable over writing the key into a script.
#' @param secrets_path Path to the confidential mapping file when \code{method = "token"}.
#'   Defaults to \code{tools::R_user_dir("TempleCBE", which = "data")/pi_mapping.json}.
#'   It must be outside any Git repository.
#' @param exclude Character vector of names that must never be generated (passed to
#'   \code{\link{generate_last_names}} when method is \code{"hmac_surname"} or
#'   \code{"synthetic"}).
#' @param prefix Character string, prefix for tokens (default \code{"PI_"}).
#' @param n_chars Integer, number of hex characters for tokens (default 16). Can be
#'   shortened for compact tokens; very short tokens (e.g. 4) collide, so different
#'   investigators may share one.
#' @param rename_fn Optional function to rename or format generated tokens/pseudonyms.
#' @param seed Optional integer, random seed when method is \code{"synthetic"}.
#' @param allow_default_key Logical, default \code{FALSE}. If \code{TRUE} and no key is
#'   available for \code{"hmac_token"} or \code{"hmac_surname"}, use the package's public
#'   built-in key instead of stopping, and warn that the output is NOT secret. This only
#'   reproduces output made without a key by earlier versions; do not use it for data you
#'   intend to protect.
#' @param allow_collisions Logical, default \code{FALSE}. If \code{TRUE}, suppresses collision
#'   errors/warnings when distinct names produce identical tokens or surnames.
#' @param normalize Logical, default \code{FALSE}. If \code{TRUE}, trims leading/trailing
#'   whitespace and converts names to lowercase before hashing.
#'
#' @return Character vector of pseudonyms matching the length of \code{name}; \code{NA}
#'   inputs stay \code{NA}.
#' @seealso \code{\link{pi_anonymizer}} for scope, limits and key setup.
#' @export
#'
#' @examples
#' # The key comes from the TEMPLECBE_SECRET_KEY environment variable. In real use
#' # keep it in your user ~/.Renviron (see ?pi_anonymizer), never in a script you
#' # commit. Here a throwaway value is set for this example only.
#' if (requireNamespace("withr", quietly = TRUE)) {
#'   withr::with_envvar(c(TEMPLECBE_SECRET_KEY = "example-only-not-a-real-key"), {
#'     # Vectorized deterministic tokens (default method = "hmac_token")
#'     print(anonymize_pi(c("Franklin", "Taylor", "Patel")))
#'
#'     # Short tokens (8 characters)
#'     print(anonymize_pi(c("Franklin", "Taylor"), n_chars = 8))
#'
#'     # Custom renaming function
#'     print(anonymize_pi(c("Franklin", "Taylor"), rename_fn = tolower))
#'
#'     # Vectorized realistic pseudonyms
#'     print(anonymize_pi(c("Franklin", "Taylor"), method = "hmac_surname"))
#'   })
#' }
#'
#' # Random tokens kept in a mapping file need no key. Here the file goes in a
#' # temporary directory; in real use leave secrets_path at its default.
#' mapping <- file.path(tempdir(), "pi_mapping_example.json")
#' anonymize_pi(c("Franklin", "Taylor", "Franklin"), method = "token", secrets_path = mapping)
#' unlink(mapping)
anonymize_pi <- function(name,
                         method = c("hmac_token", "hmac_surname", "token", "synthetic"),
                         key = Sys.getenv("TEMPLECBE_SECRET_KEY", unset = ""),
                         secrets_path = NULL,
                         exclude = character(),
                         prefix = "PI_",
                         n_chars = 16,
                         rename_fn = NULL,
                         seed = NULL,
                         allow_default_key = FALSE,
                         allow_collisions = FALSE,
                         normalize = FALSE) {
  if (missing(name) || !is.character(name) || length(name) == 0) {
    stop("`name` must be a character vector with at least one element.")
  }
  method <- match.arg(method)

  # Resolve the key before looking at the data, so a missing key fails the same
  # way whatever values (or NAs) the input happens to hold.
  if (method %in% c("hmac_token", "hmac_surname")) {
    key <- resolve_hmac_key(
      key, allow_default_key,
      hint = "\nOr use `method = \"token\"` for random tokens kept in a private mapping file."
    )
  } else if (method == "token") {
    if (!is.character(key) || length(key) != 1L || is.na(key)) {
      stop("`key` must be a single, non-missing string.", call. = FALSE)
    }
    if (!requireNamespace("jsonlite", quietly = TRUE)) {
      stop(
        "`method = \"token\"` requires the 'jsonlite' package to read and write the mapping file.",
        call. = FALSE
      )
    }
  }

  name <- enc2utf8(as.character(name))
  if (isTRUE(normalize)) {
    name <- trimws(tolower(name))
  }
  is_empty <- !is.na(name) & !nzchar(trimws(name))
  is_na <- is.na(name) | is_empty
  out <- character(length(name))
  if (all(is_na)) {
    return(rep(NA_character_, length(name)))
  }
  valid_indices <- which(!is_na)
  valid_names <- name[valid_indices]

  if (method == "hmac_token") {
    res <- withr::with_options(
      list(templecbe_suppress_weak_key_warning = TRUE),
      generate_pseudonym_token(
        name = valid_names,
        prefix = prefix,
        key = key,
        n_chars = n_chars,
        rename_fn = rename_fn,
        allow_default_key = allow_default_key,
        allow_collisions = allow_collisions
      )
    )
    out[valid_indices] <- res
    out[is_na] <- NA_character_
    return(out)
  }

  if (method == "hmac_surname") {
    u_names <- unique(valid_names)
    u_res <- vapply(u_names, function(nm) {
      h <- openssl::sha256(nm, key = key)
      s <- strtoi(substr(as.character(h), 1, 7), 16L)
      generate_last_names(n = 1, seed = s, exclude = c(nm, exclude))
    }, character(1), USE.NAMES = FALSE)
    if (!is.null(rename_fn)) {
      u_res <- rename_fn(u_res)
    }
    if (length(u_names) > 1L) {
      names_by_sn <- split(u_names, u_res)
      collided_sn <- names_by_sn[lengths(lapply(names_by_sn, unique)) > 1L]
      if (length(collided_sn) > 0L) {
        msg <- paste0(
          "Surname collision in `method = 'hmac_surname'`: ", length(u_names),
          " distinct names mapped to ", length(unique(u_res)), " surnames (",
          length(collided_sn), " shared surname(s)). ",
          "Use `method = 'token'` for guaranteed uniqueness or pass `allow_collisions = TRUE`."
        )
        if (!isTRUE(allow_collisions)) {
          warning(msg, call. = FALSE)
        }
      }
    }
    name_map <- stats::setNames(u_res, u_names)
    res <- unname(name_map[valid_names])
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
  # Canonicalize through the parent directory: normalizePath() cannot canonicalize a
  # file that doesn't exist yet, which left Windows 8.3 short names, macOS
  # /var -> /private/var and symlinks unresolved and let the repository check below miss.
  secrets_path <- tryCatch(
    file.path(.canonical_path(dirname(secrets_path)), basename(secrets_path)),
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
    if (!endsWith(repo_root_norm, "/")) repo_root_norm <- paste0(repo_root_norm, "/")
    secrets_path_slash <- if (!endsWith(secrets_path, "/")) paste0(secrets_path, "/") else secrets_path
    if (startsWith(secrets_path_slash, repo_root_norm)) {
      stop("Refusing to write mapping into repository path (", secrets_path, "). Pass an explicit secrets_path outside the repository to override.", call. = FALSE)
    }
  }

  lock_path <- paste0(secrets_path, ".lock")

  # Token for a name that is not in the mapping yet. With a key it is the keyed
  # HMAC of the name (`attempt` > 1 re-derives it after a collision). With no
  # key it is random, and the mapping file is the only record of which name got
  # which token; it used to be derived from the public default key, which
  # anyone could recompute from a list of names.
  new_token <- function(nm, attempt) {
    if (nzchar(key)) {
      withr::with_options(
        list(templecbe_suppress_weak_key_warning = TRUE),
        generate_pseudonym_token(
          name = if (attempt == 1L) nm else paste0(nm, "_coll_", attempt),
          prefix = prefix,
          key = key,
          n_chars = n_chars,
          rename_fn = rename_fn,
          allow_default_key = allow_default_key,
          allow_collisions = TRUE
        )
      )
    } else {
      generate_pseudonym_token(n = 1, prefix = prefix, n_chars = n_chars, rename_fn = rename_fn)
    }
  }

  res <- with_file_lock(lock_path, {
    # Ensure file exists
    if (!file.exists(secrets_path)) {
      mapping_data <- list(mappings = list())
      json_str <- jsonlite::toJSON(mapping_data, pretty = TRUE, auto_unbox = TRUE)
      atomic_write_file(json_str, secrets_path)
    }

    mapping_data <- jsonlite::fromJSON(secrets_path)
    mapping <- if (is.list(mapping_data$mappings)) mapping_data$mappings else list()

    # Track modifications
    modified <- FALSE
    unique_names <- unique(valid_names)

    for (nm in unique_names) {
      if (!nm %in% names(mapping)) {
        token <- new_token(nm, 1L)
        # Collision prevention across existing mappings
        existing_tokens <- as.character(unlist(mapping, use.names = FALSE))
        coll_counter <- 1L
        max_attempts <- 100L
        while (token %in% existing_tokens) {
          coll_counter <- coll_counter + 1L
          if (coll_counter > max_attempts) {
            stop(
              "Could not generate a unique token for '", nm, "' after ", max_attempts,
              " attempts. Token space exhausted for n_chars = ", n_chars, "; increase `n_chars`.",
              call. = FALSE
            )
          }
          token <- new_token(nm, coll_counter)
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
  })
  out[valid_indices] <- res
  out[is_na] <- NA_character_
  return(out)
}
