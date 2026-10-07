#' PI Name Pseudonymizer
#'
#' Provides functions in R to generate random synthetic PI names or pseudonym
#' tokens, and to map real PI names to confidential tokens stored in a
#' user-scoped pi_mapping.json outside any git repository.
#'
#' Scope: only investigator (PI) names are pseudonymized. Dates, record or
#' subject numbers, free text and other identifiers are not touched, so this is
#' not a general de-identification tool.
#'
#' Keys: this standalone copy has no secret key and no default salt. Tokens are
#' random (openssl::rand_bytes) and exist only in the mapping file, so there is
#' nothing to recompute from a list of names; the mapping file itself is the
#' confidential item. (The TempleCBE package's keyed "hmac_token" and
#' "hmac_surname" methods require TEMPLECBE_SECRET_KEY and stop without it.)

#' Generate Random PI Names or Tokens
#'
#' @param n Integer, number of names to generate (default 1).
#' @param format Character, "synthetic" (realistic surnames), "full_name" (first + last name), or "token" (PI_i, PI_j...).
#' @param prefix Character, prefix when format is "token" (default "PI_").
#' @param seed Optional integer, random seed for reproducibility.
#' @param exclude Character vector of surnames or names (case-insensitive) to omit.
#' @return Character vector of length n (or scalar if n = 1).
#' @export
generate_pi_names <- function(n = 1,
                              format = c("synthetic", "token", "full_name"),
                              prefix = "PI_",
                              seed = NULL,
                              exclude = character()) {
  format_arg <- if (is.character(format)) tolower(trimws(format[1])) else "synthetic"
  if (format_arg == "surname") format_arg <- "synthetic"
  if (format_arg == "full") format_arg <- "full_name"
  if (!format_arg %in% c("synthetic", "token", "full_name")) {
    stop("`format` must be 'synthetic', 'token', or 'full_name'.")
  }

  if (format_arg == "token") {
    if (!is.null(seed)) {
      tokens <- vapply(seq_len(n), function(i) {
        h <- as.character(openssl::sha256(paste0("templecbe_seed_", seed, "_", i)))
        paste0(prefix, substr(h, 1, 16))
      }, character(1))
    } else {
      tokens <- vapply(seq_len(n), function(i) {
        h <- as.character(openssl::sha256(openssl::rand_bytes(32)))
        paste0(prefix, substr(h, 1, 16))
      }, character(1))
    }
    return(if (n == 1) tokens[1] else tokens)
  }

  if (!is.null(seed)) {
    if (requireNamespace("withr", quietly = TRUE)) {
      return(withr::with_seed(
        seed,
        generate_pi_names(n = n, format = format_arg, prefix = prefix, seed = NULL, exclude = exclude)
      ))
    }
    # Local RNG preservation when withr is absent
    has_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    if (has_seed) {
      old_seed <- get(".Random.seed", envir = .GlobalEnv)
      on.exit(assign(".Random.seed", old_seed, envir = .GlobalEnv), add = TRUE)
    } else {
      on.exit(rm(".Random.seed", envir = .GlobalEnv), add = TRUE)
    }
    set.seed(seed)
  }

  synthetic_names <- c(
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

  first_names <- c(
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

  if (format_arg == "full_name") {
    firsts <- first_names[!tolower(first_names) %in% tolower(exclude)]
    lasts <- synthetic_names[!tolower(synthetic_names) %in% tolower(exclude)]
    if (length(firsts) == 0L) firsts <- "Investigator"
    if (length(lasts) == 0L) lasts <- as.character(seq_len(n))
    total_combos <- length(firsts) * length(lasts)
    if (n <= total_combos) {
      idx <- sample.int(total_combos, size = n, replace = FALSE) - 1L
      fi <- (idx %/% length(lasts)) + 1L
      li <- (idx %% length(lasts)) + 1L
      sampled <- paste(firsts[fi], lasts[li])
    } else {
      sampled <- paste(sample(firsts, size = n, replace = TRUE), sample(lasts, size = n, replace = TRUE))
    }
  } else {
    pool <- synthetic_names[!tolower(synthetic_names) %in% tolower(exclude)]
    if (length(pool) == 0L) {
      pool <- paste0("Investigator_", seq_len(n))
    }
    sampled <- sample(pool, size = n, replace = (n > length(pool)))
  }
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

# helper: canonical spelling of a path that may not exist yet.
.canonical_path <- function(path) {
  as_given <- cur <- normalizePath(path, winslash = "/", mustWork = FALSE)
  tail <- character()
  while (!file.exists(cur)) {
    parent <- dirname(cur)
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
    if (dir.exists(git_marker) || file.exists(git_marker)) return(p)
    parent <- dirname(p)
    if (identical(parent, p)) return(NULL)
    p <- parent
  }
}

.is_pid_alive <- function(pid) {
  if (is.na(pid) || !is.numeric(pid) || pid <= 0L) return(FALSE)
  pid <- as.integer(pid)
  res <- tryCatch(tools::psnice(pid), error = function(e) NA)
  if (!is.na(res)) return(TRUE)
  if (.Platform$OS.type != "windows") {
    return(tryCatch(isTRUE(tools::pskill(pid, 0L)), error = function(e) FALSE))
  }
  FALSE
}

#' Retrieve or Map Anonymized Token for a PI Name
#'
#' @param name Real PI name or character vector to mask.
#' @param secrets_path Path to confidential mapping file.
#' @return Anonymized string token vector.
#' @export
anonymize_pi <- function(name, secrets_path = NULL) {
  if (missing(name) || length(name) == 0L) {
    return(character(0))
  }

  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("`anonymize_pi()` requires the 'jsonlite' package to read and write the confidential mapping file.", call. = FALSE)
  }

  name <- enc2utf8(as.character(name))
  is_na <- is.na(name) | !nzchar(trimws(name))
  out <- character(length(name))
  out[is_na] <- NA_character_
  if (all(is_na)) {
    return(out)
  }
  valid_names <- name[!is_na]

  # If no path provided, use a user-scoped defaults directory (not the repo)
  if (is.null(secrets_path)) {
    secrets_path <- default_secrets_path()
  } else {
    parent_dir <- dirname(secrets_path)
    if (exists("validate_secrets_dir", mode = "function")) {
      validate_secrets_dir(parent_dir)
    }
  }
  secrets_path <- tryCatch(.canonical_path(secrets_path), error = function(e) secrets_path)

  # Refuse to write into the repository tree
  inside_repo <- !is.null(.find_repo_root(secrets_path))
  if (!inside_repo) {
    repo_root <- .find_repo_root(".")
    inside_repo <- !is.null(repo_root) &&
      startsWith(secrets_path, normalizePath(repo_root, winslash = "/", mustWork = FALSE))
  }
  if (inside_repo) {
    stop("Refusing to write mapping into repository path (", secrets_path, "). Pass an explicit secrets_path outside the repository to override.", call. = FALSE)
  }

  # Ensure parent directory exists before acquiring lock
  parent_dir <- dirname(secrets_path)
  if (!dir.exists(parent_dir)) {
    dir.create(parent_dir, recursive = TRUE, showWarnings = FALSE)
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
  acquire_lock <- function(lpath, timeout = 10, stale_age = 30) {
    t0 <- Sys.time()
    while (!dir.create(lpath, recursive = FALSE, showWarnings = FALSE)) {
      info <- file.info(lpath)
      if (!is.na(info$mtime)) {
        age <- as.numeric(difftime(Sys.time(), info$mtime, units = "secs"))
        if (age > stale_age) {
          owner_file <- file.path(lpath, "lock_owner")
          owner_pid <- NA_integer_
          if (file.exists(owner_file)) {
            lines <- tryCatch(readLines(owner_file, warn = FALSE), error = function(e) character())
            pid_line <- grep("^pid:\\s*(\\d+)", lines, value = TRUE)
            if (length(pid_line)) {
              owner_pid <- as.integer(sub("^pid:\\s*(\\d+).*", "\\1", pid_line[1]))
            }
          }
          if (!.is_pid_alive(owner_pid)) {
            stale_tok <- paste0(lpath, ".stale.", Sys.getpid(), ".", as.integer(Sys.time()))
            if (suppressWarnings(file.rename(lpath, stale_tok))) {
              warning("Removing stale advisory lock at '", lpath, "' (age: ", round(age, 1), "s).", call. = FALSE)
              unlink(stale_tok, recursive = TRUE, force = TRUE)
            }
          }
        }
      }
      if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > timeout) {
        stop("Failed to acquire advisory lock on ", lpath, call. = FALSE)
      }
      Sys.sleep(0.05)
    }
    # Record lock owner
    tryCatch(
      writeLines(paste0("pid: ", Sys.getpid(), "\ntime: ", Sys.time()), file.path(lpath, "lock_owner")),
      error = function(e) NULL
    )
  }

  acquire_lock(lock_dir)
  on.exit(if (dir.exists(lock_dir)) unlink(lock_dir, recursive = TRUE, force = TRUE), add = TRUE)

  # Ensure file exists
  if (!file.exists(secrets_path)) {
    mapping_data <- list(mappings = list())
    write_atomic(jsonlite::toJSON(mapping_data, pretty = TRUE, auto_unbox = TRUE), secrets_path)
  }
  
  mapping_data <- jsonlite::fromJSON(secrets_path)
  mapping <- if (is.list(mapping_data$mappings)) mapping_data$mappings else list()
  modified <- FALSE
  u_valid <- unique(valid_names)
  tokens_map <- list()

  for (nm in u_valid) {
    if (nm %in% names(mapping)) {
      tokens_map[[nm]] <- mapping[[nm]]
    } else {
      h <- as.character(openssl::sha256(openssl::rand_bytes(32)))
      token <- paste0("PI_", substr(h, 1, 16))
      existing <- as.character(unlist(mapping, use.names = FALSE))
      counter <- 1L
      max_attempts <- 100L
      while (token %in% existing) {
        counter <- counter + 1L
        if (counter > max_attempts) {
          stop("Could not generate unique token within attempt cap.", call. = FALSE)
        }
        token <- paste0("PI_", substr(as.character(openssl::sha256(paste0(nm, "_", counter))), 1, 16))
      }
      mapping[[nm]] <- token
      tokens_map[[nm]] <- token
      modified <- TRUE
    }
  }

  if (modified) {
    mapping_data$mappings <- mapping
    write_atomic(jsonlite::toJSON(mapping_data, pretty = TRUE, auto_unbox = TRUE), secrets_path)
  }

  out[!is_na] <- as.character(tokens_map[valid_names])
  if (length(name) == 1L) out[1] else out
}
