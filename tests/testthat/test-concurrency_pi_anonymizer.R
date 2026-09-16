test_that("with_file_lock provides mutual exclusion and cleans up on exit", {
  tmp_lock <- tempfile("test_lock_")
  on.exit(if (dir.exists(tmp_lock)) unlink(tmp_lock, recursive = TRUE), add = TRUE)

  # Lock is held during expression and removed on completion
  inside_dir_exists <- FALSE
  res <- with_file_lock(tmp_lock, {
    inside_dir_exists <- dir.exists(tmp_lock)
    42
  })
  expect_equal(res, 42)
  expect_true(inside_dir_exists)
  expect_false(dir.exists(tmp_lock)) # Cleans up on exit

  # Second lock attempt fails with timeout when lock is active
  dir.create(tmp_lock, recursive = TRUE)
  expect_error(
    with_file_lock(tmp_lock, { 1 }, timeout = 0.2, retry_interval = 0.05),
    "Failed to acquire advisory file lock"
  )
  unlink(tmp_lock, recursive = TRUE)
})

test_that("atomic_write_file updates files atomically", {
  tmp_dir <- tempfile("test_atomic_dir_")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  target <- file.path(tmp_dir, "test.txt")
  atomic_write_file("line1\nline2", target)
  expect_true(file.exists(target))
  expect_equal(readLines(target), c("line1", "line2"))

  # Atomic overwrite
  atomic_write_file("line3", target)
  expect_equal(readLines(target), "line3")
})

test_that("parallel calls to anonymize_pi maintain data integrity with locking", {
  tmp_dir <- tempfile("concurrent_secrets_")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  secrets_file <- file.path(tmp_dir, "pi_mapping.json")

  rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")

  # Workers load TempleCBE the way this session did: from source under
  # pkgload/devtools, otherwise from the library it was loaded from (R CMD check,
  # covr). load_all('.') only worked when tests ran inside the source checkout.
  pkg_path <- normalizePath(getNamespaceInfo("TempleCBE", "path"), winslash = "/")
  load_line <- if (requireNamespace("pkgload", quietly = TRUE) && pkgload::is_dev_package("TempleCBE")) {
    sprintf("pkgload::load_all(%s, quiet = TRUE)", deparse(pkg_path))
  } else {
    sprintf("library(TempleCBE, lib.loc = %s)", deparse(dirname(pkg_path)))
  }
  withr::local_envvar(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep))

  # Worker script that loads the package and anonymizes a batch of names
  worker_script <- file.path(tmp_dir, "worker.R")
  # Each worker appends timestamped progress notes to its own status file,
  # closing it after every note so it stays readable while the worker runs
  writeLines(c(
    "args <- commandArgs(trailingOnly = TRUE)",
    "secrets_file <- args[1]",
    "names_batch <- strsplit(args[2], ',')[[1]]",
    "status_file <- args[3]",
    "note <- function(...) cat(format(Sys.time(), '%H:%M:%OS2'), ..., '\\n', file = status_file, append = TRUE)",
    "note('started')",
    "tryCatch({",
    paste0("  ", load_line),
    "  note('loaded TempleCBE')",
    "  res <- TempleCBE::anonymize_pi(names_batch, method = 'token', secrets_path = secrets_file)",
    "  note('wrote', length(res), 'tokens')",
    "}, error = function(e) note('ERROR:', conditionMessage(e)))"
  ), worker_script)

  # Launch two concurrent workers with disjoint name sets
  names_worker1 <- "Alice,Bob,Charlie,David,Eve"
  names_worker2 <- "Frank,Grace,Heidi,Ivan,Judy"
  logs <- file.path(tmp_dir, c("worker1.log", "worker2.log"))
  status <- file.path(tmp_dir, c("worker1.status", "worker2.status"))

  # Run workers in background processes; logs catch output from before the script starts
  system2(rscript, c(shQuote(worker_script), shQuote(secrets_file), shQuote(names_worker1), shQuote(status[1])),
          stdout = logs[1], stderr = logs[1], wait = FALSE)
  system2(rscript, c(shQuote(worker_script), shQuote(secrets_file), shQuote(names_worker2), shQuote(status[2])),
          stdout = logs[2], stderr = logs[2], wait = FALSE)

  # Wait up to 60 seconds for both background workers to finish; loading the
  # covr-instrumented package takes several seconds per worker on slow runners
  t0 <- Sys.time()
  finished <- FALSE
  while (as.numeric(difftime(Sys.time(), t0, units = "secs")) < 60) {
    if (file.exists(secrets_file)) {
      data <- tryCatch(jsonlite::fromJSON(secrets_file), error = function(e) NULL)
      if (!is.null(data) && length(data$mappings) == 10) {
        finished <- TRUE
        break
      }
    }
    Sys.sleep(0.2)
  }

  # A worker that is still running can hold its log open (Windows locks it)
  read_worker_file <- function(path) {
    if (!file.exists(path)) return(paste0("<", basename(path), " missing>"))
    c(paste0("-- ", basename(path)), tryCatch(
      readLines(path, warn = FALSE),
      error = function(e) paste0("<unreadable: worker still running>")
    ))
  }
  worker_output <- unlist(lapply(c(status, logs), read_worker_file))
  expect_true(
    finished,
    info = paste(c("Both parallel workers completed and wrote all 10 records. Worker output:", worker_output), collapse = "\n")
  )
  if (finished) {
    data <- jsonlite::fromJSON(secrets_file)
    expect_named(data, "mappings")
    expect_equal(length(data$mappings), 10)
    all_names <- c("Alice", "Bob", "Charlie", "David", "Eve", "Frank", "Grace", "Heidi", "Ivan", "Judy")
    expect_true(all(all_names %in% names(data$mappings)))
    # Ensure no tokens collided
    all_tokens <- unlist(data$mappings)
    expect_equal(length(unique(all_tokens)), 10)
  }
})
