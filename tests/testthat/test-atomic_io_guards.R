test_that("validate_executable rejects directory paths (A5-24)", {
  td <- tempfile("exec_dir_")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)

  # Directory path must be rejected even though file.exists(td) is TRUE
  expect_error(
    TempleCBE:::validate_executable(td),
    "is a directory, not an executable file"
  )
})

test_that("validate_secrets_dir accepts sibling directory with shared prefix (A5-24)", {
  parent_td <- tempfile("not_a_repo_")
  dir.create(parent_td)
  on.exit(unlink(parent_td, recursive = TRUE), add = TRUE)

  # Create fake repo inside parent_td
  repo_dir <- file.path(parent_td, "myrepo")
  dir.create(repo_dir)
  dir.create(file.path(repo_dir, ".git"))

  # Sibling directory that starts with myrepo_sibling
  sibling_dir <- file.path(parent_td, "myrepo_sibling")

  # Validate secrets dir on sibling_dir: should NOT trigger repository guard
  expect_no_error(TempleCBE:::validate_secrets_dir(sibling_dir))
  expect_true(dir.exists(sibling_dir))
})

test_that("validate_secrets_dir rejects an existing file (A5-21)", {
  tf <- tempfile("secrets_file_")
  file.create(tf)
  on.exit(unlink(tf), add = TRUE)

  expect_error(
    TempleCBE:::validate_secrets_dir(tf),
    "points to an existing file, not a directory"
  )
})

test_that("with_file_lock errors immediately if parent directory does not exist (A5-24)", {
  non_existent_lock <- file.path(tempfile("no_such_parent_"), "secrets.lock")
  expect_error(
    TempleCBE:::with_file_lock(non_existent_lock, 1 + 1),
    "parent directory does not exist"
  )
})

test_that("with_file_lock does not delete lock if holder PID is alive (A5-16)", {
  td <- tempfile("lock_liveness_")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)

  lock_dir <- file.path(td, "test.lock")
  dir.create(lock_dir)

  # Write current process PID as owner
  my_pid <- Sys.getpid()
  owner_file <- file.path(lock_dir, "lock_owner")
  writeLines(paste0("pid: ", my_pid, "\ntime: 2026-01-01 00:00:00"), owner_file)

  # Artificially age the directory by setting mtime 60s in the past
  old_time <- Sys.time() - 60
  Sys.setFileTime(lock_dir, old_time)

  # An acquire attempt with timeout = 0.5s should fail because owner PID is alive
  expect_error(
    TempleCBE:::with_file_lock(lock_dir, code = { TRUE }, timeout = 0.5, stale_age = 1),
    "Failed to acquire advisory file lock"
  )

  # Lock directory should STILL exist because holder PID is alive
  expect_true(dir.exists(lock_dir))
})

test_that("generate_pseudonym_token rejects n_chars < 6 unless allow_collisions = TRUE (A4-02)", {
  expect_error(
    generate_pseudonym_token("Smith", n_chars = 4, key = "a_valid_secret_key_16"),
    "at least 6"
  )
  expect_error(
    generate_pseudonym_token(n = 2, n_chars = 3),
    "at least 6"
  )
  expect_no_error(
    generate_pseudonym_token("Smith", n_chars = 4, key = "a_valid_secret_key_16", allow_collisions = TRUE)
  )
})

test_that("generate_pseudonym_token detects collisions among distinct names (A4-02)", {
  # With a mock rename function that forces all tokens to be identical
  expect_error(
    generate_pseudonym_token(
      c("Alice", "Bob"),
      key = "a_valid_secret_key_16",
      rename_fn = function(tok) rep("COLLISION", length(tok))
    ),
    "Pseudonym collision detected"
  )

  # Suppressed when allow_collisions = TRUE
  expect_warning(
    toks <- generate_pseudonym_token(
      c("Alice", "Bob"),
      key = "a_valid_secret_key_16",
      rename_fn = function(tok) rep("COLLISION", length(tok)),
      allow_collisions = TRUE
    ),
    "Pseudonym collision detected"
  )
  expect_identical(toks, c("COLLISION", "COLLISION"))
})

test_that("hmac_surname is invariant to RNGkind() and sample.kind settings (A4-01)", {
  key <- "a_valid_secret_key_16"
  names <- c("Smith", "Jones", "Patel")

  orig_res <- anonymize_pi(names, method = "hmac_surname", key = key)

  # Change RNGkind to L'Ecuyer-CMRG
  withr::with_rng_version("3.5.0", {
    res_alt_rng <- anonymize_pi(names, method = "hmac_surname", key = key)
    expect_identical(res_alt_rng, orig_res)
  })
})

test_that("anonymize_pi treats empty strings and whitespace as NA (A4-11)", {
  key <- "a_valid_secret_key_16"
  res_hmac <- anonymize_pi(c("Smith", "", "   ", NA), key = key)
  expect_false(is.na(res_hmac[1]))
  expect_true(is.na(res_hmac[2]))
  expect_true(is.na(res_hmac[3]))
  expect_true(is.na(res_hmac[4]))

  toks <- generate_pseudonym_token(c("Smith", "", "   ", NA), key = key)
  expect_false(is.na(toks[1]))
  expect_true(is.na(toks[2]))
  expect_true(is.na(toks[3]))
  expect_true(is.na(toks[4]))
})

test_that("anonymize_pi supports opt-in normalize = TRUE (A4-09)", {
  key <- "a_valid_secret_key_16"
  res_norm <- anonymize_pi(c(" Smith ", "smith", "SMITH"), key = key, normalize = TRUE)
  expect_identical(res_norm[1], res_norm[2])
  expect_identical(res_norm[2], res_norm[3])

  # Without normalize = TRUE, they are different
  res_raw <- anonymize_pi(c(" Smith ", "smith", "SMITH"), key = key, normalize = FALSE)
  expect_false(res_raw[1] == res_raw[2])
})

test_that("resolve_hmac_key emits warning for weak keys < 16 chars (A4-19)", {
  expect_warning(
    anonymize_pi("Smith", key = "short_key"),
    "shorter than 16 characters"
  )
  expect_no_warning(
    anonymize_pi("Smith", key = "this_is_a_very_long_and_secure_key_123")
  )
})
