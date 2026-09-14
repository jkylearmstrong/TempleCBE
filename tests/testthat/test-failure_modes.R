test_that("failure-mode: missing executables abort with structured external_process_error", {
  # 1. safe_system2 with non-existent binary
  expect_error(
    safe_system2("definitely_not_a_command_xyz_9876", args = c("-v"), check = TRUE, log_failures = FALSE),
    class = "external_process_error"
  )

  err <- tryCatch(
    safe_system2("definitely_not_a_command_xyz_9876", check = TRUE, log_failures = FALSE),
    external_process_error = function(e) e
  )
  expect_identical(err$failure_mode, "missing_executable")
  expect_identical(err$cmd, "definitely_not_a_command_xyz_9876")

  # 2. run_sas_script with missing sas executable
  prog <- withr::local_tempfile(fileext = ".sas")
  writeLines("data _null_; run;", prog)
  expect_error(
    run_sas_script(prog, sas_path = "non_existent_sas_executable_path"),
    class = "external_process_error"
  )
})

test_that("failure-mode: permission-denied paths and insecure secrets locations", {
  # 1. validate_secrets_dir rejects a path inside the working directory's repository.
  # Use a throwaway repository: tests only run inside the git checkout under
  # some runners (not covr). Create the directory first so both paths normalize
  # to the same form (Windows 8.3 short names, macOS /var -> /private/var).
  repo <- normalizePath(withr::local_tempdir("secrets_repo"), winslash = "/")
  dir.create(file.path(repo, ".git"))
  repo_dir <- file.path(repo, "secrets_test_reject")
  dir.create(repo_dir)
  withr::with_dir(repo, {
    expect_error(
      validate_secrets_dir(repo_dir),
      "Refusing to use repository path for secrets"
    )
  })

  # 2. create_report rejects invalid location
  expect_error(
    create_report(""),
    "`location` must be a single non-empty directory path"
  )

  # 3. create_report rejects destination when file.access denies write
  # Simulate non-writable location
  loc <- withr::local_tempdir("readonly_report_dir")
  if (.Platform$OS.type != "windows") {
    Sys.chmod(loc, mode = "0555")
    on.exit(Sys.chmod(loc, mode = "0755"), add = TRUE)
    expect_error(
      create_report(loc),
      "Destination directory '.*' is not writable"
    )
  }
})

test_that("failure-mode: corrupted template directories fail early with actionable error", {
  loc <- withr::local_tempdir("corrupted_tpl_test")

  # Unrecognized or missing template file
  expect_warning(
    create_report(loc, template_name = "non_existent_tpl"),
    "`template_name` should be one of"
  )

  # Destination file that is not writable
  read_only_file <- file.path(loc, "t_test_example.qmd")
  file.create(read_only_file)
  if (.Platform$OS.type != "windows") {
    Sys.chmod(read_only_file, mode = "0444")
    on.exit(Sys.chmod(read_only_file, mode = "0644"), add = TRUE)
    expect_error(
      create_report(loc),
      "Destination file '.*' already exists and is not writable"
    )
  }
})

test_that("failure-mode: malformed and truncated archives in renv bootstrap fail early", {
  # Extract renv_bootstrap_git_extract_sha1_tar from renv/activate.R. renv/ is
  # build-ignored, so the file exists only when tests run from a source checkout.
  act_path <- file.path(here::here(), "renv", "activate.R")
  skip_if_not(file.exists(act_path), "renv/activate.R is only available in a source checkout")
  act_lines <- readLines(act_path)
  s_idx <- grep("renv_bootstrap_git_extract_sha1_tar <- function", act_lines)[1]
  e_idx <- grep("renv_bootstrap_install <- function", act_lines)[1] - 1
  extract_fn <- eval(parse(text = paste(act_lines[s_idx:e_idx], collapse = "\n")))

  tmp_dir <- withr::local_tempdir("archive_test")

  # 1. Non-existent file
  expect_error(
    extract_fn(file.path(tmp_dir, "missing.tar.gz")),
    "Archive bundle does not exist"
  )

  # 2. Truncated archive (too small to contain valid tar header)
  small_tar <- file.path(tmp_dir, "too_small.tar.gz")
  writeBin(charToRaw("truncated data"), small_tar)
  expect_error(
    extract_fn(small_tar),
    "Malformed or truncated archive"
  )

  # 3. Corrupt archive (non-gzip, junk bytes > 563 bytes)
  junk_tar <- file.path(tmp_dir, "junk.tar.gz")
  writeBin(rep(as.raw(65), 600), junk_tar)
  expect_error(
    extract_fn(junk_tar),
    "Malformed"
  )
})
