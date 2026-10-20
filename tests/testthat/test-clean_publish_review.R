# Regression tests for the L3 security review of R/clean_publish.R (release-notes/l3-clean_publish-review.md).
# Each test is for one finding and FAILS until that finding is fixed. All repositories are
# throwaway fixtures under a temp directory with a local bare repository as the only remote
# (see helper-clean_publish.R); no real remote and no network is touched.

secret <- "SECRETTOKEN"

# ---- CP-L3-01: guard_public_remote() prints credentials ----------------------------------------

test_that("CP-L3-01: guard_public_remote() does not print the credentials of any remote URL in its error", {
  skip_if_no_git()
  fx <- cp_fixture()
  cp_git(fx$repo, "remote", "add", "public", paste0("https://user:", secret, "@example.invalid/o/r.git"))
  err <- tryCatch(
    cp_quiet(guard_public_remote("https://other.invalid/x.git", repo_root = fx$repo)),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "does not match the push URL of any configured remote")
  expect_false(grepl(secret, err, fixed = TRUE))
})

test_that("CP-L3-01: guard_public_remote() does not print the credentials of the URL it is given", {
  skip_if_no_git()
  fx <- cp_fixture()
  url <- paste0("https://u2:", secret, "@other.invalid/x.git")
  msgs <- testthat::capture_messages(
    guard_public_remote(url, repo_root = fx$repo, allow_unmatched = TRUE)
  )
  expect_true(any(grepl("matches no configured remote", msgs, fixed = TRUE)))
  expect_false(any(grepl(secret, msgs, fixed = TRUE)))
  # nor is it written to the git configuration
  expect_false(any(grepl(secret, cp_git(fx$repo, "config", "--local", "--list")$out, fixed = TRUE)))
})

# ---- CP-L3-02: names git refuses as branch names are accepted, and the push goes first ---------

test_that("CP-L3-02: publish_branch = 'HEAD' is refused before anything is pushed or written", {
  skip_if_no_git()
  fx <- cp_fixture()
  before <- cp_state(fx$repo)
  err <- tryCatch(
    cp_quiet(clean_publish(repo_root = fx$repo, remote = "origin", publish_branch = "HEAD")),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "is not a valid branch name")
  expect_false(grepl("ALREADY UPDATED", err, fixed = TRUE))
  # the remote was not touched and neither was the repository
  expect_length(cp_remote_refs(fx), 0L)
  expect_identical(cp_state(fx$repo), before)
})

test_that("CP-L3-02: private_branch = 'HEAD' is refused before any backup, hook or configuration is written", {
  skip_if_no_git()
  fx <- cp_fixture()
  before <- cp_state(fx$repo)
  err <- tryCatch(
    cp_quiet(clean_publish(repo_root = fx$repo, push = FALSE, private_branch = "HEAD")),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "is not a valid branch name")
  expect_identical(cp_state(fx$repo), before)
  expect_false(file.exists(cp_hook_file(fx$repo)))
})

# ---- CP-L3-03: the hook prints the credentials of the URL it refuses ---------------------------

test_that("CP-L3-03: the hook's refusal of a guarded URL does not echo its credentials", {
  skip_if_no_git()
  fx <- cp_fixture()
  url <- paste0("https://user:", secret, "@example.invalid/o/r.git")
  cp_git(fx$repo, "remote", "add", "public", url)
  cp_quiet(guard_public_remote("public", repo_root = fx$repo))
  for (sh in cp_hook_shells()) {
    res <- cp_run_hook(sh, fx$repo, remote = "public", url = url)
    expect_false(res$status == 0L, info = sh)
    expect_true(any(grepl("is guarded", res$out, fixed = TRUE)), info = sh)
    expect_false(any(grepl(secret, res$out, fixed = TRUE)), info = sh)
  }
})

# ---- CP-L3-04: allow_ignored cannot name a path that git quotes --------------------------------

test_that("CP-L3-04: allow_ignored accepts a tracked-but-ignored file whose name git quotes", {
  skip_if_no_git()
  skip_if_not(l10n_info()[["UTF-8"]], "needs a UTF-8 locale")
  fx <- cp_fixture()
  name <- "café notes.txt"
  writeLines("keep", file.path(fx$repo, name))
  writeLines("caf*", file.path(fx$repo, ".gitignore"))
  cp_git(fx$repo, "add", "-f", "-A")
  expect_identical(cp_git(fx$repo, "commit", "--quiet", "-m", "tracked and ignored")$status, 0L)

  # without the allowance the file is still reported, by its real name
  err <- tryCatch(
    cp_quiet(clean_publish(repo_root = fx$repo, push = FALSE)),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "match .gitignore patterns")
  expect_match(err, name, fixed = TRUE)

  # naming it lets the publish through
  expect_no_error(cp_quiet(clean_publish(repo_root = fx$repo, push = FALSE, allow_ignored = name)))
})
