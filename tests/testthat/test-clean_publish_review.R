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

# ---- follow-ups from the independent review of the fixes ---------------------------------------

test_that("CP-L3-02: ordinary branch names are still accepted, and HEAD in any case is not", {
  skip_if_no_git()
  fx <- cp_fixture()
  run_git <- function(args, error_ok = FALSE) .cp_git(fx$repo, args, error_ok = error_ok)
  for (ok in c("main", "master", "private-history", "feature/x", "a/b/c", "release-1.0", "caf\u00e9")) {
    expect_no_error(.cp_check_branch_name(run_git, ok))
  }
  for (bad in c("HEAD", "head", "Head", "@", "@{-1}", "a b", "x..y", "foo.lock")) {
    expect_error(.cp_check_branch_name(run_git, bad), "is not a valid branch name")
  }
})

test_that("CP-L3-02: a branch name that collides with an existing branch is refused before the push", {
  skip_if_no_git()
  fx <- cp_fixture()
  cp_git(fx$repo, "branch", "feat")
  before <- cp_state(fx$repo)
  err <- tryCatch(
    cp_quiet(clean_publish(repo_root = fx$repo, remote = "origin", publish_branch = "feat/x")),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "collides with 'feat'")
  expect_false(grepl("ALREADY UPDATED", err, fixed = TRUE))
  expect_length(cp_remote_refs(fx), 0L)
  expect_identical(cp_state(fx$repo), before)

  # the private branch too, and the two names against each other
  err <- tryCatch(
    cp_quiet(clean_publish(repo_root = fx$repo, push = FALSE, private_branch = "feat/x")),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "collides with 'feat'")
  err <- tryCatch(
    cp_quiet(clean_publish(repo_root = fx$repo, push = FALSE, publish_branch = "pub", private_branch = "pub/old")),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "collides with")
  expect_identical(cp_state(fx$repo), before)
})

test_that("CP-L3-03: credentials are stripped from other URL shapes in the hook", {
  skip_if_no_git()
  fx <- cp_fixture()
  cp_git(fx$repo, "remote", "add", "public", "https://example.invalid/o/r.git")
  cp_quiet(guard_public_remote("public", repo_root = fx$repo))
  cases <- list(
    c("ssh://git:SECRETTOKEN@example.invalid/o/r.git", "ssh://example.invalid/o/r.git"),
    c("https://u:SECRET@TOKEN@example.invalid/o/r.git", "https://example.invalid/o/r.git"),
    c("https://example.invalid/o/r.git", "https://example.invalid/o/r.git")
  )
  # the fenced URL is matched after normalising, so these spellings are all refused
  for (sh in cp_hook_shells()) {
    for (case in cases) {
      res <- cp_run_hook(sh, fx$repo, remote = "", url = case[1])
      expect_false(res$status == 0L, info = case[1])
      out <- paste(res$out, collapse = "\n")
      expect_false(grepl("SECRET", out, fixed = TRUE), info = case[1])
      expect_true(grepl(paste0("remote '", case[2], "' is guarded"), out, fixed = TRUE), info = case[1])
    }
  }
})

test_that("CP-L3-01: an unnormalizable public_url is reported without its credentials", {
  skip_if_no_git()
  fx <- cp_fixture()
  err <- tryCatch(
    cp_quiet(guard_public_remote(paste0("https://u:", secret, "@/"), repo_root = fx$repo, allow_unmatched = TRUE)),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "could not be normalized")
  expect_false(grepl(secret, err, fixed = TRUE))
})

test_that("CP-L3-04: a tracked-but-ignored file whose name git still quotes is reported, never allowed", {
  skip_if_no_git()
  skip_on_os("windows") # a tab cannot be part of a file name there
  fx <- cp_fixture()
  name <- "tab\there.txt"
  writeLines("keep", file.path(fx$repo, name))
  writeLines("tab*", file.path(fx$repo, ".gitignore"))
  cp_git(fx$repo, "add", "-f", "-A")
  cp_git(fx$repo, "commit", "--quiet", "-m", "tracked and ignored")
  before <- cp_state(fx$repo)
  err <- tryCatch(
    cp_quiet(clean_publish(repo_root = fx$repo, push = FALSE, allow_ignored = name)),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "match .gitignore patterns")
  expect_identical(cp_state(fx$repo), before)
})

test_that("CP-L3-04: a tracked-but-ignored name that is not valid UTF-8 is reported with escaped bytes and never allowed", {
  skip_if_no_git()
  fx <- cp_fixture()
  before <- cp_state(fx$repo)
  real_git <- .cp_git
  # what git prints for a Latin-1 file name on a file system that allows one
  latin1_listing <- function(repo_root, args, error_ok = FALSE) {
    if ("ls-files" %in% args) return(list(output = "caf\xe9 notes.txt", stderr = character(), status = 0L))
    real_git(repo_root, args, error_ok = error_ok)
  }
  testthat::local_mocked_bindings(.cp_git = latin1_listing)
  err <- tryCatch(
    cp_quiet(clean_publish(repo_root = fx$repo, push = FALSE, allow_ignored = "caf\u00e9 notes.txt")),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "match .gitignore patterns")
  expect_match(err, "<e9>", fixed = TRUE)
  expect_identical(cp_state(fx$repo), before)
})

test_that("CP-L3-02: a tag with the name of a branch does not hide a branch-name collision", {
  skip_if_no_git()
  fx <- cp_fixture()
  cp_git(fx$repo, "branch", "feat")
  cp_git(fx$repo, "tag", "feat")
  run_git <- function(args, error_ok = FALSE) .cp_git(fx$repo, args, error_ok = error_ok)
  expect_error(.cp_check_branch_conflicts(run_git, c("feat/x", "private-history")), "collides with 'feat'")
})

test_that("CP-L3-02: branch names are compared ignoring case, and unrelated names do not collide", {
  skip_if_no_git()
  fx <- cp_fixture()
  cp_git(fx$repo, "branch", "Feat")
  cp_git(fx$repo, "branch", "feature")
  cp_git(fx$repo, "branch", "feat-x")
  run_git <- function(args, error_ok = FALSE) .cp_git(fx$repo, args, error_ok = error_ok)
  expect_error(.cp_check_branch_conflicts(run_git, c("feat/y", "private-history")), "collides with 'Feat'")
  # none of these shares a path prefix with an existing branch or with the other name
  expect_no_error(.cp_check_branch_conflicts(run_git, c("featx", "private-history")))
  expect_no_error(.cp_check_branch_conflicts(run_git, c("other/x", "feature-y")))
  # an existing branch that is itself one of the two names is not a collision (it is being updated)
  expect_no_error(.cp_check_branch_conflicts(run_git, c("master", "feature")))
  expect_no_error(.cp_check_branch_conflicts(run_git, "master"))
})
