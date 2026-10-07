deploy_script <- normalizePath(testthat::test_path("..", "..", "scripts", "deploy_release.R"), winslash = "/", mustWork = FALSE)
if (!file.exists(deploy_script) || !file.exists(file.path(dirname(dirname(deploy_script)), "DESCRIPTION"))) {
  testthat::skip("scripts/deploy_release.R is not available (not running from the source tree)")
}
source(deploy_script, local = TRUE)

test_that("deploy_release validates version_tag strictly (A5-11)", {
  # Invalid versions must throw an error, not just a warning
  expect_error(
    deploy_release(version_tag = "1.0 && cmd", dry_run = TRUE),
    "not a valid numeric R package version"
  )
  expect_error(
    deploy_release(version_tag = "invalid_version_string", dry_run = TRUE),
    "not a valid numeric R package version"
  )
  expect_error(
    deploy_release(version_tag = "1.2.3.alpha", dry_run = TRUE),
    "not a valid numeric R package version"
  )
})

test_that("deploy_release generates valid datetime version tags with %H%M (A5-25)", {
  res <- deploy_release(version_tag = "auto", base_version = "0.4.45", dry_run = TRUE)
  expect_true(grepl("^0\\.4\\.45\\.\\d{4}\\.\\d{2}\\.\\d{2}\\.\\d{4}$", res))
  expect_s3_class(package_version(res), "package_version")
})

test_that("deploy_release refuses execution off expected branch (A5-05)", {
  expect_error(
    deploy_release(
      version_tag = "0.4.45.1",
      commit = TRUE,
      dry_run = FALSE,
      expected_branch = "non_existent_branch_xyz"
    ),
    "must be run on branch 'non_existent_branch_xyz'"
  )
})

test_that("deploy_release dry_run mode executes cleanly without modifying files", {
  desc_path <- file.path(dirname(dirname(deploy_script)), "DESCRIPTION")
  desc_before <- readLines(desc_path)
  res <- deploy_release(version_tag = "0.4.45.9999", dry_run = TRUE)
  desc_after <- readLines(desc_path)

  expect_equal(res, "0.4.45.9999")
  expect_equal(desc_before, desc_after)
})

# ---- Stage 3: pushing is opt-in, the remote is named, a failed push is rolled back (CPD-06, CPD-16) ----
#
# deploy_release() is run against a scratch package repository under a temporary directory, with local
# bare repositories as remotes, never against this one. Only its git steps run: no tests, README or
# documentation.

# master with a DESCRIPTION, pushed to the bare repository `origin`; a second bare repository `public`
# is a remote as well. The list of cp_fixture() plus `public`.
dr_fixture <- function(repo_name = "work", env = parent.frame()) {
  fx <- cp_fixture(repo_name = repo_name, env = env)
  # LF line ends, as deploy_release() writes the file
  writeBin(
    charToRaw(paste0(c("Package: scratchpkg", "Version: 0.4.45", "Title: Scratch", "Description: Scratch.", "License: MIT"), "\n", collapse = "")),
    file.path(fx$repo, "DESCRIPTION")
  )
  cp_git(fx$repo, "add", "-A")
  stopifnot(cp_git(fx$repo, "commit", "--quiet", "-m", "add DESCRIPTION")$status == 0L)
  stopifnot(cp_git(fx$repo, "push", "--quiet", "origin", "master")$status == 0L)
  fx$public <- file.path(fx$base, "public.git")
  dir.create(fx$public)
  cp_git(fx$public, "-c", "init.defaultBranch=master", "init", "--bare", "--quiet")
  cp_git(fx$repo, "remote", "add", "public", fx$public)
  fx
}

dr_version <- function(repo) sub("^Version: ", "", grep("^Version:", readLines(file.path(repo, "DESCRIPTION")), value = TRUE))

# deploy_release() acting on the scratch repository `repo` (its root is found from the working directory)
run_deploy <- function(repo, ...) {
  stopifnot(startsWith(normalizePath(repo, winslash = "/"), normalizePath(tempdir(), winslash = "/")))
  withr::with_dir(repo, suppressMessages(deploy_release(
    version_tag = "0.4.46", run_tests = FALSE, render_readme = FALSE, document = FALSE, ...
  )))
}

# scripts/deploy_release.R in a subprocess, started in the scratch repository `repo`
run_deploy_script <- function(args, repo) {
  stopifnot(startsWith(normalizePath(repo, winslash = "/"), normalizePath(tempdir(), winslash = "/")))
  withr::with_dir(repo, withr::with_envvar(
    c(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep)),
    {
      out <- suppressWarnings(system2(
        file.path(R.home("bin"), "Rscript"), shQuote(c("--vanilla", deploy_script, args)),
        stdout = TRUE, stderr = TRUE
      ))
      list(out = out, status = attr(out, "status") %||% 0L, text = paste(out, collapse = "\n"))
    }
  ))
}

test_that("deploy_release() does not push unless asked: it commits and tags locally and sends nothing (CPD-06)", {
  skip_if_no_git()
  expect_false(formals(deploy_release)$push)
  fx <- dr_fixture()
  origin_before <- cp_remote_refs(fx)

  res <- run_deploy(fx$repo)

  expect_identical(res, "0.4.46")
  expect_identical(dr_version(fx$repo), "0.4.46")
  expect_identical(cp_git(fx$repo, "log", "-1", "--format=%s")$out, "Automated release for 0.4.46")
  expect_identical(cp_git(fx$repo, "tag", "--list")$out, "v0.4.46")
  expect_identical(cp_remote_refs(fx), origin_before)
  expect_identical(cp_git(fx$public, "for-each-ref")$out, character())
})

test_that("with push = TRUE the branch and the tag go to the remote that is named, and to no other (CPD-06, CPD-16)", {
  skip_if_no_git()
  fx <- dr_fixture()
  origin_before <- cp_remote_refs(fx)

  run_deploy(fx$repo, push = TRUE, remote = "public")

  expect_identical(cp_git(fx$public, "rev-parse", "master")$out, cp_rev(fx$repo, "HEAD"))
  expect_identical(cp_git(fx$public, "tag", "--list")$out, "v0.4.46")
  expect_identical(cp_remote_refs(fx), origin_before)
})

test_that("a push that fails rolls back the local commit and the tag, and DESCRIPTION is as it was (CPD-16)", {
  skip_if_no_git()
  fx <- dr_fixture()
  cp_reject_pushes(fx) # origin refuses every push, as a server with a rejecting hook does
  before <- cp_state(fx$repo)
  origin_before <- cp_remote_refs(fx)

  expect_error(run_deploy(fx$repo, push = TRUE), "push")

  expect_identical(cp_state(fx$repo), before)
  expect_identical(cp_git(fx$repo, "tag", "--list")$out, character())
  expect_identical(cp_git(fx$repo, "status", "--porcelain")$out, character())
  expect_identical(dr_version(fx$repo), "0.4.45")
  expect_identical(cp_remote_refs(fx), origin_before)
})

test_that("a remote that is not configured, or that looks like an option, is refused before anything changes (CPD-16)", {
  skip_if_no_git()
  fx <- dr_fixture()
  before <- cp_state(fx$repo)

  expect_error(run_deploy(fx$repo, push = TRUE, remote = "nope"), "Remote 'nope' is not configured")
  expect_error(run_deploy(fx$repo, push = TRUE, remote = "--mirror"), "not a valid remote name")

  expect_identical(cp_state(fx$repo), before)
  expect_identical(dr_version(fx$repo), "0.4.45")
})

test_that("scripts/deploy_release.R pushes only with --push, to the remote --remote names (CPD-06, CPD-16)", {
  skip_if_no_git()
  fx <- dr_fixture()
  origin_before <- cp_remote_refs(fx)

  help <- run_deploy_script("--help", fx$repo)
  expect_identical(help$status, 0L)
  expect_match(help$text, "--push", fixed = TRUE)
  expect_match(help$text, "--remote", fixed = TRUE)

  # the banner of a dry run shows what would happen
  default <- run_deploy_script("--dry-run", fx$repo)
  expect_identical(default$status, 0L, info = default$text)
  expect_match(default$text, "Step 10: Git Push *: SKIPPED")
  asked <- run_deploy_script(c("--dry-run", "--push", "--remote", "public"), fx$repo)
  expect_identical(asked$status, 0L, info = asked$text)
  expect_match(asked$text, "Step 10: Git Push *: ENABLED .*public")
  expect_true(run_deploy_script(c("--dry-run", "--remote"), fx$repo)$status != 0L)

  # a real run without --push commits and tags locally and sends nothing
  local_only <- run_deploy_script(c("--version", "0.4.47", "--no-tests", "--no-readme", "--no-doc"), fx$repo)
  expect_identical(local_only$status, 0L, info = local_only$text)
  expect_identical(cp_git(fx$repo, "tag", "--list")$out, "v0.4.47")
  expect_identical(cp_remote_refs(fx), origin_before)
  expect_identical(cp_git(fx$public, "for-each-ref")$out, character())

  # --push --remote public publishes there and only there
  pushed <- run_deploy_script(
    c("--version", "0.4.48", "--no-tests", "--no-readme", "--no-doc", "--push", "--remote", "public"), fx$repo
  )
  expect_identical(pushed$status, 0L, info = pushed$text)
  expect_identical(cp_git(fx$public, "rev-parse", "master")$out, cp_rev(fx$repo, "HEAD"))
  expect_identical(cp_git(fx$public, "tag", "--list")$out, "v0.4.48")
  expect_identical(cp_remote_refs(fx), origin_before)
})

test_that("a commit that fails leaves the index and DESCRIPTION as they were, and nothing is tagged (CPD-16)", {
  skip_if_no_git()
  fx <- dr_fixture()
  hook <- file.path(fx$repo, ".git", "hooks", "pre-commit")
  writeLines(c("#!/bin/sh", "echo 'commit refused by the test hook' >&2", "exit 1"), hook)
  Sys.chmod(hook, "0755")
  before <- cp_state(fx$repo)

  expect_error(run_deploy(fx$repo, push = TRUE, remote = "public"), "git commit failed")

  expect_identical(cp_state(fx$repo), before)
  expect_identical(cp_git(fx$repo, "status", "--porcelain")$out, character())
  expect_identical(cp_git(fx$repo, "tag", "--list")$out, character())
  expect_identical(dr_version(fx$repo), "0.4.45")
  expect_identical(cp_git(fx$public, "for-each-ref")$out, character())
})

test_that("a repository path with a space works: every git argument is quoted (CPD-16)", {
  skip_if_no_git()
  fx <- dr_fixture(repo_name = "my work")

  run_deploy(fx$repo, push = TRUE, remote = "public", commit_msg = "A release with a message of several words")

  expect_identical(cp_git(fx$public, "rev-parse", "master")$out, cp_rev(fx$repo, "HEAD"))
  expect_identical(cp_git(fx$repo, "log", "-1", "--format=%s")$out, "A release with a message of several words")
  expect_identical(cp_git(fx$public, "tag", "--list")$out, "v0.4.46")
})
