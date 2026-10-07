# The first part tests resolve_publish_branch() on its own. The rest runs
# clean_publish() itself, and only ever in throwaway repositories under a
# temporary directory with a local bare repository as their only remote (see
# helper-clean_publish.R); the real repository and the network are never touched.

exists_in <- function(...) {
  branches <- c(...)
  function(name) name %in% branches
}

test_that("an explicit publish_branch always wins", {
  expect_identical(
    resolve_publish_branch("release", "feature", "private-history", exists_in("master", "main")),
    "release"
  )
})

test_that("master is preferred over the current branch", {
  expect_identical(
    resolve_publish_branch(NULL, "feature", "private-history", exists_in("master", "main")),
    "master"
  )
  expect_identical(
    resolve_publish_branch("", "feature", "private-history", exists_in("master")),
    "master"
  )
})

test_that("main is used when there is no master", {
  expect_identical(
    resolve_publish_branch(NULL, "feature", "private-history", exists_in("main")),
    "main"
  )
})

test_that("the current branch is only a last resort, and never the private branch", {
  expect_identical(
    resolve_publish_branch(NULL, "feature", "private-history", exists_in()),
    "feature"
  )
  expect_identical(
    resolve_publish_branch(NULL, "private-history", "private-history", exists_in()),
    "master"
  )
  expect_identical(
    resolve_publish_branch(NULL, "", "private-history", exists_in()),
    "master"
  )
})

test_that("publishing onto the private branch is refused", {
  expect_error(
    resolve_publish_branch("private-history", "feature", "private-history", exists_in("master")),
    "Refusing to publish onto 'private-history'"
  )
  expect_error(
    resolve_publish_branch("history", "feature", "history", exists_in("master")),
    "Refusing to publish onto 'history'"
  )
})

test_that("a publish branch that differs from the private branch only in case is refused too (CPA-10)", {
  # on a case-insensitive file system `Master` and `master` are one branch, and publishing onto it
  # would squash the private history
  expect_error(
    resolve_publish_branch("Private-History", "feature", "private-history", exists_in("master")),
    "Refusing to publish onto 'Private-History'.*differ only in case"
  )
  expect_error(
    resolve_publish_branch(NULL, "feature", "MASTER", exists_in("master")),
    "Refusing to publish onto 'master'"
  )
})

# ------------------------------------------------------------------------------
# The fixtures themselves
# ------------------------------------------------------------------------------

test_that("the fixture's git configuration switches off background maintenance", {
  skip_if_no_git()
  cp_isolate_git()
  expect_identical(cp_git(tempdir(), "config", "--get", "maintenance.auto")$out, "false")
  expect_identical(cp_git(tempdir(), "config", "--get", "gc.auto")$out, "0")
  expect_identical(cp_git(tempdir(), "config", "--get", "gc.autoDetach")$out, "false")
})

test_that("cp_copy_template() removes a failed partial copy and tries again", {
  template <- withr::local_tempdir()
  base <- withr::local_tempdir()
  for (d in c("work", "remote.git")) {
    dir.create(file.path(template, d))
    writeLines(d, file.path(template, d, "f.txt"))
  }
  calls <- 0L
  fails_once <- function(from, to, ...) {
    calls <<- calls + 1L
    res <- file.copy(from, to, ...)
    if (calls == 1L) res[2] <- FALSE
    res
  }
  expect_true(cp_copy_template(template, base, copy = fails_once))
  expect_identical(calls, 2L)
  expect_identical(readLines(file.path(base, "work", "f.txt")), "work")
  expect_identical(readLines(file.path(base, "remote.git", "f.txt")), "remote.git")

  always_fails <- function(from, to, ...) rep(FALSE, length(from))
  expect_error(cp_copy_template(template, withr::local_tempdir(), attempts = 2L, copy = always_fails))
})

# ------------------------------------------------------------------------------
# clean_publish() in throwaway repositories
# ------------------------------------------------------------------------------

test_that("a first publish makes one parentless commit, keeps the full history and, with push = FALSE, leaves the remote alone", {
  skip_if_no_git()
  fx <- cp_fixture()
  expect_identical(cp_git(fx$repo, "push", "--quiet", "origin", "master")$status, 0L)
  remote_before <- cp_remote_refs(fx)
  expect_length(remote_before, 1L)
  old_master <- cp_rev(fx$repo, "master")
  head_tree <- cp_git(fx$repo, "rev-parse", "HEAD^{tree}")$out

  clean <- cp_first_publish(fx)

  expect_identical(cp_rev(fx$repo, "master"), clean)
  expect_identical(cp_ncommits(fx$repo, "master"), 1L)
  expect_identical(cp_git(fx$repo, "rev-list", "--parents", "-n", "1", "master")$out, clean)
  expect_identical(cp_git(fx$repo, "rev-parse", "master^{tree}")$out, head_tree)
  expect_identical(cp_ncommits(fx$repo, "private-history"), 3L)
  expect_identical(cp_rev(fx$repo, "private-history"), old_master)
  expect_identical(cp_current(fx$repo), "private-history")
  expect_identical(cp_remote_refs(fx), remote_before)
})

test_that("push = TRUE publishes exactly one commit on one branch, after printing the push URL", {
  skip_if_no_git()
  fx <- cp_fixture()

  msgs <- testthat::capture_messages(clean <- clean_publish(repo_root = fx$repo, remote = "origin"))

  expect_identical(cp_remote_refs(fx), paste("refs/heads/master", clean))
  expect_identical(cp_ncommits(fx$remote, "master"), 1L)
  expect_identical(
    cp_git(fx$remote, "rev-parse", "master^{tree}")$out,
    cp_git(fx$repo, "rev-parse", "private-history^{tree}")$out
  )
  expect_identical(cp_current(fx$repo), "private-history")
  url_line <- grep("Push URL of 'origin'", msgs, fixed = TRUE)
  push_line <- grep("Force-pushing", msgs, fixed = TRUE)
  expect_length(url_line, 1L)
  expect_true(grepl(fx$remote, msgs[url_line], fixed = TRUE))
  expect_true(url_line < push_line)
})

test_that("a re-run from the private branch keeps every commit", {
  skip_if_no_git()
  fx <- cp_fixture(published = TRUE)
  cp_commit(fx$repo, "later1.txt", "x", "later1")
  cp_commit(fx$repo, "later2.txt", "x", "later2")
  tip <- cp_rev(fx$repo, "private-history")
  expect_identical(cp_ncommits(fx$repo, "private-history"), 5L)

  clean <- cp_first_publish(fx)

  expect_identical(cp_rev(fx$repo, "private-history"), tip)
  expect_identical(cp_ncommits(fx$repo, "private-history"), 5L)
  expect_identical(cp_ncommits(fx$repo, "master"), 1L)
  expect_identical(cp_rev(fx$repo, "master"), clean)
})

# --- A5-01: private history must never be overwritten ---------------------------

test_that("running from the publish branch after a publish is refused and changes nothing (A5-01)", {
  skip_if_no_git()
  fx <- cp_fixture(published = TRUE)
  cp_commit(fx$repo, "later1.txt", "x", "later1")
  cp_commit(fx$repo, "later2.txt", "x", "later2")
  tip <- cp_rev(fx$repo, "private-history")
  cp_git(fx$repo, "checkout", "--quiet", "master")
  before <- cp_state(fx$repo)

  expect_error(
    cp_quiet(clean_publish(repo_root = fx$repo, push = FALSE)),
    "Branch 'private-history' has commits that 'master' does not contain"
  )

  expect_identical(cp_state(fx$repo), before)
  expect_identical(cp_ncommits(fx$repo, "private-history"), 5L)
  expect_identical(cp_rev(fx$repo, "private-history"), tip)
})

test_that("running from a branch that is behind the private branch is refused (A5-01)", {
  skip_if_no_git()
  fx <- cp_fixture(published = TRUE)
  cp_git(fx$repo, "branch", "feature")
  for (i in 4:6) cp_commit(fx$repo, paste0("f", i, ".txt"), "x", paste0("c", i))
  expect_identical(cp_ncommits(fx$repo, "private-history"), 6L)
  cp_git(fx$repo, "checkout", "--quiet", "feature")
  before <- cp_state(fx$repo)

  expect_error(cp_quiet(clean_publish(repo_root = fx$repo, push = FALSE)), "does not contain")

  expect_identical(cp_state(fx$repo), before)
  expect_identical(cp_ncommits(fx$repo, "private-history"), 6L)
})

test_that("running from a descendant of the private branch moves it forward", {
  skip_if_no_git()
  fx <- cp_fixture(published = TRUE)
  cp_git(fx$repo, "checkout", "--quiet", "-b", "feature")
  cp_commit(fx$repo, "feature.txt", "x", "feat")
  tip <- cp_rev(fx$repo, "HEAD")

  cp_quiet(clean_publish(repo_root = fx$repo, push = FALSE))

  expect_identical(cp_rev(fx$repo, "private-history"), tip)
  expect_identical(cp_ncommits(fx$repo, "private-history"), 4L)
  expect_identical(cp_current(fx$repo), "private-history")
  expect_identical(cp_git(fx$repo, "cat-file", "-e", "master:feature.txt")$status, 0L)
})

test_that("the old private and publish tips are kept under refs/backup/clean_publish", {
  skip_if_no_git()
  fx <- cp_fixture()
  master_1 <- cp_rev(fx$repo, "master")

  clean_1 <- cp_first_publish(fx)
  backups <- cp_backup_refs(fx$repo)
  # first run: there was no private branch yet, the old master tip is kept
  expect_length(backups, 1L)
  expect_match(backups, "^refs/backup/clean_publish/[0-9]{8}T[0-9]{6}Z(-[0-9]+)?/master ")
  expect_true(endsWith(backups, master_1))

  cp_commit(fx$repo, "later.txt", "x", "later")
  private_tip <- cp_rev(fx$repo, "private-history")
  clean_2 <- cp_first_publish(fx)
  backups <- cp_backup_refs(fx$repo)
  # the second run (possibly within the same second) adds new refs and overwrites none
  expect_length(backups, 3L)
  expect_length(unique(sub(" .*$", "", backups)), 3L)
  expect_true(any(endsWith(backups, private_tip) & grepl("/private-history ", backups, fixed = TRUE)))
  expect_true(any(endsWith(backups, clean_1) & grepl("/master ", backups, fixed = TRUE)))
  expect_false(any(endsWith(backups, clean_2)))
  expect_identical(cp_git(fx$repo, "cat-file", "-e", private_tip)$status, 0L)
})

# --- A5-02 / A5-03: the pre-push guard -----------------------------------------

expect_push_refused <- function(repo, ...) {
  res <- cp_git(repo, "push", "origin", ...)
  testthat::expect_true(res$status != 0L, info = paste(res$out, collapse = "\n"))
  testthat::expect_match(paste(res$out, collapse = "\n"), "clean_publish guard", fixed = TRUE)
}

expect_push_accepted <- function(repo, ...) {
  res <- cp_git(repo, "push", "origin", ...)
  testthat::expect_identical(res$status, 0L, info = paste(res$out, collapse = "\n"))
}

test_that("the hook refuses the private branch and lets the publish branch through", {
  skip_if_no_git()
  fx <- cp_fixture(published = TRUE)

  expect_push_refused(fx$repo, "private-history")
  expect_push_refused(fx$repo, "refs/heads/private-history:refs/heads/elsewhere")
  expect_push_refused(fx$repo, "master:private-history")
  expect_identical(cp_remote_refs(fx), character())

  expect_push_accepted(fx$repo, "master")
  expect_identical(sub(" .*$", "", cp_remote_refs(fx)), "refs/heads/master")
})

test_that("the hook refuses a tag, another branch or --all that would carry private commits (A5-03)", {
  skip_if_no_git()
  fx <- cp_fixture(published = TRUE)
  cp_git(fx$repo, "tag", "v-light", "private-history")
  cp_git(fx$repo, "tag", "-a", "-m", "annotated", "v-annotated", "private-history")
  cp_git(fx$repo, "branch", "other", "private-history")
  cp_git(fx$repo, "checkout", "--quiet", "-b", "ahead")
  cp_commit(fx$repo, "ahead.txt", "x", "ahead")
  cp_git(fx$repo, "checkout", "--quiet", "private-history")

  expect_push_refused(fx$repo, "v-light")
  expect_push_refused(fx$repo, "v-annotated")
  expect_push_refused(fx$repo, "other")
  expect_push_refused(fx$repo, "ahead")
  expect_push_refused(fx$repo, "--all")
  expect_push_refused(fx$repo, "--tags")
  expect_push_refused(fx$repo, paste0(cp_rev(fx$repo, "private-history"), ":refs/heads/by-oid"))
  expect_identical(cp_remote_refs(fx), character())
})

test_that("a tag on the published commit goes through, and so does a branch with new public commits", {
  skip_if_no_git()
  fx <- cp_fixture(published = TRUE)
  cp_git(fx$repo, "tag", "v1", "master")
  cp_git(fx$repo, "checkout", "--quiet", "-b", "hotfix", "master")
  cp_commit(fx$repo, "hotfix.txt", "x", "hotfix")

  for (what in c("v1", "hotfix", "master")) expect_push_accepted(fx$repo, what)
})

test_that("the publish branch is refused once the private branch has been merged into it (A5-03)", {
  skip_if_no_git()
  fx <- cp_fixture(published = TRUE)
  cp_git(fx$repo, "checkout", "--quiet", "master")
  res <- cp_git(fx$repo, "merge", "--quiet", "--allow-unrelated-histories", "-m", "merged", "private-history")
  expect_identical(res$status, 0L, info = paste(res$out, collapse = "\n"))

  expect_push_refused(fx$repo, "master")
  expect_identical(cp_remote_refs(fx), character())
})

test_that("every private branch name ever used stays protected (A5-02 d)", {
  skip_if_no_git()
  fx <- cp_fixture(published = TRUE)
  cp_quiet(clean_publish(repo_root = fx$repo, private_branch = "full-history", push = FALSE))

  expect_setequal(
    cp_git(fx$repo, "config", "--get-all", "cleanpublish.protectedbranch")$out,
    c("private-history", "full-history")
  )
  expect_push_refused(fx$repo, "full-history")
  expect_push_refused(fx$repo, "private-history")
})

test_that("the guard is installed where git reads hooks from when core.hooksPath is set (A5-02 c)", {
  skip_if_no_git()
  fx <- cp_fixture()
  hooks <- file.path(fx$base, "my hooks")
  dir.create(hooks)
  cp_git(fx$repo, "config", "core.hooksPath", hooks)

  cp_first_publish(fx)

  expect_true(file.exists(file.path(hooks, "pre-push")))
  expect_false(file.exists(cp_hook_file(fx$repo)))
  expect_push_refused(fx$repo, "private-history")
})

test_that("a relative core.hooksPath works when the directory is ignored, and is refused when it would join the snapshot", {
  skip_if_no_git()
  fx <- cp_fixture()
  cp_git(fx$repo, "config", "core.hooksPath", ".githooks")
  before <- cp_state(fx$repo)

  expect_error(cp_quiet(clean_publish(repo_root = fx$repo, push = FALSE)), "inside the working tree")
  expect_identical(cp_state(fx$repo), before)
  expect_false(dir.exists(file.path(fx$repo, ".githooks")))

  writeLines(".githooks", file.path(fx$repo, ".git", "info", "exclude"))
  cp_first_publish(fx)
  expect_true(file.exists(file.path(fx$repo, ".githooks", "pre-push")))
  expect_push_refused(fx$repo, "private-history")
  expect_false(any(grepl("githooks", cp_git(fx$repo, "ls-tree", "-r", "--name-only", "master")$out, fixed = TRUE)))
})

test_that("an existing pre-push hook is kept, still runs with its input, and is not duplicated (A5-02 b)", {
  skip_if_no_git()
  fx <- cp_fixture()
  seen <- file.path(fx$base, "seen.txt")
  hook <- cp_hook_file(fx$repo)
  dir.create(dirname(hook), showWarnings = FALSE)
  foreign <- c(
    "#!/bin/sh",
    "# a hook that was here first",
    paste0("while read -r ref oid rref roid; do echo \"$ref\" >> '", seen, "'; done"),
    "exit 0"
  )
  writeLines(foreign, hook)
  Sys.chmod(hook, "0755")

  cp_first_publish(fx)

  lines <- readLines(hook)
  expect_identical(lines[1], "#!/bin/sh")
  expect_true(all(foreign[-1] %in% lines))
  expect_identical(sum(startsWith(lines, "# >>> TempleCBE clean_publish guard")), 1L)
  expect_identical(sum(lines == "# <<< TempleCBE clean_publish guard <<<"), 1L)

  # the block passes the push on to the hook that was there first, input intact
  expect_push_accepted(fx$repo, "master")
  expect_identical(readLines(seen), "refs/heads/master")
  expect_push_refused(fx$repo, "private-history")

  # a second run would rewrite the marked block in place, to the same bytes
  plan <- .cp_plan_hook(hook)
  expect_identical(plan$action, "updated")
  expect_identical(plan$content, paste0(paste(lines, collapse = "\n"), "\n"))
})

test_that("an existing hook that is not a shell script stops the run before anything changes (A5-02 b)", {
  skip_if_no_git()
  fx <- cp_fixture()
  hook <- cp_hook_file(fx$repo)
  dir.create(dirname(hook), showWarnings = FALSE)
  writeLines(c("#!/usr/bin/env python3", "import sys", "sys.exit(0)"), hook)
  original <- readLines(hook)
  before <- cp_state(fx$repo)

  expect_error(cp_quiet(clean_publish(repo_root = fx$repo, push = FALSE)), "not a shell script")

  expect_identical(readLines(hook), original)
  expect_identical(cp_state(fx$repo), before)
})

test_that("the hook an earlier version wrote, LF or CRLF, is upgraded and its branch stays protected", {
  skip_if_no_git()
  legacy <- c(
    "#!/usr/bin/env bash",
    "while read -r local_ref local_oid remote_ref remote_oid; do",
    "    if [[ \"$local_ref\" == *\"old-private\"* ]]; then",
    "        echo \"ERROR: Push aborted! Attempted to push private branch '$local_ref' to remote.\" >&2",
    "        exit 1",
    "    fi",
    "done",
    "exit 0"
  )
  fx <- cp_fixture()
  hook <- cp_hook_file(fx$repo)
  dir.create(dirname(hook), showWarnings = FALSE)
  # what R's writeLines() made on Windows
  writeBin(charToRaw(paste0(paste(legacy, collapse = "\r\n"), "\r\n")), hook)
  cp_git(fx$repo, "branch", "old-private")

  cp_first_publish(fx)

  lines <- readLines(hook)
  expect_false(any(grepl("Attempted to push", lines, fixed = TRUE)))
  expect_identical(sum(startsWith(lines, "# >>> TempleCBE clean_publish guard")), 1L)
  expect_setequal(
    cp_git(fx$repo, "config", "--get-all", "cleanpublish.protectedbranch")$out,
    c("private-history", "old-private")
  )
  expect_push_refused(fx$repo, "old-private")
  expect_push_refused(fx$repo, "private-history")

  # the same hook with Unix line endings is recognised too
  unix <- tempfile()
  on.exit(unlink(unix), add = TRUE)
  writeBin(charToRaw(paste0(paste(legacy, collapse = "\n"), "\n")), unix)
  plan <- .cp_plan_hook(unix)
  expect_identical(plan$action, "upgraded")
  expect_identical(plan$legacy_names, "old-private")
})

test_that("in a linked worktree the hook is written where git reads it (A5-02 a)", {
  skip_if_no_git()
  fx <- cp_fixture()
  wt <- file.path(fx$base, "wt")
  expect_identical(cp_git(fx$repo, "worktree", "add", "--quiet", "-b", "dev", wt)$status, 0L)

  cp_quiet(clean_publish(repo_root = wt, publish_branch = "pub", push = FALSE))

  expect_true(file.exists(cp_hook_file(fx$repo)))
  expect_false(file.exists(file.path(fx$repo, ".git", "worktrees", "wt", "hooks", "pre-push")))
  expect_push_refused(wt, "private-history")
  expect_push_refused(fx$repo, "private-history")
  expect_identical(cp_current(wt), "private-history")
  expect_identical(cp_current(fx$repo), "master")
})

test_that("a publish branch checked out in another worktree is refused before anything changes", {
  skip_if_no_git()
  fx <- cp_fixture()
  wt <- file.path(fx$base, "wt")
  cp_git(fx$repo, "worktree", "add", "--quiet", "-b", "dev", wt)
  cp_commit(wt, "dev.txt", "x", "dev1")
  before <- cp_state(wt)

  # the default publish branch is master, which the main checkout has checked out
  expect_error(cp_quiet(clean_publish(repo_root = wt, push = FALSE)), "checked out in another git worktree")

  expect_identical(cp_state(wt), before)
  expect_identical(cp_current(wt), "dev")
  expect_identical(cp_ncommits(fx$repo, "master"), 3L)
  expect_true(is.na(cp_rev(fx$repo, "private-history")))
})

test_that("a private branch checked out in another worktree is refused before anything changes", {
  skip_if_no_git()
  fx <- cp_fixture(published = TRUE)
  # the main checkout moves to a branch that contains private-history, which a second worktree takes over
  cp_git(fx$repo, "checkout", "--quiet", "-b", "feature")
  wt <- file.path(fx$base, "wt")
  expect_identical(cp_git(fx$repo, "worktree", "add", "--quiet", wt, "private-history")$status, 0L)
  before <- cp_state(fx$repo)

  expect_error(
    cp_quiet(clean_publish(repo_root = fx$repo, push = FALSE)),
    "'private-history' is checked out in another git worktree"
  )
  expect_identical(cp_state(fx$repo), before)
})

# --- pre-flight and failure behaviour -----------------------------------------

test_that("a dirty working tree is refused, ignored files are not a problem and are not published", {
  skip_if_no_git()
  fx <- cp_fixture()
  writeLines("*.log", file.path(fx$repo, ".gitignore"))
  cp_git(fx$repo, "add", ".gitignore")
  cp_git(fx$repo, "commit", "--quiet", "-m", "ignore")
  writeLines("untracked", file.path(fx$repo, "scratch.txt"))
  before <- cp_state(fx$repo)

  expect_error(cp_quiet(clean_publish(repo_root = fx$repo, push = FALSE)), "uncommitted changes")
  expect_identical(cp_state(fx$repo), before)

  file.remove(file.path(fx$repo, "scratch.txt"))
  writeLines("noise", file.path(fx$repo, "debug.log"))
  cp_first_publish(fx)
  expect_false("debug.log" %in% cp_git(fx$repo, "ls-tree", "-r", "--name-only", "master")$out)
  expect_true(file.exists(file.path(fx$repo, "debug.log")))
})

test_that("a repository path with a space works", {
  skip_if_no_git()
  fx <- cp_fixture(repo_name = "my repo", remote_name = "remote repo.git")
  expect_true(grepl(" ", fx$repo, fixed = TRUE))

  clean <- cp_quiet(clean_publish(repo_root = fx$repo, remote = "origin"))

  expect_identical(cp_remote_refs(fx), paste("refs/heads/master", clean))
  # the push URL of the remote is disarmed after the push (stage 2), so the hook is not what stops this
  res <- cp_git(fx$repo, "push", "origin", "private-history")
  expect_true(res$status != 0L)
  expect_identical(cp_remote_refs(fx), paste("refs/heads/master", clean))
})

test_that("a failed push leaves the local branches and the checked-out branch as they were", {
  skip_if_no_git()
  fx <- cp_fixture()
  cp_reject_pushes(fx)
  master_tip <- cp_rev(fx$repo, "master")

  expect_error(cp_quiet(clean_publish(repo_root = fx$repo, remote = "origin")), "push")

  expect_identical(cp_current(fx$repo), "master")
  expect_identical(cp_rev(fx$repo, "master"), master_tip)
  expect_true(is.na(cp_rev(fx$repo, "private-history")))
  expect_identical(cp_remote_refs(fx), character())
  # a push that failed does not disarm the remote
  expect_identical(cp_git(fx$repo, "config", "--get", "remote.origin.pushurl")$status, 1L)
})

test_that("a remote whose refs cannot be listed stops the run before anything is written (CPD-03 c, CPC-10)", {
  skip_if_no_git()
  fx <- cp_fixture()
  cp_git(fx$repo, "remote", "add", "broken", file.path(fx$base, "missing.git"))
  before <- cp_state(fx$repo)

  expect_error(cp_quiet(clean_publish(repo_root = fx$repo, remote = "broken")), "Could not list the refs.*Nothing was changed")

  expect_identical(cp_state(fx$repo), before)
  expect_false(file.exists(cp_hook_file(fx$repo)))
})

test_that("a failure after the checkout puts the original branch back", {
  skip_if_no_git()
  fx <- cp_fixture(published = TRUE)
  cp_git(fx$repo, "checkout", "--quiet", "-b", "feature")
  cp_commit(fx$repo, "feature.txt", "x", "feat")
  real_git <- .cp_git
  testthat::local_mocked_bindings(
    .cp_git = function(repo_root, args, error_ok = FALSE) {
      if (identical(args[1:2], c("branch", "-f")) && identical(args[3], "master")) stop("simulated failure")
      real_git(repo_root, args, error_ok = error_ok)
    }
  )

  msgs <- testthat::capture_messages(
    expect_error(clean_publish(repo_root = fx$repo, push = FALSE), "simulated failure")
  )

  expect_identical(cp_current(fx$repo), "feature")
  expect_match(msgs, "[RESTORE]", fixed = TRUE, all = FALSE)
})

test_that("a detached HEAD that contains the private branch can be published from", {
  skip_if_no_git()
  fx <- cp_fixture(published = TRUE)
  cp_git(fx$repo, "checkout", "--quiet", "--detach", "private-history")
  cp_commit(fx$repo, "detached.txt", "x", "det")
  tip <- cp_rev(fx$repo, "HEAD")

  cp_quiet(clean_publish(repo_root = fx$repo, push = FALSE))

  expect_identical(cp_rev(fx$repo, "private-history"), tip)
  expect_identical(cp_current(fx$repo), "private-history")
})

test_that("the run stops when the guard cannot be shown to work (A5-02)", {
  skip_if_no_git()
  fx <- cp_fixture()
  testthat::local_mocked_bindings(.cp_write_hook = function(plan) invisible(NULL))
  before <- cp_state(fx$repo)

  expect_error(
    cp_quiet(clean_publish(repo_root = fx$repo, push = FALSE)),
    "pre-push guard does not run"
  )

  heads <- function(s) s$refs[startsWith(s$refs, "refs/heads/")]
  expect_identical(heads(cp_state(fx$repo)), heads(before))
  expect_identical(cp_current(fx$repo), "master")
})

test_that("a hook that refuses for another reason is an error before anything moves, unless confirm_guard = FALSE (CPC-01)", {
  skip_if_no_git()
  fx <- cp_fixture()
  testthat::local_mocked_bindings(
    .cp_write_hook = function(plan) {
      writeLines(c("#!/bin/sh", "exit 1"), plan$path)
      Sys.chmod(plan$path, "0755")
    }
  )
  before <- cp_state(fx$repo)

  expect_error(
    cp_quiet(clean_publish(repo_root = fx$repo, push = FALSE)),
    "Could not confirm that the push guard works.*confirm_guard = FALSE"
  )
  heads <- function(s) s$refs[startsWith(s$refs, "refs/heads/")]
  expect_identical(heads(cp_state(fx$repo)), heads(before))
  expect_identical(cp_current(fx$repo), "master")
  expect_true(is.na(cp_rev(fx$repo, "private-history")))

  # the explicit opt-out goes on with a warning
  expect_warning(
    cp_quiet(clean_publish(repo_root = fx$repo, push = FALSE, confirm_guard = FALSE)),
    "Could not confirm"
  )
  expect_identical(cp_ncommits(fx$repo, "master"), 1L)
})

test_that("bad arguments, an unknown remote and a bad repository are refused before anything changes", {
  skip_if_no_git()
  fx <- cp_fixture(published = TRUE)
  before <- cp_state(fx$repo)

  expect_error(clean_publish(repo_root = fx$repo, private_branch = "-x"), "does not start with '-'")
  expect_error(clean_publish(repo_root = fx$repo, remote = c("a", "b")), "`remote` must be")
  expect_error(clean_publish(repo_root = fx$repo, push = "yes"), "TRUE or FALSE")
  expect_error(clean_publish(repo_root = fx$repo, push = NA), "TRUE or FALSE")
  expect_error(cp_quiet(clean_publish(repo_root = fx$repo, private_branch = "bad name", push = FALSE)), "not a valid branch name")
  expect_error(cp_quiet(clean_publish(repo_root = fx$repo, remote = "nope")), "not configured")
  # a protected private branch can not be chosen as the publish branch
  expect_error(
    cp_quiet(clean_publish(repo_root = fx$repo, publish_branch = "private-history", private_branch = "release", push = FALSE)),
    "protected private branch"
  )
  expect_identical(cp_state(fx$repo), before)

  not_git <- normalizePath(withr::local_tempdir(), winslash = "/")
  withr::local_envvar(c(GIT_CEILING_DIRECTORIES = dirname(not_git)))
  expect_error(cp_quiet(clean_publish(repo_root = not_git, push = FALSE)), "not inside a git working tree")

  empty <- file.path(fx$base, "empty")
  dir.create(empty)
  cp_git(empty, "init", "--quiet")
  expect_error(cp_quiet(clean_publish(repo_root = empty, push = FALSE)), "no commits")
})

test_that("the hook block is valid POSIX sh", {
  skip_if_no_git()
  shells <- cp_posix_shells()
  skip_if(!length(shells), "no POSIX shell found")
  block <- tempfile(fileext = ".sh")
  on.exit(unlink(block), add = TRUE)
  writeBin(charToRaw(paste0(paste(c("#!/bin/sh", .cp_hook_block()), collapse = "\n"), "\n")), block)
  for (shell in shells) {
    status <- suppressWarnings(system2(shell, shQuote(c("-n", block)), stdout = FALSE, stderr = FALSE))
    expect_identical(status, 0L, info = shell)
  }
})

# ------------------------------------------------------------------------------
# Command line of scripts/clean_publish.R (A5-04)
# ------------------------------------------------------------------------------

test_that("the command line defaults to a dry run and needs --push to publish", {
  parsed <- parse_clean_publish_args(character())
  expect_false(parsed$push)
  expect_false(parsed$help)
  expect_null(parsed$publish_branch)
  expect_null(parsed$private_branch)
  expect_null(parsed$remote)
  expect_null(parsed$commit_msg)

  expect_true(parse_clean_publish_args("--push")$push)
  expect_false(parse_clean_publish_args("--no-push")$push)
  expect_true(parse_clean_publish_args("-h")$help)
  expect_true(parse_clean_publish_args("--help")$help)
})

test_that("every option takes its value as '--flag value' or '--flag=value'", {
  spaced <- parse_clean_publish_args(c(
    "--branch", "main", "--private", "history", "--remote", "backup", "-m", "Release one", "--push"
  ))
  equals <- parse_clean_publish_args(c(
    "--branch=main", "--private=history", "--remote=backup", "--message=Release one", "--push"
  ))
  expect_identical(spaced, equals)
  expect_identical(spaced$publish_branch, "main")
  expect_identical(spaced$private_branch, "history")
  expect_identical(spaced$remote, "backup")
  expect_identical(spaced$commit_msg, "Release one")
  expect_true(spaced$push)
  expect_identical(parse_clean_publish_args(c("--message", "Release one"))$commit_msg, "Release one")
  # a message may start with '-' in the --message=value form
  expect_identical(parse_clean_publish_args("--message=- tidy")$commit_msg, "- tidy")
})

test_that("an unknown or mistyped option is an error that lists the accepted options", {
  for (bad in c("--nopush", "--dry-run", "--no_push", "--remote-name", "-x", "--PUSH", "--push=yes", "-m=text", "--force")) {
    err <- tryCatch(parse_clean_publish_args(bad), error = function(e) conditionMessage(e))
    expect_type(err, "character")
    expect_match(err, paste0("Unknown option '", bad, "'"), fixed = TRUE, info = bad)
    expect_match(err, "--push", fixed = TRUE, info = bad)
    expect_match(err, "--no-push", fixed = TRUE, info = bad)
    expect_match(err, "--remote", fixed = TRUE, info = bad)
  }
  expect_error(parse_clean_publish_args(c("--push", "--nopush")), "Unknown option")
})

test_that("a missing value, or a flag where the value should be, is an error", {
  expect_error(parse_clean_publish_args("--remote"), "needs a value")
  expect_error(parse_clean_publish_args(c("--remote", "--no-push")), "needs a value")
  expect_error(parse_clean_publish_args(c("--branch", "--push")), "needs a value")
  expect_error(parse_clean_publish_args(c("-m", "--push")), "needs a value")
  expect_error(parse_clean_publish_args(c("--remote", "")), "needs a value")
  expect_error(parse_clean_publish_args("--remote="), "needs a value")
  expect_error(parse_clean_publish_args("--remote=-x"), "must not start with '-'")
  expect_error(parse_clean_publish_args("--branch=-x"), "must not start with '-'")
})

test_that("stray arguments, repeated options and contradictions are errors", {
  expect_error(parse_clean_publish_args("origin"), "Unexpected argument 'origin'")
  expect_error(parse_clean_publish_args(c("--push", "now")), "Unexpected argument 'now'")
  expect_error(parse_clean_publish_args(c("--remote", "a", "--remote", "b")), "more than once")
  expect_error(parse_clean_publish_args(c("-m", "a", "--message=b")), "more than once")
  expect_error(parse_clean_publish_args(c("--push", "--push")), "more than once")
  expect_error(parse_clean_publish_args(c("--push", "--no-push")), "contradict")
})

# The script itself, run in a subprocess in a throwaway repository: a mistyped
# flag must not push, the default must not push, and only --push may.
run_clean_publish_script <- function(args, repo) {
  script <- normalizePath(testthat::test_path("..", "..", "scripts", "clean_publish.R"), winslash = "/", mustWork = FALSE)
  if (!file.exists(script) || !file.exists(file.path(dirname(dirname(script)), "DESCRIPTION"))) {
    testthat::skip("scripts/clean_publish.R is not available (not running from the source tree)")
  }
  testthat::skip_if_not_installed("pkgload")
  withr::with_dir(repo, withr::with_envvar(
    c(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep)),
    {
      out <- suppressWarnings(system2(
        file.path(R.home("bin"), "Rscript"), shQuote(c("--vanilla", script, args)),
        stdout = TRUE, stderr = TRUE
      ))
      list(out = out, status = attr(out, "status") %||% 0L)
    }
  ))
}

test_that("scripts/clean_publish.R refuses a mistyped flag instead of force-pushing (A5-04)", {
  skip_if_no_git()
  fx <- cp_fixture()
  before <- cp_state(fx$repo)

  res <- run_clean_publish_script("--nopush", fx$repo)

  expect_identical(res$status, 2L, info = paste(res$out, collapse = "\n"))
  expect_match(paste(res$out, collapse = "\n"), "Unknown option '--nopush'", fixed = TRUE)
  expect_match(paste(res$out, collapse = "\n"), "--push", fixed = TRUE)
  expect_identical(cp_remote_refs(fx), character())
  expect_identical(cp_state(fx$repo), before)
})

test_that("scripts/clean_publish.R is a dry run by default and publishes only with --push (A5-04)", {
  skip_if_no_git()
  fx <- cp_fixture()

  dry <- run_clean_publish_script(character(), fx$repo)
  expect_identical(dry$status, 0L, info = paste(dry$out, collapse = "\n"))
  expect_match(paste(dry$out, collapse = "\n"), "Dry run", fixed = TRUE)
  expect_identical(cp_remote_refs(fx), character())
  expect_identical(cp_ncommits(fx$repo, "master"), 1L)
  expect_identical(cp_ncommits(fx$repo, "private-history"), 3L)

  pushed <- run_clean_publish_script(c("--push", "--remote", "origin", "-m", "Release one"), fx$repo)
  expect_identical(pushed$status, 0L, info = paste(pushed$out, collapse = "\n"))
  out <- paste(pushed$out, collapse = "\n")
  expect_match(out, "FORCE-PUSHED to remote 'origin'", fixed = TRUE)
  expect_match(out, paste0("Push URL of 'origin': ", fx$remote), fixed = TRUE)
  expect_identical(sub(" .*$", "", cp_remote_refs(fx)), "refs/heads/master")
  expect_identical(cp_ncommits(fx$remote, "master"), 1L)
  expect_identical(cp_git(fx$remote, "log", "-1", "--format=%s", "master")$out, "Release one")
})

test_that("guard_public_remote() blocks direct push to public remote, while clean_publish() succeeds (C5)", {
  skip_if_no_git()
  fx <- cp_fixture()

  guard_public_remote(fx$remote, repo_root = fx$repo)
  pub_urls <- TempleCBE:::.cp_config_get_all(fx$repo, "cleanpublish.publicurl")
  expect_true(length(pub_urls) >= 1L)

  res_direct <- cp_git(fx$repo, "push", "origin", "master")
  expect_true(res_direct$status != 0L)
  expect_true(any(grepl("direct push to public remote .* is guarded", res_direct$out)))

  clean <- clean_publish(repo_root = fx$repo, remote = "origin", push = TRUE)
  expect_identical(cp_ncommits(fx$remote, "master"), 1L)
  expect_identical(cp_git(fx$remote, "rev-parse", "master")$out, clean)
})

test_that("clean_publish() rejects tracked-but-ignored files unless in allow_ignored (A5-15)", {
  skip_if_no_git()
  fx <- cp_fixture()
  writeLines("ignored_secret.txt", file.path(fx$repo, ".gitignore"))
  cp_git(fx$repo, "add", ".gitignore")
  cp_git(fx$repo, "commit", "-m", "add gitignore")
  writeLines("SECRET_DATA", file.path(fx$repo, "ignored_secret.txt"))
  cp_git(fx$repo, "add", "-f", "ignored_secret.txt")
  cp_git(fx$repo, "commit", "-m", "track ignored secret")

  expect_error(
    clean_publish(repo_root = fx$repo, push = FALSE),
    "Found tracked files that match .gitignore patterns"
  )

  clean_allowed <- clean_publish(repo_root = fx$repo, push = FALSE, allow_ignored = "ignored_secret.txt")
  expect_true(is.character(clean_allowed) && nzchar(clean_allowed))
})


test_that("snapshot mode validates remote branch, rejects identical tree, and fast-forwards (C5)", {
  skip_if_no_git()
  fx <- cp_fixture()

  # Error when remote publish branch does not exist yet
  expect_error(
    clean_publish(repo_root = fx$repo, remote = "origin", mode = "snapshot", push = TRUE),
    "Snapshot mode requires an existing remote branch to build upon"
  )

  # Initial publish via standalone mode
  clean1 <- clean_publish(repo_root = fx$repo, remote = "origin", mode = "standalone", push = TRUE)
  expect_identical(cp_ncommits(fx$remote, "master"), 1L)

  # Error when working tree is unchanged
  expect_error(
    clean_publish(repo_root = fx$repo, remote = "origin", mode = "snapshot", push = TRUE),
    "The tree of the current commit is identical"
  )

  # Add a new commit to private-history and snapshot publish
  cp_commit(fx$repo, "snapshot_feature.txt", "v2 content", "add snapshot feature")
  clean2 <- clean_publish(repo_root = fx$repo, remote = "origin", mode = "snapshot", push = TRUE, commit_msg = "Snapshot v2")

  expect_identical(cp_ncommits(fx$remote, "master"), 2L)
  expect_identical(cp_git(fx$remote, "rev-parse", "master~1")$out, clean1)
  expect_identical(cp_git(fx$remote, "rev-parse", "master")$out, clean2)
})

test_that("scripts/clean_publish.R supports --fence and --snapshot (C5)", {
  skip_if_no_git()
  fx <- cp_fixture()

  fence_res <- run_clean_publish_script(c("--fence", fx$remote), fx$repo)
  expect_identical(fence_res$status, 0L, info = paste(fence_res$out, collapse = "\n"))
  expect_match(paste(fence_res$out, collapse = "\n"), "[GUARD] Guarded public remote", fixed = TRUE)

  # Direct push is blocked by the fence
  res_direct <- cp_git(fx$repo, "push", "origin", "master")
  expect_true(res_direct$status != 0L)

  # Standalone push via script succeeds
  pushed1 <- run_clean_publish_script(c("--push", "--remote", "origin", "-m", "V1 release"), fx$repo)
  expect_identical(pushed1$status, 0L)
  expect_identical(cp_ncommits(fx$remote, "master"), 1L)

  # Make a change and snapshot push via script
  cp_commit(fx$repo, "cli_v2.txt", "cli v2", "commit for v2")
  pushed2 <- run_clean_publish_script(c("--snapshot", "--push", "--remote", "origin", "-m", "V2 snapshot"), fx$repo)
  expect_identical(pushed2$status, 0L, info = paste(pushed2$out, collapse = "\n"))
  expect_match(paste(pushed2$out, collapse = "\n"), "fast-forward pushed to remote 'origin'", fixed = TRUE)
  expect_identical(cp_ncommits(fx$remote, "master"), 2L)
})


# ---- Findings of the pre-0.5.0 review (R-CP) --------------------------------------------------

test_that("there is no default remote: pushing or snapshotting without one stops before anything changes", {
  skip_if_no_git()
  fx <- cp_fixture()
  before <- cp_state(fx$repo)
  # `origin` of a clone of the private repository is the private repository, and the default
  # used to force-push one commit over its branch (3 commits became 1)
  expect_error(clean_publish(repo_root = fx$repo), "Name the remote to publish to")
  expect_error(clean_publish(repo_root = fx$repo, push = TRUE), "no default")
  expect_error(clean_publish(repo_root = fx$repo, mode = "snapshot", push = FALSE), "Name the remote")
  expect_identical(cp_state(fx$repo), before)
  expect_identical(cp_ncommits(fx$remote, "master"), NA_integer_) # nothing reached the remote

  # a dry run of a standalone publish needs no remote
  cp_quiet(clean_publish(repo_root = fx$repo, push = FALSE))
  expect_identical(cp_ncommits(fx$repo, "master"), 1L)
})


test_that("snapshot mode still builds on a previous snapshot", {
  skip_if_no_git()
  fx <- cp_fixture()
  clean1 <- cp_quiet(clean_publish(repo_root = fx$repo, remote = "origin", mode = "standalone", push = TRUE))
  cp_commit(fx$repo, "v2.txt", "v2", "second release content")
  clean2 <- cp_quiet(clean_publish(repo_root = fx$repo, remote = "origin", mode = "snapshot", push = TRUE))
  expect_identical(cp_git(fx$remote, "rev-parse", "master~1")$out, clean1)
  # the default message of a snapshot is not "Initial clean commit"
  expect_match(cp_git(fx$remote, "log", "-1", "--format=%s", "master")$out, "^Snapshot [0-9]{4}-[0-9]{2}-[0-9]{2}$")
  expect_identical(cp_git(fx$remote, "log", "--format=%s", "master~1")$out[1], "Initial clean commit")
})

test_that("the tracked-but-ignored check sees the whole work tree when started from a subdirectory", {
  skip_if_no_git()
  fx <- cp_fixture()
  writeLines("secret", file.path(fx$repo, "secret.txt"))
  writeLines("secret.txt", file.path(fx$repo, ".gitignore"))
  expect_identical(cp_git(fx$repo, "add", "-f", "secret.txt", ".gitignore")$status, 0L)
  expect_identical(cp_git(fx$repo, "commit", "--quiet", "-m", "force-added secret")$status, 0L)
  dir.create(file.path(fx$repo, "sub"))
  # from the subdirectory `git ls-files` lists only that directory: the file at the top went unreported
  expect_error(
    cp_quiet(clean_publish(repo_root = file.path(fx$repo, "sub"), push = FALSE)),
    "secret.txt"
  )
  expect_identical(cp_rev(fx$repo, "private-history"), NA_character_)
})

test_that("guard_public_remote() refuses a URL with whitespace, which the hook could not match", {
  skip_if_no_git()
  fx <- cp_fixture()
  expect_error(guard_public_remote("C:/Users/First Last/public.git", repo_root = fx$repo), "whitespace")
  expect_length(TempleCBE:::.cp_config_get_all(fx$repo, "cleanpublish.publicurl"), 0L)
})

test_that("scripts/clean_publish.R needs --remote for --push and --snapshot, and --fence takes no other option", {
  skip_if_no_git()
  fx <- cp_fixture()
  no_remote <- run_clean_publish_script("--push", fx$repo)
  expect_identical(no_remote$status, 2L)
  expect_match(paste(no_remote$out, collapse = "\n"), "name the remote to publish to with --remote", fixed = TRUE)
  expect_identical(cp_ncommits(fx$remote, "master"), NA_integer_)

  mixed <- run_clean_publish_script(c("--fence", fx$remote, "--push"), fx$repo)
  expect_identical(mixed$status, 2L)
  expect_match(paste(mixed$out, collapse = "\n"), "cannot be combined with --push", fixed = TRUE)
  expect_length(TempleCBE:::.cp_config_get_all(fx$repo, "cleanpublish.publicurl"), 0L)
})


# ---- L3 review, stage 1: the pre-push hook, the fence and the self-test -------------------------
#
# The hook protects private commits by commit id: the live tips of the protected branch names and
# every private tip that clean_publish() recorded (cleanpublish.privatetip) before it pushed or
# moved anything. Every test below pushes with real git to a local bare repository, or runs the
# generated hook by hand under the POSIX shells that are installed.

expect_push_refused_with <- function(repo, pattern, ...) {
  res <- cp_git(repo, "push", "origin", ...)
  testthat::expect_true(res$status != 0L, info = paste(res$out, collapse = "\n"))
  testthat::expect_match(paste(res$out, collapse = "\n"), pattern)
}

cp_private_tips <- function(repo) cp_git(repo, "config", "--get-all", "cleanpublish.privatetip")$out

test_that("every private tip is recorded as a commit id, and clean commits are not (CPB-01, CPB-02)", {
  skip_if_no_git()
  fx <- cp_fixture()
  tip_1 <- cp_rev(fx$repo, "master")
  expect_length(cp_private_tips(fx$repo), 0L)

  clean_1 <- cp_first_publish(fx)
  # no private branch yet: the commit that is published is the old master
  expect_identical(cp_private_tips(fx$repo), tip_1)

  cp_commit(fx$repo, "later.txt", "x", "later")
  tip_2 <- cp_rev(fx$repo, "private-history")
  clean_2 <- cp_first_publish(fx)
  # the new head is added; the old private tip is not repeated and the old master is a clean commit
  expect_setequal(cp_private_tips(fx$repo), c(tip_1, tip_2))
  expect_false(any(c(clean_1, clean_2) %in% cp_private_tips(fx$repo)))
  cp_first_publish(fx)
  expect_length(cp_private_tips(fx$repo), 2L)
})

test_that("a failed first publish leaves the private history protected: a plain push of master is refused (CPB-01)", {
  skip_if_no_git()
  fx <- cp_fixture()
  cp_reject_pushes(fx)
  tip <- cp_rev(fx$repo, "master")

  expect_error(cp_quiet(clean_publish(repo_root = fx$repo, remote = "origin")), "push")

  # the private branch was never created and master still holds the whole history ...
  expect_true(is.na(cp_rev(fx$repo, "private-history")))
  expect_identical(cp_ncommits(fx$repo, "master"), 3L)
  # ... but the tip had been recorded before the push was tried
  expect_identical(cp_private_tips(fx$repo), tip)
  expect_push_refused(fx$repo, "master")
  expect_push_refused(fx$repo, "HEAD:refs/heads/other")
  expect_push_refused(fx$repo, "--all")
  expect_identical(cp_remote_refs(fx), character())
})

test_that("renaming the private branch does not disable the guard (CPB-02, CPC-03, CPA-04, CPD-02)", {
  skip_if_no_git()
  fx <- cp_fixture(published = TRUE)
  old <- cp_rev(fx$repo, "private-history~1")
  cp_git(fx$repo, "branch", "-m", "private-history", "archive")

  expect_push_refused(fx$repo, "archive:refs/heads/oops")
  expect_push_refused(fx$repo, "HEAD:refs/heads/oops2")
  expect_push_refused(fx$repo, paste0(old, ":refs/heads/by-oid"))
  expect_push_refused(fx$repo, "archive")
  expect_identical(cp_remote_refs(fx), character())
  expect_push_accepted(fx$repo, "master")
})

test_that("deleting the private branch does not disable the guard: ids, tags, backup refs and --mirror stay refused (CPB-02, CPC-03, CPD-02)", {
  skip_if_no_git()
  fx <- cp_fixture(published = TRUE)
  tip <- cp_rev(fx$repo, "private-history")
  old <- cp_rev(fx$repo, "private-history~1")
  backup <- sub(" .*$", "", cp_backup_refs(fx$repo))
  expect_length(backup, 1L)
  cp_git(fx$repo, "checkout", "--quiet", "master")
  expect_identical(cp_git(fx$repo, "branch", "-D", "private-history")$status, 0L)
  cp_git(fx$repo, "tag", "v-private", old)
  cp_git(fx$repo, "tag", "-a", "-m", "annotated", "v-annotated", tip)

  expect_push_refused(fx$repo, paste0(backup, ":refs/heads/from-backup"))
  expect_push_refused(fx$repo, paste0(tip, ":refs/heads/by-oid"))
  expect_push_refused(fx$repo, "v-private")
  expect_push_refused(fx$repo, "v-annotated")
  expect_push_refused(fx$repo, "--tags")
  expect_push_refused(fx$repo, "--mirror")
  expect_identical(cp_remote_refs(fx), character())
  expect_push_accepted(fx$repo, "master")
})

test_that("moving the private branch onto a clean commit does not disable the guard (CPB-02)", {
  skip_if_no_git()
  fx <- cp_fixture(published = TRUE)
  tip <- cp_rev(fx$repo, "private-history")
  old <- cp_rev(fx$repo, "private-history~1")
  backup <- sub(" .*$", "", cp_backup_refs(fx$repo))
  cp_git(fx$repo, "checkout", "--quiet", "master")
  expect_identical(cp_git(fx$repo, "branch", "-f", "private-history", "master")$status, 0L)
  cp_git(fx$repo, "tag", "v-private", old)

  expect_push_refused(fx$repo, paste0(tip, ":refs/heads/by-oid"))
  expect_push_refused(fx$repo, paste0(old, ":refs/heads/by-oid"))
  expect_push_refused(fx$repo, paste0(backup, ":refs/heads/from-backup"))
  expect_push_refused(fx$repo, "v-private")
  expect_push_refused(fx$repo, "--mirror")
  expect_identical(cp_remote_refs(fx), character())
  expect_push_accepted(fx$repo, "master")
})

test_that("with protected branches configured and no tip to be found, only clean commits go through, and the message says how to release it (CPD-02)", {
  skip_if_no_git()
  fx <- cp_fixture(published = TRUE)
  tip <- cp_rev(fx$repo, "private-history")
  cp_git(fx$repo, "branch", "scratch", tip)
  cp_git(fx$repo, "checkout", "--quiet", "master")
  cp_git(fx$repo, "branch", "-D", "private-history")
  # what a repository looks like that was configured before the tips were recorded
  cp_git(fx$repo, "config", "--unset-all", "cleanpublish.privatetip")

  expect_push_refused_with(fx$repo, "git config --unset-all cleanpublish.protectedbranch", "scratch")
  expect_push_refused(fx$repo, "--mirror")
  expect_identical(cp_remote_refs(fx), character())
  expect_push_accepted(fx$repo, "master")

  # releasing the guard is possible, and deliberate
  cp_git(fx$repo, "config", "--unset-all", "cleanpublish.protectedbranch")
  expect_push_accepted(fx$repo, "scratch")
})

test_that("a ref that does not lead to a commit is refused unless it is part of a clean commit (CPB-07)", {
  skip_if_no_git()
  fx <- cp_fixture(published = TRUE)
  old_tree <- cp_git(fx$repo, "rev-parse", "private-history~2^{tree}")$out
  clean_tree <- cp_git(fx$repo, "rev-parse", "master^{tree}")$out
  clean_blob <- cp_git(fx$repo, "rev-parse", "master:f1.txt")$out
  secret <- file.path(fx$base, "secret.txt")
  writeLines("STUDY-ID-1234", secret)
  secret_blob <- cp_git(fx$repo, "hash-object", "-w", secret)$out
  cp_git(fx$repo, "tag", "-a", "-m", "on a tree", "tag-on-tree", clean_tree)
  cp_git(fx$repo, "tag", "-a", "-m", "on a blob", "tag-on-blob", secret_blob)

  expect_push_refused(fx$repo, paste0(old_tree, ":refs/tags/old-tree"))
  expect_push_refused(fx$repo, paste0(secret_blob, ":refs/tags/secret-blob"))
  # an annotated tag is never part of a commit, whatever it points at
  expect_push_refused(fx$repo, "tag-on-tree")
  expect_push_refused(fx$repo, "tag-on-blob")
  expect_identical(cp_remote_refs(fx), character())

  expect_push_accepted(fx$repo, paste0(clean_tree, ":refs/tags/clean-tree"))
  expect_push_accepted(fx$repo, paste0(clean_blob, ":refs/tags/clean-blob"))
})

test_that("clean commits and private tips are used only as full object ids (CPB-06)", {
  skip_if_no_git()
  fx <- cp_fixture(published = TRUE)
  tip <- cp_rev(fx$repo, "private-history")
  old <- cp_rev(fx$repo, "private-history~1")
  cp_git(fx$repo, "tag", "-a", "-m", "annotated", "v-annotated", tip)
  annotated <- cp_git(fx$repo, "rev-parse", "v-annotated")$out
  # each of these names the private history, but none is a full commit id: used as a revision
  # (it was) every one of them made the whole private history count as a clean commit
  for (bad in c("private-history", "HEAD", "master", paste0(tip, "^{commit}"), substr(tip, 1L, 12L), toupper(tip), annotated)) {
    cp_git(fx$repo, "config", "--add", "cleanpublish.cleancommit", bad)
  }

  expect_push_refused(fx$repo, paste0(old, ":refs/heads/by-oid"))
  expect_push_refused(fx$repo, paste0(tip, ":refs/heads/by-oid"))
  expect_push_refused(fx$repo, "v-annotated")
  expect_identical(cp_remote_refs(fx), character())
  expect_push_accepted(fx$repo, "master")
})

test_that("a stale or mistyped private tip is ignored with a note and disables nothing (CPB-06)", {
  skip_if_no_git()
  fx <- cp_fixture(published = TRUE)
  old <- cp_rev(fx$repo, "private-history~1")
  cp_git(fx$repo, "config", "--add", "cleanpublish.privatetip", "not-an-id")
  cp_git(fx$repo, "config", "--add", "cleanpublish.privatetip", strrep("a", 40L))

  res <- cp_git(fx$repo, "push", "origin", paste0(old, ":refs/heads/by-oid"))
  expect_true(res$status != 0L)
  expect_match(paste(res$out, collapse = "\n"), "contains commits", fixed = TRUE)
  expect_match(paste(res$out, collapse = "\n"), "ignoring cleanpublish.privatetip 'not-an-id'", fixed = TRUE)
  expect_match(paste(res$out, collapse = "\n"), paste0("ignoring cleanpublish.privatetip '", strrep("a", 40L), "'"), fixed = TRUE)

  res <- cp_git(fx$repo, "push", "origin", "master")
  expect_identical(res$status, 0L, info = paste(res$out, collapse = "\n"))
})

test_that("TEMPLECBE_PUBLISHING=1 lets through only recorded clean commits, and bypasses the fence rule alone (CPD-04)", {
  skip_if_no_git()
  fx <- cp_fixture(published = TRUE)
  old <- cp_rev(fx$repo, "private-history~1")
  cp_quiet(guard_public_remote(fx$remote, repo_root = fx$repo))
  cp_git(fx$repo, "checkout", "--quiet", "-b", "hotfix", "master")
  cp_commit(fx$repo, "hotfix.txt", "x", "hotfix")
  cp_git(fx$repo, "checkout", "--quiet", "private-history")

  # without the variable the fence refuses everything, hotfix included
  expect_push_refused_with(fx$repo, "is guarded", "master")
  expect_push_refused_with(fx$repo, "is guarded", "hotfix")

  withr::local_envvar(TEMPLECBE_PUBLISHING = "1")
  expect_push_accepted(fx$repo, "master")
  # public work that passes the content check, but is not a clean commit made by clean_publish()
  expect_push_refused_with(fx$repo, "only the clean commits", "hotfix")
  # the private history stays refused whatever the variable says
  expect_push_refused(fx$repo, "private-history")
  expect_push_refused(fx$repo, paste0(old, ":refs/heads/by-oid"))
  expect_push_refused(fx$repo, "--mirror")
  expect_identical(sub(" .*$", "", cp_remote_refs(fx)), "refs/heads/master")
})

# The URLs of remotes as they are spelled in practice. Whitespace is left out: the hook compares
# words, and guard_public_remote() refuses a URL with whitespace.
cp_url_corpus <- c(
  "https://github.com/Org/Repo.git", "https://github.com/org/repo", "https://github.com/org/repo/",
  "https://github.com/org/repo.git/", "https://github.com/org/repo.git//", "https://github.com/org/repo.git.git",
  "https://github.com/org/repo.GIT", "HTTPS://GitHub.com/o/r", "https://www.github.com/org/repo.git",
  "git@github.com:org/repo.git", "git@github.com:Org/Repo", "git@github.com:/o/r", "git@gitlab.com:g/sub/repo.git",
  "ssh://git@github.com/org/repo.git", "ssh://git@github.com:22/org/repo.git", "git://github.com/org/repo.git",
  "https://user:tok@github.com/org/repo.git", "https://token@github.com/o/r", "https://user:p@ss@github.com/o/r.git",
  "https://github.com/org/repo@v2.git", "git@github.com:org/repo@x.git", "/home/me/repo@v2.git", "ext::ssh git@x/y",
  "C:/Users/me/pub.git", "C:\\Users\\me\\pub.git", "C:\\Users\\me\\pub.git\\", "C:/Users/me/pub.git/",
  "C:\\Users\\kyle@site.edu\\pub.git", "file:///C:/Users/me/pub.git", "file:///srv/git/pub.git",
  "/srv/git/pub.git", "/srv/git/pub.git/.", "../pub.git", "./pub.git", "pub.git", "org/repo", "github.com:org/repo",
  "https://github.com//org//repo.git", "https://github.com/org/repo.git?x=1", "https://github.com/o/r/.git",
  "http://localhost:8080/a/b.git", "host:~user/repo", "@host/repo", "a//b", "x://y", "/", ""
)

test_that("the R and the shell URL normalisers give the same result for every spelling, also on their own output (CPB-08, CPD-14, R-CP-17)", {
  skip_if_no_git()
  shells <- cp_hook_shells()
  skip_if(!length(shells), "no POSIX shell found")
  block <- .cp_hook_block()
  first <- grep("^_tcbe_norm_url\\(\\) \\{$", block)
  last <- first + which(block[-seq_len(first)] == "}")[1L]
  script <- withr::local_tempfile(fileext = ".sh")
  writeBin(charToRaw(paste0(
    paste(c(block[first:last], "while IFS= read -r u; do _tcbe_norm_url \"$u\"; done"), collapse = "\n"), "\n"
  )), script)
  sh_normalise <- function(shell, urls) {
    input <- withr::local_tempfile()
    writeBin(charToRaw(paste0(paste(urls, collapse = "\n"), "\n")), input)
    suppressWarnings(system2(shell, shQuote(script), stdin = input, stdout = TRUE, stderr = TRUE))
  }

  by_r <- .cp_normalize_url(cp_url_corpus)
  expect_length(by_r, length(cp_url_corpus))
  # one normalisation is the same as two: what the hook is given stored by R, or raw, compares equal
  expect_identical(.cp_normalize_url(by_r), by_r)
  for (shell in shells) {
    expect_identical(sh_normalise(shell, cp_url_corpus), by_r, info = shell)
    expect_identical(sh_normalise(shell, by_r), by_r, info = shell)
  }
  # the ones that used to differ
  expect_identical(.cp_normalize_url("https://github.com/org/repo@v2.git"), "github.com/org/repo@v2")
  expect_identical(.cp_normalize_url("C:\\Users\\kyle@site.edu\\pub.git"), "c/users/kyle@site.edu/pub")
  expect_identical(.cp_normalize_url("https://github.com/o/r.GIT"), .cp_normalize_url("https://github.com/o/r"))
})

test_that("only sh, dash, bash and ksh hooks are extended: the block relies on word splitting (CPB-10)", {
  planned <- function(first_line) {
    hook <- withr::local_tempfile()
    writeLines(c(first_line, "exit 0"), hook)
    tryCatch(.cp_plan_hook(hook)$action, error = function(e) conditionMessage(e))
  }
  for (ok in c("#!/bin/sh", "#!/bin/sh -e", "#!/usr/bin/env sh", "#!/bin/dash", "#!/bin/bash", "#!/usr/bin/env bash", "#!/bin/ksh", "#! /bin/sh")) {
    expect_identical(planned(ok), "extended", info = ok)
  }
  for (bad in c("#!/bin/zsh", "#!/usr/bin/env zsh", "#!/bin/mksh", "#!/bin/fish", "#!/usr/bin/env python3", "#!/bin/csh")) {
    expect_match(planned(bad), "not a shell script", info = bad)
  }
})

test_that("guard_public_remote() resolves the name of a configured remote to its URLs (R-CP-07, CPB-03, CPC-02)", {
  skip_if_no_git()
  fx <- cp_fixture()
  cp_git(fx$repo, "remote", "rename", "origin", "public")

  cp_quiet(guard_public_remote("public", repo_root = fx$repo))

  stored <- cp_git(fx$repo, "config", "--get-all", "cleanpublish.publicurl")$out
  expect_false("public" %in% stored)
  expect_true(.cp_normalize_url(fx$remote) %in% .cp_normalize_url(stored))
  res <- cp_git(fx$repo, "push", "public", "master")
  expect_true(res$status != 0L)
  expect_match(paste(res$out, collapse = "\n"), "direct push to public remote .* is guarded")
  expect_identical(cp_remote_refs(fx), character())
})

test_that("a remote with a push URL of its own is fenced by its push and its fetch URL (CPB-03)", {
  skip_if_no_git()
  fx <- cp_fixture()
  fetch_only <- file.path(fx$base, "fetch-only.git")
  cp_git(fx$base, "init", "--bare", "--quiet", fetch_only)
  cp_git(fx$repo, "remote", "add", "public", fetch_only)
  cp_git(fx$repo, "config", "remote.public.pushurl", fx$remote)

  cp_quiet(guard_public_remote("public", repo_root = fx$repo))

  stored <- .cp_normalize_url(cp_git(fx$repo, "config", "--get-all", "cleanpublish.publicurl")$out)
  expect_true(all(.cp_normalize_url(c(fetch_only, fx$remote)) %in% stored))
  res <- cp_git(fx$repo, "push", "public", "master")
  expect_true(res$status != 0L)
  expect_match(paste(res$out, collapse = "\n"), "is guarded")
  expect_identical(cp_remote_refs(fx), character())

  # the URL of the fetch-only spelling alone is not a push URL: the fence would guard nothing
  cp_git(fx$repo, "config", "--unset-all", "cleanpublish.publicurl")
  expect_error(cp_quiet(guard_public_remote(fetch_only, repo_root = fx$repo)), "does not match the push URL of any configured remote")
})

test_that("a URL that matches no configured remote is an error unless allow_unmatched = TRUE (R-CP-08, CPC-02)", {
  skip_if_no_git()
  fx <- cp_fixture()
  later <- file.path(fx$base, "later.git")
  cp_git(fx$base, "init", "--bare", "--quiet", later)

  expect_error(
    cp_quiet(guard_public_remote(later, repo_root = fx$repo)),
    "does not match the push URL of any configured remote.*allow_unmatched = TRUE"
  )
  expect_length(.cp_config_get_all(fx$repo, "cleanpublish.publicurl"), 0L)
  expect_false(file.exists(cp_hook_file(fx$repo)))

  msgs <- testthat::capture_messages(guard_public_remote(later, repo_root = fx$repo, allow_unmatched = TRUE))
  expect_match(msgs, "matches no configured remote", fixed = TRUE, all = FALSE)
  expect_true(.cp_normalize_url(later) %in% .cp_normalize_url(.cp_config_get_all(fx$repo, "cleanpublish.publicurl")))
  # a remote that is added later is fenced
  cp_git(fx$repo, "remote", "add", "public", later)
  res <- cp_git(fx$repo, "push", "public", "master")
  expect_true(res$status != 0L)
  expect_match(paste(res$out, collapse = "\n"), "is guarded")
})

test_that("a URL spelled differently from the remote's, a relative path or a link, still matches and fences it (CPC-02)", {
  skip_if_no_git()
  fx <- cp_fixture()
  cp_git(fx$repo, "remote", "set-url", "origin", "../remote.git")

  cp_quiet(guard_public_remote(fx$remote, repo_root = fx$repo))

  res <- cp_git(fx$repo, "push", "origin", "master")
  expect_true(res$status != 0L, info = paste(res$out, collapse = "\n"))
  expect_match(paste(res$out, collapse = "\n"), "is guarded")
  expect_identical(cp_remote_refs(fx), character())
})

test_that("guard_public_remote() stops when the hook it wrote does not refuse a push to the fenced remote (CPB-03, CPC-02)", {
  skip_if_no_git()
  fx <- cp_fixture()
  # a hook that lets everything through
  testthat::local_mocked_bindings(
    .cp_write_hook = function(plan) {
      writeLines(c("#!/bin/sh", "exit 0"), plan$path)
      Sys.chmod(plan$path, "0755")
    }
  )
  expect_error(cp_quiet(guard_public_remote(fx$remote, repo_root = fx$repo)), "did not refuse a push")
})

test_that("guard_public_remote() stops when there is no hook, or no executable hook (CPB-03)", {
  skip_if_no_git()
  fx <- cp_fixture()
  testthat::local_mocked_bindings(.cp_write_hook = function(plan) invisible(NULL))
  expect_error(cp_quiet(guard_public_remote(fx$remote, repo_root = fx$repo)), "no pre-push hook")

  # git skips a hook that is not executable (git for Windows runs it whatever its mode)
  testthat::local_mocked_bindings(
    .cp_write_hook = function(plan) {
      writeBin(charToRaw(plan$content), plan$path)
      Sys.chmod(plan$path, "0755")
    },
    .cp_hook_is_executable = function(path) FALSE
  )
  expect_error(cp_quiet(guard_public_remote(fx$remote, repo_root = fx$repo)), "not executable")
})

test_that(".cp_default_repo_root() is the git toplevel of the working directory, and guard_public_remote() starts from it (H8)", {
  skip_if_no_git()
  fx <- cp_fixture()
  sub <- file.path(fx$repo, "sub")
  dir.create(sub)
  expect_identical(
    normalizePath(withr::with_dir(sub, .cp_default_repo_root()), winslash = "/"),
    normalizePath(fx$repo, winslash = "/")
  )
  withr::with_dir(sub, cp_quiet(guard_public_remote(fx$remote)))
  expect_true(.cp_normalize_url(fx$remote) %in% .cp_normalize_url(.cp_config_get_all(fx$repo, "cleanpublish.publicurl")))

  not_git <- normalizePath(withr::local_tempdir(), winslash = "/")
  withr::local_envvar(c(GIT_CEILING_DIRECTORIES = dirname(not_git)))
  withr::with_dir(not_git, {
    expect_error(.cp_default_repo_root(), "not inside a git repository")
    expect_error(guard_public_remote(fx$remote), "not inside a git repository")
  })
})

test_that("the self-test pushes the private tip under another branch name and as a tag, and stops when either is accepted (CPB-05)", {
  skip_if_no_git()
  # a hook that knows the branch name of the private branch only
  fx <- cp_fixture()
  testthat::local_mocked_bindings(
    .cp_write_hook = function(plan) {
      writeLines(c(
        "#!/bin/sh",
        "while read -r l o r p; do",
        "  case \"$r\" in refs/heads/private-history) echo 'clean_publish guard: push refused: name' >&2; exit 1 ;; esac",
        "done",
        "exit 0"
      ), plan$path)
      Sys.chmod(plan$path, "0755")
    }
  )
  before <- cp_state(fx$repo)
  expect_error(cp_quiet(clean_publish(repo_root = fx$repo, push = FALSE)), "does not stop a push of private commits")
  # accepted pushes are an error whatever confirm_guard says
  expect_error(
    cp_quiet(clean_publish(repo_root = fx$repo, push = FALSE, confirm_guard = FALSE)),
    "does not stop a push of private commits"
  )
  expect_identical(cp_current(fx$repo), "master")
  expect_true(is.na(cp_rev(fx$repo, "private-history")))

  # a hook that checks branches but not tags
  fx <- cp_fixture()
  testthat::local_mocked_bindings(
    .cp_write_hook = function(plan) {
      writeLines(c(
        "#!/bin/sh",
        "while read -r l o r p; do",
        "  case \"$r\" in refs/tags/*) ;; *) echo 'clean_publish guard: push refused: contains commits of x' >&2; exit 1 ;; esac",
        "done",
        "exit 0"
      ), plan$path)
      Sys.chmod(plan$path, "0755")
    }
  )
  expect_error(cp_quiet(clean_publish(repo_root = fx$repo, push = FALSE)), "does not stop a push of private commits.*tag")
})

test_that("the self-test passes with the real hook, on the first run and on a re-run", {
  skip_if_no_git()
  fx <- cp_fixture()
  msgs <- testthat::capture_messages(cp_first_publish(fx))
  expect_false(any(grepl("Could not confirm", msgs, fixed = TRUE)))
  expect_identical(cp_ncommits(fx$repo, "master"), 1L)
  cp_commit(fx$repo, "more.txt", "x", "more")
  cp_first_publish(fx)
  expect_identical(cp_ncommits(fx$repo, "private-history"), 4L)
})

test_that("the hook gives the same verdicts under every POSIX shell (CPC-12)", {
  skip_if_no_git()
  shells <- cp_hook_shells()
  skip_if(!length(shells), "no POSIX shell found")
  fx <- cp_fixture(published = TRUE)
  clean <- cp_rev(fx$repo, "master")
  tip <- cp_rev(fx$repo, "private-history")
  old <- cp_rev(fx$repo, "private-history~1")
  old_tree <- cp_git(fx$repo, "rev-parse", "private-history~2^{tree}")$out
  clean_tree <- cp_git(fx$repo, "rev-parse", "master^{tree}")$out
  zero <- strrep("0", nchar(tip))
  verdicts <- list(
    list("nothing to push", character(), 0L),
    list("a blank record", "", 0L),
    list("the clean commit", cp_record("refs/heads/master", clean), 0L),
    list("a deletion", cp_record("(delete)", zero, "refs/heads/private-history", tip), 0L),
    list("the private branch by name", cp_record("refs/heads/private-history", tip), 1L),
    list("a branch of that name made of clean commits", cp_record("refs/heads/master", clean, "refs/heads/private-history"), 1L),
    list("the private tip under another name", cp_record("refs/heads/x", tip), 1L),
    list("an older private commit", cp_record("refs/tags/x", old), 1L),
    list("a clean and a private ref together", c(cp_record("refs/heads/master", clean), cp_record("refs/heads/x", old)), 1L),
    list("a tree of a private commit", cp_record("refs/tags/t", old_tree), 1L),
    list("the tree of the clean commit", cp_record("refs/tags/t", clean_tree), 0L)
  )
  for (shell in shells) {
    for (v in verdicts) {
      res <- cp_run_hook(shell, fx$repo, v[[2]])
      expect_identical(res$status, v[[3]], info = paste(basename(shell), v[[1]], paste(res$out, collapse = " | ")))
    }
  }

  # with the private branch gone, and with a stale value in place of a clean commit
  cp_git(fx$repo, "checkout", "--quiet", "master")
  cp_git(fx$repo, "branch", "-D", "private-history")
  cp_git(fx$repo, "config", "--add", "cleanpublish.cleancommit", "master")
  for (shell in shells) {
    res <- cp_run_hook(shell, fx$repo, cp_record("refs/heads/x", old))
    expect_identical(res$status, 1L, info = paste(basename(shell), "deleted branch", paste(res$out, collapse = " | ")))
    expect_match(paste(res$out, collapse = "\n"), "contains commits", info = basename(shell))
    res <- cp_run_hook(shell, fx$repo, cp_record("refs/heads/master", clean))
    expect_identical(res$status, 0L, info = paste(basename(shell), "clean after deletion"))
  }
  # no tip at all to be found: everything that is not clean is refused, with the way out
  cp_git(fx$repo, "config", "--unset-all", "cleanpublish.privatetip")
  for (shell in shells) {
    res <- cp_run_hook(shell, fx$repo, cp_record("refs/heads/x", old))
    expect_identical(res$status, 1L, info = paste(basename(shell), "nothing to find"))
    expect_match(paste(res$out, collapse = "\n"), "git config --unset-all cleanpublish.protectedbranch", fixed = TRUE, info = basename(shell))
    expect_identical(cp_run_hook(shell, fx$repo, cp_record("refs/heads/master", clean))$status, 0L, info = basename(shell))
  }
})

test_that("the fence and the TEMPLECBE_PUBLISHING bypass behave the same under every POSIX shell (CPD-04, CPC-12)", {
  skip_if_no_git()
  shells <- cp_hook_shells()
  skip_if(!length(shells), "no POSIX shell found")
  fx <- cp_fixture(published = TRUE)
  cp_git(fx$repo, "remote", "rename", "origin", "public")
  cp_quiet(guard_public_remote("public", repo_root = fx$repo))
  clean <- cp_rev(fx$repo, "master")
  tip <- cp_rev(fx$repo, "private-history")
  other <- file.path(fx$base, "somewhere-else.git")
  for (shell in shells) {
    name <- basename(shell)
    # nothing on stdin is what `sh <hook> <remote> <url>` gives: the fence alone decides
    res <- cp_run_hook(shell, fx$repo, remote = "public", url = fx$remote)
    expect_identical(res$status, 1L, info = name)
    expect_match(paste(res$out, collapse = "\n"), "is guarded", info = name)
    expect_identical(cp_run_hook(shell, fx$repo, remote = "public", url = paste0(fx$remote, "/"))$status, 1L, info = name)
    expect_identical(cp_run_hook(shell, fx$repo, remote = "public", url = toupper(fx$remote))$status, 1L, info = name)
    expect_identical(cp_run_hook(shell, fx$repo, remote = "other", url = other)$status, 0L, info = name)
    # the variable lets a clean commit through, and nothing else
    env <- c(TEMPLECBE_PUBLISHING = "1")
    expect_identical(cp_run_hook(shell, fx$repo, cp_record("refs/heads/master", clean), "public", fx$remote, env)$status, 0L, info = name)
    expect_identical(cp_run_hook(shell, fx$repo, cp_record("refs/heads/x", tip), "public", fx$remote, env)$status, 1L, info = name)
    expect_identical(cp_run_hook(shell, fx$repo, cp_record("refs/heads/master", clean), "public", fx$remote, c(TEMPLECBE_PUBLISHING = "true"))$status, 1L, info = name)
  }
})

test_that("a hook that was there first still gets every record, and no blank one when nothing is pushed, also under sh -eu (R-CP-09)", {
  skip_if_no_git()
  shells <- cp_hook_shells()
  skip_if(!length(shells), "no POSIX shell found")
  fx <- cp_fixture()
  seen <- file.path(fx$base, "seen.txt")
  hook <- cp_hook_file(fx$repo)
  dir.create(dirname(hook), showWarnings = FALSE)
  writeLines(c(
    "#!/bin/sh -eu",
    "n=0",
    paste0("while read -r ref oid rref roid; do n=$((n+1)); echo \"$ref\" >> '", seen, "'; done"),
    paste0("echo \"records=$n\" >> '", seen, "'"),
    "exit 0"
  ), hook)
  Sys.chmod(hook, "0755")
  cp_first_publish(fx)
  clean <- cp_rev(fx$repo, "master")

  for (shell in shells) {
    unlink(seen)
    res <- cp_run_hook(shell, fx$repo, c(cp_record("refs/heads/master", clean), cp_record("refs/tags/v1", clean)))
    expect_identical(res$status, 0L, info = paste(basename(shell), paste(res$out, collapse = " | ")))
    expect_identical(readLines(seen), c("refs/heads/master", "refs/tags/v1", "records=2"), info = basename(shell))
    unlink(seen)
    res <- cp_run_hook(shell, fx$repo)
    expect_identical(res$status, 0L, info = basename(shell))
    expect_identical(readLines(seen), "records=0", info = basename(shell))
  }
  # and the refusals still come first
  unlink(seen)
  res <- cp_run_hook(shells[1], fx$repo, cp_record("refs/heads/private-history", cp_rev(fx$repo, "private-history")))
  expect_identical(res$status, 1L)
  expect_false(file.exists(seen))
})


# ---- L3 review, stage 2: the main flow of clean_publish() -----------------------------------------
#
# Every test works in a throwaway repository whose remotes are local bare repositories next to it.
# The repository a call acts on is named with `repo_root`, except in the one test of the default,
# which stands inside the scratch repository and does not let here::here() answer.

test_that("repo_root defaults to the git toplevel of the working directory, never here::here(), and the repository is printed first (CPA-05)", {
  skip_if_no_git()
  fx <- cp_fixture()
  # here::here() is the repository this test is run from. If the default were still here::here(),
  # this call must fail rather than touch it.
  testthat::local_mocked_bindings(here = function(...) stop("here::here() must not be used"), .package = "here")
  sub <- file.path(fx$repo, "sub")
  dir.create(sub)

  msgs <- testthat::capture_messages(withr::with_dir(sub, clean_publish(push = FALSE)))

  expect_identical(cp_ncommits(fx$repo, "master"), 1L)
  expect_identical(cp_current(fx$repo), "private-history")
  repo_line <- grep("^Repository: ", msgs, value = TRUE)
  expect_length(repo_line, 1L)
  expect_identical(
    normalizePath(sub("^Repository: ", "", trimws(repo_line)), winslash = "/"),
    normalizePath(fx$repo, winslash = "/")
  )
  expect_match(msgs, "^Git directory: .*[.]git", all = FALSE)
  expect_match(msgs, "^Current branch: master", all = FALSE)
  expect_match(msgs, paste0("^Remote 'origin': fetch ", fx$remote, ", push ", fx$remote), all = FALSE)
  # all of it comes before the first thing that changes
  first_change <- grep("[GUARD]", msgs, fixed = TRUE)[1]
  expect_true(all(vapply(c("^Repository: ", "^Git directory: ", "^Current branch: ", "^Remote 'origin'"), function(p) grep(p, msgs)[1] < first_change, logical(1))))
})

test_that("git must have an identity for the clean commit: without one nothing is written (CPA-06, CPC-11, CPD-10)", {
  skip_if_no_git()
  fx <- cp_fixture()
  withr::local_envvar(c(
    GIT_AUTHOR_NAME = NA, GIT_AUTHOR_EMAIL = NA, GIT_COMMITTER_NAME = NA, GIT_COMMITTER_EMAIL = NA, EMAIL = NA
  ))
  # a fresh clone: no name, no e-mail, and git is told not to make one up
  cp_git(fx$repo, "config", "--unset", "user.name")
  cp_git(fx$repo, "config", "--unset", "user.email")
  cp_git(fx$repo, "config", "user.useConfigOnly", "true")
  before <- cp_state(fx$repo)

  expect_error(
    cp_quiet(clean_publish(repo_root = fx$repo, push = FALSE)),
    "identity.*Nothing was changed"
  )

  # not a hook, not a config key, not a backup ref
  expect_identical(cp_state(fx$repo), before)
  expect_false(file.exists(cp_hook_file(fx$repo)))
  expect_length(cp_backup_refs(fx$repo), 0L)
})

test_that("the author, committer and time zone that will be published are printed, and are the ones the commit has (CPA-06, CPC-11)", {
  skip_if_no_git()
  fx <- cp_fixture()
  withr::local_envvar(c(
    GIT_AUTHOR_NAME = "Public Author", GIT_AUTHOR_EMAIL = "author@example.invalid",
    GIT_COMMITTER_NAME = "Public Committer", GIT_COMMITTER_EMAIL = "committer@example.invalid",
    GIT_AUTHOR_DATE = "2026-01-02T03:04:05+0530", GIT_COMMITTER_DATE = "2026-01-02T03:04:05+0530"
  ))

  msgs <- testthat::capture_messages(clean <- clean_publish(repo_root = fx$repo, push = FALSE))

  line <- grep("^Identity published in the clean commit", msgs, value = TRUE)
  expect_length(line, 1L)
  expect_match(line, "author Public Author <author@example.invalid>", fixed = TRUE)
  expect_match(line, "committer Public Committer <committer@example.invalid>", fixed = TRUE)
  expect_match(line, "time zone +0530", fixed = TRUE)
  header <- cp_git(fx$repo, "cat-file", "-p", clean)$out
  expect_true(any(grepl("^author Public Author <author@example.invalid> [0-9]+ [+]0530$", header)))
  expect_true(any(grepl("^committer Public Committer <committer@example.invalid> [0-9]+ [+]0530$", header)))
})

test_that("branch names are checked as refs/heads/<name>: @{-1}, a leading @ and malformed names are refused before anything changes (CPC-08, CPA-10)", {
  skip_if_no_git()
  fx <- cp_fixture(published = TRUE)
  # there is a previous branch, so that `@{-1}` would mean something to `git check-ref-format --branch`
  cp_git(fx$repo, "checkout", "--quiet", "master")
  cp_git(fx$repo, "checkout", "--quiet", "private-history")
  before <- cp_state(fx$repo)

  for (bad in c("@{-1}", "@x", "a@{b", "a..b", "x.lock", "bad name")) {
    expect_error(
      cp_quiet(clean_publish(repo_root = fx$repo, private_branch = bad, push = FALSE)),
      "is not a valid branch name", info = paste("private_branch", bad)
    )
    expect_error(
      cp_quiet(clean_publish(repo_root = fx$repo, publish_branch = bad, push = FALSE)),
      "is not a valid branch name", info = paste("publish_branch", bad)
    )
  }
  expect_identical(cp_state(fx$repo), before)
})

test_that("a private branch that differs from the publish branch only in case is refused before anything changes (CPA-10)", {
  skip_if_no_git()
  fx <- cp_fixture()
  before <- cp_state(fx$repo)

  expect_error(
    cp_quiet(clean_publish(repo_root = fx$repo, private_branch = "MASTER", push = FALSE)),
    "Refusing to publish onto 'master'"
  )
  expect_error(
    cp_quiet(clean_publish(repo_root = fx$repo, publish_branch = "Release", private_branch = "release", push = FALSE)),
    "Refusing to publish onto 'Release'"
  )
  expect_identical(cp_state(fx$repo), before)
})

test_that("a branch move that did not take effect is detected, with the way back (CPA-10)", {
  skip_if_no_git()
  fx <- cp_fixture()
  real_git <- .cp_git
  # `git branch -f private-history` that does nothing, as a branch name that collides on a
  # case-insensitive file system can do
  testthat::local_mocked_bindings(
    .cp_git = function(repo_root, args, error_ok = FALSE) {
      if (identical(args[1:2], c("branch", "-f")) && identical(args[3], "private-history")) {
        return(list(output = character(), stderr = character(), status = 0L))
      }
      real_git(repo_root, args, error_ok = error_ok)
    }
  )

  err <- tryCatch(cp_quiet(clean_publish(repo_root = fx$repo, push = FALSE)), error = conditionMessage)

  expect_match(err, "did not take effect", fixed = TRUE)
  expect_match(err, "refs/backup/clean_publish/", fixed = TRUE)
})

# --- snapshot mode builds only on a remote made of clean commits (CPA-02, CPD-01, CPA-13, CPC-07, CPA-08) ---

# The trailer lines of a commit, as git reads them
cp_trailer <- function(repo, ref) {
  out <- cp_git(repo, "log", "-1", "--format=%(trailers:key=Clean-Publish,valueonly)", ref)$out
  out[nzchar(out)]
}

# A bare repository holding the history of `fx$repo`'s master REWRITTEN: every commit replayed with
# a new id and another message, as an archive made by a history-rewriting tool is
cp_rewritten_archive <- function(fx, name = "archive.git") {
  path <- file.path(fx$base, name)
  dir.create(path)
  cp_git(path, "-c", "init.defaultBranch=master", "init", "--bare", "--quiet")
  tip <- NULL
  for (id in cp_git(fx$repo, "rev-list", "--reverse", "master")$out) {
    tree <- cp_git(fx$repo, "rev-parse", paste0(id, "^{tree}"))$out
    tip <- cp_git(fx$repo, "commit-tree", tree, if (!is.null(tip)) c("-p", tip), "-m", paste0("archived-", substr(id, 1L, 7L)))$out
  }
  stopifnot(cp_git(fx$repo, "push", "--quiet", path, paste0(tip, ":refs/heads/master"))$status == 0L)
  path
}

test_that("every clean commit carries the Clean-Publish trailer after a blank line, in both modes (CPA-02, CPD-01)", {
  skip_if_no_git()
  fx <- cp_fixture()

  clean1 <- cp_quiet(clean_publish(repo_root = fx$repo, remote = "origin", commit_msg = "Release one\n\nwith a body\n"))
  cp_commit(fx$repo, "two.txt", "x", "two")
  clean2 <- cp_quiet(clean_publish(repo_root = fx$repo, remote = "origin", mode = "snapshot"))

  for (id in c(clean1, clean2)) {
    expect_identical(cp_trailer(fx$repo, id), "v1")
    body <- cp_git(fx$repo, "log", "-1", "--format=%B", id)$out
    line <- which(body == "Clean-Publish: v1")
    expect_length(line, 1L)
    expect_identical(body[line - 1L], "")
  }
  expect_identical(cp_git(fx$repo, "log", "-1", "--format=%s", clean1)$out, "Release one")
  expect_true(any(cp_git(fx$repo, "log", "-1", "--format=%b", clean1)$out == "with a body"))
  expect_identical(cp_git(fx$remote, "rev-parse", "master")$out, clean2)
})

test_that(".cp_clean_chain_problem() accepts a linear chain of trailer commits and says what is wrong otherwise (CPD-01)", {
  skip_if_no_git()
  fx <- cp_fixture()
  tree <- cp_git(fx$repo, "rev-parse", "HEAD^{tree}")$out
  make <- function(subject, parents = character(), trailer = "Clean-Publish: v1") {
    pargs <- unlist(lapply(parents, function(p) c("-p", p)))
    cp_git(fx$repo, "commit-tree", tree, pargs, "-m", subject, if (!is.null(trailer)) c("-m", trailer))$out
  }
  c1 <- make("one")
  c2 <- make("two", c1)
  c3 <- make("three", c2)
  expect_null(.cp_clean_chain_problem(fx$repo, c3))

  # a commit without the trailer, a trailer that is only mentioned in a line, a different version
  expect_match(.cp_clean_chain_problem(fx$repo, make("public-fix", c3, trailer = NULL)), "1 of 4 commits.*public-fix")
  expect_match(.cp_clean_chain_problem(fx$repo, make("mention", c3, trailer = "see Clean-Publish: v1 for details")), "1 of 4 commits.*mention")
  expect_match(.cp_clean_chain_problem(fx$repo, make("future", c3, trailer = "Clean-Publish: v2")), "1 of 4 commits.*future")
  # the root counts too
  expect_match(.cp_clean_chain_problem(fx$repo, make("two-b", make("one-b", trailer = NULL))), "1 of 2 commits.*one-b")
  # a merge makes the chain not linear, even when every commit has the trailer
  other <- make("other-root")
  expect_match(.cp_clean_chain_problem(fx$repo, make("merge", c(c3, other))), "merge commit")
})

test_that("snapshot mode refuses a remote that is the private repository, a rewritten private archive and a remote with a hand-made commit (CPA-02, CPD-01, CPA-13)", {
  skip_if_no_git()
  fx <- cp_fixture()
  # (a) the private repository: `origin` of a throwaway clone has the 3 private commits on master
  expect_identical(cp_git(fx$repo, "push", "--quiet", "origin", "master")$status, 0L)
  cp_commit(fx$repo, "next.txt", "more", "fourth")
  before <- cp_state(fx$repo)
  err <- tryCatch(
    cp_quiet(clean_publish(repo_root = fx$repo, remote = "origin", mode = "snapshot", push = FALSE)),
    error = conditionMessage
  )
  expect_match(err, "not made only of clean commits", fixed = TRUE)
  expect_match(err, "3 of 3 commits", fixed = TRUE)
  expect_match(err, "'c3'", fixed = TRUE)
  expect_match(err, "Nothing was changed", fixed = TRUE)
  expect_identical(cp_state(fx$repo), before)

  # (b) an archive of the private history that was rewritten: same content, new commit ids, so that
  # no commit is shared with this repository. It used to be accepted, and the hook then trusted it.
  archive <- cp_rewritten_archive(fx)
  cp_git(fx$repo, "remote", "add", "arch", archive)
  before <- cp_state(fx$repo)
  err <- tryCatch(
    cp_quiet(clean_publish(repo_root = fx$repo, remote = "arch", mode = "snapshot", push = FALSE)),
    error = conditionMessage
  )
  expect_match(err, "4 of 4 commits", fixed = TRUE)
  expect_match(err, "archived-", fixed = TRUE)
  expect_identical(cp_state(fx$repo), before)
  expect_false(file.exists(cp_hook_file(fx$repo)))
})

test_that("snapshot mode refuses a public branch that holds a commit clean_publish() did not make (CPD-01)", {
  skip_if_no_git()
  fx <- cp_fixture()
  clean1 <- cp_quiet(clean_publish(repo_root = fx$repo, remote = "origin"))
  # somebody commits to the public branch directly
  tree <- cp_git(fx$repo, "rev-parse", paste0(clean1, "^{tree}"))$out
  fix <- cp_git(fx$repo, "commit-tree", tree, "-p", clean1, "-m", "public-fix")$out
  expect_identical(cp_git(fx$repo, "push", "--quiet", fx$remote, paste0(fix, ":refs/heads/master"))$status, 0L)
  cp_commit(fx$repo, "two.txt", "x", "two")
  before <- cp_state(fx$repo)

  err <- tryCatch(cp_quiet(clean_publish(repo_root = fx$repo, remote = "origin", mode = "snapshot")), error = conditionMessage)

  expect_match(err, "1 of 2 commits.*public-fix")
  expect_identical(cp_state(fx$repo), before)
  expect_identical(cp_git(fx$remote, "rev-parse", "master")$out, fix)
})

test_that("snapshot mode is refused in a shallow repository (CPA-02)", {
  skip_if_no_git()
  fx <- cp_fixture()
  shallow <- file.path(fx$base, "shallow")
  # --depth is honoured for a URL only, not for a path
  url <- paste0("file://", if (startsWith(fx$repo, "/")) "" else "/", fx$repo)
  expect_identical(cp_git(fx$base, "clone", "--quiet", "--depth", "1", url, shallow)$status, 0L)
  cp_git(shallow, "config", "user.name", "Test User")
  cp_git(shallow, "config", "user.email", "test@example.invalid")
  expect_identical(cp_git(shallow, "rev-parse", "--is-shallow-repository")$out, "true")
  before <- cp_state(shallow)

  # `origin` is the private repository: a snapshot on its tip would carry the private history, and
  # in a shallow clone the history that would show it is cut off
  expect_error(
    cp_quiet(clean_publish(repo_root = shallow, remote = "origin", mode = "snapshot", push = FALSE)),
    "shallow repository.*unshallow.*Nothing was changed"
  )
  expect_identical(cp_state(shallow), before)
})

test_that("snapshot mode accepts a remote that holds only clean commits, whichever clone made them (CPA-02, CPC-07)", {
  skip_if_no_git()
  fx <- cp_fixture()
  clean1 <- cp_quiet(clean_publish(repo_root = fx$repo, remote = "origin"))

  # a second throwaway clone of the private repository publishes to the same remote: the commit on
  # it was made by the first clone, so it is no recorded clean commit here
  other <- cp_clone(fx)
  cp_git(other, "remote", "add", "pub", fx$remote)
  cp_commit(other, "from-other.txt", "x", "from the other clone")
  clean2 <- cp_quiet(clean_publish(repo_root = other, remote = "pub", mode = "snapshot"))
  expect_identical(cp_git(fx$remote, "rev-parse", "master~1")$out, clean1)
  expect_identical(cp_git(fx$remote, "rev-parse", "master")$out, clean2)
  expect_identical(cp_ncommits(fx$remote, "master"), 2L)
})

test_that("snapshot mode still works after the public tip has been merged back into the private branch (CPC-07, CPA-08)", {
  skip_if_no_git()
  fx <- cp_fixture()
  clean1 <- cp_quiet(clean_publish(repo_root = fx$repo, remote = "origin"))
  # the clean commit becomes part of the private history
  res <- cp_git(fx$repo, "merge", "--quiet", "--allow-unrelated-histories", "-m", "merged", clean1)
  expect_identical(res$status, 0L, info = paste(res$out, collapse = "\n"))
  cp_commit(fx$repo, "after-merge.txt", "x", "after")

  clean2 <- cp_quiet(clean_publish(repo_root = fx$repo, remote = "origin", mode = "snapshot"))

  expect_identical(cp_git(fx$remote, "rev-parse", "master~1")$out, clean1)
  expect_identical(cp_git(fx$remote, "rev-parse", "master")$out, clean2)
  expect_identical(cp_ncommits(fx$remote, "master"), 2L)
})

test_that("snapshot mode fetches the remote tip without writing a remote-tracking ref or a tag", {
  skip_if_no_git()
  fx <- cp_fixture()
  cp_quiet(clean_publish(repo_root = fx$repo, remote = "origin"))
  cp_git(fx$repo, "tag", "v1", "master")
  cp_git(fx$repo, "push", "--quiet", fx$remote, "v1")
  cp_git(fx$repo, "tag", "-d", "v1")
  cp_commit(fx$repo, "two.txt", "x", "two")
  before <- cp_state(fx$repo)

  cp_quiet(clean_publish(repo_root = fx$repo, remote = "origin", mode = "snapshot", push = FALSE))

  heads <- function(s) s$refs[!startsWith(s$refs, "refs/backup/") & !startsWith(s$refs, "refs/heads/")]
  expect_identical(heads(cp_state(fx$repo)), heads(before))
})

test_that("a git call that fails in a gate stops the run: ls-files -ci and diff-tree are not read as 'nothing found' (CPC-06, CPA-13)", {
  skip_if_no_git()
  fx <- cp_fixture()
  clean1 <- cp_quiet(clean_publish(repo_root = fx$repo, remote = "origin"))
  cp_commit(fx$repo, "two.txt", "x", "two")
  real_git <- .cp_git
  # what `git <subcommand>` does when it fails with status 128, as .cp_git() reports it
  failing <- function(subcommand) {
    function(repo_root, args, error_ok = FALSE) {
      if (identical(args[1], subcommand)) {
        if (error_ok) return(list(output = character(), stderr = "fatal: simulated", status = 128L))
        stop("git ", subcommand, " failed (status 128): simulated", call. = FALSE)
      }
      real_git(repo_root, args, error_ok = error_ok)
    }
  }
  before <- cp_state(fx$repo)

  testthat::local_mocked_bindings(.cp_git = failing("ls-files"))
  expect_error(cp_quiet(clean_publish(repo_root = fx$repo, push = FALSE)), "ls-files")
  expect_identical(cp_state(fx$repo), before)

  testthat::local_mocked_bindings(.cp_git = failing("diff-tree"))
  expect_error(cp_quiet(clean_publish(repo_root = fx$repo, remote = "origin", mode = "snapshot", push = FALSE)), "diff-tree")
  expect_identical(cp_state(fx$repo), before)
})

test_that("snapshot mode prints what the snapshot changes in the public tree, deletions included (CPA-12)", {
  skip_if_no_git()
  fx <- cp_fixture()
  cp_quiet(clean_publish(repo_root = fx$repo, remote = "origin"))
  expect_identical(cp_git(fx$repo, "rm", "--quiet", "f1.txt")$status, 0L)
  expect_identical(cp_git(fx$repo, "commit", "--quiet", "-m", "dropf1")$status, 0L)
  cp_commit(fx$repo, "new.txt", "x", "addnew")

  msgs <- testthat::capture_messages(clean_publish(repo_root = fx$repo, remote = "origin", mode = "snapshot", push = FALSE))

  text <- paste(msgs, collapse = "\n")
  expect_match(text, "Changes of the snapshot against the public tip", fixed = TRUE)
  expect_match(text, "new.txt", fixed = TRUE)
  expect_match(text, "f1.txt", fixed = TRUE)
  expect_match(text, "2 files changed", fixed = TRUE)
  expect_match(text, "1 path deleted", fixed = TRUE)
})

# --- the remote is examined before anything changes; the push carries an explicit lease -------------
# (CPA-03, CPD-03, CPC-10, CPA-07, CPD-05)

test_that("a remote with refs other than the target branch is refused before anything changes; allow_other_refs lets it through and the SUCCESS message lists them (CPD-03)", {
  skip_if_no_git()
  fx <- cp_fixture()
  # what GitHub keeps for ever: a pull request ref, notes, and someone's tag
  expect_identical(cp_git(fx$repo, "push", "--quiet", "origin", "HEAD:refs/pull/1/head", "HEAD:refs/notes/old")$status, 0L)
  cp_git(fx$remote, "tag", "v0.1.0", "refs/pull/1/head")
  before <- cp_state(fx$repo)
  remote_before <- cp_remote_refs(fx)

  err <- tryCatch(cp_quiet(clean_publish(repo_root = fx$repo, remote = "origin")), error = conditionMessage)

  for (ref in c("refs/pull/1/head", "refs/notes/old", "refs/tags/v0.1.0")) expect_match(err, ref, fixed = TRUE)
  expect_match(err, "refs/pull/*", fixed = TRUE)
  expect_match(err, "NEW EMPTY repository", fixed = TRUE)
  expect_match(err, "allow_other_refs = TRUE", fixed = TRUE)
  expect_match(err, "Nothing was changed", fixed = TRUE)
  expect_identical(cp_state(fx$repo), before)
  expect_identical(cp_remote_refs(fx), remote_before)
  expect_false(file.exists(cp_hook_file(fx$repo)))

  msgs <- testthat::capture_messages(clean <- clean_publish(repo_root = fx$repo, remote = "origin", allow_other_refs = TRUE))
  expect_identical(cp_git(fx$remote, "rev-parse", "master")$out, clean)
  expect_true(all(c("refs/pull/1/head", "refs/notes/old", "refs/tags/v0.1.0") %in% sub(" .*$", "", cp_remote_refs(fx))))
  text <- paste(msgs, collapse = "\n")
  expect_match(text, "now points to the clean commit", fixed = TRUE)
  expect_match(text, "refs/pull/1/head", fixed = TRUE)
  expect_false(grepl("exactly 1 clean commit", text, fixed = TRUE))
})

test_that("a remote that holds this repository's history is refused unless allow_overwrite_history = TRUE (CPA-03, CPD-05, CPC-10)", {
  skip_if_no_git()
  fx <- cp_fixture()
  # `origin` is the private repository: it has the 3 commits on master
  expect_identical(cp_git(fx$repo, "push", "--quiet", "origin", "master")$status, 0L)
  before <- cp_state(fx$repo)

  expect_error(
    cp_quiet(clean_publish(repo_root = fx$repo, remote = "origin")),
    "holds this repository's history.*'master'.*allow_overwrite_history = TRUE.*Nothing was changed"
  )
  expect_identical(cp_state(fx$repo), before)
  expect_identical(cp_ncommits(fx$remote, "master"), 3L)

  # the private branch is another head of the remote, and the target a new branch: refused all the same
  cp_git(fx$repo, "push", "--quiet", "origin", "master:dev")
  expect_error(
    cp_quiet(clean_publish(repo_root = fx$repo, remote = "origin", publish_branch = "release", allow_other_refs = TRUE)),
    "holds this repository's history.*'dev'"
  )
  expect_identical(cp_ncommits(fx$remote, "master"), 3L)

  # explicitly allowed: the private repository is overwritten, which is what was asked for
  cp_git(fx$remote, "update-ref", "-d", "refs/heads/dev")
  clean <- cp_quiet(clean_publish(repo_root = fx$repo, remote = "origin", allow_overwrite_history = TRUE))
  expect_identical(cp_ncommits(fx$remote, "master"), 1L)
  expect_identical(cp_git(fx$remote, "rev-parse", "master")$out, clean)
})

test_that("a remote head at a recorded private tip counts as this repository's history, whatever the current commit is (CPD-05)", {
  skip_if_no_git()
  fx <- cp_fixture(published = TRUE)
  tip <- cp_rev(fx$repo, "private-history")
  # a private copy of the history on the remote, put there past the hook
  expect_identical(cp_git(fx$repo, "push", "--quiet", "--no-verify", "origin", paste0(tip, ":refs/heads/dev"))$status, 0L)
  # a work branch that does not contain the private history, and a new private branch name
  cp_git(fx$repo, "checkout", "--quiet", "-b", "clean-work", "master")
  cp_commit(fx$repo, "work.txt", "x", "work")
  before <- cp_state(fx$repo)

  expect_error(
    cp_quiet(clean_publish(repo_root = fx$repo, remote = "origin", private_branch = "other-private", allow_other_refs = TRUE)),
    "holds this repository's history.*'dev'"
  )
  expect_identical(cp_state(fx$repo), before)
})

test_that("a standalone re-publish onto a remote with earlier clean commits works, and a stale remote-tracking ref does not matter (CPA-07)", {
  skip_if_no_git()
  fx <- cp_fixture()
  clean1 <- cp_quiet(clean_publish(repo_root = fx$repo, remote = "origin"))
  cp_commit(fx$repo, "more.txt", "x", "more")
  clean2 <- cp_quiet(clean_publish(repo_root = fx$repo, remote = "origin"))
  expect_false(identical(clean1, clean2))
  expect_identical(cp_remote_refs(fx), paste("refs/heads/master", clean2))
  expect_identical(cp_ncommits(fx$remote, "master"), 1L)

  # the runbook's clone: `origin` renamed to `public` and pointed at the public repository, which
  # holds a clean commit made elsewhere, while refs/remotes/public/* still hold the private tips (the
  # push used to fail with "stale info")
  fx2 <- cp_fixture()
  cp_git(fx2$repo, "push", "--quiet", "origin", "master")
  cp_git(fx2$repo, "remote", "rename", "origin", "public")
  cp_git(fx2$repo, "remote", "set-url", "public", fx$remote)
  expect_false(is.na(cp_rev(fx2$repo, "refs/remotes/public/master")))
  clean <- cp_quiet(clean_publish(repo_root = fx2$repo, remote = "public"))
  expect_identical(cp_git(fx$remote, "rev-parse", "master")$out, clean)
  expect_false(identical(clean, clean2))
})

test_that("the push carries an explicit lease from the listing: a branch that moved after it was listed is not overwritten (CPA-07)", {
  skip_if_no_git()
  fx <- cp_fixture()
  other <- cp_build_repo(fx$base, 2L, remote = FALSE, repo_name = "other")$repo
  expect_identical(cp_git(other, "push", "--quiet", fx$remote, "HEAD~1:refs/heads/master")$status, 0L)
  real_git <- .cp_git
  moved <- FALSE
  testthat::local_mocked_bindings(
    .cp_git = function(repo_root, args, error_ok = FALSE) {
      if (identical(args[1], "push") && !moved) {
        moved <<- TRUE
        # somebody else publishes after the remote was listed and before this push
        stopifnot(cp_git(other, "push", "--quiet", fx$remote, "HEAD:refs/heads/master")$status == 0L)
      }
      real_git(repo_root, args, error_ok = error_ok)
    }
  )

  expect_error(cp_quiet(clean_publish(repo_root = fx$repo, remote = "origin")), "push")

  expect_true(moved)
  expect_identical(cp_git(fx$remote, "rev-parse", "master")$out, cp_git(other, "rev-parse", "HEAD")$out)
  expect_identical(cp_current(fx$repo), "master")
  expect_true(is.na(cp_rev(fx$repo, "private-history")))
  expect_identical(cp_git(fx$repo, "config", "--get", "remote.origin.pushurl")$status, 1L)
})

# --- the push URL is disarmed after a successful push (CPA-01, CPB-04, CPA-09) ---------------------

test_that("after a successful push the push URL of the remote is disarmed and the real one recorded: git, --no-verify and gert cannot push by the remote's name any more (CPA-01, CPB-04)", {
  skip_if_no_git()
  fx <- cp_fixture()

  msgs <- testthat::capture_messages(clean <- clean_publish(repo_root = fx$repo, remote = "origin"))

  expect_identical(cp_git(fx$repo, "config", "--get", "remote.origin.pushurl")$out, "DISABLED-use-clean_publish")
  expect_identical(cp_git(fx$repo, "config", "--get", "cleanpublish.origin.realpushurl")$out, fx$remote)
  expect_match(msgs, "[DISARM]", fixed = TRUE, all = FALSE)
  refs <- paste("refs/heads/master", clean)
  for (args in list(
    c("push", "origin", "master"),
    c("push", "--no-verify", "origin", "private-history"),
    c("-c", "core.hooksPath=/dev/null", "push", "origin", "private-history:refs/heads/leak")
  )) {
    res <- cp_git(fx$repo, args)
    expect_true(res$status != 0L, info = paste(args, collapse = " "))
    expect_match(paste(res$out, collapse = "\n"), "DISABLED-use-clean_publish", fixed = TRUE, info = paste(args, collapse = " "))
  }
  # a client that does not run hooks (usethis uses it) reads the same push URL
  testthat::skip_if_not_installed("gert")
  git_push <- getExportedValue("gert", "git_push")
  res <- try(
    git_push(remote = "origin", refspec = "refs/heads/private-history:refs/heads/leak2", repo = fx$repo, verbose = FALSE),
    silent = TRUE
  )
  expect_s3_class(res, "try-error")
  expect_identical(cp_remote_refs(fx), refs)
  # reading is not affected
  expect_identical(cp_git(fx$repo, "fetch", "--quiet", "origin")$status, 0L)
})

test_that("a later run works from a disarmed remote, standalone and snapshot, through the recorded URL (CPA-01, CPB-04)", {
  skip_if_no_git()
  fx <- cp_fixture()
  clean1 <- cp_quiet(clean_publish(repo_root = fx$repo, remote = "origin"))
  cp_commit(fx$repo, "two.txt", "x", "two")

  msgs <- testthat::capture_messages(clean2 <- clean_publish(repo_root = fx$repo, remote = "origin"))
  expect_match(msgs, paste0("Push URL of 'origin': ", fx$remote), fixed = TRUE, all = FALSE)
  expect_identical(cp_remote_refs(fx), paste("refs/heads/master", clean2))
  cp_commit(fx$repo, "three.txt", "x", "three")
  clean3 <- cp_quiet(clean_publish(repo_root = fx$repo, remote = "origin", mode = "snapshot"))

  expect_identical(cp_git(fx$remote, "rev-parse", "master~1")$out, clean2)
  expect_identical(cp_git(fx$remote, "rev-parse", "master")$out, clean3)
  expect_identical(cp_git(fx$repo, "config", "--get", "remote.origin.pushurl")$out, "DISABLED-use-clean_publish")
  expect_identical(cp_git(fx$repo, "config", "--get-all", "cleanpublish.origin.realpushurl")$out, fx$remote)
})

test_that("a disarmed remote with no recorded URL is an error that says how to fix it; nothing changes (CPB-04)", {
  skip_if_no_git()
  fx <- cp_fixture()
  cp_quiet(clean_publish(repo_root = fx$repo, remote = "origin"))
  cp_git(fx$repo, "config", "--unset", "cleanpublish.origin.realpushurl")
  cp_commit(fx$repo, "two.txt", "x", "two")
  before <- cp_state(fx$repo)

  expect_error(
    cp_quiet(clean_publish(repo_root = fx$repo, remote = "origin")),
    "disarmed.*no real URL is recorded.*cleanpublish.origin.realpushurl.*Nothing was changed"
  )
  expect_identical(cp_state(fx$repo), before)
})

test_that("a remote with more than one push URL is refused, and the URLs are listed without credentials (CPA-09, CPA-01)", {
  skip_if_no_git()
  fx <- cp_fixture()
  cp_git(fx$repo, "config", "--add", "remote.origin.pushurl", fx$remote)
  cp_git(fx$repo, "config", "--add", "remote.origin.pushurl", "https://user:SECRET@example.invalid/mirror.git")
  before <- cp_state(fx$repo)

  err <- tryCatch(cp_quiet(clean_publish(repo_root = fx$repo, remote = "origin")), error = conditionMessage)

  expect_match(err, "has 2 push URLs", fixed = TRUE)
  expect_match(err, fx$remote, fixed = TRUE)
  expect_match(err, "https://example.invalid/mirror.git", fixed = TRUE)
  expect_false(grepl("SECRET", err, fixed = TRUE))
  expect_match(err, "Nothing was changed", fixed = TRUE)
  expect_identical(cp_state(fx$repo), before)
})

test_that("a remote with a push URL of its own is pushed by that URL, and that is the URL that is recorded (CPA-01)", {
  skip_if_no_git()
  fx <- cp_fixture()
  fetch_only <- file.path(fx$base, "fetch-only.git")
  cp_git(fx$base, "init", "--bare", "--quiet", fetch_only)
  cp_git(fx$repo, "remote", "set-url", "origin", fetch_only)
  cp_git(fx$repo, "config", "remote.origin.pushurl", fx$remote)

  clean <- cp_quiet(clean_publish(repo_root = fx$repo, remote = "origin"))

  expect_identical(cp_git(fx$remote, "rev-parse", "master")$out, clean)
  expect_identical(cp_git(fx$repo, "config", "--get", "cleanpublish.origin.realpushurl")$out, fx$remote)
  expect_identical(cp_git(fx$repo, "config", "--get", "remote.origin.url")$out, fetch_only)
  expect_identical(cp_git(fetch_only, "for-each-ref")$out, character())
})

test_that("disarm_push_url = FALSE leaves the remote as it was, and the hook is then the guard (CPA-01)", {
  skip_if_no_git()
  fx <- cp_fixture()

  msgs <- testthat::capture_messages(clean <- clean_publish(repo_root = fx$repo, remote = "origin", disarm_push_url = FALSE))

  expect_identical(cp_git(fx$repo, "config", "--get", "remote.origin.pushurl")$status, 1L)
  expect_identical(cp_git(fx$repo, "config", "--get", "cleanpublish.origin.realpushurl")$status, 1L)
  expect_false(any(grepl("[DISARM]", msgs, fixed = TRUE)))
  expect_push_refused(fx$repo, "private-history")
  expect_push_accepted(fx$repo, "master")
  expect_identical(cp_git(fx$remote, "rev-parse", "master")$out, clean)
})

test_that("a step that fails after the push says the remote is already updated and names the clean commit; the remote is disarmed by then (CPA-09)", {
  skip_if_no_git()
  fx <- cp_fixture()
  real_git <- .cp_git
  testthat::local_mocked_bindings(
    .cp_git = function(repo_root, args, error_ok = FALSE) {
      if (identical(args[1:2], c("branch", "-f")) && identical(args[3], "master")) stop("simulated failure")
      real_git(repo_root, args, error_ok = error_ok)
    }
  )

  err <- tryCatch(cp_quiet(clean_publish(repo_root = fx$repo, remote = "origin")), error = conditionMessage)

  clean <- cp_git(fx$remote, "rev-parse", "master")$out
  expect_match(err, "ALREADY UPDATED", fixed = TRUE)
  expect_match(err, clean, fixed = TRUE)
  expect_match(err, "simulated failure", fixed = TRUE)
  expect_identical(cp_git(fx$repo, "config", "--get", "remote.origin.pushurl")$out, "DISABLED-use-clean_publish")
})

test_that("no URL is printed or reported with its credentials (CPA-09, R-CP-13)", {
  skip_if_no_git()
  expect_identical(.cp_strip_credentials("https://user:tok@github.com/o/r.git"), "https://github.com/o/r.git")
  expect_identical(.cp_strip_credentials("https://user:p@ss@github.com/o/r.git"), "https://github.com/o/r.git")
  expect_identical(.cp_strip_credentials("https://github.com/o/r@v2.git"), "https://github.com/o/r@v2.git")
  expect_identical(.cp_strip_credentials("git@github.com:o/r.git"), "git@github.com:o/r.git")
  expect_identical(.cp_strip_credentials("C:/Users/me@site/pub.git"), "C:/Users/me@site/pub.git")
  expect_identical(
    .cp_redact("fatal: unable to access 'http://u:SECRET@127.0.0.1:9/x.git/': failed"),
    "fatal: unable to access 'http://127.0.0.1:9/x.git/': failed"
  )

  # a remote that cannot be reached: git's own error names the URL
  fx <- cp_fixture()
  cp_git(fx$repo, "remote", "add", "web", "http://user:SECRET@127.0.0.1:9/repo.git")
  msgs <- character()
  err <- withCallingHandlers(
    tryCatch(clean_publish(repo_root = fx$repo, remote = "web"), error = conditionMessage),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_match(err, "Could not list the refs", fixed = TRUE)
  expect_false(grepl("SECRET", paste(c(msgs, err), collapse = "\n"), fixed = TRUE))
  expect_match(paste(msgs, collapse = "\n"), "http://127.0.0.1:9/repo.git", fixed = TRUE)
})

test_that("guard_public_remote() on a disarmed remote fences its real URL, not the disarmed one (CPA-01, CPB-03)", {
  skip_if_no_git()
  fx <- cp_fixture()
  cp_quiet(clean_publish(repo_root = fx$repo, remote = "origin"))

  cp_quiet(guard_public_remote("origin", repo_root = fx$repo))

  stored <- cp_git(fx$repo, "config", "--get-all", "cleanpublish.publicurl")$out
  expect_true(.cp_normalize_url(fx$remote) %in% .cp_normalize_url(stored))
  expect_false(any(grepl("DISABLED", stored, fixed = TRUE)))
  # and clean_publish() still gets through the fence it made
  cp_commit(fx$repo, "two.txt", "x", "two")
  clean <- cp_quiet(clean_publish(repo_root = fx$repo, remote = "origin"))
  expect_identical(cp_git(fx$remote, "rev-parse", "master")$out, clean)
})

# --- submodules (CPA-11) ---------------------------------------------------------------------------

cp_add_submodule <- function(repo, url = "https://token@example.invalid/private/sub.git") {
  oid <- cp_rev(repo, "HEAD") # any commit id will do as the commit of a gitlink
  dir.create(file.path(repo, "vendor", "sub"), recursive = TRUE) # an empty directory: not checked out
  cp_git(repo, "update-index", "--add", "--cacheinfo", paste0("160000,", oid, ",vendor/sub"))
  writeLines(c("[submodule \"sub\"]", "\tpath = vendor/sub", paste0("\turl = ", url)), file.path(repo, ".gitmodules"))
  cp_git(repo, "add", ".gitmodules")
  stopifnot(cp_git(repo, "commit", "--quiet", "-m", "addsubmodule")$status == 0L)
}

test_that("a commit with a gitlink or a .gitmodules is refused, listing the URLs, unless allow_submodules = TRUE (CPA-11)", {
  skip_if_no_git()
  fx <- cp_fixture()
  cp_add_submodule(fx$repo)
  before <- cp_state(fx$repo)

  err <- tryCatch(cp_quiet(clean_publish(repo_root = fx$repo, push = FALSE)), error = conditionMessage)

  expect_match(err, "contains submodules", fixed = TRUE)
  expect_match(err, "vendor/sub", fixed = TRUE)
  expect_match(err, "https://example.invalid/private/sub.git", fixed = TRUE)
  expect_false(grepl("token@", err, fixed = TRUE))
  expect_match(err, "allow_submodules = TRUE", fixed = TRUE)
  expect_match(err, "Nothing was changed", fixed = TRUE)
  expect_identical(cp_state(fx$repo), before)

  clean <- cp_quiet(clean_publish(repo_root = fx$repo, push = FALSE, allow_submodules = TRUE))
  expect_identical(cp_git(fx$repo, "ls-tree", clean, "vendor/sub")$out, paste0("160000 commit ", cp_rev(fx$repo, "private-history~1"), "\tvendor/sub"))
})

test_that("a tracked .gitmodules without a gitlink is refused as well (CPA-11)", {
  skip_if_no_git()
  fx <- cp_fixture()
  cp_commit(fx$repo, ".gitmodules", "[submodule \"x\"]\n\tpath = x\n\turl = ../private/x.git", "gitmodules")
  expect_error(cp_quiet(clean_publish(repo_root = fx$repo, push = FALSE)), "contains submodules.*\\.\\./private/x\\.git")
  expect_identical(cp_ncommits(fx$repo, "master"), 4L)
})

# --- the command line (stage 2) --------------------------------------------------------------------

test_that("the command line takes --allow-other-refs, --allow-overwrite-history, --allow-submodules and --no-disarm", {
  none <- parse_clean_publish_args(character())
  expect_false(none$allow_other_refs)
  expect_false(none$allow_overwrite_history)
  expect_false(none$allow_submodules)
  expect_true(none$disarm_push_url)

  all <- parse_clean_publish_args(c("--allow-other-refs", "--allow-overwrite-history", "--allow-submodules", "--no-disarm"))
  expect_true(all$allow_other_refs)
  expect_true(all$allow_overwrite_history)
  expect_true(all$allow_submodules)
  expect_false(all$disarm_push_url)

  for (flag in c("--allow-other-refs", "--allow-overwrite-history", "--allow-submodules", "--no-disarm")) {
    expect_error(parse_clean_publish_args(c(flag, flag)), "more than once", info = flag)
    expect_match(paste(clean_publish_usage(), collapse = "\n"), flag, fixed = TRUE, info = flag)
    expect_error(parse_clean_publish_args(paste0(flag, "=yes")), "Unknown option", info = flag)
  }
})

test_that("scripts/clean_publish.R passes the new flags on: other refs on the remote stop it unless --allow-other-refs is given (CPD-03)", {
  skip_if_no_git()
  fx <- cp_fixture()
  # a branch of the remote that has nothing of this repository
  other <- cp_build_repo(fx$base, 1L, remote = FALSE, repo_name = "other")$repo
  expect_identical(cp_git(other, "push", "--quiet", fx$remote, "HEAD:refs/heads/old-branch")$status, 0L)

  refused <- run_clean_publish_script(c("--push", "--remote", "origin"), fx$repo)
  expect_true(refused$status != 0L)
  expect_match(paste(refused$out, collapse = "\n"), "refs/heads/old-branch", fixed = TRUE)
  expect_match(paste(refused$out, collapse = "\n"), "--allow-other-refs", fixed = TRUE)
  expect_identical(cp_ncommits(fx$remote, "master"), NA_integer_)

  pushed <- run_clean_publish_script(c("--push", "--remote", "origin", "--allow-other-refs", "--no-disarm"), fx$repo)
  expect_identical(pushed$status, 0L, info = paste(pushed$out, collapse = "\n"))
  expect_identical(cp_ncommits(fx$remote, "master"), 1L)
  expect_identical(cp_git(fx$repo, "config", "--get", "remote.origin.pushurl")$status, 1L)

  mixed <- run_clean_publish_script(c("--fence", fx$remote, "--allow-submodules"), fx$repo)
  expect_identical(mixed$status, 2L)
  expect_match(paste(mixed$out, collapse = "\n"), "cannot be combined with --allow-submodules", fixed = TRUE)
})

test_that("the new arguments must be TRUE or FALSE (stage 2)", {
  skip_if_no_git()
  fx <- cp_fixture()
  before <- cp_state(fx$repo)
  for (arg in c("allow_other_refs", "allow_overwrite_history", "allow_submodules", "disarm_push_url")) {
    expect_error(
      do.call(clean_publish, stats::setNames(list(fx$repo, "yes"), c("repo_root", arg))),
      paste0("`", arg, "` must be TRUE or FALSE"), fixed = TRUE, info = arg
    )
  }
  expect_identical(cp_state(fx$repo), before)
})

# ---- Stage 3: the command line as a function, tested in-process (CPC-04) ------------------------------
#
# The body of scripts/clean_publish.R is clean_publish_cli(), an internal function that returns the exit
# status. scripts/ is build-ignored, so the subprocess tests above skip under R CMD check; these do not.
# Every run changes into the throwaway repository first: the CLI acts on the working directory.

# Run clean_publish_cli(args) in `dir` and return its exit status and everything it printed
run_cli <- function(args, dir) {
  stopifnot(dir.exists(dir), startsWith(normalizePath(dir, winslash = "/"), normalizePath(tempdir(), winslash = "/")))
  out <- character()
  msgs <- character()
  status <- NULL
  withr::with_dir(dir, withCallingHandlers(
    out <- utils::capture.output(status <- clean_publish_cli(args)),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  ))
  list(status = status, out = out, msgs = msgs, text = paste(c(out, msgs), collapse = "\n"))
}

test_that("a bad command line is refused with status 2 and nothing is pushed or changed (CPC-04)", {
  skip_if_no_git()
  fx <- cp_fixture()
  before <- cp_state(fx$repo)
  bad <- list(
    "--nopush", "--dry-run", c("--push", "--nopush"), c("--push", "--remote"), c("--push", "--remote", "--no-push"),
    c("--remote", "origin", "--push", "--push"), c("--push", "--no-push", "--remote", "origin"),
    c("--remote", "origin", "stray"), "--remote=-x", c("-m", "--push"), "--allow-other-refs=yes"
  )
  for (args in bad) {
    res <- run_cli(args, fx$repo)
    info <- paste(args, collapse = " ")
    expect_identical(res$status, 2L, info = info)
    expect_match(res$text, "Error: ", fixed = TRUE, info = info)
    expect_match(res$text, "--no-push", fixed = TRUE, info = info) # the usage text is part of the message
    expect_identical(cp_state(fx$repo), before, info = info)
    expect_identical(cp_remote_refs(fx), character(), info = info)
    expect_false(file.exists(cp_hook_file(fx$repo)), info = info)
  }
  expect_match(run_cli("--nopush", fx$repo)$text, "Unknown option '--nopush'", fixed = TRUE)
})

test_that("--help prints the usage, which describes both modes, and returns 0 (CPC-04, CPD-12)", {
  skip_if_no_git()
  fx <- cp_fixture()
  before <- cp_state(fx$repo)
  for (flag in c("--help", "-h")) {
    res <- run_cli(flag, fx$repo)
    expect_identical(res$status, 0L, info = flag)
    expect_identical(res$out, clean_publish_usage(), info = flag)
    expect_identical(res$msgs, character(), info = flag)
  }
  usage <- paste(clean_publish_usage(), collapse = "\n")
  expect_match(usage, "standalone", fixed = TRUE)
  expect_match(usage, "snapshot", fixed = TRUE)
  expect_identical(cp_state(fx$repo), before)
})

test_that("--push and --snapshot need --remote: status 2, nothing pushed or changed (CPC-04, R-CP-02)", {
  skip_if_no_git()
  fx <- cp_fixture()
  before <- cp_state(fx$repo)
  for (args in list("--push", "--snapshot", c("--snapshot", "--push"), c("--push", "-m", "x"))) {
    res <- run_cli(args, fx$repo)
    info <- paste(args, collapse = " ")
    expect_identical(res$status, 2L, info = info)
    expect_match(res$text, "name the remote to publish to with --remote", fixed = TRUE, info = info)
    expect_identical(cp_state(fx$repo), before, info = info)
    expect_identical(cp_remote_refs(fx), character(), info = info)
  }
})

test_that("--fence takes no other option: each one is refused with status 2 and the fence is not written (CPC-04, R-CP-12)", {
  skip_if_no_git()
  fx <- cp_fixture()
  before <- cp_state(fx$repo)
  others <- list(
    "--push" = "--push", "--snapshot" = "--snapshot", "--branch" = c("--branch", "main"),
    "--private" = c("--private", "keep"), "--remote" = c("--remote", "origin"), "--message" = c("-m", "x"),
    "--allow-ignored" = c("--allow-ignored", "a.txt"), "--allow-other-refs" = "--allow-other-refs",
    "--allow-overwrite-history" = "--allow-overwrite-history", "--allow-submodules" = "--allow-submodules",
    "--no-disarm" = "--no-disarm"
  )
  for (name in names(others)) {
    res <- run_cli(c("--fence", fx$remote, others[[name]]), fx$repo)
    expect_identical(res$status, 2L, info = name)
    expect_match(res$text, paste0("cannot be combined with .*", name), info = name)
    expect_identical(cp_state(fx$repo), before, info = name)
    expect_false(file.exists(cp_hook_file(fx$repo)), info = name)
  }
})

test_that("the default is a dry run: local branches are rewritten and nothing is pushed (CPC-04, A5-04)", {
  skip_if_no_git()
  fx <- cp_fixture()
  res <- run_cli(character(), fx$repo)
  expect_identical(res$status, 0L, info = res$text)
  expect_match(res$text, "Dry run: local branches are rewritten, nothing is pushed", fixed = TRUE)
  expect_match(res$text, "[PUSH] Skipped (push = FALSE)", fixed = TRUE)
  expect_false(grepl("FORCE-PUSHED", res$text, fixed = TRUE))
  expect_identical(cp_remote_refs(fx), character())
  expect_identical(cp_ncommits(fx$repo, "master"), 1L)
  expect_identical(cp_ncommits(fx$repo, "private-history"), 3L)
  expect_identical(cp_current(fx$repo), "private-history")

  # --no-push is the same thing; a remote that is named but not pushed to is not touched either
  fx2 <- cp_fixture()
  res2 <- run_cli(c("--no-push", "--remote", "origin", "-m", "Dry"), fx2$repo)
  expect_identical(res2$status, 0L, info = res2$text)
  expect_identical(cp_remote_refs(fx2), character())
  expect_identical(cp_git(fx2$repo, "log", "-1", "--format=%s", "master")$out[1], "Dry")
})

test_that("--push publishes one commit by force and --snapshot --push adds a fast-forward commit (CPC-04)", {
  skip_if_no_git()
  fx <- cp_fixture()
  first <- run_cli(c("--push", "--remote", "origin", "-m", "Release one"), fx$repo)
  expect_identical(first$status, 0L, info = first$text)
  expect_match(first$text, "FORCE-PUSHED to remote 'origin'", fixed = TRUE)
  expect_match(first$text, paste0("Push URL of 'origin': ", fx$remote), fixed = TRUE)
  expect_identical(cp_ncommits(fx$remote, "master"), 1L)
  expect_identical(cp_git(fx$remote, "log", "-1", "--format=%s", "master")$out, "Release one")

  cp_commit(fx$repo, "v2.txt", "v2", "second release content")
  second <- run_cli(c("--snapshot", "--push", "--remote", "origin", "--message=Release two"), fx$repo)
  expect_identical(second$status, 0L, info = second$text)
  expect_match(second$text, "fast-forward pushed to remote 'origin'", fixed = TRUE)
  expect_identical(cp_ncommits(fx$remote, "master"), 2L)
  expect_identical(cp_git(fx$remote, "log", "-1", "--format=%s", "master")$out, "Release two")
  expect_identical(cp_git(fx$remote, "rev-parse", "master~1")$out, cp_git(fx$repo, "rev-parse", "master~1")$out)
})

test_that("--fence NAME and --fence URL install the fence and exit 0; a fence that matches no remote exits 1 (CPC-04, CPC-02)", {
  skip_if_no_git()
  fx <- cp_fixture()
  # from a subdirectory: the repository is its git toplevel, and it is named
  dir.create(file.path(fx$repo, "sub"))
  by_name <- run_cli(c("--fence", "origin"), file.path(fx$repo, "sub"))
  expect_identical(by_name$status, 0L, info = by_name$text)
  expect_match(by_name$text, "[GUARD] Guarded public remote", fixed = TRUE)
  repo_line <- grep("^Repository: ", by_name$msgs, value = TRUE)
  expect_length(repo_line, 1L)
  expect_identical(normalizePath(sub("^Repository: ", "", trimws(repo_line)), winslash = "/"), normalizePath(fx$repo, winslash = "/"))
  direct <- cp_git(fx$repo, "push", "origin", "master")
  expect_true(direct$status != 0L)
  expect_match(paste(direct$out, collapse = "\n"), "direct push to public remote .* is guarded")
  expect_identical(cp_remote_refs(fx), character())

  fx2 <- cp_fixture()
  by_url <- run_cli(c("--fence", fx2$remote), fx2$repo)
  expect_identical(by_url$status, 0L, info = by_url$text)
  expect_true(cp_git(fx2$repo, "push", "origin", "master")$status != 0L)
  expect_identical(cp_remote_refs(fx2), character())

  fx3 <- cp_fixture()
  before <- cp_state(fx3$repo)
  none <- run_cli(c("--fence", "nowhere"), fx3$repo)
  expect_identical(none$status, 1L)
  expect_match(none$text, "Error: .*does not match the push URL of any configured remote")
  expect_identical(cp_state(fx3$repo), before)
})

test_that("a run that stops with an error exits 1 and nothing is pushed: unknown remote, not a repository (CPC-04)", {
  skip_if_no_git()
  fx <- cp_fixture()
  before <- cp_state(fx$repo)
  res <- run_cli(c("--push", "--remote", "nope"), fx$repo)
  expect_identical(res$status, 1L)
  expect_match(res$text, "Error: .*not configured")
  expect_identical(cp_state(fx$repo), before)
  expect_identical(cp_remote_refs(fx), character())

  # a dirty tree
  writeLines("x", file.path(fx$repo, "untracked.txt"))
  dirty <- run_cli(c("--push", "--remote", "origin"), fx$repo)
  expect_identical(dirty$status, 1L)
  expect_match(dirty$text, "uncommitted changes")
  expect_identical(cp_remote_refs(fx), character())

  # outside a repository the CLI acts on nothing
  plain <- normalizePath(withr::local_tempdir("cp-"), winslash = "/")
  dir.create(file.path(plain, "empty"))
  withr::local_envvar(c(GIT_CEILING_DIRECTORIES = plain))
  outside <- run_cli(c("--push", "--remote", "origin"), file.path(plain, "empty"))
  expect_identical(outside$status, 1L)
  expect_match(outside$text, "is not inside a git repository")
})

test_that("--allow-other-refs lets a push through to a remote that has other refs, and it is refused without it (CPC-04, CPD-03)", {
  skip_if_no_git()
  fx <- cp_fixture()
  other <- cp_build_repo(fx$base, 1L, remote = FALSE, repo_name = "other")$repo
  expect_identical(cp_git(other, "push", "--quiet", fx$remote, "HEAD:refs/heads/old-branch")$status, 0L)
  before <- cp_state(fx$repo)

  refused <- run_cli(c("--push", "--remote", "origin"), fx$repo)
  expect_identical(refused$status, 1L)
  expect_match(refused$text, "refs/heads/old-branch", fixed = TRUE)
  expect_match(refused$text, "allow_other_refs = TRUE", fixed = TRUE)
  expect_identical(cp_state(fx$repo), before)
  expect_identical(cp_ncommits(fx$remote, "master"), NA_integer_)

  pushed <- run_cli(c("--push", "--remote", "origin", "--allow-other-refs"), fx$repo)
  expect_identical(pushed$status, 0L, info = pushed$text)
  expect_identical(cp_ncommits(fx$remote, "master"), 1L)
  expect_match(pushed$text, "refs/heads/old-branch", fixed = TRUE)
})

test_that("--allow-overwrite-history lets a push overwrite a remote that holds this repository's history (CPC-04, CPD-05)", {
  skip_if_no_git()
  fx <- cp_fixture()
  expect_identical(cp_git(fx$repo, "push", "--quiet", "origin", "master")$status, 0L)
  before <- cp_state(fx$repo)

  refused <- run_cli(c("--push", "--remote", "origin"), fx$repo)
  expect_identical(refused$status, 1L)
  expect_match(refused$text, "holds this repository's history", fixed = TRUE)
  expect_identical(cp_state(fx$repo), before)
  expect_identical(cp_ncommits(fx$remote, "master"), 3L)

  pushed <- run_cli(c("--push", "--remote", "origin", "--allow-overwrite-history"), fx$repo)
  expect_identical(pushed$status, 0L, info = pushed$text)
  expect_identical(cp_ncommits(fx$remote, "master"), 1L)
})

test_that("--allow-submodules lets a tree with a submodule be published, and it is refused without it (CPC-04, CPA-11)", {
  skip_if_no_git()
  fx <- cp_fixture()
  cp_add_submodule(fx$repo)
  before <- cp_state(fx$repo)

  refused <- run_cli(c("--push", "--remote", "origin"), fx$repo)
  expect_identical(refused$status, 1L)
  expect_match(refused$text, "contains submodules", fixed = TRUE)
  expect_match(refused$text, "https://example.invalid/private/sub.git", fixed = TRUE)
  expect_identical(cp_state(fx$repo), before)
  expect_identical(cp_ncommits(fx$remote, "master"), NA_integer_)

  pushed <- run_cli(c("--push", "--remote", "origin", "--allow-submodules"), fx$repo)
  expect_identical(pushed$status, 0L, info = pushed$text)
  expect_identical(cp_ncommits(fx$remote, "master"), 1L)
  expect_match(cp_git(fx$remote, "ls-tree", "master", "vendor/sub")$out, "^160000 commit ")
})

test_that("--no-disarm keeps the push URL of the remote usable, and by default it is disarmed after the push (CPC-04, CPA-01)", {
  skip_if_no_git()
  fx <- cp_fixture()
  kept <- run_cli(c("--push", "--remote", "origin", "--no-disarm"), fx$repo)
  expect_identical(kept$status, 0L, info = kept$text)
  expect_identical(cp_git(fx$repo, "config", "--get", "remote.origin.pushurl")$status, 1L)
  expect_match(kept$text, "stays usable", fixed = TRUE)

  fx2 <- cp_fixture()
  disarmed <- run_cli(c("--push", "--remote", "origin"), fx2$repo)
  expect_identical(disarmed$status, 0L, info = disarmed$text)
  expect_identical(cp_git(fx2$repo, "config", "--get", "remote.origin.pushurl")$out, "DISABLED-use-clean_publish")
  expect_identical(cp_git(fx2$repo, "config", "--get", "cleanpublish.origin.realpushurl")$out, fx2$remote)
})

test_that("every option reaches clean_publish() under the name the function takes (CPC-04)", {
  skip_if_no_git()
  fx <- cp_fixture()
  seen <- NULL
  testthat::local_mocked_bindings(clean_publish = function(...) {
    seen <<- list(...)
    invisible(strrep("a", 40))
  })
  res <- run_cli(c(
    "--push", "--remote", "pub", "--snapshot", "--branch", "release", "--private", "keep", "-m", "A message",
    "--allow-ignored", "a.txt,b.txt", "--allow-other-refs", "--allow-overwrite-history", "--allow-submodules", "--no-disarm"
  ), fx$repo)
  expect_identical(res$status, 0L, info = res$text)
  expect_identical(seen$remote, "pub")
  expect_identical(seen$mode, "snapshot")
  expect_identical(seen$publish_branch, "release")
  expect_identical(seen$private_branch, "keep")
  expect_identical(seen$commit_msg, "A message")
  expect_identical(seen$allow_ignored, c("a.txt", "b.txt"))
  expect_true(seen$push)
  expect_true(seen$allow_other_refs)
  expect_true(seen$allow_overwrite_history)
  expect_true(seen$allow_submodules)
  expect_false(seen$disarm_push_url)
  expect_null(seen$repo_root) # the function's own default: the git toplevel of the working directory
  expect_null(seen$confirm_guard) # there is no way to switch the guard confirmation off from the command line

  res <- run_cli(character(), fx$repo)
  expect_identical(res$status, 0L, info = res$text)
  expect_false(seen$push)
  expect_identical(seen$mode, "standalone")
  expect_identical(seen$private_branch, "private-history")
  expect_null(seen$publish_branch)
  expect_false(seen$allow_other_refs)
  expect_false(seen$allow_overwrite_history)
  expect_false(seen$allow_submodules)
  expect_true(seen$disarm_push_url)
})

test_that("warnings are shown when they happen, and one raised before the push stops the run: status 1, nothing pushed (CPC-01, CPC-04)", {
  skip_if_no_git()
  fx <- cp_fixture()
  warn_before <- getOption("warn")
  seen_warn <- NULL
  testthat::local_mocked_bindings(.cp_selftest_guard = function(...) {
    seen_warn <<- getOption("warn")
    warning("the guard could not be confirmed (test)", call. = FALSE)
    invisible(NA)
  })

  res <- run_cli(c("--push", "--remote", "origin"), fx$repo)

  expect_identical(seen_warn, 1L) # options(warn = 1): the warning is printed at once, not after SUCCESS
  expect_identical(getOption("warn"), warn_before) # and the option is put back
  expect_identical(res$status, 1L)
  expect_match(res$text, "the guard could not be confirmed (test)", fixed = TRUE)
  expect_match(res$text, "nothing was pushed", ignore.case = TRUE)
  expect_false(grepl("SUCCESS", res$text, fixed = TRUE))
  expect_identical(cp_remote_refs(fx), character())
  expect_identical(cp_ncommits(fx$repo, "master"), 3L) # no branch moved either
  expect_true(is.na(cp_rev(fx$repo, "private-history")))

  # a dry run stops at the warning as well
  res <- run_cli(character(), fx$repo)
  expect_identical(res$status, 1L)
  expect_identical(cp_ncommits(fx$repo, "master"), 3L)
})

test_that("a warning raised after the push does not turn a published run into a failure (CPC-04)", {
  skip_if_no_git()
  fx <- cp_fixture()
  real_git <- .cp_git
  warned <- FALSE
  testthat::local_mocked_bindings(.cp_git = function(repo_root, args, error_ok = FALSE) {
    if (!warned && identical(args[1:2], c("branch", "-f"))) {
      warned <<- TRUE
      warning("late warning (test)", call. = FALSE)
    }
    real_git(repo_root, args, error_ok = error_ok)
  })

  expect_warning(res <- run_cli(c("--push", "--remote", "origin"), fx$repo), "late warning \\(test\\)")

  expect_identical(res$status, 0L, info = res$text)
  expect_identical(cp_ncommits(fx$remote, "master"), 1L)
  expect_identical(cp_ncommits(fx$repo, "master"), 1L)
})

# ---- Stage 3: gaps that mutation testing found (CPC-05) -------------------------------------------------
#
# A reviewer changed the code in 22 places, one at a time, and ran the tests. These cover the mutants that
# survived or were only caught by accident.

test_that("a publish branch or a remote whose name starts with '-' is refused before anything changes (CPC-05)", {
  skip_if_no_git()
  fx <- cp_fixture()
  before <- cp_state(fx$repo)
  # a name like this would be read by git as an option (`git branch -f -x <commit>`, `git push -x`)
  expect_error(
    clean_publish(repo_root = fx$repo, publish_branch = "-x", push = FALSE),
    "`publish_branch` must be a single non-empty name that does not start with '-'", fixed = TRUE
  )
  expect_error(
    clean_publish(repo_root = fx$repo, publish_branch = "--force", remote = "origin"),
    "`publish_branch` must be", fixed = TRUE
  )
  for (name in c("-x", "--mirror")) {
    expect_error(
      clean_publish(repo_root = fx$repo, remote = name, push = FALSE),
      "`remote` must be a single non-empty name that does not start with '-'", fixed = TRUE, info = name
    )
    expect_error(clean_publish(repo_root = fx$repo, remote = name), "`remote` must be", fixed = TRUE, info = name)
    expect_error(clean_publish(repo_root = fx$repo, remote = name, mode = "snapshot"), "`remote` must be", fixed = TRUE, info = name)
  }
  expect_identical(cp_state(fx$repo), before)
  expect_identical(cp_remote_refs(fx), character())
})

test_that("a private branch that now holds history no recorded tip reaches is protected by its live tip (CPC-05)", {
  skip_if_no_git()
  fx <- cp_fixture(published = TRUE)
  # new, unrelated private history under the protected name: the recorded tips do not reach it
  cp_git(fx$repo, "checkout", "--quiet", "--orphan", "fresh")
  cp_commit(fx$repo, "unrelated.txt", "STUDY-ID-1234", "unrelated private work")
  cp_git(fx$repo, "branch", "-f", "private-history", "fresh")
  tip <- cp_rev(fx$repo, "private-history")
  expect_false(tip %in% cp_private_tips(fx$repo))

  # under another name: only the content rule, fed by the live tip of the protected branch, refuses it
  expect_push_refused_with(fx$repo, "contains commits of the private history", paste0(tip, ":refs/heads/oops"))
  expect_push_refused_with(fx$repo, "contains commits of the private history", "fresh:refs/heads/oops")
  expect_identical(cp_remote_refs(fx), character())
})
