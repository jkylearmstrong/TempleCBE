# Fixtures for test-clean_publish.R.
#
# clean_publish() rewrites branches and force-pushes, so every test works in a
# throwaway repository under withr::local_tempdir() whose ONLY remote is a bare
# repository next to it. Nothing here reads or writes the real repository, its
# remotes or the network, and git is isolated from the user's and the system's
# configuration (a global core.hooksPath or template directory would otherwise
# change what the tests see).
#
# Starting a git process is slow on Windows, so the two repositories most tests
# start from are built once per run (cp_template()) and copied for each test:
#   plain      3 commits on master, remote "origin" is an empty bare repository
#   published  plain, after a first clean_publish(push = FALSE): on private-history
#              (the 3 commits), master is the one clean commit, the guard is installed

skip_if_no_git <- function() {
  testthat::skip_on_cran()
  if (!nzchar(Sys.which("git"))) testthat::skip("git is not available")
  version <- suppressWarnings(system2("git", "--version", stdout = TRUE, stderr = FALSE))
  number <- sub("^git version ([0-9]+\\.[0-9]+).*$", "\\1", version[1])
  if (!grepl("^[0-9]+\\.[0-9]+$", number) || numeric_version(number) < "2.20") {
    testthat::skip("git 2.20 or newer is needed")
  }
  if (!nzchar(Sys.which("sh")) && .Platform$OS.type != "windows") {
    testthat::skip("no POSIX shell for git hooks")
  }
}

# git sees neither the user's nor the system configuration, and none of the
# GIT_* variables a parent git process (a hook running the tests) would set.
# The one global setting it does get switches off background maintenance and
# auto-gc: a git process still working inside a repository while cp_fixture()
# copies it made file.copy() fail on the macOS runner ("problem reading
# directory .../.git/objects/maintenance").
cp_isolate_git <- function(env = parent.frame()) {
  config <- withr::local_tempfile(fileext = ".gitconfig", .local_envir = env)
  writeLines(c("[gc]", "\tauto = 0", "\tautoDetach = false", "[maintenance]", "\tauto = false"), config)
  withr::local_envvar(
    c(
      GIT_CONFIG_NOSYSTEM = "1", GIT_CONFIG_GLOBAL = config, GIT_TERMINAL_PROMPT = "0",
      GIT_DIR = NA, GIT_WORK_TREE = NA, GIT_INDEX_FILE = NA, GIT_COMMON_DIR = NA,
      GIT_PREFIX = NA, GIT_OBJECT_DIRECTORY = NA, GIT_ALTERNATE_OBJECT_DIRECTORIES = NA,
      GIT_NAMESPACE = NA, GIT_CEILING_DIRECTORIES = NA
    ),
    .local_envir = env
  )
}

# Run git in `repo`; stderr is merged into `out`, so hook messages can be matched
cp_git <- function(repo, ...) {
  out <- suppressWarnings(system2("git", shQuote(c("-C", repo, c(...))), stdout = TRUE, stderr = TRUE))
  status <- attr(out, "status") %||% 0L
  attr(out, "status") <- NULL
  list(out = out, status = status)
}

cp_commit <- function(repo, file, text, message) {
  writeLines(text, file.path(repo, file))
  cp_git(repo, "add", "-A")
  res <- cp_git(repo, "commit", "--quiet", "-m", message)
  stopifnot(res$status == 0L)
  invisible(NULL)
}

# `n_commits` commits on master and, unless `remote = FALSE`, a bare repository as
# the only remote ("origin"), both under `base`. Paths use forward slashes.
cp_build_repo <- function(base, n_commits = 3L, remote = TRUE, repo_name = "work", remote_name = "remote.git") {
  repo <- file.path(base, repo_name)
  dir.create(repo)
  cp_git(repo, "-c", "init.defaultBranch=master", "init", "--quiet")
  for (kv in list(
    c("user.name", "Test User"), c("user.email", "test@example.invalid"),
    c("core.autocrlf", "false"), c("commit.gpgsign", "false"), c("core.longpaths", "true")
  )) {
    cp_git(repo, "config", kv[1], kv[2])
  }
  bare <- NULL
  if (remote) {
    bare <- file.path(base, remote_name)
    dir.create(bare)
    cp_git(bare, "-c", "init.defaultBranch=master", "init", "--bare", "--quiet")
    cp_git(bare, "config", "core.longpaths", "true")
    cp_git(repo, "remote", "add", "origin", bare)
  }
  for (i in seq_len(n_commits)) cp_commit(repo, paste0("f", i, ".txt"), paste("line", i), paste0("c", i))
  list(base = base, repo = repo, remote = bare)
}

.cp_templates <- new.env(parent = emptyenv())

# Directory holding a built "plain" or "published" pair of repositories (see the top)
cp_template <- function(published = FALSE) {
  key <- if (published) "published" else "plain"
  dir <- .cp_templates[[key]]
  if (!is.null(dir) && dir.exists(dir)) return(dir)
  if (is.null(.cp_templates$root)) {
    .cp_templates$root <- normalizePath(tempfile("cp-templates-"), winslash = "/", mustWork = FALSE)
    dir.create(.cp_templates$root)
    reg.finalizer(
      .cp_templates, function(e) unlink(e$root, recursive = TRUE, force = TRUE), onexit = TRUE
    )
  }
  dir <- file.path(.cp_templates$root, key)
  dir.create(dir)
  cp_build_repo(dir)
  if (published) cp_quiet(clean_publish(repo_root = file.path(dir, "work"), push = FALSE))
  .cp_templates[[key]] <- dir
  dir
}

# Copy the "work" and "remote.git" repositories of `template` into `base`. A
# copy that fails part way (a file that vanishes while it is read) is removed and
# tried again; the last attempt reports its own warnings.
cp_copy_template <- function(template, base, attempts = 3L, copy = file.copy) {
  from <- file.path(template, c("work", "remote.git"))
  copy_once <- function() all(copy(from, base, recursive = TRUE, copy.date = TRUE))
  for (i in seq_len(attempts - 1L)) {
    if (suppressWarnings(copy_once())) return(invisible(TRUE))
    unlink(file.path(base, c("work", "remote.git")), recursive = TRUE, force = TRUE)
    Sys.sleep(0.25 * i)
  }
  stopifnot(copy_once())
  invisible(TRUE)
}

# A fresh copy of a template (or, for another shape, a repository built from
# scratch). Returns list(base, repo, remote).
cp_fixture <- function(published = FALSE, n_commits = 3L, remote = TRUE, repo_name = "work",
                       remote_name = "remote.git", env = parent.frame()) {
  cp_isolate_git(env)
  base <- normalizePath(withr::local_tempdir("cp-", .local_envir = env), winslash = "/")
  if (n_commits != 3L || !remote) {
    stopifnot(!published)
    return(cp_build_repo(base, n_commits, remote, repo_name, remote_name))
  }
  template <- cp_template(published)
  cp_copy_template(template, base)
  stopifnot(file.rename(file.path(base, "work"), file.path(base, repo_name)))
  stopifnot(file.rename(file.path(base, "remote.git"), file.path(base, remote_name)))
  fx <- list(base = base, repo = file.path(base, repo_name), remote = file.path(base, remote_name))
  cp_git(fx$repo, "config", "remote.origin.url", fx$remote)
  fx
}

cp_rev <- function(repo, ref) {
  res <- cp_git(repo, "rev-parse", "--verify", "--quiet", paste0(ref, "^{commit}"))
  if (res$status == 0L) res$out[1] else NA_character_
}

cp_ncommits <- function(repo, ref) {
  res <- cp_git(repo, "rev-list", "--count", ref)
  if (res$status == 0L) as.integer(res$out[1]) else NA_integer_
}

cp_current <- function(repo) {
  res <- cp_git(repo, "symbolic-ref", "--quiet", "--short", "HEAD")
  if (res$status == 0L) res$out[1] else ""
}

# Everything clean_publish() could change in the repository, for "nothing was changed"
cp_state <- function(repo) {
  list(
    refs = cp_git(repo, "for-each-ref", "--format=%(refname) %(objectname)")$out,
    head = cp_git(repo, "rev-parse", "HEAD")$out,
    branch = cp_git(repo, "symbolic-ref", "--quiet", "HEAD")$out,
    status = cp_git(repo, "status", "--porcelain")$out,
    config = cp_git(repo, "config", "--local", "--list")$out
  )
}

# refs of the fake remote (sorted), as seen from outside the working repository
cp_remote_refs <- function(fx) {
  sort(cp_git(fx$remote, "for-each-ref", "--format=%(refname) %(objectname)")$out)
}

cp_hook_file <- function(repo) file.path(repo, ".git", "hooks", "pre-push")

cp_backup_refs <- function(repo) {
  cp_git(repo, "for-each-ref", "--format=%(refname) %(objectname)", "refs/backup/clean_publish")$out
}

cp_quiet <- function(expr) suppressMessages(expr)

# Make the bare repository of `fx` list its refs as usual but refuse every push, as a server with a
# rejecting pre-receive hook does: a push that fails after the client has seen the remote
cp_reject_pushes <- function(fx) {
  hook <- file.path(fx$remote, "hooks", "pre-receive")
  dir.create(dirname(hook), showWarnings = FALSE)
  writeLines(c("#!/bin/sh", "echo 'rejected by the test remote' >&2", "exit 1"), hook)
  Sys.chmod(hook, "0755")
  invisible(hook)
}

# A repository that has cloned `fx$repo` (the private repository) the way a throwaway clone is made:
# its own identity, `origin` is the private repository. Returns its path.
cp_clone <- function(fx, name = "clone") {
  path <- file.path(fx$base, name)
  res <- cp_git(fx$base, "clone", "--quiet", fx$repo, path)
  stopifnot(res$status == 0L)
  for (kv in list(c("user.name", "Test User"), c("user.email", "test@example.invalid"), c("commit.gpgsign", "false"))) {
    cp_git(path, "config", kv[1], kv[2])
  }
  path
}

# Publish once without pushing, leaving the repository on private-history as the
# function does; returns the clean commit id
cp_first_publish <- function(fx, ...) cp_quiet(clean_publish(repo_root = fx$repo, push = FALSE, ...))

# POSIX shells that can run the hook: those on the PATH and, on Windows, the ones
# Git for Windows ships (they are not on the PATH). `bash` is only looked for on the
# PATH off Windows, where it can be the launcher of the Windows Subsystem for Linux.
cp_posix_shells <- function() {
  found <- Sys.which(c("sh", "dash", if (.Platform$OS.type != "windows") "bash"))
  found <- unname(found[nzchar(found)])
  if (.Platform$OS.type == "windows") {
    exec_path <- suppressWarnings(system2("git", "--exec-path", stdout = TRUE, stderr = FALSE))
    root <- normalizePath(file.path(exec_path[1], "..", "..", ".."), winslash = "/", mustWork = FALSE)
    candidates <- file.path(root, "usr", "bin", c("sh.exe", "dash.exe", "bash.exe"))
    found <- c(found, candidates[file.exists(candidates)])
  }
  unique(found)
}

# One shell of each kind (sh, dash, bash), for tests that run the hook once per shell
cp_hook_shells <- function() {
  shells <- cp_posix_shells()
  shells[!duplicated(tolower(sub("[.]exe$", "", basename(shells))))]
}

# One record of what git writes to the hook's standard input
cp_record <- function(local_ref, local_oid, remote_ref = local_ref, remote_oid = strrep("0", nchar(local_oid))) {
  paste(local_ref, local_oid, remote_ref, remote_oid)
}

# Run the pre-push hook of `repo` by hand under `shell`, as `git push` would: in the top of the
# work tree, with the remote's name and URL as arguments and `records` on its standard input
# (nothing, when empty). `env` is a named character vector of variables for the run; the
# TEMPLECBE_PUBLISHING of the caller's environment is not passed on unless `env` sets it.
# Returns list(status, out), stderr merged into out.
cp_run_hook <- function(shell, repo, records = character(), remote = "origin", url = "", env = character()) {
  hook <- cp_hook_file(repo)
  withr::local_dir(repo)
  withr::local_envvar(c(TEMPLECBE_PUBLISHING = NA, env))
  withr::local_path(dirname(shell), action = "prefix")
  # the records as git writes them: one line each, ended by a bare line feed (not the carriage
  # return and line feed that a text-mode file would get on Windows)
  stdin <- withr::local_tempfile()
  writeBin(charToRaw(if (length(records)) paste0(paste(records, collapse = "\n"), "\n") else ""), stdin)
  out <- suppressWarnings(system2(
    shell, shQuote(c(hook, remote, url)), stdin = stdin, stdout = TRUE, stderr = TRUE
  ))
  status <- attr(out, "status") %||% 0L
  attr(out, "status") <- NULL
  list(status = status, out = out)
}
