#' Publish a Single Clean Commit to a Remote
#'
#' Collapses the entire current working tree into one zero-parent commit and
#' force-pushes it to \code{remote}, while preserving the full local commit
#' history on \code{private_branch}. A pre-push hook is installed (once) that
#' blocks \code{private_branch} from ever being pushed by accident.
#'
#' \code{remote} is a plain argument rather than hard-coded, so the same
#' clean snapshot can be published to more than one host -- e.g. run once
#' with the default \code{"origin"} and again with a second remote added via
#' \code{git remote add med-cbe <url>} -- without duplicating this logic.
#'
#' @param repo_root Path to the git repository root. Defaults to
#'   \code{\link[here]{here}()}.
#' @param publish_branch Branch to overwrite with the clean commit. Defaults
#'   to the current branch, or \code{master}/\code{main} if currently on
#'   \code{private_branch}.
#' @param private_branch Local branch that retains full history. Default
#'   \code{"private-history"}.
#' @param remote Name of a configured remote to force-push to. Default
#'   \code{"origin"}.
#' @param commit_msg Commit message for the single clean commit.
#' @param push Logical (default \code{TRUE}); if \code{FALSE}, rewrite the
#'   local branches but skip the force-push, as a dry run of the destructive
#'   step.
#' @return The clean commit SHA, invisibly.
#' @export
#' @examples
#' \dontrun{
#' clean_publish()
#' clean_publish(remote = "med-cbe")
#' }
clean_publish <- function(
  repo_root = NULL,
  publish_branch = NULL,
  private_branch = "private-history",
  remote = "origin",
  commit_msg = "Initial clean commit",
  push = TRUE
) {
  repo_root <- repo_root %||% here::here()

  run_git <- function(args, error_ok = FALSE) {
    stderr_file <- tempfile()
    on.exit(unlink(stderr_file), add = TRUE)
    out <- system2("git", c("-C", repo_root, args), stdout = TRUE, stderr = stderr_file)
    status <- attr(out, "status") %||% 0L
    err <- if (file.exists(stderr_file)) readLines(stderr_file, warn = FALSE) else character(0)
    if (status != 0L && !error_ok) {
      stop(
        "git ", paste(args, collapse = " "), " failed (status ", status, "):\n",
        paste(c(out, err), collapse = "\n"),
        call. = FALSE
      )
    }
    list(output = out, status = status)
  }

  # ----------------------------------------------------------------------------
  # 1. Resolve current + target publish branch
  # ----------------------------------------------------------------------------
  current_branch <- trimws(run_git(c("branch", "--show-current"))$output)

  if (is.null(publish_branch) || !nzchar(publish_branch)) {
    if (nzchar(current_branch) && current_branch != private_branch) {
      publish_branch <- current_branch
    } else if (run_git(c("show-ref", "--verify", "--quiet", "refs/heads/master"), error_ok = TRUE)$status == 0L) {
      publish_branch <- "master"
    } else if (run_git(c("show-ref", "--verify", "--quiet", "refs/heads/main"), error_ok = TRUE)$status == 0L) {
      publish_branch <- "main"
    } else {
      publish_branch <- "master"
    }
  }

  message("=== Clean Publish ===")
  message("Target publish branch: ", publish_branch)
  message("Private history branch: ", private_branch)
  message("Remote: ", remote)

  # ----------------------------------------------------------------------------
  # 2. Ensure working tree is clean
  # ----------------------------------------------------------------------------
  status_lines <- run_git(c("status", "--porcelain"))$output
  if (any(nzchar(status_lines))) {
    stop("You have uncommitted changes. Commit or stash them first.", call. = FALSE)
  }

  # ----------------------------------------------------------------------------
  # 3. Save current state to private_branch
  # ----------------------------------------------------------------------------
  if (current_branch != private_branch) {
    message("[BACKUP] Updating ", private_branch, " to current HEAD (", current_branch, ")...")
    run_git(c("branch", "-f", private_branch, "HEAD"))
  }

  # ----------------------------------------------------------------------------
  # 4. Guard private_branch against accidental push
  # ----------------------------------------------------------------------------
  message("[GUARD] Protecting ", private_branch, " from accidental remote push...")
  run_git(c("config", paste0("branch.", private_branch, ".pushRemote"), "no_push"), error_ok = TRUE)

  git_dir <- trimws(run_git(c("rev-parse", "--git-dir"))$output)
  git_dir <- if (fs::is_absolute_path(git_dir)) git_dir else fs::path(repo_root, git_dir)
  hook_dir <- fs::path(git_dir, "hooks")
  pre_push_hook <- fs::path(hook_dir, "pre-push")

  if (!fs::file_exists(pre_push_hook)) {
    fs::dir_create(hook_dir)
    writeLines(
      c(
        "#!/usr/bin/env bash",
        "while read -r local_ref local_oid remote_ref remote_oid; do",
        sprintf('    if [[ "$local_ref" == *"%s"* ]]; then', private_branch),
        "        echo \"ERROR: Push aborted! Attempted to push private branch '$local_ref' to remote.\" >&2",
        "        exit 1",
        "    fi",
        "done",
        "exit 0"
      ),
      pre_push_hook
    )
    tryCatch(Sys.chmod(pre_push_hook, mode = "0755"), error = function(e) NULL)
  }

  # ----------------------------------------------------------------------------
  # 5. Create a single zero-parent commit matching the full working tree
  # ----------------------------------------------------------------------------
  message("[REWRITE] Generating single clean commit for ", publish_branch, "...")
  run_git(c("checkout", private_branch))

  tree_id <- trimws(run_git(c("write-tree"))$output)

  # commit-tree's message is written via -F <file> rather than -m <string>:
  # git.exe's argv handling on Windows mis-splits a multi-word -m value when
  # invoked through R's system2(), even though the identical argument vector
  # works fine from a native shell.
  msg_file <- tempfile()
  on.exit(unlink(msg_file), add = TRUE)
  writeLines(commit_msg, msg_file, useBytes = TRUE)
  clean_commit_id <- trimws(run_git(c("commit-tree", tree_id, "-F", msg_file))$output)

  # ----------------------------------------------------------------------------
  # 6. Point publish_branch to the clean commit
  # ----------------------------------------------------------------------------
  run_git(c("branch", "-f", publish_branch, clean_commit_id))

  # ----------------------------------------------------------------------------
  # 7. Force-push clean branch to remote
  # ----------------------------------------------------------------------------
  if (isTRUE(push)) {
    message("[PUSH] Force-pushing single-commit ", publish_branch, " to ", remote, "...")
    run_git(c("push", remote, publish_branch, "--force"))
  } else {
    message("[PUSH] Skipped (push = FALSE). ", publish_branch, " rewritten locally only.")
  }

  # ----------------------------------------------------------------------------
  # 8. Stay on private development branch
  # ----------------------------------------------------------------------------
  run_git(c("checkout", private_branch))
  run_git(c("branch", "--unset-upstream"), error_ok = TRUE)

  message("============================================================")
  message("SUCCESS!")
  if (isTRUE(push)) {
    message(remote, "/", publish_branch, " now has exactly 1 clean commit.")
  }
  message("Full history is preserved locally on: ", private_branch)
  message("============================================================")

  invisible(clean_commit_id)
}
