#' Publish a Clean Snapshot of the Current Commit to a Remote
#'
#' Publishes the tree of the current commit as a clean commit that carries none of the history
#' behind it, and keeps the full history on \code{private_branch}. There are two modes.
#' \code{mode = "standalone"} (the default) makes one commit without a parent and, with
#' \code{push = TRUE}, force-pushes it over the publish branch of \code{remote}; it is the first
#' publication, into a new empty repository. \code{mode = "snapshot"} makes one commit whose
#' parent is the tip of the publish branch of \code{remote}, which must consist only of commits
#' this function made, and pushes it as an ordinary fast-forward; it is every later release.
#' With \code{push = FALSE} only the local branches are rewritten. A pre-push hook keeps
#' \code{private_branch} and every commit that only it contains from being pushed by accident.
#'
#' \strong{The target must be a NEW EMPTY repository.} A force-push replaces one branch of the
#' remote and nothing else: its other branches, its tags, its notes and GitHub's hidden
#' \code{refs/pull/*} keep the commits they point to, so a repository that ever held the private
#' history keeps it readable, and no push, force-push included, can remove a hidden ref. Create
#' the public repository for this purpose, add it to a throwaway clone as the only remote and
#' publish into it (the steps are in \file{REVIEW.md}, "Publishing to the public repository").
#'
#' \code{remote} is the name of a remote you have added, for example \code{git remote add public
#' <url>}, and has no default: in a clone of the private repository \code{origin} is the private
#' repository, and a standalone publish force-pushes one commit over its branch. To publish the
#' same snapshot to a second host, add a second remote (\code{git remote add med-cbe <url>}) and
#' run again with \code{remote = "med-cbe"}.
#'
#' \strong{Nothing is changed until every check has passed.} The function
#' stops, and leaves branches, the working tree, the git configuration, the hooks and the
#' remote as they were, when
#' \itemize{
#'   \item git has no identity for the clean commit (\code{git var GIT_AUTHOR_IDENT} or
#'     \code{GIT_COMMITTER_IDENT} fails, as in a fresh clone);
#'   \item a branch name is not valid as \code{refs/heads/<name>}, starts with an at sign or
#'     contains an at sign followed by a brace, or the private and the publish branch differ only
#'     in case;
#'   \item the working tree has uncommitted changes or untracked files (files
#'     ignored by \code{.gitignore} do not count and are never published: the
#'     snapshot is exactly the tree of the current commit, tracked files that
#'     are also ignored included);
#'   \item the current commit contains submodules (a gitlink or a tracked
#'     \file{.gitmodules}), unless \code{allow_submodules = TRUE};
#'   \item \code{private_branch} exists but is not contained in the current
#'     commit, so that moving it to the current commit would drop commits from
#'     every branch. Run the function from \code{private_branch} (or a branch
#'     that contains it), not from the publish branch or an older branch;
#'   \item the publish branch, or \code{private_branch} when it is not the
#'     current branch, is checked out in another git worktree;
#'   \item \code{push = TRUE} and \code{remote} is not a configured remote, has more than one
#'     push URL, or its refs cannot be listed, or (standalone mode) holds this repository's
#'     history or has refs other than the publish branch (see the section on the remote);
#'   \item \code{mode = "snapshot"} and the history of the remote branch is not made only of
#'     clean commits, or the repository is shallow;
#'   \item an existing \code{pre-push} hook is not a shell script, or the hook
#'     directory lies inside the working tree and is not ignored.
#' }
#' Every git call a check depends on stops the run when it fails; none is read as "nothing
#' found". After the checks the hook and the configuration are written and the guard is
#' tested (see below); if that fails, branches and remote are still untouched.
#'
#' \strong{The identity that is published.} The clean commit takes its author and committer,
#' and the time zone offset of its dates, from git's own settings (\code{user.name},
#' \code{user.email}, the \code{GIT_AUTHOR_*} and \code{GIT_COMMITTER_*} environment variables,
#' dates included). They are printed before anything changes; set them in the environment of
#' the R session to publish another identity.
#'
#' \strong{The remote.} \code{push = TRUE} looks at the remote before anything changes.
#' \itemize{
#'   \item The real push URL is read with \code{git remote get-url --push --all}; a remote with
#'     more than one push URL is refused. The push goes to that URL (\code{git push <url>
#'     <commit>:refs/heads/<branch>}), not to the name of the remote, and every URL that is
#'     printed or reported has its credentials removed.
#'   \item Standalone mode lists \emph{all} refs of the remote (\code{git ls-remote}; a failing
#'     listing is an error) and refuses, unless \code{allow_other_refs = TRUE}, when there is any
#'     ref besides the publish branch. A force-push replaces one branch only: tags, other
#'     branches, notes and GitHub's hidden \code{refs/pull/*} keep the old commits, and the last
#'     cannot be removed at all. \strong{Publish into a new, empty repository.} It also refuses,
#'     unless \code{allow_overwrite_history = TRUE}, when the tip of a branch of the remote is a
#'     commit that this repository has (reachable from the commit being published, from the
#'     private or the publish branch or from a recorded private tip) and that is not a clean
#'     commit: the remote then holds this repository's history, as \code{origin} does in a
#'     clone of the private repository.
#'   \item The standalone push is a force-push with an explicit lease on what the listing
#'     showed: \code{--force-with-lease=refs/heads/<branch>:<id>} (empty when the branch does not
#'     exist), so it fails when the branch moved in the meantime and does not depend on a
#'     possibly stale remote-tracking ref.
#'   \item Snapshot mode builds on the tip of the remote branch, so that history is part of the
#'     snapshot. Every commit this function makes ends with the trailer line
#'     \code{Clean-Publish: v1}, and snapshot mode refuses a remote branch unless its whole
#'     history is linear, has one parentless root and consists of such commits (otherwise the
#'     message gives the number of commits that lack the trailer and the newest of them). A remote
#'     that holds the private repository, a rewritten archive of it or someone else's commits is
#'     refused; a shallow repository is refused too, because it cannot show the history.
#'   \item After a successful push the real push URL is recorded in the git config key
#'     \code{cleanpublish.<remote>.realpushurl} and \code{remote.<remote>.pushurl} is set to
#'     \code{DISABLED-use-clean_publish}, which no client can use (unless
#'     \code{disarm_push_url = FALSE}). A later \code{git push <remote>}, \code{git push
#'     --no-verify}, a different \code{core.hooksPath} and \pkg{gert} (which \pkg{usethis} uses)
#'     then fail instead of publishing private history; fetching is not affected, and later runs
#'     of this function read the recorded URL. To undo it: \code{git remote set-url --push
#'     <remote> <url>}. An explicit push to the real URL, \code{git send-pack} and
#'     other tools that do not read the remote's configuration still work.
#'   \item The success message states only what is guaranteed: the publish branch of the remote
#'     now points to the clean commit. It lists the other refs of the remote, which keep whatever
#'     they point to. If a step fails after the push, the message says that the remote is already
#'     updated and names the clean commit.
#' }
#'
#' \strong{Safety net.} Before a branch is moved, the old tips of the private
#' and the publish branch are kept as
#' \code{refs/backup/clean_publish/<UTC time>/<branch>}. To restore one, run \code{git branch -f
#' <branch> <that ref>} from another branch; git refuses it for the branch that is checked out,
#' and the private branch is the checked-out one after every run, so there run \code{git checkout
#' <other branch>} first, or \code{git reset --hard <that ref>}. The remote is updated first and the
#' local branches are only moved afterwards, so a failed push leaves the local
#' branches untouched; if any later step fails, the branch that was checked out
#' is checked out again. After the moves the function checks that both branches are
#' where it put them (on a case-insensitive file system a name that differs in case is the
#' same branch) and stops with the backup refs when they are not.
#'
#' \strong{The push guard.} The hook is installed where git reads it (the
#' \code{hooks/pre-push} of \code{git rev-parse --git-path}: shared by linked
#' worktrees, and honouring \code{core.hooksPath}). It goes into an existing
#' hook for \command{sh}, \command{dash}, \command{bash} or \command{ksh} as a clearly
#' marked block right after its first line, so the hook keeps working (it still
#' receives its input, and none when nothing is pushed), and that block is
#' re-written, not duplicated, on the next run. A hook in another language,
#' \command{zsh} included, stops the run with a message, and the hook an earlier
#' version of this function wrote is replaced; no other hook is replaced or skipped
#' silently. The private history is protected by commit id, whatever becomes of the
#' branch names. Before it pushes or moves anything, the function records every commit
#' that holds private history -- the commit it publishes and the earlier tips of the
#' private and the publish branch -- as a full id in the multi-valued git config key
#' \code{cleanpublish.privatetip} (never removed), and the names of the private branches
#' in \code{cleanpublish.protectedbranch} (one per \code{private_branch} ever used). The
#' hook refuses to push
#' \itemize{
#'   \item any ref named like a protected branch;
#'   \item any ref, tag or other branch from which a commit can be reached that the live tip
#'     of a protected branch or a recorded private tip reaches, and that is not part of a clean
#'     commit made by this function (their ids are the values of
#'     \code{cleanpublish.cleancommit}). Pushing a tag on a private commit, an object id, the
#'     publish branch after merging the private branch into it, or \code{--mirror} is
#'     therefore refused too, also after the private branch was renamed, deleted or moved, and
#'     after a first publish whose push failed;
#'   \item anything that is not made of clean commits, when protected branches are configured
#'     and neither they nor a recorded tip can be found in the repository (the message says how
#'     to release the guard: \code{git config --unset-all cleanpublish.protectedbranch});
#'   \item a tag, tree or blob that does not lead to a commit, unless a clean commit has it.
#' }
#' Both keys take full commit ids and nothing else: a revision such as a branch name or an
#' abbreviated id is ignored. See \code{\link{guard_public_remote}()} for the fence on the
#' public URL and for \code{TEMPLECBE_PUBLISHING=1}, which this function sets for its own push
#' and which lets through nothing but the clean commits it recorded.
#'
#' \strong{The guard is confirmed before anything moves.} After installing the hook the
#' function pushes to a temporary local repository: the private branch name, and the commit it
#' publishes under another branch name and as a lightweight tag. Each push must be refused by
#' the hook. It stops with an error, before any branch moves or anything is pushed, when a push
#' is accepted (the hook does not run, or knows only the branch name), and also when a push is
#' refused for any other reason, because then the guard is unconfirmed. Use
#' \code{confirm_guard = FALSE} to go on with a warning when a push was refused for another reason
#' and that cannot be helped.
#'
#' \strong{What the guard does not stop.} It is a hook of command-line \command{git}, so
#' it does not stop \code{git push --no-verify}, a push made by a client that does not run
#' hooks (the \pkg{gert} package, which \pkg{usethis} uses, is one), a push after
#' \code{core.hooksPath} has been pointed elsewhere or the hook has been deleted, \code{git
#' send-pack}, or private commits that no recorded tip and no protected branch reaches
#' (history that was created after the last run and is not an ancestor of a recorded tip).
#' Disarming the push URL (see above) stops the first two and the last client for pushes by
#' the name of the remote, not for a push to its real URL. The guard also needs the objects: if
#' \command{git gc} has removed the commit of a recorded tip, that tip is ignored with a note.
#' The safest procedure is to publish from a throwaway clone and to keep no pushable public
#' remote in the clone you work in.
#'
#' \strong{The command line.} \code{Rscript scripts/clean_publish.R --help} lists the options of
#' the same function. Unlike \code{clean_publish()} it is a dry run unless \code{--push} is given
#' (\code{--snapshot} selects the snapshot mode, \code{--remote NAME} names the remote and is
#' required for both, \code{--fence NAME|URL} calls \code{\link{guard_public_remote}()} and takes
#' no other option). An unknown or mistyped option is an error. It runs with
#' \code{options(warn = 1)}, and a warning raised before the push stops the run: nothing is pushed
#' and the exit status is 1 (2 for a bad command line, 0 for success). The logic is the internal
#' function \code{clean_publish_cli()}; the script only loads the package and calls it.
#'
#' @param repo_root Path to the git repository root (or any directory of its
#'   working tree; it may be a linked worktree and may contain spaces).
#'   Defaults to the git toplevel of the working directory (an error when that is not in a
#'   repository). The repository that is acted on, its git directory, the current branch and
#'   the remotes with their URLs are printed first.
#' @param publish_branch Branch to overwrite with the clean commit. Defaults
#'   to \code{master}, else \code{main}, else the current branch (unless that
#'   is \code{private_branch}). \code{master}/\code{main} are checked before
#'   the current branch on purpose: running this from an arbitrary feature
#'   branch must not silently overwrite that branch with a one-commit
#'   history. \code{private_branch} itself is refused, since publishing onto
#'   it would squash away the full history it exists to keep, and so is a name that differs
#'   from it only in case.
#' @param private_branch Local branch that retains full history. Default
#'   \code{"private-history"}. Created from the current commit if it does not
#'   exist, otherwise moved forward to the current commit (never backward or
#'   sideways, see above).
#' @param remote Name of a configured remote to publish to. Required for
#'   \code{push = TRUE} and for \code{mode = "snapshot"}, and deliberately without a
#'   default: in a clone of the private repository \code{"origin"} is the private
#'   repository, and a standalone publish force-pushes one commit over its branch. Its
#'   push URL is printed before the push. In snapshot mode the remote must be the one
#'   that holds the public snapshots: a remote branch whose history is not made only of
#'   clean commits made by this function is refused, because the snapshot would carry that
#'   history.
#' @param commit_msg Commit message for the clean commit. Default: \code{"Initial
#'   clean commit"}, or \code{"Snapshot <date>"} for \code{mode = "snapshot"}. The trailer
#'   line \code{Clean-Publish: v1} is appended after a blank line.
#' @param push Logical (default \code{TRUE}); if \code{FALSE}, rewrite the
#'   local branches but skip the force-push, as a dry run of the destructive
#'   step. (The command-line script \code{scripts/clean_publish.R} is the other
#'   way round: it pushes only when given \code{--push}.)
#' @param mode Mode of publishing: \code{"standalone"} (default) creates a
#'   root (parentless) commit and force-pushes with lease to the remote;
#'   \code{"snapshot"} fetches the remote publish branch and creates a fast-forward
#'   commit parented on the remote tip.
#' @param allow_ignored Character vector of file paths that are tracked in git
#'   and match \code{.gitignore} patterns, explicitly allowed to be included in
#'   the clean commit. By default, clean_publish stops if any tracked-but-ignored
#'   files exist to prevent accidental leakage.
#' @param confirm_guard Logical (default \code{TRUE}). A push guard that could not be confirmed
#'   by the test pushes is an error before anything moves. \code{FALSE} turns that case into a
#'   warning; a guard that is shown not to work is an error either way.
#' @param allow_other_refs Logical (default \code{FALSE}). Standalone mode with \code{push = TRUE}
#'   refuses a remote that has refs other than the publish branch (branches, tags, notes,
#'   \code{refs/pull/*}), because they keep the old history. \code{TRUE} goes on and lists them
#'   in the success message.
#' @param allow_overwrite_history Logical (default \code{FALSE}). Standalone mode with
#'   \code{push = TRUE} refuses a remote that holds this repository's own history. \code{TRUE}
#'   overwrites the branch all the same.
#' @param allow_submodules Logical (default \code{FALSE}). A current commit with submodules
#'   (gitlinks or a tracked \file{.gitmodules}) is refused, because their URLs and commit ids
#'   would be published. \code{TRUE} publishes them and prints their URLs.
#' @param disarm_push_url Logical (default \code{TRUE}). After a successful push, record the
#'   real push URL of \code{remote} and replace its \code{remote.<remote>.pushurl} by an
#'   unusable value (see above). \code{FALSE} leaves the remote as it was; the pre-push hook is
#'   then the only guard of pushes by the name of the remote.
#' @return The clean commit SHA, invisibly.
#' @export
#' @examples
#' \dontrun{
#' clean_publish(push = FALSE) # rewrite locally, send nothing
#' clean_publish(remote = "public")
#' clean_publish(remote = "med-cbe")
#' clean_publish(remote = "public", mode = "snapshot")
#' }
clean_publish <- function(
  repo_root = NULL,
  publish_branch = NULL,
  private_branch = "private-history",
  remote = NULL,
  commit_msg = NULL,
  push = TRUE,
  mode = c("standalone", "snapshot"),
  allow_ignored = character(),
  confirm_guard = TRUE,
  allow_other_refs = FALSE,
  allow_overwrite_history = FALSE,
  allow_submodules = FALSE,
  disarm_push_url = TRUE
) {
  repo_root <- repo_root %||% .cp_default_repo_root()
  mode <- match.arg(mode)
  .cp_check_flag(confirm_guard, "confirm_guard")
  .cp_check_flag(allow_other_refs, "allow_other_refs")
  .cp_check_flag(allow_overwrite_history, "allow_overwrite_history")
  .cp_check_flag(allow_submodules, "allow_submodules")
  .cp_check_flag(disarm_push_url, "disarm_push_url")
  if (!is.character(allow_ignored)) {
    stop("`allow_ignored` must be a character vector of file paths.", call. = FALSE)
  }
  .cp_check_name(private_branch, "private_branch")
  if (!is.null(remote)) .cp_check_name(remote, "remote")
  if (!is.null(publish_branch) && !identical(publish_branch, "")) {
    .cp_check_name(publish_branch, "publish_branch")
  }
  if (is.null(commit_msg)) {
    commit_msg <- if (mode == "snapshot") {
      paste("Snapshot", format(Sys.time(), "%Y-%m-%d", tz = "UTC"))
    } else {
      "Initial clean commit"
    }
  }
  if (!is.character(commit_msg) || length(commit_msg) != 1L || is.na(commit_msg)) {
    stop("`commit_msg` must be a single string.", call. = FALSE)
  }
  .cp_check_flag(push, "push")
  if (is.null(remote) && (isTRUE(push) || mode == "snapshot")) {
    stop(
      "Name the remote to publish to, for example `remote = \"public\"`. There is deliberately no default: ",
      "in a clone of the private repository `origin` is the private repository, and a standalone ",
      "publish force-pushes one commit over its branch. Nothing was changed.",
      call. = FALSE
    )
  }
  remote_label <- if (is.null(remote)) "(none)" else remote
  run_git <- function(args, error_ok = FALSE) .cp_git(repo_root, args, error_ok = error_ok)
  # The commit a ref names: NA when there is none (git answers 1); any other failure is an error
  ref_oid <- function(ref) {
    res <- run_git(c("rev-parse", "--verify", "--quiet", paste0(ref, "^{commit}")), error_ok = TRUE)
    if (res$status == 0L && length(res$output)) return(trimws(res$output[1]))
    if (res$status != 1L) {
      stop(
        "git rev-parse ", ref, " failed (status ", res$status, "):\n",
        paste(c(res$output, res$stderr), collapse = "\n"),
        call. = FALSE
      )
    }
    NA_character_
  }
  branch_oid <- function(name) ref_oid(paste0("refs/heads/", name))
  short <- function(oid) substr(oid, 1L, 10L)
  # ----------------------------------------------------------------------------
  # 1. Pre-flight: read-only checks. Nothing below changes anything until
  #    every one of them has passed.
  # ----------------------------------------------------------------------------
  inside <- run_git(c("rev-parse", "--is-inside-work-tree"), error_ok = TRUE)
  if (inside$status != 0L || !identical(trimws(inside$output[1]), "true")) {
    stop("'", repo_root, "' is not inside a git working tree.", call. = FALSE)
  }
  # Everything below works on the whole work tree: from a subdirectory, `git ls-files` lists only
  # that directory, so the tracked-but-ignored check would miss files at the top level.
  repo_root <- trimws(run_git(c("rev-parse", "--show-toplevel"))$output[1])
  git_dir <- trimws(run_git(c("rev-parse", "--absolute-git-dir"))$output[1])
  head_oid <- ref_oid("HEAD")
  if (is.na(head_oid)) {
    stop("The repository has no commits yet; there is nothing to publish.", call. = FALSE)
  }
  head_ref <- run_git(c("symbolic-ref", "--quiet", "HEAD"), error_ok = TRUE)
  if (!head_ref$status %in% c(0L, 1L)) {
    stop("git symbolic-ref HEAD failed:\n", paste(c(head_ref$output, head_ref$stderr), collapse = "\n"), call. = FALSE)
  }
  current_branch <- if (head_ref$status == 0L) sub("^refs/heads/", "", trimws(head_ref$output[1])) else ""
  current_label <- if (nzchar(current_branch)) paste0("'", current_branch, "'") else "the current (detached) commit"
  message("=== Clean Publish ===")
  message("Repository: ", repo_root)
  message("Git directory: ", git_dir)
  message("Current branch: ", if (nzchar(current_branch)) current_branch else paste0("(detached HEAD at ", short(head_oid), ")"))
  remote_names <- trimws(run_git("remote")$output)
  remote_names <- remote_names[nzchar(remote_names)]
  for (nm in remote_names) message(.cp_remote_summary(repo_root, nm))
  if (!length(remote_names)) message("Remotes: none")
  # git must be able to name the author and the committer of the commit that is published
  author <- .cp_identity(run_git, "AUTHOR")
  committer <- .cp_identity(run_git, "COMMITTER")
  message(
    "Identity published in the clean commit: author ", author$who, ", committer ", committer$who,
    if (identical(author$tz, committer$tz)) {
      paste0(", time zone ", author$tz)
    } else {
      paste0(", time zones ", author$tz, " (author) and ", committer$tz, " (committer)")
    }
  )
  .cp_check_branch_name(run_git, private_branch)
  publish_branch <- resolve_publish_branch(
    publish_branch,
    current_branch = current_branch,
    private_branch = private_branch,
    branch_exists = function(name) !is.na(branch_oid(name))
  )
  .cp_check_branch_name(run_git, publish_branch)
  publish_oid <- branch_oid(publish_branch)
  private_oid <- branch_oid(private_branch)
  protected_names <- .cp_config_get_all(repo_root, "cleanpublish.protectedbranch")
  if (publish_branch %in% protected_names) {
    stop(
      "Refusing to publish onto '", publish_branch, "': it is a protected private branch ",
      "(git config cleanpublish.protectedbranch).",
      call. = FALSE
    )
  }
  message("Target publish branch: ", publish_branch)
  message("Private history branch: ", private_branch)
  message(
    "Remote: ", remote_label,
    if (isTRUE(push)) "" else if (mode == "snapshot") " (read only: push = FALSE)" else " (not used: push = FALSE)"
  )
  status_lines <- run_git(c("status", "--porcelain"))$output
  if (any(nzchar(status_lines))) {
    stop(
      "You have uncommitted changes. Commit or stash them first.\n",
      "(Untracked files count; files ignored by .gitignore do not and are never published.)\n",
      paste(utils::head(status_lines, 10L), collapse = "\n"),
      call. = FALSE
    )
  }
  # Check for tracked files that match .gitignore patterns (A5-15)
  ignored_res <- run_git(c("ls-files", "-ci", "--exclude-standard"))
  tracked_ignored <- trimws(ignored_res$output[nzchar(ignored_res$output)])
  if (length(tracked_ignored) > 0L) {
    norm_ti <- fs::path_norm(tracked_ignored)
    norm_allowed <- fs::path_norm(allow_ignored)
    disallowed <- tracked_ignored[!norm_ti %in% norm_allowed]
    if (length(disallowed) > 0L) {
      stop(
        "Found tracked files that match .gitignore patterns. Because clean_publish publishes the ",
        "entire tree of the current commit, these files would be published:\n",
        paste0("  - ", disallowed, collapse = "\n"), "\n",
        "Untrack them (`git rm --cached <file>`) or specify them in `allow_ignored` if intentional.\n",
        "Nothing was changed.",
        call. = FALSE
      )
    }
    message("[NOTE] Publishing tracked-but-ignored files permitted via allow_ignored:\n",
            paste0("  - ", tracked_ignored, collapse = "\n"))
  }
  # Submodules: a gitlink names the commit of another repository and .gitmodules its URL
  tree_lines <- run_git(c("ls-tree", "-r", head_oid))$output
  gitlinks <- tree_lines[grepl("^160000 ", tree_lines)]
  has_gitmodules <- any(grepl("\t[.]gitmodules$", tree_lines))
  if (length(gitlinks) || has_gitmodules) {
    listing <- paste0(
      "  gitlinks: ",
      if (length(gitlinks)) paste(sub("^160000 commit ([0-9a-f]+)\t(.*)$", "\\2 (commit \\1)", utils::head(gitlinks, 10L)), collapse = ", ") else "none",
      "\n  URLs in .gitmodules: ",
      if (has_gitmodules) {
        urls <- .cp_gitmodules_urls(run_git, head_oid)
        if (is.na(urls[1])) "(could not be read)" else if (length(urls)) paste(urls, collapse = ", ") else "none"
      } else {
        "(no .gitmodules)"
      }
    )
    if (!isTRUE(allow_submodules)) {
      stop(
        "The commit being published contains submodules, whose URLs and commit ids would be published:\n",
        listing, "\n",
        "Remove them from the commit, or pass `allow_submodules = TRUE` (`--allow-submodules` of the script) if they are meant to be public. ",
        "Nothing was changed.",
        call. = FALSE
      )
    }
    message("[NOTE] Publishing submodules (allow_submodules = TRUE):\n", listing)
  }
  busy <- setdiff(.cp_worktree_branches(repo_root), current_branch)
  for (nm in unique(c(publish_branch, private_branch))) {
    if (nm %in% busy) {
      stop(
        "Branch '", nm, "' is checked out in another git worktree, so it cannot be rewritten from here. ",
        "Run the function from that worktree or check out another branch there. Nothing was changed.",
        call. = FALSE
      )
    }
  }
  if (!is.na(private_oid) && !identical(private_oid, head_oid)) {
    anc <- run_git(c("merge-base", "--is-ancestor", private_oid, head_oid), error_ok = TRUE)
    if (anc$status == 1L) {
      stop(
        "Branch '", private_branch, "' has commits that ", current_label, " does not contain. ",
        "Moving '", private_branch, "' to the current commit would drop them from every branch.\n",
        "Check out '", private_branch, "' (or a branch that contains it) and run again. ",
        "Nothing was changed.",
        call. = FALSE
      )
    } else if (anc$status != 0L) {
      stop(
        "Could not compare '", private_branch, "' with the current commit: ",
        paste(c(anc$output, anc$stderr), collapse = "\n"),
        call. = FALSE
      )
    }
  }
  # The remote: where the push goes, what is on it, and (snapshot mode) whether it may be built on
  push_url <- NA_character_
  push_info <- NULL
  expected_oid <- ""
  other_refs <- character()
  if (isTRUE(push) || mode == "snapshot") {
    push_info <- .cp_remote_push_urls(repo_root, remote)
    if (!push_info$exists) {
      stop(
        "Remote '", remote, "' is not configured in this repository (see `git remote -v`). ",
        "Nothing was changed.",
        call. = FALSE
      )
    }
    message(
      "Push URL of '", remote, "': ",
      if (length(push_info$urls)) paste(.cp_strip_credentials(push_info$urls), collapse = " ") else "(disarmed, no real URL recorded)",
      if (push_info$disarmed) " (recorded by clean_publish(); the push URL of the remote is disarmed)" else ""
    )
    if (isTRUE(push)) {
      if (push_info$disarmed && !push_info$recorded) {
        stop(
          "The push URL of remote '", remote, "' is disarmed (", .cp_disarmed_url, "), but no real URL is recorded in `",
          .cp_realpushurl_key(remote), "`. Set it, for example `git config ", .cp_realpushurl_key(remote),
          " <url>`, or point the remote at the real URL again with `git remote set-url --push ", remote,
          " <url>`. Nothing was changed.",
          call. = FALSE
        )
      }
      if (length(push_info$urls) != 1L) {
        stop(
          "Remote '", remote, "' has ", length(push_info$urls), " push URLs (",
          paste(.cp_strip_credentials(push_info$urls), collapse = ", "), "): git pushes to all of them, and ",
          "clean_publish() pushes to exactly one. Remove the others (`git remote set-url --delete --push ",
          remote, " <url>`) or use another remote. Nothing was changed.",
          call. = FALSE
        )
      }
      push_url <- push_info$urls[1]
    }
  }
  if (isTRUE(push) && mode == "standalone") {
    listing <- .cp_ls_remote(repo_root, push_url, remote)
    target_ref <- paste0("refs/heads/", publish_branch)
    expected_oid <- listing$oid[match(target_ref, listing$ref)]
    if (is.na(expected_oid)) expected_oid <- ""
    heads <- listing[startsWith(listing$ref, "refs/heads/"), , drop = FALSE]
    if (!isTRUE(allow_overwrite_history) && nrow(heads)) {
      # Does the remote hold this repository's own history? A branch tip of the remote that is a
      # commit here, is reachable from what is being published (or from a recorded private tip),
      # and is not a clean commit is: this is the private repository, or a copy of it.
      clean_ids <- .cp_config_get_all(repo_root, "cleanpublish.cleancommit")
      roots <- unique(c(head_oid, private_oid, publish_oid, .cp_config_get_all(repo_root, "cleanpublish.privatetip")))
      roots <- roots[!is.na(roots) & grepl("^[0-9a-f]{40}([0-9a-f]{24})?$", roots) & !roots %in% clean_ids]
      roots <- roots[vapply(roots, function(o) !is.na(ref_oid(o)), logical(1))]
      is_mine <- function(oid) {
        if (oid %in% clean_ids || !length(roots) || is.na(ref_oid(oid))) return(FALSE)
        unreached <- trimws(run_git(c("rev-list", "--count", oid, "--not", roots))$output[1])
        identical(unreached, "0") && !is.null(.cp_clean_chain_problem(repo_root, oid))
      }
      mine <- heads[vapply(heads$oid, is_mine, logical(1)), , drop = FALSE]
      if (nrow(mine)) {
        stop(
          "Remote '", remote, "' (", .cp_strip_credentials(push_url), ") holds this repository's history: ",
          paste0("'", sub("^refs/heads/", "", mine$ref), "' is at ", short(mine$oid), collapse = ", "),
          ", a commit that is in this repository and reachable from what is being published. A standalone ",
          "publish force-pushes ONE commit over its branch, so the remote would lose that history; this is ",
          "what happens when `remote` is the private repository (for example `origin` in a clone of it). ",
          "Publish into a NEW EMPTY repository, or pass `allow_overwrite_history = TRUE` (`--allow-overwrite-history`) if overwriting is ",
          "what you want. Nothing was changed.",
          call. = FALSE
        )
      }
    }
    other_refs <- setdiff(listing$ref, target_ref)
    if (length(other_refs) && !isTRUE(allow_other_refs)) {
      stop(
        "Remote '", remote, "' (", .cp_strip_credentials(push_url), ") has refs other than '", target_ref, "':\n",
        paste0("  - ", utils::head(other_refs, 20L), collapse = "\n"),
        if (length(other_refs) > 20L) paste0("\n  ... and ", length(other_refs) - 20L, " more") else "",
        "\nA force-push replaces '", target_ref, "' only: every other ref keeps the commits it points to, and ",
        "GitHub's hidden refs/pull/* of pull requests cannot be removed at all, so old history stays readable. ",
        "Publish into a NEW EMPTY repository, or pass `allow_other_refs = TRUE` (`--allow-other-refs`) to go on knowing that these ",
        "refs stay. Nothing was changed.",
        call. = FALSE
      )
    }
  }
  remote_tip <- NA_character_
  if (mode == "snapshot") {
    shallow <- trimws(run_git(c("rev-parse", "--is-shallow-repository"))$output[1])
    if (identical(shallow, "true")) {
      stop(
        "Snapshot mode needs the whole history of the remote branch to know that it is clean, and this is a ",
        "shallow repository, which cuts history off. Run `git fetch --unshallow` (or use a full clone) and ",
        "run again. Nothing was changed.",
        call. = FALSE
      )
    } else if (!identical(shallow, "false")) {
      stop("Could not tell whether the repository is shallow (git printed '", shallow, "'). Nothing was changed.", call. = FALSE)
    }
    message("[FETCH] Fetching '", publish_branch, "' from remote '", remote, "'...")
    # objects and FETCH_HEAD only: no remote-tracking ref and no tag is written
    fetch_res <- run_git(
      c("fetch", "--no-tags", "--no-recurse-submodules", "--refmap=", remote, publish_branch),
      error_ok = TRUE
    )
    if (fetch_res$status != 0L) {
      stop(
        "Remote '", remote, "' does not have a branch named '", publish_branch, "'. ",
        "Snapshot mode requires an existing remote branch to build upon.\n",
        .cp_redact(paste(c(fetch_res$output, fetch_res$stderr), collapse = "\n")),
        call. = FALSE
      )
    }
    remote_tip <- ref_oid("FETCH_HEAD")
    if (is.na(remote_tip)) {
      stop(
        "Could not determine remote tip commit for '", remote, "/", publish_branch, "'.",
        call. = FALSE
      )
    }
    message("[FETCH] Remote tip of '", remote, "/", publish_branch, "' is ", short(remote_tip))
    # A snapshot becomes a child of the remote tip, and the pre-push hook trusts every commit this
    # function makes: on top of the private repository, a rewritten archive of it, or a branch with
    # commits of other people, a snapshot would upload all of that.
    problem <- .cp_clean_chain_problem(repo_root, remote_tip)
    if (!is.null(problem)) {
      stop(
        "Branch '", publish_branch, "' of remote '", remote, "' is not made only of clean commits: ", problem, ". ",
        "A snapshot is built on the tip of that branch, and the pre-push hook trusts what this function makes, so ",
        "a snapshot on top of the private repository, a rewritten archive of it or commits that clean_publish() ",
        "did not make would carry them. Use the remote that holds only the snapshots made by clean_publish() ",
        "(every one ends with the trailer '", .cp_clean_trailer, "'), or publish standalone into a new empty ",
        "repository. Nothing was changed.",
        call. = FALSE
      )
    }
    tree_id <- trimws(run_git(c("rev-parse", paste0(head_oid, "^{tree}")))$output[1])
    diff_res <- run_git(c("diff-tree", "--quiet", remote_tip, tree_id), error_ok = TRUE)
    if (diff_res$status == 0L) {
      stop(
        "The tree of the current commit is identical to the tree of remote '", remote, "/", publish_branch, "'. ",
        "There are no changes to snapshot.",
        call. = FALSE
      )
    } else if (diff_res$status != 1L) {
      stop(
        "git diff-tree failed (status ", diff_res$status, "):\n",
        paste(c(diff_res$output, diff_res$stderr), collapse = "\n"),
        call. = FALSE
      )
    }
    # what the snapshot does to the public tree, for the person who reads it (nothing is decided by it)
    stat <- run_git(c("diff", "--no-color", "--no-ext-diff", "--stat=100", remote_tip, tree_id))$output
    deleted <- run_git(c("diff", "--no-color", "--no-ext-diff", "--no-renames", "--name-only", "--diff-filter=D", remote_tip, tree_id))$output
    deleted <- deleted[nzchar(deleted)]
    message(
      "Changes of the snapshot against the public tip ", short(remote_tip), ":\n",
      paste(utils::head(stat, 40L), collapse = "\n"),
      if (length(stat) > 40L) paste0("\n ... ", length(stat) - 40L, " more lines") else "",
      "\n", length(deleted), " path", if (length(deleted) != 1L) "s", " deleted",
      if (length(deleted)) paste0(" (public-only files among them are lost): ", paste(utils::head(deleted, 10L), collapse = ", ")) else ""
    )
  }
  hook_path <- .cp_hook_path(repo_root)
  hook_plan <- .cp_plan_hook(hook_path)
  if (.cp_path_in_worktree(repo_root, hook_path) &&
    .cp_git(repo_root, c("check-ignore", "-q", "--", hook_path), error_ok = TRUE)$status != 0L) {
    stop(
      "The pre-push hook would be written to '", hook_path, "', inside the working tree, where it ",
      "would become part of the published snapshot. Point core.hooksPath outside the working tree, ",
      "or ignore that path, and run again. Nothing was changed.",
      call. = FALSE
    )
  }
  # ----------------------------------------------------------------------------
  # 2. Guard the private branch against accidental push (hook + git config only;
  #    no branch is touched yet)
  # ----------------------------------------------------------------------------
  message("[GUARD] Protecting ", private_branch, " from accidental remote push...")
  .cp_write_hook(hook_plan)
  for (nm in setdiff(unique(c(private_branch, hook_plan$legacy_names)), protected_names)) {
    run_git(c("config", "--add", "cleanpublish.protectedbranch", nm))
  }
  # every private tip is recorded before a push is tried or a branch moves: the names of branches
  # can change, and on a first run the private branch does not exist yet
  .cp_record_private_tips(repo_root, c(head_oid, private_oid, publish_oid))
  message("[GUARD] pre-push hook ", hook_plan$action, ": ", hook_path)
  .cp_selftest_guard(repo_root, private_branch, tip = head_oid, confirm = confirm_guard)
  # a plain `git push` from the private branch has nowhere to go (explicit pushes
  # are what the hook is for)
  run_git(c("config", paste0("branch.", private_branch, ".pushRemote"), "no_push"), error_ok = TRUE)
  # ----------------------------------------------------------------------------
  # 3. Keep the old tips, then build the clean commit (an object only: no ref
  #    moves yet)
  # ----------------------------------------------------------------------------
  stamp <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
  backup_base <- paste0("refs/backup/clean_publish/", stamp)
  n_try <- 1L
  while (length(run_git(c("for-each-ref", "--count=1", "--format=%(refname)", backup_base))$output)) {
    n_try <- n_try + 1L
    backup_base <- paste0("refs/backup/clean_publish/", stamp, "-", n_try)
  }
  backups <- character()
  old_tips <- c(private_oid, publish_oid)
  names(old_tips) <- c(private_branch, publish_branch)
  for (nm in names(old_tips)[!is.na(old_tips)]) {
    ref <- paste0(backup_base, "/", nm)
    run_git(c("update-ref", "-m", "clean_publish backup", ref, old_tips[[nm]]))
    backups[[nm]] <- ref
    message("[BACKUP] ", nm, " was ", short(old_tips[[nm]]), ", kept as ", ref)
  }
  tree_id <- trimws(run_git(c("rev-parse", paste0(head_oid, "^{tree}")))$output[1])
  # commit-tree's message is written via -F <file> rather than -m <string>:
  # git.exe's argv handling on Windows mis-splits a multi-word -m value when
  # invoked through R's system2(), even though the identical argument vector
  # works fine from a native shell. The file is written as bytes, so that a line
  # ends with a line feed on every platform and the trailer is a line of its own.
  msg_file <- tempfile()
  on.exit(unlink(msg_file), add = TRUE)
  body <- sub("[[:space:]]+$", "", commit_msg)
  writeBin(charToRaw(paste0(if (nzchar(body)) paste0(body, "\n\n"), .cp_clean_trailer, "\n")), msg_file)
  clean_commit_id <- if (mode == "standalone") {
    message("[REWRITE] Generating single clean commit for ", publish_branch, "...")
    trimws(run_git(c("commit-tree", tree_id, "-F", msg_file))$output[1])
  } else {
    message("[REWRITE] Generating clean snapshot commit for ", publish_branch, " (parent: ", short(remote_tip), ")...")
    trimws(run_git(c("commit-tree", tree_id, "-p", remote_tip, "-F", msg_file))$output[1])
  }
  run_git(c("config", "--add", "cleanpublish.cleancommit", clean_commit_id))
  # ----------------------------------------------------------------------------
  # 4. Push the clean commit. This comes before any local branch moves, so
  #    a failed push (wrong URL, no access) leaves the local branches as they were.
  # ----------------------------------------------------------------------------
  pushed <- FALSE
  if (isTRUE(push)) {
    old_pub <- Sys.getenv("TEMPLECBE_PUBLISHING", unset = NA)
    Sys.setenv(TEMPLECBE_PUBLISHING = "1")
    on.exit({
      if (is.na(old_pub)) Sys.unsetenv("TEMPLECBE_PUBLISHING") else Sys.setenv(TEMPLECBE_PUBLISHING = old_pub)
    }, add = TRUE)
    shown_url <- paste0("'", remote, "' (", .cp_strip_credentials(push_url), ")")
    refspec <- paste0(clean_commit_id, ":refs/heads/", publish_branch)
    if (mode == "standalone") {
      message("[PUSH] Force-pushing single-commit ", publish_branch, " to ", shown_url, "...")
      # an explicit lease on what the listing showed: it does not depend on a remote-tracking ref
      run_git(c("push", paste0("--force-with-lease=refs/heads/", publish_branch, ":", expected_oid), "--", push_url, refspec))
    } else {
      message("[PUSH] Fast-forward pushing snapshot commit ", publish_branch, " to ", shown_url, "...")
      run_git(c("push", "--", push_url, refspec))
    }
    pushed <- TRUE
  } else {
    message("[PUSH] Skipped (push = FALSE). ", publish_branch, " rewritten locally only.")
  }
  switched <- FALSE
  finished <- FALSE
  on.exit(
    if (!finished && switched) {
      back <- if (nzchar(current_branch)) {
        c("checkout", "--quiet", current_branch)
      } else {
        c("checkout", "--quiet", "--detach", head_oid)
      }
      restored <- try(.cp_git(repo_root, back, error_ok = TRUE)$status == 0L, silent = TRUE)
      message(
        "[RESTORE] clean_publish() failed; ",
        if (isTRUE(restored)) "checked out again " else "could not check out again ",
        current_label, "."
      )
    },
    add = TRUE
  )
  # What is left to do cannot undo the push: when a step fails after it, say so
  tryCatch({
    if (pushed) {
      if (isTRUE(disarm_push_url) && !push_info$disarmed) {
        run_git(c("config", "--replace-all", .cp_realpushurl_key(remote), push_url))
        run_git(c("config", "--replace-all", paste0("remote.", remote, ".pushurl"), .cp_disarmed_url))
        message(
          "[DISARM] The push URL of '", remote, "' is disabled (remote.", remote, ".pushurl = ", .cp_disarmed_url,
          "): `git push ", remote, "`, `git push --no-verify` and gert cannot publish by its name any more. The real URL ",
          "is kept in ", .cp_realpushurl_key(remote), " and used by clean_publish(). To undo: git remote set-url --push ",
          remote, " ", .cp_strip_credentials(push_url)
        )
      } else if (!isTRUE(disarm_push_url)) {
        message(
          "[NOTE] The push URL of '", remote, "' stays usable (disarm_push_url = FALSE): the pre-push hook is the only ",
          "guard of a `git push ", remote, "`."
        )
      }
    }
    # ----------------------------------------------------------------------------
    # 5. Move the local branches and stay on the private development branch
    # ----------------------------------------------------------------------------
    # a branch must be where it was put: on a case-insensitive file system two names can be one
    # branch, and a move of one then moves the other
    verify_branch <- function(name, expected) {
      actual <- branch_oid(name)
      if (identical(actual, expected)) return(invisible(TRUE))
      stop(
        "After the branch moves, '", name, "' is at ", if (is.na(actual)) "nothing" else short(actual), " instead of ",
        short(expected), ": the move did not take effect, or another move undid it (names that differ only in ",
        "case are one branch on a case-insensitive file system). ",
        if (length(backups)) {
          paste0(
            "The old tips are kept: ", paste0(names(backups), " = ", backups, collapse = "; "),
            ". To restore one, check out another branch and run `git branch -f <branch> <that ref>`."
          )
        } else {
          paste0("No branch existed before this run; the commit that was published from is ", head_oid, ".")
        },
        call. = FALSE
      )
    }
    if (!identical(current_branch, private_branch)) {
      message("[BACKUP] Updating ", private_branch, " to ", current_label, " (", short(head_oid), ")...")
      run_git(c("branch", "-f", private_branch, head_oid))
      verify_branch(private_branch, head_oid)
      run_git(c("checkout", "--quiet", private_branch))
      switched <- TRUE
    }
    run_git(c("branch", "-f", publish_branch, clean_commit_id))
    verify_branch(publish_branch, clean_commit_id)
    verify_branch(private_branch, head_oid)
    # a private branch created from a tracking branch would keep pushing there
    upstream <- run_git(
      c("rev-parse", "--abbrev-ref", "--symbolic-full-name", paste0(private_branch, "@{upstream}")),
      error_ok = TRUE
    )
    if (upstream$status == 0L) run_git(c("branch", "--unset-upstream"), error_ok = TRUE)
    finished <- TRUE
  }, error = function(e) {
    if (!pushed) stop(e)
    stop(
      "The push succeeded: '", remote, "/", publish_branch, "' now points to the clean commit ", clean_commit_id,
      ", so the remote is ALREADY UPDATED. A later step failed:\n", conditionMessage(e),
      call. = FALSE
    )
  })
  message("============================================================")
  message("SUCCESS!")
  if (isTRUE(push)) {
    message("'", remote, "/", publish_branch, "' now points to the clean commit ", clean_commit_id, ".")
    if (length(other_refs)) {
      message(
        "These other refs of the remote were not touched and keep whatever they point to:\n",
        paste0("  - ", utils::head(other_refs, 20L), collapse = "\n"),
        if (length(other_refs) > 20L) paste0("\n  ... and ", length(other_refs) - 20L, " more") else ""
      )
    }
  }
  message("Full history is preserved locally on: ", private_branch)
  if (length(backups)) {
    message("Previous tips are kept under refs/backup/clean_publish/ (", paste(names(backups), collapse = ", "), ").")
  }
  message("============================================================")
  invisible(clean_commit_id)
}
#' Guard a Public Remote Against Accidental Direct Push
#'
#' Records the URL(s) of the public remote in git config (\code{cleanpublish.publicurl}) and
#' makes sure the \code{pre-push} hook is installed. The hook refuses a \code{git push} whose
#' push URL is one of them, after git has applied its own rewriting
#' (\code{url.<base>.insteadOf}, \code{pushInsteadOf}) and the URLs have been normalised: case,
#' a trailing \code{.git} or slash, a scheme, a user name, \code{host:path} and backslash
#' notation do not matter, and a local directory counts by its physical path as well. Nothing
#' else is resolved (a symbolic link on a remote host, a changed DNS name), and a URL of
#' another spelling is not the same URL.
#'
#' The one push that goes through is the one \code{\link{clean_publish}()} makes: it sets the
#' environment variable \code{TEMPLECBE_PUBLISHING=1}, and with that variable the hook lets
#' through the clean commits \code{clean_publish()} recorded, and nothing else. The variable
#' bypasses the fence only: any other ref, the private branch for one, is still refused.
#'
#' After writing, the function checks the result. It runs the hook by hand, as git does, for
#' every configured remote and for every fenced URL, with \code{TEMPLECBE_PUBLISHING} unset,
#' and stops with an error when the hook is missing, is not executable, or does not refuse
#' a push to a remote the fence matches: a fence that is not confirmed is not left to guard
#' nothing in silence.
#'
#' \strong{What the guard does not stop.} It is a hook of command-line \command{git}. It does
#' not stop \code{git push --no-verify}, a push made by a client that does not run hooks (the
#' \pkg{gert} package, which \pkg{usethis} uses, is one), a push after \code{core.hooksPath}
#' has been pointed elsewhere or the hook has been deleted or made non-executable, and a push
#' to the URL in a spelling that is not one of the fenced ones, for example an explicit URL
#' that names the same repository through another host name. It checks pushes, not reads. The
#' safest procedure is to publish from a throwaway clone and to keep no pushable public remote
#' in the clone you work in.
#'
#' @param public_url The name of a configured remote (its fetch and push URLs, as
#'   \code{git remote get-url --all} and \code{--push --all} report them, are stored), or a URL.
#'   A URL must match the push URL of at least one configured remote after normalising, so that
#'   a fence that guards nothing is an error, unless \code{allow_unmatched = TRUE}. It must not
#'   contain whitespace, which the hook cannot compare word by word.
#' @param repo_root Path to the git repository root. Defaults to the git toplevel of the
#'   working directory (an error when that is not in a repository).
#' @param allow_unmatched Logical (default \code{FALSE}). Set \code{TRUE} to fence a URL that
#'   no configured remote has yet, for a remote that will be added later.
#' @return The normalized fenced URL(s), invisibly.
#' @export
#' @examples
#' \dontrun{
#' # the remote of the public repository, by name ...
#' guard_public_remote("public")
#' # ... or by URL, which must be the push URL of a configured remote
#' guard_public_remote("https://github.com/OWNER/NEW-PUBLIC-REPO.git")
#' # a URL for a remote that is added after this call
#' guard_public_remote("https://github.com/OWNER/NEW-PUBLIC-REPO.git", allow_unmatched = TRUE)
#' }
guard_public_remote <- function(public_url, repo_root = NULL, allow_unmatched = FALSE) {
  repo_root <- repo_root %||% .cp_default_repo_root()
  if (!is.character(public_url) || length(public_url) != 1L || is.na(public_url) || !nzchar(trimws(public_url))) {
    stop("`public_url` must be a single non-empty URL string.", call. = FALSE)
  }
  if (!is.logical(allow_unmatched) || length(allow_unmatched) != 1L || is.na(allow_unmatched)) {
    stop("`allow_unmatched` must be TRUE or FALSE.", call. = FALSE)
  }
  public_url <- trimws(public_url)
  # the hook compares URLs word by word: a URL with whitespace could never be matched
  refuse_whitespace <- function(urls) {
    if (any(grepl("[[:space:]]", urls))) {
      stop(
        "`public_url` must not contain whitespace: the pre-push hook compares URLs word by word and ",
        "could not match it, so the guard would silently do nothing. Use a URL or path without spaces.",
        call. = FALSE
      )
    }
  }
  refuse_whitespace(public_url)

  run_git <- function(args, error_ok = FALSE) .cp_git(repo_root, args, error_ok = error_ok)

  inside <- run_git(c("rev-parse", "--is-inside-work-tree"), error_ok = TRUE)
  if (inside$status != 0L || !identical(trimws(inside$output[1]), "true")) {
    stop("'", repo_root, "' is not inside a git working tree.", call. = FALSE)
  }
  # git runs hooks in the top of the work tree, and relative remote URLs are read from there
  top <- run_git(c("rev-parse", "--show-toplevel"), error_ok = TRUE)
  top <- if (top$status == 0L && length(top$output)) trimws(top$output[1]) else repo_root

  # the fence: the URLs git will give the hook, never the name of a remote
  remotes <- trimws(run_git("remote")$output)
  remotes <- remotes[nzchar(remotes)]
  remote_urls <- function(remote, push) {
    # a remote whose push URL clean_publish() disarmed is fenced by the real URL it recorded
    if (push) return(.cp_remote_push_urls(repo_root, remote)$urls)
    urls <- run_git(c("remote", "get-url", "--all", remote))$output
    trimws(urls[nzchar(trimws(urls))])
  }
  push_urls <- stats::setNames(lapply(remotes, remote_urls, push = TRUE), remotes)
  if (public_url %in% remotes) {
    matched <- public_url
    fence <- c(remote_urls(public_url, push = FALSE), push_urls[[public_url]])
  } else {
    wanted <- .cp_url_spellings(top, public_url)
    matched <- remotes[vapply(push_urls, function(urls) {
      any(unlist(lapply(urls, function(u) .cp_url_spellings(top, u))) %in% wanted)
    }, logical(1))]
    fence <- c(public_url, unlist(push_urls[matched]))
    if (!length(matched) && !isTRUE(allow_unmatched)) {
      stop(
        "`public_url` '", public_url, "' does not match the push URL of any configured remote (",
        if (length(remotes)) paste0(remotes, ": ", vapply(push_urls, paste, "", collapse = ", "), collapse = "; ") else "there is none",
        "), so the fence would guard nothing. Pass the name of the remote, or the URL as ",
        "`git remote get-url --push <name>` prints it; for a remote that will be added later pass ",
        "`allow_unmatched = TRUE`. Nothing was changed.",
        call. = FALSE
      )
    }
  }
  fence <- unique(.cp_strip_credentials(fence))
  if (any(!nzchar(.cp_normalize_url(fence)))) {
    stop("`public_url` could not be normalized: '", public_url, "'.", call. = FALSE)
  }
  refuse_whitespace(fence)

  norm_existing <- .cp_normalize_url(.cp_config_get_all(repo_root, "cleanpublish.publicurl"))
  for (url in fence[!.cp_normalize_url(fence) %in% norm_existing]) {
    run_git(c("config", "--add", "cleanpublish.publicurl", url))
  }

  hook_path <- .cp_hook_path(repo_root)
  hook_plan <- .cp_plan_hook(hook_path)
  .cp_write_hook(hook_plan)

  message("[GUARD] Guarded public remote: ", paste(unique(.cp_normalize_url(fence)), collapse = ", "))
  message("[GUARD] Pre-push hook ", hook_plan$action, ": ", hook_path)
  if (!length(matched)) {
    message(
      "[GUARD] '", public_url, "' matches no configured remote: it guards pushes to that URL once a ",
      "remote with it exists."
    )
  }
  .cp_check_fence(top, hook_path, push_urls, fence, matched)
  invisible(unique(.cp_normalize_url(fence)))
}

# `sh` for running the hook by hand: Git for Windows's own on Windows, else the one on the PATH
.cp_find_sh <- function() {
  if (identical(.Platform$OS.type, "windows")) {
    exec_path <- suppressWarnings(system2("git", "--exec-path", stdout = TRUE, stderr = FALSE))
    root <- normalizePath(file.path(exec_path[1], "..", "..", ".."), winslash = "/", mustWork = FALSE)
    own <- file.path(root, "usr", "bin", "sh.exe")
    if (file.exists(own)) return(own)
  }
  sh <- unname(Sys.which("sh"))
  if (nzchar(sh)) sh else NA_character_
}

# Run the hook the way git does for a push of nothing: `sh <hook> <remote> <url>` in the top
# of the work tree, empty standard input, TEMPLECBE_PUBLISHING unset. Returns list(status, output).
.cp_run_hook <- function(sh, hook_path, top, remote, url) {
  old_wd <- setwd(top)
  old_env <- Sys.getenv(c("TEMPLECBE_PUBLISHING", "PATH"), unset = NA)
  on.exit({
    setwd(old_wd)
    Sys.setenv(PATH = old_env[["PATH"]])
    if (!is.na(old_env[["TEMPLECBE_PUBLISHING"]])) Sys.setenv(TEMPLECBE_PUBLISHING = old_env[["TEMPLECBE_PUBLISHING"]])
  }, add = TRUE)
  Sys.unsetenv("TEMPLECBE_PUBLISHING")
  # the hook uses cut, grep and tr, which sit next to a Windows sh
  Sys.setenv(PATH = paste(dirname(sh), old_env[["PATH"]], sep = .Platform$path.sep))
  # standard input is an empty file, as git gives a push of nothing (a line of "input" would be
  # a carriage return under a Windows dash)
  no_input <- tempfile("clean_publish_stdin_")
  file.create(no_input)
  on.exit(unlink(no_input), add = TRUE)
  out <- suppressWarnings(system2(
    sh, shQuote(c(hook_path, remote, url)), stdin = no_input, stdout = TRUE, stderr = TRUE
  ))
  status <- attr(out, "status") %||% 0L
  attr(out, "status") <- NULL
  list(status = status, output = out)
}

# git for Windows runs a hook whatever its mode; elsewhere git skips a hook that is not executable
.cp_hook_is_executable <- function(path) {
  identical(.Platform$OS.type, "windows") || file.access(path, 1L) == 0L
}

# After guard_public_remote() has written the fence: a push to every fenced URL, and to every
# remote the fence matched, must be refused by the hook that is installed. Stops when the hook
# is missing or cannot be run, or lets one of those pushes through.
.cp_check_fence <- function(top, hook_path, push_urls, fence, matched) {
  if (!file.exists(hook_path)) {
    stop(
      "There is no pre-push hook at '", hook_path, "', so git will not refuse a direct push to the ",
      "fenced remote. The fence was recorded in git config.",
      call. = FALSE
    )
  }
  if (!.cp_hook_is_executable(hook_path)) {
    stop(
      "The pre-push hook '", hook_path, "' is not executable, so git does not run it and a direct push ",
      "to the fenced remote is not refused. Make it executable (chmod +x). The fence was recorded in git config.",
      call. = FALSE
    )
  }
  sh <- .cp_find_sh()
  if (is.na(sh)) {
    stop("Could not confirm the fence: no `sh` was found to run the hook with. The fence was recorded in git config.", call. = FALSE)
  }
  runs <- c(
    unlist(lapply(names(push_urls), function(r) lapply(push_urls[[r]], function(u) list(remote = r, url = u, must = r %in% matched))), recursive = FALSE),
    lapply(fence, function(u) list(remote = "", url = u, must = TRUE))
  )
  for (run in runs) {
    res <- .cp_run_hook(sh, hook_path, top, run$remote, run$url)
    refused <- res$status != 0L && any(grepl("is guarded", res$output, fixed = TRUE))
    if (nzchar(run$remote)) {
      message("[GUARD] A push to remote '", run$remote, "' (", .cp_strip_credentials(run$url), ") is ", if (refused) "refused." else "not fenced.")
    }
    if (run$must && !refused) {
      stop(
        "The pre-push hook did not refuse a push to ", if (nzchar(run$remote)) paste0("remote '", run$remote, "' at ") else "",
        "'", .cp_strip_credentials(run$url), "', which the fence covers, so git will not protect it. Run by hand: `sh ", hook_path,
        " <remote> <url>` printed:\n", paste(res$output, collapse = "\n"),
        "\nThe fence was recorded in git config.",
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}

# Normalize a remote URL or repository path for comparison. The hook block has the same
# steps in the same order (_tcbe_norm_url), and a test runs a corpus of spellings through both:
# backslashes become slashes; a URL scheme and the userinfo (everything up to the last @ that
# comes before the first slash) are dropped; colons become slashes; case, runs of slashes,
# trailing slashes and any number of trailing ".git" are ignored. Applying it twice changes
# nothing more, so a value that was stored normalised still compares equal.
.cp_normalize_url <- function(url) {
  if (!is.character(url) || length(url) == 0L) return(character(0))
  u <- trimws(url)
  u <- gsub("\\", "/", u, fixed = TRUE)
  u <- sub("^[a-zA-Z0-9+_.-]+://", "", u)
  u <- sub("^[^/]*@", "", u)
  u <- gsub(":", "/", u, fixed = TRUE)
  u <- tolower(u)
  u <- gsub("/+", "/", u)
  while (any(grepl("([.]git|/)$", u))) {
    u <- sub("([.]git|/)+$", "", u)
  }
  sub("^/+", "", u)
}
# The URL without the credentials of its userinfo ("https://user:token@host/x" -> "https://host/x";
# everything up to the last @ before the first slash goes, a password with an @ in it included): the
# fence is stored in .git/config and compared after normalising, which drops them anyway, and no URL
# is printed with them. A URL without a scheme (scp-style `git@host:path`, a path) is left as it is.
.cp_strip_credentials <- function(url) sub("^([a-zA-Z0-9+_.-]+://)[^/]*@", "\\1", url)
# Free text (git's messages, a command line) with the credentials of every URL in it removed
.cp_redact <- function(text) gsub("([a-zA-Z0-9+_.-]+://)[^/[:space:]]*@", "\\1", text)
# The git toplevel of the working directory: the default repository of guard_public_remote(),
# which writes into the repository's config and hooks. here::here() is fixed when the `here`
# package is loaded and can name another repository than the one the user stands in.
.cp_default_repo_root <- function() {
  top <- .cp_git(getwd(), c("rev-parse", "--show-toplevel"), error_ok = TRUE)
  if (top$status != 0L || !length(top$output) || !nzchar(trimws(top$output[1]))) {
    stop(
      "The working directory '", getwd(), "' is not inside a git repository. Change into the repository ",
      "or name it with `repo_root`. Nothing was changed.",
      call. = FALSE
    )
  }
  trimws(top$output[1])
}
# Every way the hook can recognise `x` (a URL, a path or the name of a remote) as the URL that is
# pushed to, normalised: itself, the URL(s) git makes of it (the push URLs of a remote of that
# name, else the url.<base>.insteadOf expansion) and, for a local directory, its physical path.
# Relative paths are taken from `top`, the directory git runs hooks in. The hook block does the
# same with _tcbe_spellings.
.cp_url_spellings <- function(top, x) {
  urls <- c(x, .cp_git(top, c("remote", "get-url", "--push", "--all", "--", x), error_ok = TRUE)$output)
  if (length(urls) == 1L) {
    urls <- c(urls, .cp_git(top, c("ls-remote", "--get-url", "--", x), error_ok = TRUE)$output)
  }
  dirs <- file.path(top, urls)
  is_dir <- dir.exists(dirs) | dir.exists(urls)
  physical <- vapply(which(is_dir), function(i) {
    path <- if (dir.exists(urls[i])) urls[i] else dirs[i]
    normalizePath(path, winslash = "/", mustWork = FALSE)
  }, character(1))
  unique(.cp_normalize_url(c(urls, physical)))
}
#' Resolve the Branch \code{clean_publish()} Will Overwrite
#'
#' Explicit \code{publish_branch} first, then \code{master}, then \code{main},
#' then the current branch (unless it is \code{private_branch}). The current
#' branch comes last on purpose: this is meant to be run from
#' \code{private_branch} to republish onto the public branch, and running it
#' with no argument from some other branch used to silently target that branch,
#' which is how stray orphan branches were created instead of updating
#' \code{master}.
#'
#' @param publish_branch Explicit branch, or \code{NULL}/\code{""} to resolve.
#' @param current_branch Currently checked-out branch (\code{""} if detached).
#' @param private_branch Local branch that retains full history.
#' @param branch_exists Function taking a branch name and returning whether a
#'   local branch of that name exists.
#' @return The branch name; an error if that is \code{private_branch}.
#' @keywords internal
#' @noRd
resolve_publish_branch <- function(publish_branch, current_branch, private_branch, branch_exists) {
  if (is.null(publish_branch) || !nzchar(publish_branch)) {
    publish_branch <- if (branch_exists("master")) {
      "master"
    } else if (branch_exists("main")) {
      "main"
    } else if (nzchar(current_branch) && current_branch != private_branch) {
      current_branch
    } else {
      "master"
    }
  }
  if (identical(tolower(publish_branch), tolower(private_branch))) {
    stop(
      "Refusing to publish onto '", publish_branch, "': it is the private branch '", private_branch,
      "' itself (names that differ only in case are one branch on a case-insensitive file system), and ",
      "that would squash away its full history.",
      call. = FALSE
    )
  }
  publish_branch
}
# ------------------------------------------------------------------------------
# Internal helpers of clean_publish() (all named .cp_*)
# ------------------------------------------------------------------------------
# Run git in `repo_root`. Every argument is quoted, so a repository path with a
# space (or a branch or remote name with shell characters) reaches git as one
# argument on Windows and on Unix alike. Returns list(output, stderr, status) and
# stops on a non-zero status unless `error_ok`.
.cp_git <- function(repo_root, args, error_ok = FALSE) {
  stderr_file <- tempfile("clean_publish_stderr_")
  on.exit(unlink(stderr_file), add = TRUE)
  out <- suppressWarnings(system2(
    "git", shQuote(c("-C", repo_root, args)),
    stdout = TRUE, stderr = stderr_file
  ))
  status <- attr(out, "status") %||% 0L
  attr(out, "status") <- NULL
  err <- if (file.exists(stderr_file)) readLines(stderr_file, warn = FALSE) else character(0)
  if (status != 0L && !error_ok) {
    # a URL in the arguments or in git's own message can carry a password
    stop(
      "git ", .cp_redact(paste(args, collapse = " ")), " failed (status ", status, "):\n",
      .cp_redact(paste(c(out, err), collapse = "\n")),
      call. = FALSE
    )
  }
  list(output = out, stderr = err, status = status)
}
# A branch or remote name: one non-empty string that cannot be taken for an option
.cp_check_name <- function(x, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x) || startsWith(x, "-")) {
    stop("`", arg, "` must be a single non-empty name that does not start with '-'.", call. = FALSE)
  }
  invisible(x)
}
# A logical flag: TRUE or FALSE, nothing else
.cp_check_flag <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", arg, "` must be TRUE or FALSE.", call. = FALSE)
  }
  invisible(x)
}
# A branch name as git will use it: valid as refs/heads/<name> (not `check-ref-format --branch`,
# which expands `@{-1}` to the previous branch), and with no leading `@` and no `@{`
.cp_check_branch_name <- function(run_git, name) {
  ok <- !startsWith(name, "@") && !grepl("@{", name, fixed = TRUE) &&
    run_git(c("check-ref-format", paste0("refs/heads/", name)), error_ok = TRUE)$status == 0L
  if (!ok) stop("'", name, "' is not a valid branch name.", call. = FALSE)
  invisible(name)
}
# The author or committer (`which`) git will give a commit, as list(who = "Name <email>", tz = "+0200").
# Stops when git has none (a fresh clone with no user.name or user.email).
.cp_identity <- function(run_git, which) {
  var <- paste0("GIT_", which, "_IDENT")
  res <- run_git(c("var", var), error_ok = TRUE)
  line <- if (res$status == 0L && length(res$output)) trimws(res$output[1]) else ""
  parts <- regmatches(line, regexec("^(.+ <[^<>]*>) [0-9]+ ([+-][0-9]{4})$", line))[[1]]
  if (length(parts) != 3L) {
    stop(
      "git has no identity for the ", tolower(which), " of the clean commit (`git var ", var, "` failed",
      if (nzchar(line)) paste0(": it printed '", line, "'") else paste0(": ", paste(trimws(c(res$output, res$stderr)), collapse = " ")),
      "). Set user.name and user.email (`git config user.name \"...\"`, `git config user.email \"...\"`), ",
      "or the GIT_AUTHOR_NAME, GIT_AUTHOR_EMAIL, GIT_COMMITTER_NAME and GIT_COMMITTER_EMAIL environment ",
      "variables, and run again. Nothing was changed.",
      call. = FALSE
    )
  }
  list(who = parts[2], tz = parts[3])
}
# One line for the banner: a remote with its fetch and push URLs, credentials removed
.cp_remote_summary <- function(repo_root, remote) {
  urls <- function(push) {
    res <- .cp_git(repo_root, c("remote", "get-url", if (push) "--push", "--all", remote), error_ok = TRUE)
    urls <- trimws(res$output[nzchar(trimws(res$output))])
    if (res$status == 0L && length(urls)) paste(.cp_strip_credentials(urls), collapse = " ") else "(none)"
  }
  paste0("Remote '", remote, "': fetch ", urls(FALSE), ", push ", urls(TRUE))
}
# The URLs in the .gitmodules of `commit`, credentials removed; NA when the file cannot be read
.cp_gitmodules_urls <- function(run_git, commit) {
  res <- run_git(
    c("config", "--blob", paste0(commit, ":.gitmodules"), "--get-regexp", "^submodule[.].*[.]url$"),
    error_ok = TRUE
  )
  if (res$status == 1L) return(character())
  if (res$status != 0L) return(NA_character_)
  unique(.cp_strip_credentials(trimws(sub("^[^ ]+ ", "", res$output[nzchar(res$output)]))))
}
# What clean_publish() sets remote.<name>.pushurl to after a push: no client can use it, and git's
# own message ("'DISABLED-use-clean_publish' does not appear to be a git repository") says what to do
.cp_disarmed_url <- "DISABLED-use-clean_publish"
# Where the real push URL of a disarmed remote is kept (the remote name is the config subsection)
.cp_realpushurl_key <- function(remote) paste0("cleanpublish.", remote, ".realpushurl")
# The push URL(s) clean_publish() would use for `remote`: what `git remote get-url --push --all`
# says, or, when the remote is disarmed, the URL that was recorded. Returns list(exists, urls,
# disarmed, recorded); `urls` is empty for a disarmed remote with no record.
.cp_remote_push_urls <- function(repo_root, remote) {
  res <- .cp_git(repo_root, c("remote", "get-url", "--push", "--all", remote), error_ok = TRUE)
  urls <- trimws(res$output[nzchar(trimws(res$output))])
  if (res$status != 0L || !length(urls)) {
    return(list(exists = FALSE, urls = character(), disarmed = FALSE, recorded = FALSE))
  }
  disarmed <- identical(urls, .cp_disarmed_url)
  recorded <- FALSE
  if (disarmed) {
    urls <- .cp_config_get_all(repo_root, .cp_realpushurl_key(remote))
    recorded <- length(urls) > 0L
  }
  list(exists = TRUE, urls = urls, disarmed = disarmed, recorded = recorded)
}
# All refs of the remote at `url` as data.frame(oid, ref). Stops when git cannot list them: a
# force-push must not go ahead on a remote that was never looked at.
.cp_ls_remote <- function(repo_root, url, remote) {
  res <- .cp_git(repo_root, c("ls-remote", "--refs", "--", url), error_ok = TRUE)
  if (res$status != 0L) {
    stop(
      "Could not list the refs of remote '", remote, "' at its push URL (", .cp_strip_credentials(url), "): ",
      .cp_redact(paste(trimws(c(res$output, res$stderr)), collapse = " ")),
      "\nWithout the list of its refs a force-push could overwrite history unseen. Nothing was changed.",
      call. = FALSE
    )
  }
  lines <- trimws(res$output[nzchar(trimws(res$output))])
  data.frame(
    oid = sub("[\t ].*$", "", lines), ref = sub("^[^\t ]+[\t ]+", "", lines),
    stringsAsFactors = FALSE
  )
}
# The trailer line that ends the message of every commit clean_publish() makes
.cp_clean_trailer <- "Clean-Publish: v1"
# Is the history of `tip` made only of commits clean_publish() made? It must be linear with one
# parentless root, and every commit must have the trailer as a line of its own. Returns NULL when
# it is, else what is wrong with it, for the message of a refusal. Replace refs are ignored.
.cp_clean_chain_problem <- function(repo_root, tip) {
  git <- function(...) .cp_git(repo_root, c("--no-replace-objects", ...))$output
  lines <- git("rev-list", "--parents", tip)
  lines <- lines[nzchar(lines)]
  n_parents <- lengths(strsplit(lines, " ", fixed = TRUE)) - 1L
  total <- length(lines)
  if (any(n_parents > 1L)) {
    first <- sub(" .*$", "", lines[n_parents > 1L][1])
    return(paste0("its history is not linear: it contains a merge commit (", substr(first, 1L, 10L), ")"))
  }
  roots <- sum(n_parents == 0L)
  if (roots != 1L) {
    return(paste0("its history has ", roots, " parentless commits instead of one root"))
  }
  with_trailer <- c("--extended-regexp", paste0("--grep=^", .cp_clean_trailer, "[[:space:]]*$"))
  good <- suppressWarnings(as.integer(git("rev-list", "--count", with_trailer, tip)[1]))
  if (!identical(good, total)) {
    newest <- git("log", "-n", "1", "--invert-grep", with_trailer, "--format=%h %s", tip)[1]
    return(paste0(
      total - good, " of ", total, " commits do not end with the trailer '", .cp_clean_trailer, "' (the newest is ",
      sub("^([^ ]+) ?(.*)$", "\\1 '\\2'", newest), ")"
    ))
  }
  NULL
}
# All values of a (possibly multi-valued) git config key; character(0) if unset (git answers 1).
# Any other failure, a damaged config file for one, is an error: it must not read as "unset".
.cp_config_get_all <- function(repo_root, key) {
  res <- .cp_git(repo_root, c("config", "--get-all", key), error_ok = TRUE)
  if (res$status == 0L) return(trimws(res$output[nzchar(res$output)]))
  if (res$status != 1L) {
    stop(
      "git config --get-all ", key, " failed (status ", res$status, "):\n",
      paste(c(res$output, res$stderr), collapse = "\n"),
      call. = FALSE
    )
  }
  character(0)
}
# Branches checked out in any worktree of the repository, this one included
.cp_worktree_branches <- function(repo_root) {
  lines <- .cp_git(repo_root, c("worktree", "list", "--porcelain"))$output
  sub("^branch refs/heads/", "", grep("^branch refs/heads/", lines, value = TRUE))
}
# Where git will look for the pre-push hook: hooks/pre-push of `rev-parse
# --git-path`, which is the common git directory for a linked worktree and
# honours core.hooksPath. The path git prints is relative to `repo_root`.
.cp_hook_path <- function(repo_root) {
  out <- .cp_git(repo_root, c("rev-parse", "--git-path", "hooks/pre-push"))$output[1]
  as.character(fs::path_norm(fs::path_abs(out, start = repo_root)))
}
# Does `path` lie inside the working tree of `repo_root` (and not inside its git directory)?
.cp_path_in_worktree <- function(repo_root, path) {
  dirs <- .cp_git(repo_root, c("rev-parse", "--show-toplevel", "--git-common-dir"))$output
  top <- dirs[1]
  common <- as.character(fs::path_abs(dirs[2], start = repo_root))
  spell <- function(p) {
    p <- .canonical_path(p)
    if (identical(.Platform$OS.type, "windows")) tolower(p) else p
  }
  inside <- function(p, dir) startsWith(paste0(p, "/"), paste0(sub("/$", "", dir), "/"))
  path <- spell(path)
  inside(path, spell(top)) && !inside(path, spell(common))
}
.cp_marker_begin <- "# >>> TempleCBE clean_publish guard"
.cp_marker_end <- "# <<< TempleCBE clean_publish guard <<<"
# The managed block of the pre-push hook: POSIX sh only (sh, dash, bash and ksh; git runs it
# with /bin/sh, which is dash on Debian and Ubuntu). It reads everything it needs from git
# config, so one hook serves every private branch name:
#   cleanpublish.protectedbranch  names of branches that hold private history
#   cleanpublish.privatetip       full ids of every private tip clean_publish() recorded
#   cleanpublish.cleancommit      full ids of the clean commits it made
#   cleanpublish.publicurl        fenced URLs
# It consumes the hook's standard input and hands it on again, so it can sit in front of
# another hook that reads it. It must work under `set -e` and `set -u` of a hook it sits in.
.cp_hook_block <- function() {
  c(
    paste0(.cp_marker_begin, " (managed block: clean_publish() rewrites it, do not edit) >>>"),
    r"---(# Refuses a push that would upload history meant to stay private:)---",
    r"---(#  - any ref named like a protected branch (git config cleanpublish.protectedbranch))---",
    r"---(#  - any ref, tag or object from which a commit of the private history can be reached)---",
    r"---(#    that is not part of a clean commit (git config cleanpublish.cleancommit). The)---",
    r"---(#    private history is what the live tips of the protected branches and every recorded)---",
    r"---(#    private tip (git config cleanpublish.privatetip) reach. Both keys take full commit)---",
    r"---(#    ids and nothing else.)---",
    r"---(#  - everything that is not made of clean commits, when protected branches are configured)---",
    r"---(#    and neither they nor a recorded tip can be found here. To release the guard:)---",
    r"---(#    git config --unset-all cleanpublish.protectedbranch)---",
    r"---(#  - a ref that does not lead to a commit, unless it is part of a clean commit.)---",
    r"---(#  - any direct push to a guarded public remote (git config cleanpublish.publicurl))---",
    r"---(#    unless TEMPLECBE_PUBLISHING=1 (set by clean_publish()); with it only the clean)---",
    r"---(#    commits pass.)---",
    r"---(# `git push --no-verify` bypasses it.)---",
    r"---(_tcbe_in=$(cat))---",
    r"---(_tcbe_noglob=)---",
    r"---(case $- in *f*) _tcbe_noglob=1 ;; esac)---",
    r"---(set -f)---",
    r"---(_tcbe_nl=')---",
    r"---(')---",
    r"---(_tcbe_say() { printf '%s\n' "clean_publish guard: $*" >&2; })---",
    r"---(_tcbe_protected=)---",
    r"---(_tcbe_clean=)---",
    r"---(_tcbe_tips=)---",
    r"---(_tcbe_public=)---",
    r"---(while read -r _tcbe_k _tcbe_v; do)---",
    r"---(  case "$_tcbe_k" in)---",
    r"---(    cleanpublish.protectedbranch) _tcbe_protected="$_tcbe_protected $_tcbe_v" ;;)---",
    r"---(    cleanpublish.cleancommit) _tcbe_clean="$_tcbe_clean $_tcbe_v" ;;)---",
    r"---(    cleanpublish.privatetip) _tcbe_tips="$_tcbe_tips $_tcbe_v" ;;)---",
    r"---(    cleanpublish.publicurl) _tcbe_public="$_tcbe_public $_tcbe_v" ;;)---",
    r"---(  esac)---",
    r"---(done <<_TCBE_EOF_)---",
    r"---($(git config --get-regexp '^cleanpublish[.]' || :))---",
    r"---(_TCBE_EOF_)---",
    r"---(# Same steps, in the same order, as .cp_normalize_url() in R.)---",
    r"---(_tcbe_norm_url() {)---",
    r"---(  _tcbe_u=$(printf '%s\n' "$1" | tr '\\' '/'))---",
    r"---(  case "$_tcbe_u" in)---",
    r"---(    *://*))---",
    r"---(      _tcbe_s=${_tcbe_u%%://*})---",
    r"---(      case "$_tcbe_s" in)---",
    r"---(        ''|*[!a-zA-Z0-9+_.-]*) ;;)---",
    r"---(        *) _tcbe_u=${_tcbe_u#*://} ;;)---",
    r"---(      esac ;;)---",
    r"---(  esac)---",
    r"---(  _tcbe_s=${_tcbe_u%%/*})---",
    r"---(  case "$_tcbe_s" in *@*) _tcbe_u=${_tcbe_u#"${_tcbe_s%@*}"@} ;; esac)---",
    r"---(  _tcbe_u=$(printf '%s\n' "$_tcbe_u" | tr ':' '/' | tr -s '/' | tr '[:upper:]' '[:lower:]'))---",
    r"---(  while :; do)---",
    r"---(    case "$_tcbe_u" in)---",
    r"---(      *.git) _tcbe_u=${_tcbe_u%.git} ;;)---",
    r"---(      */) _tcbe_u=${_tcbe_u%/} ;;)---",
    r"---(      *) break ;;)---",
    r"---(    esac)---",
    r"---(  done)---",
    r"---(  case "$_tcbe_u" in /*) _tcbe_u=${_tcbe_u#/} ;; esac)---",
    r"---(  printf '%s\n' "$_tcbe_u")---",
    r"---(})---",
    r"---(# The ways a URL, a path or a remote name can be recognised: itself, the push URLs of a)---",
    r"---(# remote of that name or else its url.<base>.insteadOf expansion, and the physical path of)---",
    r"---(# a local directory.)---",
    r"---(_tcbe_spellings() {)---",
    r"---(  printf '%s\n' "$1")---",
    r"---(  git remote get-url --push --all -- "$1" 2>/dev/null || git ls-remote --get-url -- "$1" 2>/dev/null || :)---",
    r"---(  if [ -d "$1" ]; then (cd "$1" 2>/dev/null && pwd -P) || :; fi)---",
    r"---(})---",
    r"---(_tcbe_norm_lines() {)---",
    r"---(  while IFS= read -r _tcbe_l; do)---",
    r"---(    if [ -n "$_tcbe_l" ]; then _tcbe_norm_url "$_tcbe_l"; fi)---",
    r"---(  done)---",
    r"---(})---",
    r"---(_tcbe_is_id() {)---",
    r"---(  case "$1" in *[!0-9a-f]*) return 1 ;; esac)---",
    r"---(  case ${#1} in 40|64) return 0 ;; esac)---",
    r"---(  return 1)---",
    r"---(})---",
    r"---(# The values of config key $1 (the other arguments) that are full ids of commits that exist)---",
    r"---(# here, as ' id' words. Anything else is ignored: a value is never read as a revision.)---",
    r"---(_tcbe_commits() {)---",
    r"---(  _tcbe_key=$1; shift)---",
    r"---(  for _tcbe_o in "$@"; do)---",
    r"---(    if _tcbe_is_id "$_tcbe_o"; then printf '%s\n' "$_tcbe_o"; else)---",
    r"---(      _tcbe_say "ignoring $_tcbe_key '$_tcbe_o': it is not a full object id.")---",
    r"---(    fi)---",
    r"---(  done | git cat-file --batch-check='%(objectname) %(objecttype)' | while read -r _tcbe_o _tcbe_y; do)---",
    r"---(    if [ "$_tcbe_y" = commit ]; then)---",
    r"---(      printf ' %s' "$_tcbe_o")---",
    r"---(    elif [ "$_tcbe_key" = cleanpublish.privatetip ]; then)---",
    r"---(      _tcbe_say "ignoring $_tcbe_key '$_tcbe_o': it is not a commit in this repository.")---",
    r"---(    fi)---",
    r"---(  done || :)---",
    r"---(})---",
    r"---(_tcbe_not=$(_tcbe_commits cleanpublish.cleancommit $_tcbe_clean))---",
    r"---(_tcbe_tipc=$(_tcbe_commits cleanpublish.privatetip $_tcbe_tips))---",
    r"---(# the private history: the recorded tips and the live tips of the protected branches)---",
    r"---(_tcbe_roots=$_tcbe_tipc)---",
    r"---(for _tcbe_b in $_tcbe_protected; do)---",
    r"---(  _tcbe_t=$(git rev-parse --verify --quiet "refs/heads/$_tcbe_b^{commit}" || :))---",
    r"---(  if [ -n "$_tcbe_t" ]; then _tcbe_roots="$_tcbe_roots $_tcbe_t"; fi)---",
    r"---(done)---",
    r"---(_tcbe_nothing=)---",
    r"---(if [ -n "$_tcbe_protected$_tcbe_tips" ] && [ -z "$_tcbe_roots" ]; then _tcbe_nothing=1; fi)---",
    r"---(if [ -n "$_tcbe_public" ] && [ "${TEMPLECBE_PUBLISHING-}" != 1 ]; then)---",
    r"---(  _tcbe_target=${2-})---",
    r"---(  if [ -z "$_tcbe_target" ] && [ -n "${1-}" ]; then)---",
    r"---(    _tcbe_target=$(git remote get-url --push -- "$1" 2>/dev/null || printf '%s' "$1"))---",
    r"---(  fi)---",
    r"---(  if [ -n "$_tcbe_target" ]; then)---",
    r"---(    _tcbe_nf=)---",
    r"---(    for _tcbe_p in $_tcbe_public; do)---",
    r"---(      _tcbe_nf="$_tcbe_nf$_tcbe_nl$(_tcbe_spellings "$_tcbe_p" | _tcbe_norm_lines)")---",
    r"---(    done)---",
    r"---(    while IFS= read -r _tcbe_n; do)---",
    r"---(      if [ -n "$_tcbe_n" ]; then)---",
    r"---(        case "$_tcbe_nl$_tcbe_nf$_tcbe_nl" in)---",
    r"---(          *"$_tcbe_nl$_tcbe_n$_tcbe_nl"*))---",
    r"---(            _tcbe_say "push refused: direct push to public remote '$_tcbe_target' is guarded.")---",
    r"---(            _tcbe_say "Use clean_publish() (or scripts/clean_publish.R --push) to publish a clean snapshot, or re-point the remote.")---",
    r"---(            exit 1 ;;)---",
    r"---(        esac)---",
    r"---(      fi)---",
    r"---(    done <<_TCBE_EOF_)---",
    r"---($(_tcbe_spellings "$_tcbe_target" | _tcbe_norm_lines))---",
    r"---(_TCBE_EOF_)---",
    r"---(  fi)---",
    r"---(fi)---",
    r"---(while read -r _tcbe_lref _tcbe_loid _tcbe_rref _tcbe_roid; do)---",
    r"---(  case "$_tcbe_loid" in *[!0]*) ;; *) continue ;; esac)---",
    r"---(  if [ "${TEMPLECBE_PUBLISHING-}" = 1 ]; then)---",
    r"---(    case "$_tcbe_not " in)---",
    r"---(      *" $_tcbe_loid "*) ;;)---",
    r"---(      *))---",
    r"---(        _tcbe_say "push refused: with TEMPLECBE_PUBLISHING=1 only the clean commits made by clean_publish() may be pushed, and '$_tcbe_lref' is not one.")---",
    r"---(        exit 1 ;;)---",
    r"---(    esac)---",
    r"---(  fi)---",
    r"---(  for _tcbe_b in $_tcbe_protected; do)---",
    r"---(    _tcbe_hit=)---",
    r"---(    case "$_tcbe_lref" in "$_tcbe_b"|"refs/heads/$_tcbe_b") _tcbe_hit=1 ;; esac)---",
    r"---(    case "$_tcbe_rref" in "refs/heads/$_tcbe_b") _tcbe_hit=1 ;; esac)---",
    r"---(    if [ -n "$_tcbe_hit" ]; then)---",
    r"---(      _tcbe_say "push refused: '$_tcbe_lref' -> '$_tcbe_rref' involves the private branch '$_tcbe_b'.")---",
    r"---(      exit 1)---",
    r"---(    fi)---",
    r"---(  done)---",
    r"---(  _tcbe_c=$(git rev-parse --verify --quiet "$_tcbe_loid^{commit}" || :))---",
    r"---(  if [ -z "$_tcbe_c" ]; then)---",
    r"---(    # a tag, tree or blob that does not lead to a commit: only a tree or blob that a clean)---",
    r"---(    # commit has is demonstrably clean)---",
    r"---(    _tcbe_y=$(git cat-file -t "$_tcbe_loid" 2>/dev/null || :))---",
    r"---(    case "$_tcbe_y" in)---",
    r"---(      tree|blob))---",
    r"---(        if [ -n "$_tcbe_not" ] && git rev-list --objects $_tcbe_not | cut -d ' ' -f 1 | grep -q -x -F -- "$_tcbe_loid"; then continue; fi ;;)---",
    r"---(    esac)---",
    r"---(    _tcbe_say "push refused: '$_tcbe_lref' does not lead to a commit (it is a ${_tcbe_y:-unknown} object) and is not part of a clean commit.")---",
    r"---(    exit 1)---",
    r"---(  fi)---",
    r"---(  _tcbe_a=$(git rev-list --count "$_tcbe_c" --not $_tcbe_not) || {)---",
    r"---(    _tcbe_say "push refused: could not inspect '$_tcbe_lref'.")---",
    r"---(    exit 1)---",
    r"---(  })---",
    r"---(  if [ -n "$_tcbe_roots" ]; then)---",
    r"---(    # commits the push carries that no clean commit has, with and without those the)---",
    r"---(    # private history reaches: a difference means that it carries private commits)---",
    r"---(    _tcbe_d=$(git rev-list --count "$_tcbe_c" --not $_tcbe_not $_tcbe_roots) || {)---",
    r"---(      _tcbe_say "push refused: could not inspect '$_tcbe_lref'.")---",
    r"---(      exit 1)---",
    r"---(    })---",
    r"---(    if [ "$_tcbe_a" != "$_tcbe_d" ]; then)---",
    r"---(      _tcbe_say "push refused: '$_tcbe_lref' contains commits of the private history (protected branches:${_tcbe_protected:- none}).")---",
    r"---(      _tcbe_say "Push only the branch made by clean_publish(), not the private history.")---",
    r"---(      exit 1)---",
    r"---(    fi)---",
    r"---(  elif [ -n "$_tcbe_nothing" ] && [ "$_tcbe_a" != 0 ]; then)---",
    r"---(    _tcbe_say "push refused: '$_tcbe_lref' is not made of clean commits, and the private history cannot be found here (protected branches:$_tcbe_protected).")---",
    r"---(    _tcbe_say "To release this guard: git config --unset-all cleanpublish.protectedbranch (and: git config --unset-all cleanpublish.privatetip).")---",
    r"---(    exit 1)---",
    r"---(  fi)---",
    r"---(done <<_TCBE_EOF_)---",
    r"---($_tcbe_in)---",
    r"---(_TCBE_EOF_)---",
    r"---(if [ -n "$_tcbe_in" ]; then)---",
    r"---(  exec <<_TCBE_EOF_)---",
    r"---($_tcbe_in)---",
    r"---(_TCBE_EOF_)---",
    r"---(else)---",
    r"---(  exec </dev/null)---",
    r"---(fi)---",
    r"---(if [ -z "$_tcbe_noglob" ]; then set +f; fi)---",
    r"---(unset -f _tcbe_say _tcbe_norm_url _tcbe_spellings _tcbe_norm_lines _tcbe_is_id _tcbe_commits)---",
    r"---(unset _tcbe_in _tcbe_noglob _tcbe_nl _tcbe_protected _tcbe_clean _tcbe_tips _tcbe_public _tcbe_k _tcbe_v)---",
    r"---(unset _tcbe_not _tcbe_tipc _tcbe_roots _tcbe_b _tcbe_t _tcbe_nothing _tcbe_target _tcbe_nf _tcbe_p _tcbe_n)---",
    r"---(unset _tcbe_lref _tcbe_loid _tcbe_rref _tcbe_roid _tcbe_hit _tcbe_c _tcbe_y _tcbe_a _tcbe_d _tcbe_u _tcbe_s _tcbe_l _tcbe_o _tcbe_key)---",
    .cp_marker_end
  )
}
# The hook this function wrote before it had a managed block: 8 fixed lines naming
# one branch. Returns that branch name, or NA when `lines` is not that hook.
.cp_legacy_hook_branch <- function(lines) {
  lines <- sub("\r$", "", lines)
  if (length(lines) != 8L) return(NA_character_)
  fixed <- c(
    "#!/usr/bin/env bash",
    "while read -r local_ref local_oid remote_ref remote_oid; do"
  )
  tail_fixed <- c(
    "        echo \"ERROR: Push aborted! Attempted to push private branch '$local_ref' to remote.\" >&2",
    "        exit 1",
    "    fi",
    "done",
    "exit 0"
  )
  pat <- "^    if \\[\\[ \"\\$local_ref\" == \\*\"(.+)\"\\* \\]\\]; then$"
  if (!identical(lines[1:2], fixed) || !identical(lines[4:8], tail_fixed) || !grepl(pat, lines[3])) {
    return(NA_character_)
  }
  sub(pat, "\\1", lines[3])
}
# Decide what the hook file must contain, without writing it. Stops when an
# existing hook cannot be combined safely with the managed block.
.cp_plan_hook <- function(hook_path) {
  block <- .cp_hook_block()
  plan <- function(action, lines, legacy = character()) {
    list(path = hook_path, action = action, content = paste0(paste(lines, collapse = "\n"), "\n"), legacy_names = legacy)
  }
  if (!file.exists(hook_path)) {
    return(plan("created", c("#!/bin/sh", block, "exit 0")))
  }
  if (dir.exists(hook_path) || any(readBin(hook_path, "raw", min(file.size(hook_path), 65536L)) == as.raw(0L))) {
    stop("'", hook_path, "' is not a text file, so the push guard cannot be added to it. Nothing was changed.", call. = FALSE)
  }
  lines <- readLines(hook_path, warn = FALSE)
  if (!any(nzchar(trimws(lines)))) {
    return(plan("created", c("#!/bin/sh", block, "exit 0")))
  }
  legacy <- .cp_legacy_hook_branch(lines)
  if (!is.na(legacy)) {
    return(plan("upgraded", c("#!/bin/sh", block, "exit 0"), legacy))
  }
  begin <- which(startsWith(lines, .cp_marker_begin))
  end <- which(trimws(lines) == .cp_marker_end)
  if (length(begin) == 1L && length(end) == 1L && begin < end) {
    return(plan("updated", c(lines[seq_len(begin - 1L)], block, lines[-seq_len(end)])))
  }
  if (length(begin) || length(end)) {
    stop(
      "'", hook_path, "' has a damaged TempleCBE guard block (markers '", .cp_marker_begin, "' and '",
      .cp_marker_end, "' must appear once each, in that order). Fix or remove the block and run again. ",
      "Nothing was changed.",
      call. = FALSE
    )
  }
  # the block relies on word splitting and on `read -r`: sh, dash, bash and ksh only (not zsh)
  if (!grepl("^#![[:space:]]*(/usr/bin/env[[:space:]]+)?([^[:space:]]*/)?(ba|da|k)?sh([[:space:]]|$)", lines[1])) {
    stop(
      "'", hook_path, "' already exists and is not a shell script for sh, dash, bash or ksh (first line: '", lines[1], "'), so the ",
      "push guard cannot be added to it. Merge the guard into it by hand (the block that clean_publish() ",
      "writes into a new hook starts with '", .cp_marker_begin, "'), or remove the hook and run again. ",
      "Nothing was changed.",
      call. = FALSE
    )
  }
  plan("extended", c(lines[1], block, lines[-1]))
}
.cp_write_hook <- function(plan) {
  fs::dir_create(dirname(plan$path))
  writeBin(charToRaw(plan$content), plan$path)
  tryCatch(Sys.chmod(plan$path, mode = "0755"), error = function(e) NULL)
  invisible(plan$path)
}
# Record every commit that holds private history as a full id in the multi-valued config
# key cleanpublish.privatetip, which the hook protects like the tip of a protected branch,
# whatever becomes of the branch names: renamed, deleted, moved, or never created because
# the first push failed. Append only. Clean commits are left out (the hook never counts
# them as private) and so are ids that are recorded already. clean_publish() calls it
# before it pushes or moves anything.
.cp_record_private_tips <- function(repo_root, oids) {
  oids <- unique(oids[!is.na(oids) & nzchar(oids)])
  known <- c(
    .cp_config_get_all(repo_root, "cleanpublish.cleancommit"),
    .cp_config_get_all(repo_root, "cleanpublish.privatetip")
  )
  new <- setdiff(oids, known)
  for (oid in new) .cp_git(repo_root, c("config", "--add", "cleanpublish.privatetip", oid))
  invisible(new)
}
# Test pushes to a temporary local repository. The hook runs before anything is sent, so a
# working guard refuses each push and the temporary repository stays empty.
#   1. the private branch name: refused by the hook's name rule
#   2. `tip` (the commit that is being published, recorded as a private tip) under another
#      branch name and as a lightweight tag: refused by its content rule, "contains commits"
# A push that is accepted shows that the guard does not work (stop). A push that is refused
# in another way cannot be told from a refusal by the guard, so the guard is unconfirmed: an
# error, unless `confirm` is FALSE, when it is a warning.
.cp_selftest_guard <- function(repo_root, private_branch, tip = NA_character_, confirm = TRUE) {
  tmp <- tempfile("clean_publish_selftest_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)
  unconfirmed <- function(why) {
    msg <- paste0("Could not confirm that the push guard works: ", why)
    if (isTRUE(confirm)) {
      stop(
        msg, "\nNothing was pushed and no branch was moved. Fix the cause and run again, or pass ",
        "`confirm_guard = FALSE` to go on without the confirmation (the guard may then not protect you).",
        call. = FALSE
      )
    }
    warning(msg, call. = FALSE)
    invisible(NA)
  }
  init <- .cp_git(repo_root, c("init", "--bare", "--quiet", tmp), error_ok = TRUE)
  if (init$status != 0L) return(unconfirmed("no temporary repository for a test push."))
  test_push <- function(src, ref) .cp_git(repo_root, c("push", tmp, paste0(src, ":", ref)), error_ok = TRUE)
  said <- function(res, text) any(grepl(text, c(res$output, res$stderr), fixed = TRUE))
  shown <- function(res) paste(c(res$output, res$stderr), collapse = "\n")
  res <- test_push("HEAD", paste0("refs/heads/", private_branch))
  if (res$status == 0L) {
    stop(
      "The pre-push guard does not run: a test push of '", private_branch, "' to a temporary repository ",
      "was accepted. Check core.hooksPath, the execute bit of the hook and whether hooks are disabled. ",
      "No branch was changed.",
      call. = FALSE
    )
  }
  if (!said(res, "clean_publish guard")) {
    return(unconfirmed(paste0("a test push of '", private_branch, "' was refused, but not by the guard:\n", shown(res))))
  }
  if (!is.na(tip) && !tip %in% .cp_config_get_all(repo_root, "cleanpublish.cleancommit")) {
    id <- sub("^clean_publish_selftest_", "", basename(tmp))
    for (ref in paste0(c("refs/heads/", "refs/tags/"), "selftest-", id)) {
      res <- test_push(tip, ref)
      if (res$status == 0L) {
        stop(
          "The pre-push guard does not stop a push of private commits: a test push of the commit being ",
          "published (", substr(tip, 1L, 10L), ") as '", sub("-[0-9a-f]+$", "", ref), "' to a temporary ",
          "repository was accepted. It recognises the name of the private branch only. No branch was changed.",
          call. = FALSE
        )
      }
      if (!said(res, "contains commits")) {
        return(unconfirmed(paste0(
          "a test push of the commit being published as '", sub("-[0-9a-f]+$", "", ref), "' was refused, ",
          "but not for containing private commits:\n", shown(res)
        )))
      }
    }
  }
  invisible(TRUE)
}
# ------------------------------------------------------------------------------
# Command line (scripts/clean_publish.R)
# ------------------------------------------------------------------------------
#' Usage Text of \code{scripts/clean_publish.R}
#' @return Character vector of lines.
#' @keywords internal
#' @noRd
clean_publish_usage <- function() {
  c(
    "Usage: Rscript scripts/clean_publish.R [options]",
    "",
    "Publishes the tree of HEAD as a clean commit on the publish branch and keeps the full",
    "history on the private branch. Two modes: standalone (the default) makes one parentless",
    "commit and force-pushes it into a NEW EMPTY repository; --snapshot makes one commit on top",
    "of the remote branch (which must hold only clean commits made by this tool) and pushes it",
    "as a fast-forward, for every later release. This is a DRY RUN unless --push is given:",
    "the local branches are rewritten, nothing is sent to any remote.",
    "",
    "Options (a value is given as '--name value' or '--name=value'):",
    "  --push                Push the clean commit to the remote (standalone mode force-pushes:",
    "                        destructive)",
    "  --no-push             Dry run; already the default, accepted for old command lines",
    "  --snapshot            Create a clean snapshot commit parented on remote tip",
    "                        (fast-forward only; default is standalone parentless commit)",
    "  --fence NAME|URL      Guard a public remote against accidental direct pushes: the name of a",
    "                        configured remote, or a URL that is the push URL of one",
    "                        (configures cleanpublish.publicurl, installs and checks the pre-push hook)",
    "  --allow-ignored PATH  Allow a tracked-but-ignored file in the clean snapshot",
    "                        (may be specified multiple times, or comma-separated)",
    "  --allow-other-refs    Push although the remote has refs other than the publish branch",
    "                        (branches, tags, notes, refs/pull/*): they keep the old history",
    "  --allow-overwrite-history",
    "                        Push although the remote holds this repository's own history",
    "                        (it is overwritten)",
    "  --allow-submodules    Publish a tree with submodules (their URLs become public)",
    "  --no-disarm           After the push keep the push URL of the remote usable (by default",
    "                        it is replaced by an unusable value and the real URL is recorded)",
    "  --branch NAME         Branch to overwrite with the clean commit",
    "                        (default: master, else main, else the current branch;",
    "                        never the --private branch)",
    "  --private NAME        Local branch retaining full history (default: private-history)",
    "  --remote NAME         Remote to publish to; required with --push or --snapshot. There is",
    "                        deliberately no default: in a clone of the private repository",
    "                        'origin' is the private repository",
    "  -m, --message MSG     Commit message (default: 'Initial clean commit', or",
    "                        'Snapshot <date>' with --snapshot)",
    "  -h, --help            Show this help message",
    "",
    "Publish into a NEW EMPTY repository: a force-push replaces one branch only, and the",
    "other refs of the remote (GitHub's hidden refs/pull/* among them) keep the old history.",
    "",
    "Any other option is an error, and so is a value that starts with '-' after",
    "'--name' (a message that starts with '-' can be given as --message=-text)."
  )
}
#' Parse the Command Line of \code{scripts/clean_publish.R}
#'
#' Strict on purpose: the last step is a force-push, so a mistyped flag must
#' never be ignored. An unknown option, a stray argument, a repeated option, a
#' missing value, a value that starts with \code{-} (it is most likely another
#' flag) and \code{--push} together with \code{--no-push} are errors whose
#' message lists the accepted options. The default is a dry run: \code{push} is
#' \code{TRUE} only for an explicit \code{--push}.
#'
#' @param args Character vector as from \code{commandArgs(trailingOnly = TRUE)}.
#' @return A list with \code{help}, \code{push}, \code{snapshot},
#'   \code{allow_other_refs}, \code{allow_overwrite_history}, \code{allow_submodules}
#'   (logical, \code{FALSE} unless given), \code{disarm_push_url} (logical, \code{FALSE}
#'   only with \code{--no-disarm}) and \code{fence}, \code{allow_ignored},
#'   \code{publish_branch}, \code{private_branch}, \code{remote}, \code{commit_msg}
#'   (each \code{NULL} when not given).
#' @keywords internal
#' @noRd
parse_clean_publish_args <- function(args) {
  args <- as.character(args)
  fail <- function(...) {
    stop(paste0(..., "\n\n", paste(clean_publish_usage(), collapse = "\n")), call. = FALSE)
  }
  value_flags <- c(
    "--branch" = "publish_branch", "--private" = "private_branch", "--remote" = "remote",
    "--message" = "commit_msg", "-m" = "commit_msg",
    "--fence" = "fence", "--allow-ignored" = "allow_ignored"
  )
  switch_flags <- c(
    "--push" = "push", "--no-push" = "no_push", "--snapshot" = "snapshot",
    "--allow-other-refs" = "allow_other_refs", "--allow-overwrite-history" = "allow_overwrite_history",
    "--allow-submodules" = "allow_submodules", "--no-disarm" = "no_disarm"
  )
  vals <- list()
  allow_ignored <- character(0)
  help <- FALSE
  switches <- stats::setNames(rep(FALSE, length(switch_flags)), switch_flags)
  i <- 1L
  while (i <= length(args)) {
    arg <- args[[i]]
    i <- i + 1L
    if (is.na(arg)) fail("Unexpected missing argument.")
    if (arg %in% c("-h", "--help")) {
      help <- TRUE
    } else if (arg %in% names(switch_flags)) {
      key <- switch_flags[[arg]]
      if (switches[[key]]) fail("'", arg, "' was given more than once.")
      switches[[key]] <- TRUE
    } else if (startsWith(arg, "-")) {
      # only a long option takes '--name=value'; '-m=text' is not an option
      has_inline <- startsWith(arg, "--") && grepl("=", arg, fixed = TRUE)
      flag <- if (has_inline) sub("=.*$", "", arg) else arg
      if (!flag %in% names(value_flags)) fail("Unknown option '", arg, "'.")
      key <- value_flags[[flag]]
      if (has_inline) {
        value <- sub("^[^=]*=", "", arg)
        if (!nzchar(value)) fail("'", flag, "' needs a value, but '", arg, "' is empty.")
        # the --name=value form takes a leading '-' as text, but only for a message
        if (startsWith(value, "-") && key != "commit_msg") {
          fail("The value of '", flag, "' must not start with '-': '", value, "'.")
        }
      } else {
        if (i > length(args)) fail("'", flag, "' needs a value.")
        value <- args[[i]]
        i <- i + 1L
        if (is.na(value) || !nzchar(value) || startsWith(value, "-")) {
          fail("'", flag, "' needs a value, but the next argument is '", value, "'.")
        }
      }
      if (key == "allow_ignored") {
        split_vals <- unlist(strsplit(value, ",", fixed = TRUE))
        allow_ignored <- c(allow_ignored, trimws(split_vals[nzchar(trimws(split_vals))]))
      } else {
        if (!is.null(vals[[key]])) fail("'", flag, "' (or its alias) was given more than once.")
        vals[[key]] <- value
      }
    } else {
      fail("Unexpected argument '", arg, "'.")
    }
  }
  if (switches[["push"]] && switches[["no_push"]]) fail("'--push' and '--no-push' contradict each other.")
  list(
    help = help,
    push = switches[["push"]],
    snapshot = switches[["snapshot"]],
    allow_other_refs = switches[["allow_other_refs"]],
    allow_overwrite_history = switches[["allow_overwrite_history"]],
    allow_submodules = switches[["allow_submodules"]],
    disarm_push_url = !switches[["no_disarm"]],
    fence = vals[["fence"]],
    allow_ignored = allow_ignored,
    publish_branch = vals[["publish_branch"]],
    private_branch = vals[["private_branch"]],
    remote = vals[["remote"]],
    commit_msg = vals[["commit_msg"]]
  )
}
#' The Command Line of \code{scripts/clean_publish.R} as a Function
#'
#' All the push control of the command line lives here, not in the script: the script is
#' build-ignored, so tests of it skip under \command{R CMD check}, while this function is
#' installed with the package and is tested in-process. The script only loads the package and
#' calls it. The repository is the git toplevel of the working directory.
#'
#' Messages go to standard error, \code{--help} to standard output. \code{options(warn = 1)}
#' holds for the run, so a warning appears when it happens and not after \code{SUCCESS}. A
#' warning raised before the push stops the run, because the push is irreversible and a
#' warning is something the caller did not expect: nothing is pushed and the status is 1. One
#' raised after the push is shown and changes nothing.
#'
#' @param args Character vector as from \code{commandArgs(trailingOnly = TRUE)}.
#' @return The exit status as an integer: \code{0} for success and \code{--help}, \code{2} for a
#'   bad command line (unknown or repeated option, \code{--fence} with another option, no
#'   \code{--remote} for \code{--push} or \code{--snapshot}), \code{1} when the run stopped with
#'   an error or with a warning raised before the push.
#' @keywords internal
#' @noRd
clean_publish_cli <- function(args = commandArgs(trailingOnly = TRUE)) {
  old_options <- options(warn = 1L)
  on.exit(options(old_options), add = TRUE)
  failed <- function(status, text) {
    message("Error: ", text)
    status
  }
  opts <- tryCatch(parse_clean_publish_args(args), error = function(e) e)
  if (inherits(opts, "error")) return(failed(2L, conditionMessage(opts)))

  if (opts$help) {
    writeLines(clean_publish_usage())
    return(0L)
  }

  if (!is.null(opts$fence)) {
    # --fence only records the URL and installs the hook; any other option would be dropped silently
    others <- c(
      "--push" = opts$push, "--snapshot" = opts$snapshot,
      "--branch" = !is.null(opts$publish_branch), "--private" = !is.null(opts$private_branch),
      "--remote" = !is.null(opts$remote), "--message" = !is.null(opts$commit_msg),
      "--allow-ignored" = length(opts$allow_ignored) > 0L,
      "--allow-other-refs" = opts$allow_other_refs, "--allow-overwrite-history" = opts$allow_overwrite_history,
      "--allow-submodules" = opts$allow_submodules, "--no-disarm" = !opts$disarm_push_url
    )
    if (any(others)) {
      return(failed(2L, paste0(
        "--fence only records the URL and installs the hook; it cannot be combined with ",
        paste(names(others)[others], collapse = ", "), "."
      )))
    }
    fenced <- tryCatch({
      repo_root <- .cp_default_repo_root()
      message("Repository: ", repo_root)
      guard_public_remote(opts$fence, repo_root = repo_root)
    }, error = function(e) e)
    if (inherits(fenced, "error")) return(failed(1L, conditionMessage(fenced)))
    return(0L)
  }

  mode <- if (isTRUE(opts$snapshot)) "snapshot" else "standalone"
  remote <- opts$remote
  if (is.null(remote) && (opts$push || identical(mode, "snapshot"))) {
    return(failed(2L, "name the remote to publish to with --remote NAME (there is deliberately no default)."))
  }
  if (opts$push) {
    if (identical(mode, "snapshot")) {
      message("--push given: the publish branch will be fast-forward pushed to remote '", remote, "'.")
    } else {
      message("--push given: the publish branch will be FORCE-PUSHED to remote '", remote, "'.")
    }
  } else {
    message("Dry run: local branches are rewritten, nothing is pushed. Add --push to publish.")
  }

  # the push is the one step that cannot be undone: a warning before it stops the run
  pushing <- FALSE
  published <- tryCatch(
    withCallingHandlers(
      clean_publish(
        publish_branch = opts$publish_branch,
        private_branch = if (is.null(opts$private_branch)) "private-history" else opts$private_branch,
        remote = remote,
        commit_msg = opts$commit_msg,
        push = opts$push,
        mode = mode,
        allow_ignored = opts$allow_ignored,
        allow_other_refs = opts$allow_other_refs,
        allow_overwrite_history = opts$allow_overwrite_history,
        allow_submodules = opts$allow_submodules,
        disarm_push_url = opts$disarm_push_url
      ),
      message = function(m) {
        if (grepl("^\\[PUSH\\] (Force-pushing|Fast-forward pushing)", conditionMessage(m))) pushing <<- TRUE
      },
      warning = function(w) {
        if (!pushing) {
          stop(
            "A warning was raised before the push, so the run was stopped. Nothing was pushed. The warning: ",
            conditionMessage(w),
            call. = FALSE
          )
        }
      }
    ),
    error = function(e) e
  )
  if (inherits(published, "error")) return(failed(1L, conditionMessage(published)))
  0L
}
