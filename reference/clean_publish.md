# Publish a Clean Snapshot of the Current Commit to a Remote

Publishes the tree of the current commit as a clean commit that carries
none of the history behind it, and keeps the full history on
`private_branch`. There are two modes. `mode = "standalone"` (the
default) makes one commit without a parent and, with `push = TRUE`,
force-pushes it over the publish branch of `remote`; it is the first
publication, into a new empty repository. `mode = "snapshot"` makes one
commit whose parent is the tip of the publish branch of `remote`, which
must consist only of commits this function made, and pushes it as an
ordinary fast-forward; it is every later release. With `push = FALSE`
only the local branches are rewritten. A pre-push hook keeps
`private_branch` and every commit that only it contains from being
pushed by accident.

## Usage

``` r
clean_publish(
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
)
```

## Arguments

- repo_root:

  Path to the git repository root (or any directory of its working tree;
  it may be a linked worktree and may contain spaces). Defaults to the
  git toplevel of the working directory (an error when that is not in a
  repository). The repository that is acted on, its git directory, the
  current branch and the remotes with their URLs are printed first.

- publish_branch:

  Branch to overwrite with the clean commit. Defaults to `master`, else
  `main`, else the current branch (unless that is `private_branch`).
  `master`/`main` are checked before the current branch on purpose:
  running this from an arbitrary feature branch must not silently
  overwrite that branch with a one-commit history. `private_branch`
  itself is refused, since publishing onto it would squash away the full
  history it exists to keep, and so is a name that differs from it only
  in case.

- private_branch:

  Local branch that retains full history. Default `"private-history"`.
  Created from the current commit if it does not exist, otherwise moved
  forward to the current commit (never backward or sideways, see above).

- remote:

  Name of a configured remote to publish to. Required for `push = TRUE`
  and for `mode = "snapshot"`, and deliberately without a default: in a
  clone of the private repository `"origin"` is the private repository,
  and a standalone publish force-pushes one commit over its branch. Its
  push URL is printed before the push. In snapshot mode the remote must
  be the one that holds the public snapshots: a remote branch whose
  history is not made only of clean commits made by this function is
  refused, because the snapshot would carry that history.

- commit_msg:

  Commit message for the clean commit. Default:
  `"Initial clean commit"`, or `"Snapshot <date>"` for
  `mode = "snapshot"`. The trailer line `Clean-Publish: v1` is appended
  after a blank line.

- push:

  Logical (default `TRUE`); if `FALSE`, rewrite the local branches but
  skip the force-push, as a dry run of the destructive step. (The
  command-line script `scripts/clean_publish.R` is the other way round:
  it pushes only when given `--push`.)

- mode:

  Mode of publishing: `"standalone"` (default) creates a root
  (parentless) commit and force-pushes with lease to the remote;
  `"snapshot"` fetches the remote publish branch and creates a
  fast-forward commit parented on the remote tip.

- allow_ignored:

  Character vector of file paths that are tracked in git and match
  `.gitignore` patterns, explicitly allowed to be included in the clean
  commit. By default, clean_publish stops if any tracked-but-ignored
  files exist to prevent accidental leakage.

- confirm_guard:

  Logical (default `TRUE`). A push guard that could not be confirmed by
  the test pushes is an error before anything moves. `FALSE` turns that
  case into a warning; a guard that is shown not to work is an error
  either way.

- allow_other_refs:

  Logical (default `FALSE`). Standalone mode with `push = TRUE` refuses
  a remote that has refs other than the publish branch (branches, tags,
  notes, `refs/pull/*`), because they keep the old history. `TRUE` goes
  on and lists them in the success message.

- allow_overwrite_history:

  Logical (default `FALSE`). Standalone mode with `push = TRUE` refuses
  a remote that holds this repository's own history. `TRUE` overwrites
  the branch all the same.

- allow_submodules:

  Logical (default `FALSE`). A current commit with submodules (gitlinks
  or a tracked `.gitmodules`) is refused, because their URLs and commit
  ids would be published. `TRUE` publishes them and prints their URLs.

- disarm_push_url:

  Logical (default `TRUE`). After a successful push, record the real
  push URL of `remote` and replace its `remote.<remote>.pushurl` by an
  unusable value (see above). `FALSE` leaves the remote as it was; the
  pre-push hook is then the only guard of pushes by the name of the
  remote.

## Value

The clean commit SHA, invisibly.

## Details

**The target must be a NEW EMPTY repository.** A force-push replaces one
branch of the remote and nothing else: its other branches, its tags, its
notes and GitHub's hidden `refs/pull/*` keep the commits they point to,
so a repository that ever held the private history keeps it readable,
and no push, force-push included, can remove a hidden ref. Create the
public repository for this purpose, add it to a throwaway clone as the
only remote and publish into it (the steps are in `REVIEW.md`,
"Publishing to the public repository").

`remote` is the name of a remote you have added, for example
`git remote add public <url>`, and has no default: in a clone of the
private repository `origin` is the private repository, and a standalone
publish force-pushes one commit over its branch. To publish the same
snapshot to a second host, add a second remote
(`git remote add med-cbe <url>`) and run again with
`remote = "med-cbe"`.

**Nothing is changed until every check has passed.** The function stops,
and leaves branches, the working tree, the git configuration, the hooks
and the remote as they were, when

- git has no identity for the clean commit (`git var GIT_AUTHOR_IDENT`
  or `GIT_COMMITTER_IDENT` fails, as in a fresh clone);

- a branch name is not valid as `refs/heads/<name>`, starts with an at
  sign or contains an at sign followed by a brace, or the private and
  the publish branch differ only in case;

- the working tree has uncommitted changes or untracked files (files
  ignored by `.gitignore` do not count and are never published: the
  snapshot is exactly the tree of the current commit, tracked files that
  are also ignored included);

- the current commit contains submodules (a gitlink or a tracked
  `.gitmodules`), unless `allow_submodules = TRUE`;

- `private_branch` exists but is not contained in the current commit, so
  that moving it to the current commit would drop commits from every
  branch. Run the function from `private_branch` (or a branch that
  contains it), not from the publish branch or an older branch;

- the publish branch, or `private_branch` when it is not the current
  branch, is checked out in another git worktree;

- `push = TRUE` and `remote` is not a configured remote, has more than
  one push URL, or its refs cannot be listed, or (standalone mode) holds
  this repository's history or has refs other than the publish branch
  (see the section on the remote);

- `mode = "snapshot"` and the history of the remote branch is not made
  only of clean commits, or the repository is shallow;

- an existing `pre-push` hook is not a shell script, or the hook
  directory lies inside the working tree and is not ignored.

Every git call a check depends on stops the run when it fails; none is
read as "nothing found". After the checks the hook and the configuration
are written and the guard is tested (see below); if that fails, branches
and remote are still untouched.

**The identity that is published.** The clean commit takes its author
and committer, and the time zone offset of its dates, from git's own
settings (`user.name`, `user.email`, the `GIT_AUTHOR_*` and
`GIT_COMMITTER_*` environment variables, dates included). They are
printed before anything changes; set them in the environment of the R
session to publish another identity.

**The remote.** `push = TRUE` looks at the remote before anything
changes.

- The real push URL is read with `git remote get-url --push --all`; a
  remote with more than one push URL is refused. The push goes to that
  URL (`git push <url> <commit>:refs/heads/<branch>`), not to the name
  of the remote, and every URL that is printed or reported has its
  credentials removed.

- Standalone mode lists *all* refs of the remote (`git ls-remote`; a
  failing listing is an error) and refuses, unless
  `allow_other_refs = TRUE`, when there is any ref besides the publish
  branch. A force-push replaces one branch only: tags, other branches,
  notes and GitHub's hidden `refs/pull/*` keep the old commits, and the
  last cannot be removed at all. **Publish into a new, empty
  repository.** It also refuses, unless
  `allow_overwrite_history = TRUE`, when the tip of a branch of the
  remote is a commit that this repository has (reachable from the commit
  being published, from the private or the publish branch or from a
  recorded private tip) and that is not a clean commit: the remote then
  holds this repository's history, as `origin` does in a clone of the
  private repository.

- The standalone push is a force-push with an explicit lease on what the
  listing showed: `--force-with-lease=refs/heads/<branch>:<id>` (empty
  when the branch does not exist), so it fails when the branch moved in
  the meantime and does not depend on a possibly stale remote-tracking
  ref.

- Snapshot mode builds on the tip of the remote branch, so that history
  is part of the snapshot. Every commit this function makes ends with
  the trailer line `Clean-Publish: v1`, and snapshot mode refuses a
  remote branch unless its whole history is linear, has one parentless
  root and consists of such commits (otherwise the message gives the
  number of commits that lack the trailer and the newest of them). A
  remote that holds the private repository, a rewritten archive of it or
  someone else's commits is refused; a shallow repository is refused
  too, because it cannot show the history.

- After a successful push the real push URL is recorded in the git
  config key `cleanpublish.<remote>.realpushurl` and
  `remote.<remote>.pushurl` is set to `DISABLED-use-clean_publish`,
  which no client can use (unless `disarm_push_url = FALSE`). A later
  `git push <remote>`, `git push --no-verify`, a different
  `core.hooksPath` and gert (which usethis uses) then fail instead of
  publishing private history; fetching is not affected, and later runs
  of this function read the recorded URL. To undo it:
  `git remote set-url --push <remote> <url>`. An explicit push to the
  real URL, `git send-pack` and other tools that do not read the
  remote's configuration still work.

- The success message states only what is guaranteed: the publish branch
  of the remote now points to the clean commit. It lists the other refs
  of the remote, which keep whatever they point to. If a step fails
  after the push, the message says that the remote is already updated
  and names the clean commit.

**Safety net.** Before a branch is moved, the old tips of the private
and the publish branch are kept as
`refs/backup/clean_publish/<UTC time>/<branch>`. To restore one, run
`git branch -f <branch> <that ref>` from another branch; git refuses it
for the branch that is checked out, and the private branch is the
checked-out one after every run, so there run
`git checkout <other branch>` first, or `git reset --hard <that ref>`.
The remote is updated first and the local branches are only moved
afterwards, so a failed push leaves the local branches untouched; if any
later step fails, the branch that was checked out is checked out again.
After the moves the function checks that both branches are where it put
them (on a case-insensitive file system a name that differs in case is
the same branch) and stops with the backup refs when they are not.

**The push guard.** The hook is installed where git reads it (the
`hooks/pre-push` of `git rev-parse --git-path`: shared by linked
worktrees, and honouring `core.hooksPath`). It goes into an existing
hook for `sh`, `dash`, `bash` or `ksh` as a clearly marked block right
after its first line, so the hook keeps working (it still receives its
input, and none when nothing is pushed), and that block is re-written,
not duplicated, on the next run. A hook in another language, `zsh`
included, stops the run with a message, and the hook an earlier version
of this function wrote is replaced; no other hook is replaced or skipped
silently. The private history is protected by commit id, whatever
becomes of the branch names. Before it pushes or moves anything, the
function records every commit that holds private history – the commit it
publishes and the earlier tips of the private and the publish branch –
as a full id in the multi-valued git config key
`cleanpublish.privatetip` (never removed), and the names of the private
branches in `cleanpublish.protectedbranch` (one per `private_branch`
ever used). The hook refuses to push

- any ref named like a protected branch;

- any ref, tag or other branch from which a commit can be reached that
  the live tip of a protected branch or a recorded private tip reaches,
  and that is not part of a clean commit made by this function (their
  ids are the values of `cleanpublish.cleancommit`). Pushing a tag on a
  private commit, an object id, the publish branch after merging the
  private branch into it, or `--mirror` is therefore refused too, also
  after the private branch was renamed, deleted or moved, and after a
  first publish whose push failed;

- anything that is not made of clean commits, when protected branches
  are configured and neither they nor a recorded tip can be found in the
  repository (the message says how to release the guard:
  `git config --unset-all cleanpublish.protectedbranch`);

- a tag, tree or blob that does not lead to a commit, unless a clean
  commit has it.

Both keys take full commit ids and nothing else: a revision such as a
branch name or an abbreviated id is ignored. See
[`guard_public_remote()`](https://jkylearmstrong.github.io/TempleCBE/reference/guard_public_remote.md)
for the fence on the public URL and for `TEMPLECBE_PUBLISHING=1`, which
this function sets for its own push and which lets through nothing but
the clean commits it recorded.

**The guard is confirmed before anything moves.** After installing the
hook the function pushes to a temporary local repository: the private
branch name, and the commit it publishes under another branch name and
as a lightweight tag. Each push must be refused by the hook. It stops
with an error, before any branch moves or anything is pushed, when a
push is accepted (the hook does not run, or knows only the branch name),
and also when a push is refused for any other reason, because then the
guard is unconfirmed. Use `confirm_guard = FALSE` to go on with a
warning when a push was refused for another reason and that cannot be
helped.

**What the guard does not stop.** It is a hook of command-line `git`, so
it does not stop `git push --no-verify`, a push made by a client that
does not run hooks (the gert package, which usethis uses, is one), a
push after `core.hooksPath` has been pointed elsewhere or the hook has
been deleted, `git send-pack`, or private commits that no recorded tip
and no protected branch reaches (history that was created after the last
run and is not an ancestor of a recorded tip). Disarming the push URL
(see above) stops the first two and the last client for pushes by the
name of the remote, not for a push to its real URL. The guard also needs
the objects: if `git gc` has removed the commit of a recorded tip, that
tip is ignored with a note. The safest procedure is to publish from a
throwaway clone and to keep no pushable public remote in the clone you
work in.

**The command line.** `Rscript scripts/clean_publish.R --help` lists the
options of the same function. Unlike `clean_publish()` it is a dry run
unless `--push` is given (`--snapshot` selects the snapshot mode,
`--remote NAME` names the remote and is required for both,
`--fence NAME|URL` calls
[`guard_public_remote()`](https://jkylearmstrong.github.io/TempleCBE/reference/guard_public_remote.md)
and takes no other option). An unknown or mistyped option is an error.
It runs with `options(warn = 1)`, and a warning raised before the push
stops the run: nothing is pushed and the exit status is 1 (2 for a bad
command line, 0 for success). The logic is the internal function
`clean_publish_cli()`; the script only loads the package and calls it.

## Examples

``` r
if (FALSE) { # \dontrun{
clean_publish(push = FALSE) # rewrite locally, send nothing
clean_publish(remote = "public")
clean_publish(remote = "med-cbe")
clean_publish(remote = "public", mode = "snapshot")
} # }
```
