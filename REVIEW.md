# Review policy

How changes to TempleCBE are reviewed, and how much review each one
gets. The goal is proportionate review: routine changes stay cheap, and
changes that can produce wrong statistics or expose identifiers get real
scrutiny.

This file is a working draft (adopted 2026-09-29). Update it when the
process changes.

## Review levels

The level is set by the files touched and the kind of change. The
highest level that applies wins.

| Level | Applies to | Required | AI-review subagents |
|----|----|----|----|
| **L0** | Docs, `NEWS.md`, generated `man/`, vignette prose, tests-only changes, comment edits | CI passes (R CMD check, tests, pkgdown check, lint, spelling) | 0 |
| **L1** | Small bug fix, new non-statistical helper, plot or styling change, template edits | Author checklist in the PR; one low-effort AI review of the diff | 0-1 |
| **L2** | Statistical computation, public API (exports, arguments, defaults, aliases), file writers, de-identification helpers, anything that runs a subprocess, shell, git or delete | Regression test that fails before the fix and passes after; the verification ladder below; a high-effort AI review of the diff | 2-4 |
| **L3** | Changes to privacy behavior, release/CI/Docker files, secrets, license, dependencies or R version in `DESCRIPTION`, `.Rbuildignore`; every minor release | Security review, high-effort AI review, and written maintainer sign-off | 4-10 |

Sizing: use one reviewer per touched area of the code, within the
level’s range. State the number before the review starts. Never use more
than 10 agents in one review, never more than 3 at once, and never nest
agents or add per-finding verifier agents.

## Verification ladder

Statistical changes need independent evidence, recorded in the PR. In
order:

1.  A reference implementation or analytic check written as a test (for
    example
    [`survival::coxph`](https://rdrr.io/pkg/survival/man/coxph.html),
    `exact2x2`, or a hand-computed small case).
2.  SAS parity, where a benchmark exists: re-run the affected benchmark,
    or say it was not re-run.
3.  A review by an AI agent that did not write the change, in fresh
    context.
4.  Maintainer sign-off in the PR description.

A statistical change with nothing on rungs 1 or 2 is treated as
unverified and is reviewed at L3.

## Pull requests

- Keep L2 and L3 pull requests to roughly 400 changed lines of R. Split
  larger work.
- Every fix records a regression test, a `NEWS.md` entry, and an update
  to the review ledger.
- Any new code path that writes files, deletes, shells out, or uses the
  network is L2 at minimum.
- Do not commit real study names, person names, subject identifiers,
  keys, or local paths. Examples use synthetic data.

## Branches, merging and releases

- **Branching model:**
  - `master`: The protected production branch. Direct pushes and
    force-pushes are strictly forbidden. All modifications must arrive
    via pull request with passing CI (Ubuntu, macOS, Windows) and
    appropriate review sign-off. The scripted exceptions are listed
    under “Pushes that do not go through a pull request” below.
  - `dev`: The ongoing integration branch. Feature branches and PRs
    integrate into `dev` prior to release consolidation.
  - Short-lived feature and fix branches are branched from `dev` (or
    `master` for hotfixes) and named for their review level and topic
    (e.g. `l2/imputation-pca-followups`). GitHub automatically deletes
    branches upon PR merge.
- One pull request per change, merged with a merge commit (not a
  squash), so the branch’s own commits stay in the history, one per fix
  or finding, and each can be read or reverted on its own; the merge
  commit still reverts the whole change at once. Nothing is pushed to
  `master` by hand; the scripted exceptions are listed below.
- Merge only when every CI job on the latest commit is green, whoever
  wrote the change has finished, the final diff has been reviewed at the
  change’s level, and the maintainer approves.
- “It passes on my machine” is not enough: tests must not depend on
  installed tools, paths, time zones or the operating system. The
  Ubuntu, macOS and Windows checks decide.
- A merge that combines more than about 400 changed lines is reviewed at
  L3, and the list of exported functions is compared before and after; a
  removed export needs a `NEWS.md` entry.
- Releases are prepared in a pull request (version and `NEWS.md`). Some
  scripted paths push without one (next section): a release workflow,
  the site deployment, a release script and the publication to the
  public repository.
- Version in `DESCRIPTION`: Semantic versioning `MAJOR.MINOR.PATCH`
  (e.g., `0.5.0`), with development builds using timestamped
  sub-versions if necessary. Set it to the release version in a pull
  request before the first publication.
- Git tags: see “Tags” below; the tags of the private repository and of
  the public repository follow different schemes.
- Branch protection rules on `master` enforce PR reviews, status checks
  passing, and prevent force pushes. They must also be set up so that
  the exceptions in the next section cannot do more than they are meant
  to (see there); that is a setting of the hosting service and cannot be
  checked from the repository, so check it in the repository settings.

## Pushes that do not go through a pull request

These push to a branch or tag with no review step. Each one has to be
started by the maintainer, and none runs on its own except the site
deployment.

| What | When | Pushes |
|----|----|----|
| `.github/workflows/release.yaml` (`permissions: contents: write`) | Manual run; only with the input `commit_and_push` switched on, which is **off by default** | Commits `DESCRIPTION`, `README`, `man/`, `NAMESPACE` and `NEWS.md`, then pushes `master` and a tag with the workflow’s token |
| `.github/workflows/pkgdown.yaml` (`peaceiris/actions-gh-pages`) | Every push to `master`, a published release, or a manual run | The built site to the `gh-pages` branch |
| `scripts/deploy_release.R` | Only with `--push`, which is off by default; `--remote NAME` names the remote (default `origin`) | `HEAD` to the branch given by `--branch` (default `master`) and the tag, in one atomic push; if it fails, the local release commit and tag are rolled back |
| [`clean_publish()`](https://jkylearmstrong.github.io/TempleCBE/reference/clean_publish.md) / `scripts/clean_publish.R --push` | When called in R (`push = TRUE` is its default) or with `--push` on the command line, and only with a remote that is named | The `master` of the **public** repository: a force-push into the empty repository the first time, fast-forward pushes afterwards (see “Publishing to the public repository”) |

Branch protection has to cover them. Whether it does cannot be seen from
the repository, so the maintainer checks, in the settings of each
repository:

- `master` of the private repository: pull requests required,
  force-pushes and deletion refused, and the workflow’s token and
  administrators not on the bypass list (otherwise `release.yaml` and
  `deploy_release.R --push` can write to `master`). Protection of a
  branch does not cover **tags**: add a tag rule for the release tags.
- `gh-pages`: it is written by the site deployment only; nobody should
  push to it by hand.
- `master` of the public repository: see the runbook below. It is the
  one branch that is pushed to directly on purpose.

## Tags

- **Private repository, development builds:** the full version that
  `release.yaml` writes into `DESCRIPTION`,
  `MAJOR.MINOR.PATCH.YYYY.MM.DD.HHMM` (for example
  `0.4.0004.2026.10.05.1058`), is the tag, without a `v`.
  `scripts/deploy_release.R` makes the same version and puts a `v` in
  front of it (`v0.4.0004.2026.10.05.1058`). Both are builds of the
  private repository and are not releases. The tags that exist were made
  by hand: the recent ones have the version without a `v` (hours and
  minutes separated by a dot, as `0.4.0003.2026.09.30.01.11`), the old
  ones are releases of the form `v0.3.3141`; they are not renamed.
- **Public repository, releases:** `v<MAJOR.MINOR.PATCH>` (for example
  `v0.5.0`), made by hand after the first publication, on the public
  repository:
  `gh release create v0.5.0 --repo <owner>/<public repository> --target master`
  (add `--title` and `--notes-file`; not run here, there is no network).
  Neither `release.yaml` nor `deploy_release.R` makes these, and nothing
  is tagged by
  [`clean_publish()`](https://jkylearmstrong.github.io/TempleCBE/reference/clean_publish.md).

## Publishing to the public repository

The public repository gets one clean commit, never the private history:
[`clean_publish()`](https://jkylearmstrong.github.io/TempleCBE/reference/clean_publish.md)
(see
[`?clean_publish`](https://jkylearmstrong.github.io/TempleCBE/reference/clean_publish.md)).
The steps below were run literally against local bare repositories.
Replace the URLs.

1.  **Create the public repository, new and empty** (no README, no
    licence file, no `.gitignore`). A force-push replaces one branch
    only: the other branches, tags, notes and, on GitHub, the hidden
    `refs/pull/*` of a repository that ever held the private history
    stay readable, and no push removes them. A repository that held it
    once is not a valid target; delete it and create a new one.

2.  **Make a throwaway clone of the private repository**, on the branch
    that is to be published (normally `master`), and give it one remote
    only, named `public`:

        git clone <private repository URL> clean-publish-clone
        cd clean-publish-clone
        git remote remove origin
        git remote add public <URL of the new empty repository>

    Remove `origin` instead of renaming it: a renamed remote keeps
    `refs/remotes/public/*` that point at private commits. Run R from
    this directory (the repository that is acted on is the git toplevel
    of the working directory, and it is printed first).

3.  **Dry run, then compare.**
    `clean_publish(remote = "public", push = FALSE)` rewrites the local
    branches only. `master` is now the one clean commit and
    `private-history` holds the full history. Check what would be
    public:

        git diff --stat private-history master     # prints nothing: the trees are identical
        git ls-tree -r --name-only master          # every file that will be public

4.  **Fence the public remote:** `guard_public_remote("public")`. From
    now on `git push public ...` is refused by the pre-push hook;
    [`clean_publish()`](https://jkylearmstrong.github.io/TempleCBE/reference/clean_publish.md)
    is the one thing that can push there.

5.  **Publish:** `clean_publish(remote = "public")` (or
    `Rscript scripts/clean_publish.R --push --remote public`). It
    force-pushes the one commit, then records the real URL in
    `cleanpublish.public.realpushurl` and replaces
    `remote.public.pushurl` by `DISABLED-use-clean_publish`, so that
    `git push public`, `git push --no-verify` and `gert` cannot push by
    the name of the remote any more.

6.  **Verify the guard.** A push of the private branch to an empty bare
    repository must be refused by the hook, and it sends nothing
    (`--dry-run` still runs the hook):

        git init --bare ../empty.git
        git push --dry-run ../empty.git private-history

    The output must contain
    `clean_publish guard: push refused: ... involves the private branch 'private-history'`
    and the exit status must not be 0.
    `git push --dry-run ../empty.git master` is accepted: it is the
    clean commit.

7.  **Protect the public `master` now, not before the first
    publication:** no force-pushes, no deletion, and pushes restricted
    to the maintainer, who is the only account that can fast-forward it
    for the next snapshot. Do not require pull requests there: a
    snapshot is pushed directly. (Not checked here: it is a setting of
    the hosting service.)

8.  **Later releases, `mode = "snapshot"`.** Merge the change into the
    private repository by pull request as always, then make a new
    throwaway clone as in step 2 and run
    `clean_publish(remote = "public", mode = "snapshot", push = FALSE)`
    first. It builds one commit on the tip of the public `master`,
    prints `git diff --stat` against that tip and the number of paths
    the snapshot deletes (the public tree is replaced as a whole). When
    that is what you want, run `guard_public_remote("public")` and
    `clean_publish(remote = "public", mode = "snapshot")`. It pushes a
    fast-forward and refuses a public branch that is not made only of
    commits this tool made.

9.  **If the push stops with `(stale info)`:** the public branch moved
    between the moment the tool listed the remote and the push (the push
    carries the lease
    `--force-with-lease=refs/heads/<branch>:<id from the listing>`).
    Nothing was changed on the remote. Find out who pushed, and run
    again.

What the tool cannot stop: `git push --no-verify` to the real URL,
`git send-pack`, a push after the hook has been deleted or
`core.hooksPath` pointed elsewhere, and any client that does not read
the remote’s configuration pushing to the real URL by hand. It stops the
accidents, not someone who is working around it, and it protects only
what it recorded: commits made in the clone after the last run and not
reachable from a recorded private tip are not known to it. Publish from
the throwaway clone and delete the clone afterwards.

**Never merge an outside pull request on the public repository.** The
next snapshot replaces the public tree with the private one and would
silently revert the change, and snapshot mode refuses a public branch
that holds a commit this tool did not make, so it would stop altogether.
Close such a pull request with a pointer, apply an accepted change to
the private repository by a pull request there, and let the next
snapshot carry it.

## Full and delta audits

A full audit is due at a minor version bump, a public release, a major
change in a critical dependency (`survival`, `glmnet`, `recipes`,
`parsnip`/`censored`, `gtsummary`, `survex`, `hardhat`), or after ten
merged L2/L3 pull requests since the last one.

Between full audits, review only files whose content changed since the
ledger’s last-reviewed version. The ledger records, per file: review
tier, last-reviewed version, and what evidence was used. Cost then
follows the amount of change rather than the size of the repository.

## Cost limits for AI-assisted review

- Run free tooling first (R CMD check, coverage, lint, workflow and
  Dockerfile linters, scans).
- Keep each reviewer’s input to roughly 3,000 lines of source plus its
  tests.
- Confirm a finding by running a small R reproduction, not by asking
  more agents.
- For any review planned with more than 4 agents, run one area first,
  check the actual cost, then decide on the rest.
- Write review results to disk or the PR so a lost session does not lose
  work.
