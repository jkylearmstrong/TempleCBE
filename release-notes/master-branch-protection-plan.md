# Protecting `master`, tags, secrets and the workflow token

Status: **plan; nothing below has been applied.** These are repository settings on GitHub, which only the repository owner can change; no file in the repository changes them. Written 2026-10-21 after the independent L3 review of the CI workflows (finding 2: `master` unprotected, no rulesets, default workflow token set to write, Docker Hub secrets at repository level). `REVIEW.md` currently describes a protected `master`; after the steps below are done, check that it matches (see "After you have applied it").

Repository: `jkylearmstrong/TempleCBE` (public, one maintainer). Everything below is in **Settings** on github.com.

## What is at risk today

* Anyone with write access can push to `master`, force-push it, or delete it.
* Any workflow can write to the repository: the default `GITHUB_TOKEN` permission is write, and "allow GitHub Actions to create and approve pull requests" is on.
* `DOCKERHUB_TOKEN` is a repository secret. A user with write access can push a branch that changes a workflow, run it with `workflow_dispatch` and read the token, or push a `v*` tag to publish an arbitrary image.
* Tags are unprotected, so a `v*` tag can be created, moved or deleted by anyone with write access.

With a single maintainer the practical threat is a compromised account, token or action, or a mistake, rather than a hostile colleague. The measures are cheap and mostly one-time.

## Order of work (about 30 minutes)

1. Merge the workflow hardening PR (it makes the workflows work with the settings below).
2. Actions permissions (section 3), then the `master` ruleset (section 1), then the tag ruleset (section 2).
3. Move the Docker Hub secrets (section 4) and merge the one-line workflow change that uses the environment.
4. Verify (section 5) and update `REVIEW.md`.

Do 1 before 2: a required-check rule applies to the pull request that adds it.

## 1. Ruleset for `master`

**Settings → Rules → Rulesets → New ruleset → New branch ruleset.**

* Name: `master`. Enforcement status: **Active**.
* Target branches: **Add target → Include default branch**.
* Bypass list: leave **empty** for normal work. If you want an emergency route, add **Repository admin** with bypass mode **For pull requests only**, so even an admin cannot push directly; they can merge a PR whose checks are failing. Do not add "Deploy keys" or GitHub Actions apps.
* Rules to tick:
  * **Restrict deletions** (nobody deletes `master`).
  * **Block force pushes**.
  * **Require a pull request before merging**:
    * Required approvals: **0**. With one maintainer, a required approval can never be given (GitHub does not let an author approve their own PR, and PRs opened through your account count as yours). Set it to 1 when a second maintainer exists.
    * Dismiss stale pull request approvals when new commits are pushed: on.
    * Require conversation resolution before merging: on.
    * Allowed merge methods: **Merge** (the repository's history uses merge commits; leave squash and rebase off so the history stays uniform).
  * **Require status checks to pass**, with these checks (the names GitHub shows today; add each from the search box after it has run once on a PR):
    * `ubuntu-latest (release)`
    * `ubuntu-24.04-arm (release)`
    * `macOS-latest (release)`
    * `windows-latest (release)` (this takes 45 to 50 minutes; a PR cannot be merged before it finishes)
    * `optional-tests-ubuntu-latest`, `optional-tests-ubuntu-24.04-arm`, `optional-tests-windows-latest`
    * `test-coverage`
  * Leave **Require branches to be up to date before merging** off at first: it forces a re-run of all checks (about an hour) after every other merge. Turn it on if you start merging several PRs a day.
  * Leave **Require linear history** off (it conflicts with merge commits). Leave **Require signed commits** off unless you sign commits.
  * **Require review from Code Owners**: off until there is a second maintainer (see "Later").
* Save. Rulesets are evaluated on every push, so a direct `git push origin master` is now refused with a message naming the ruleset.

What this changes in the workflow you use now: nothing. Branches are pushed, PRs are opened, checks run, you click Merge. Auto-fix pushes to PR branches, which are not covered. The `master` push check (`ubuntu-latest (release)` only) still runs after merge, as before.

## 2. Ruleset for release tags

**New ruleset → New tag ruleset.**

* Name: `release tags`. Enforcement: **Active**. Target tags: **Include by pattern** `v*`.
* Bypass list: **Repository admin**, bypass mode **Always** (you create the tag yourself).
* Rules: **Restrict creations**, **Restrict updates**, **Restrict deletions**, **Block force pushes**.

Effect: only a repository admin can create a `v*` tag, and no one can move or delete one. Creating a release in the GitHub UI or `git push origin v0.6.4` still works for you. The Docker workflow already refuses a tag whose commit is not on `master`.

## 3. Actions permissions

**Settings → Actions → General.**

* **Workflow permissions → Read repository contents and packages permissions.** (Workflows that need more, such as `pkgdown` and the Docker jobs, ask for it in their own `permissions:` block, which the hardening PR does.)
* **Untick "Allow GitHub Actions to create and approve pull requests".**
* **Fork pull request workflows → Require approval for first-time contributors** (already on; keep it).
* **Actions permissions**: "Allow OWNER, and select non-OWNER, actions and reusable workflows", allowing GitHub-created actions and the verified creators you use (`docker/*`, `r-lib/*`, `quarto-dev/*`, `codecov/*`, `peaceiris/*`). This is optional; the pinned-SHA requirement below is the stronger control.
* **Require actions to be pinned to a full-length commit SHA**: do **not** turn this on yet. The workflows pin by major-version tag, and it would fail them all. The hardening PR adds Dependabot for GitHub Actions; once you have SHA-pinned the actions, turn it on.

## 4. Docker Hub credentials

1. On Docker Hub, create an access token with **Read & Write** (not Delete, not Admin), limited to this one repository if your Docker Hub plan offers repository-scoped tokens, and revoke the token that is in use now once the new one works.
2. **Settings → Environments → New environment** named `dockerhub`.
   * **Deployment branches and tags → Selected branches and tags**: add branch `master` and tag pattern `v*`.
   * Required reviewers: none (otherwise every merge to `master` waits for you to approve a deployment). Add yourself if you would rather approve each publish.
   * **Environment secrets**: add `DOCKERHUB_TOKEN` (the new token).
3. **Settings → Secrets and variables → Actions → Variables**: add `DOCKERHUB_USERNAME` as a repository **variable** (the workflows already read it before looking for a secret of that name). A user name is not secret; keeping it a secret made GitHub mask the repository owner's name in logs, which is what broke the Docker workflow in PR 27.
4. Delete the old repository-level `DOCKERHUB_TOKEN` and `DOCKERHUB_USERNAME` secrets.
5. Add `environment: dockerhub` to the `build` and `merge` jobs of `docker-publish.yml` (a one-line change in each; ask for it after step 2 is done, because a job that names an environment before it exists fails). From then on only runs on `master` or a `v*` tag can read the token: a branch or a modified workflow on another branch cannot.

## 5. Verify

* `gh api repos/jkylearmstrong/TempleCBE/rulesets` lists both rulesets, enforcement `active`.
* `gh api repos/jkylearmstrong/TempleCBE/actions/permissions/workflow` shows `default_workflow_permissions: read` and `can_approve_pull_request_reviews: false`.
* Open a throwaway PR: the eight checks above appear as **Required**, and the merge button stays disabled until they pass.
* Try `git push origin HEAD:master` from a throwaway branch tip: it must be refused.
* A manual run of the Docker workflow from a branch must build without pushing (see the hardening PR), and a run from `master` must publish `latest` and `master` for both architectures: `docker buildx imagetools inspect <image>:latest` lists `linux/amd64` and `linux/arm64`.

## After you have applied it

* Update `REVIEW.md` (the passages that describe `master` protection and the token's bypass status) so that it states what is actually set. Ask for this change once the settings are in place, so the file never describes settings that are not there.
* Add a line to `release-notes/review-ledger.md` for the workflow files and the settings: date, what was applied, who signed off.

## Rollback

Each ruleset can be set to **Disabled** from its page without deleting it, and the Actions permission changes are single toggles. If a required check name is wrong, GitHub shows the PR as "waiting for status to be reported": fix the name in the ruleset rather than disabling it.

## Later (second maintainer)

Required approvals 1, **Require review from Code Owners**, and a `CODEOWNERS` file naming the owner for the L3 paths in `REVIEW.md` (`.github/workflows/`, `Dockerfile`, `DESCRIPTION`, `.Rbuildignore`, `renv.lock`, `R/clean_publish.R`). That makes the "independent review plus written sign-off" rule enforceable by GitHub; with one maintainer it would block every PR.
