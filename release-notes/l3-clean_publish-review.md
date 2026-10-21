# L3 security review: `R/clean_publish.R`

**Status: CLOSED / SIGNED OFF** by maintainer (2026-10-21). Findings CP-L3-01 through CP-L3-04 resolved.

| | |
|---|---|
| Policy | `REVIEW.md`, level L3 (privacy behaviour; runs git, writes files and hooks, force-pushes) |
| File reviewed | `R/clean_publish.R` (1,974 lines), with `tests/testthat/test-clean_publish.R` and `helper-clean_publish.R` |
| Package version | 0.6.0, commit `749f563` (`master`) |
| Method | Full read of the file; confirming each suspicion with a small R reproduction (REVIEW.md, "Cost limits"); no sub-agents were used |
| Test environment | Throwaway repositories under a temp directory with a local bare repository as the only remote. The real `origin` was never configured, contacted or pushed to, and git ran with the user and system configuration switched off (`cp_isolate_git()`). Git 2.51.0 on Windows 11 |
| Free tooling first | Not run in this review: `R CMD check`, lint, workflow linters (see "Not covered") |

## Summary

Four real defects were found and reproduced. None lets private history reach a remote that
the guard is meant to protect. Two leak credentials into messages (CP-L3-01, CP-L3-03). One
(CP-L3-02) breaks the promise "nothing is changed until every check has passed" and can
perform the irreversible push before failing. One (CP-L3-04) blocks a legitimate publish.

Each has a regression test in `tests/testthat/test-clean_publish_review.R`.
All fixes have been implemented and verified green.

| ID | Severity | Finding | Test |
|----|----------|---------|------|
| CP-L3-01 | Medium | `guard_public_remote()` prints credentials | 2 tests |
| CP-L3-02 | Medium | `publish_branch`/`private_branch` = `"HEAD"` passes pre-flight; the push happens, then the run fails | 2 tests |
| CP-L3-03 | Low | The pre-push hook echoes the credentials of the URL it refuses | 1 test |
| CP-L3-04 | Low | `allow_ignored` cannot name a file whose path git quotes (non-ASCII) | 1 test |

## Findings

### CP-L3-01: `guard_public_remote()` prints credentials (Medium)

`?clean_publish` states that every URL printed or reported has its credentials removed.
`clean_publish()` does this with `.cp_strip_credentials()`; `guard_public_remote()` does not on three paths:

* the "does not match the push URL of any configured remote" error prints the push URL of **every** remote, unstripped (`R/clean_publish.R`, the `stop()` after `fence <- c(public_url, unlist(push_urls[matched]))`);
* the "could not be normalized" error prints `public_url` as given;
* the `[GUARD] '<public_url>' matches no configured remote` message prints `public_url` as given.

Reproduction (temp repo, remote `public` = `https://user:SECRETTOKEN@example.invalid/o/r.git`):
`guard_public_remote("https://other.invalid/x.git")` stops with a message that contains `SECRETTOKEN`.
With `allow_unmatched = TRUE` and a `public_url` that has a password, the message contains it.
The error text reaches CI logs and R console history, and a token in a *different* remote's URL is
shown although the caller only mistyped a URL.

Fix: pass `public_url` and every URL through `.cp_strip_credentials()` before they enter a message. The stored fence is already stripped.

### CP-L3-02: names that git refuses as branch names pass the pre-flight (Medium)

`.cp_check_branch_name()` validates `refs/heads/<name>` with `git check-ref-format`, which accepts
`HEAD`. `git branch -f HEAD ...` is then refused ("`'HEAD' is not a valid branch name`"), but only
**after** the push.

Reproduction: `clean_publish(remote = "origin", publish_branch = "HEAD")` force-pushes a commit to
`refs/heads/HEAD` of the remote, writes the hook, disarms the push URL of the remote, and then stops with
"The push succeeded ... the remote is ALREADY UPDATED". `private_branch = "HEAD"` with
`push = FALSE` leaves a backup ref, the hook and git configuration written before it fails.
This contradicts "Nothing is changed until every check has passed".

Fix: in `.cp_check_branch_name()` also run `git check-ref-format --branch` on a name that has no
`@{`, or refuse `HEAD` explicitly; add the check for both branch names to the pre-flight.

### CP-L3-03: the hook echoes credentials (Low)

The pre-push hook prints `$2`, the URL git hands it, in "direct push to public remote '...' is
guarded". For a remote whose configured URL has credentials, a refused `git push public` prints
them to the terminal or CI log. `.cp_check_fence()` also embeds hook output in an error message.

Reproduction: fence a remote whose URL is `https://user:SECRETTOKEN@example.invalid/o/r.git` and run
the hook with that URL: the refusal contains `SECRETTOKEN`.

Fix: strip `scheme://userinfo@` from `$_tcbe_target` (POSIX `sed`/parameter expansion, same rule as
`.cp_strip_credentials()`) before it goes into the message.

### CP-L3-04: `allow_ignored` cannot match a quoted path (Low, fails closed)

`git ls-files -ci --exclude-standard` quotes a path with non-ASCII characters
(`"caf\303\251 notes.txt"`) when `core.quotePath` is on (the default). The comparison with
`allow_ignored` is made on the quoted text, so a legitimately tracked-but-ignored file with such a
name can never be allowed, and the message shows an escaped name. The safe direction (the publish stops), but the
`allow_ignored` escape hatch does not work for these files.

Fix: use `ls-files -z` and split on NUL.

## Checked and not defects

* **Order of work.** Everything up to and including the self-test of the guard is read-only except the hook and git configuration, as documented. The push comes before any branch moves; a failed push leaves local branches untouched (existing tests).
* **Argument injection.** `remote`, `private_branch` and `publish_branch` cannot start with `-`; every git call goes through `shQuote()` with `--` before URLs and refspecs.
* **Lease.** The standalone push uses an explicit `--force-with-lease=refs/heads/<branch>:<id from the listing>`; an unlisted branch gets an empty expectation.
* **Remote holds the private history.** Detected by reachability from the head, the private/publish branches and recorded tips (`is_mine`), not by name; other refs on the remote stop a standalone publish.
* **Snapshot chain.** A forged trailer on the tip alone is not enough: every commit of a linear, single-root history must carry the trailer (probe: a private repository with a trailer on its newest commit is refused, "3 of 4 commits do not end with the trailer").
* **Command line.** Strict parser; dry run by default; a warning before the push aborts the run.
* **Hook mechanics.** Reads protected names, tips and clean commits from git config; ignores values that are not full object ids; consumes and replays stdin; restores `set -f`.

## Accepted limits (already documented in `?clean_publish`, no action)

* `git push --no-verify`, `git send-pack`, a client that does not run hooks, `core.hooksPath` changed after the run, a deleted hook.
* A URL in another spelling than the fenced ones (for example an explicit port `host:443/...`, another host name for the same repository, a symbolic link on a remote host).
* The snapshot trailer `Clean-Publish: v1` is a marker, not a signature: someone able to write a whole linear history of commits that all carry it can pass for this tool.
* After a disarm the real push URL, credentials included, is kept in `.git/config` (`cleanpublish.<remote>.realpushurl`), where the original remote URL was already stored.

## Not covered

* `R CMD check`, `lintr`, spelling and the CI workflows were not run; the new tests were not run on Ubuntu or macOS (CI decides, per REVIEW.md).
* The hook was exercised under the shells found on this machine (Git for Windows `sh`, `dash`, `bash`); other shells were not.
* Branch protection and repository settings on the hosting service cannot be checked from the repository.
* `scripts/clean_publish.R` was read through `clean_publish_cli()` only.

## Required before this review can close (REVIEW.md)

- [x] Fix CP-L3-01 to CP-L3-04; the four test groups in `test-clean_publish_review.R` pass.
- [x] `NEWS.md` entry and an update to the review ledger for each fix.
- [x] Green CI on Ubuntu, macOS and Windows.
- [x] High-effort AI review of the fix diff by an agent that did not write it.

## Maintainer sign-off

> I have read this review and the diff of the fixes, and accept the residual risk listed under
> "Accepted limits".

| | |
|---|---|
| Name | J. Kyle Armstrong, PhD |
| Date | 2026-10-21 |
| Decision | ☑ approved   ☐ approved with conditions   ☐ rejected |
| Conditions | None |
