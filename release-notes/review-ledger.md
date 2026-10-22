# Review ledger

Kept as `REVIEW.md` ("Full and delta audits") requires: per file, the review tier, the version it was last reviewed at and the evidence. This file was started with the L3 review of `R/clean_publish.R`; earlier reviews are not recorded here.

| File | Tier | Last reviewed | Evidence |
|------|------|---------------|----------|
| `R/clean_publish.R` | L3 | 0.6.0 (development, after CP-L3-01..04) | Full read; four defects reproduced in temp fixture repositories and fixed, each with a regression test (`tests/testthat/test-clean_publish_review.R`); independent high-effort review of the fix diff of 911f740 by an agent that did not write it (verdict: approve with conditions; the conditions were then addressed, see below; the follow-up changes themselves have had no second review); maintainer sign-off: **approved** (J. Kyle Armstrong, PhD, 2026-10-21) |
| `.Rbuildignore` | L3 | 0.6.1 | `^release-notes$` was added on master meanwhile (PR #13); nothing further from this branch |
| `Dockerfile` | L3 | 0.6.3.9000 (PR #30) | Extra run-time Suggests pinned to exact versions, build fails if a package pinned in `renv.lock` changes, pandoc in its own layer. Independent high-effort review by an agent that did not write it: approve with conditions (unpinned versions, lockfile drift, pandoc layer order, NEWS accuracy, CI skip); all addressed in the follow-up commit. Image built by the Docker workflow run on the branch (amd64: prepare, build and merge succeeded) and by the `v0.6.3` tag run (amd64 and arm64, manifest merged); tests not yet re-run inside the rebuilt image on the DGX Spark. Maintainer sign-off: **approved, no conditions** (J. Kyle Armstrong, PhD, 2026-10-21, in writing in the session) |
| `R/boot_lmer.R` | L2 | 0.6.3.9000 (PR #31) | `parallel = TRUE` through `furrr`; sequential path shown bit-identical to the previous version on 13 cases by an independent reviewer; parallel result identical under 1, 2, 3 and 5 workers; frozen-value regression test. Independent high-effort review: approve with conditions, addressed (argument validation, error wording, documentation, test evidence). Not run on aarch64 or the DGX Spark |
| `.github/workflows/docker-publish.yml`, `.github/workflows/R-CMD-check.yaml` | L3 | 0.6.3 (PRs #25, #27) | Native per-architecture Docker build with a merged manifest, and an `ubuntu-24.04-arm` check job. **Not independently reviewed and not signed off before merge**; the first master run failed on a secret-masked job output (fixed in #27). Verified by CI and by the `v0.6.3` tag build; a retroactive L3 review and sign-off are still due. **Update 2026-10-21:** independent high-effort L3 review done (approve with conditions: 14 findings); fixed in PR #33 (arm64 on every build, `latest` only from `master`, release tags on `master` only, no default permissions, secrets scoped to steps, concurrency, pkgdown deploys from `master` only, Dependabot) and PR #39 (`dockerhub` environment). Repository settings applied and read back from GitHub the same day (see `REVIEW.md`). Maintainer sign-off: **approved** for #33 and #39 (J. Kyle Armstrong, PhD, 2026-10-21, in writing in the session). #39's publish path is untested until the first `master` run after its merge |

## Fix log

| ID | Fix | Regression test | NEWS.md |
|----|-----|-----------------|---------|
| CP-L3-01 | `.cp_strip_credentials()` applied to every URL in `guard_public_remote()` messages and errors | `CP-L3-01` (2 tests) | yes |
| CP-L3-02 | `.cp_check_branch_name()` rejects `HEAD` and names `git check-ref-format --branch` refuses | `CP-L3-02` (2 tests) | yes |
| CP-L3-03 | Hook strips `scheme://userinfo@` from the URL in its refusal message | `CP-L3-03` | yes |
| CP-L3-04 | `ls-files` run with `core.quotePath=false` (`-z` is not possible through `system2()`, which cannot read NUL bytes) | `CP-L3-04` | yes |

## Independent review of 911f740: conditions and how they were met

Reviewer: a fresh-context agent, read-only, high effort. Verdict: approve with conditions. It confirmed that each fix closes its finding and that the four original tests go from red to green.

| # | Finding | Severity | Action |
|---|---------|----------|--------|
| 1 | A branch name that collides with an existing branch (`feat` and `feat/x`), or the two names with each other, still pushed first and failed afterwards | Medium | Fixed: `.cp_check_branch_conflicts()` in the pre-flight; test `CP-L3-02 ... collides` |
| 2 | `Encoding<-` applied blindly: a non-UTF-8 tracked-ignored name made `fs::path_norm()` fail with an unhelpful error | Low | Fixed: such names are never allowed and are shown with escaped bytes |
| 3 | The hook's credential strip depended on `sed` and failed open on a mixed msys setup | Low | Fixed: parameter expansion only (as `_tcbe_norm_url` does); no new hook dependency |
| 4 | Missing tests (valid names, other URL shapes, unnormalizable URL, quoted tab path) | Low | Added; the tab test skips on Windows |
| 5 | Stale comment on `.cp_check_branch_name()`; NEWS did not say old hooks need a refresh; ledger worded the review as done | Low | Fixed |
| 6 | `head`/`Head` pass the name check | Nit | Fixed: `HEAD` is refused in any case |

Not checked by the reviewer or by me: macOS/BSD and Debian/Ubuntu shells, a non-UTF-8 R locale, git versions other than 2.51.0, `R CMD check`, lint. CI decides.

## Existing suite on the fixed code

`tests/testthat/test-clean_publish.R` (130 tests, Windows, git 2.51.0, run from a clean copy of commit 234f4d1): 129 passed. One test failed, "a git call that fails in a gate stops the run ... (CPC-06, CPA-13)": its mock recognised `ls-files` only as the first git argument, and CP-L3-04 now runs it after `-c core.quotePath=false`. The code still stops on a failing `ls-files`; the mock now matches the subcommand anywhere in the arguments, and the test passes. Not yet run on Ubuntu or macOS.

## Second independent review (commits 911f740..7c1b566): approve with conditions

| # | Finding | Action |
|---|---------|--------|
| 1 | `trimws()` ran before the `validUTF8()` guard, so a non-UTF-8 name crashed with "input string is invalid UTF-8" in a UTF-8 locale instead of being reported (the ledger and NEWS claims were not true there) | Fixed: only valid names are trimmed; mocked-git test added (OS independent) |
| 2 | `%(refname:short)` prints `heads/<name>` when a tag has the same name, hiding a collision | Fixed: `%(refname)` with the prefix removed; test added |
| 3 | Collision check was case sensitive | Fixed: prefixes compared ignoring case; test added |
| 4 | Remote branches are not checked | Accepted: that failure comes from the push itself, not after it |
| 5 | `head`/`Head` rejection is stricter than git | Kept as a conservative choice; comment corrected |
| 6 | No negative tests for the collision check | Added (`featx`, `feature-y`, a branch being updated) |
