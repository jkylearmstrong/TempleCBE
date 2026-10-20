# Review ledger

Kept as `REVIEW.md` ("Full and delta audits") requires: per file, the review tier, the version it was last reviewed at and the evidence. This file was started with the L3 review of `R/clean_publish.R`; earlier reviews are not recorded here.

| File | Tier | Last reviewed | Evidence |
|------|------|---------------|----------|
| `R/clean_publish.R` | L3 | 0.6.0 (development, after CP-L3-01..04) | Full read; four defects reproduced in temp fixture repositories and fixed, each with a regression test (`tests/testthat/test-clean_publish_review.R`); independent high-effort review of the fix diff (see `l3-clean_publish-review.md`); maintainer sign-off: **pending** |
| `.Rbuildignore` | L3 | 0.6.0 (development) | One line added (`^release-notes$`); no other change |

## Fix log

| ID | Fix | Regression test | NEWS.md |
|----|-----|-----------------|---------|
| CP-L3-01 | `.cp_strip_credentials()` applied to every URL in `guard_public_remote()` messages and errors | `CP-L3-01` (2 tests) | yes |
| CP-L3-02 | `.cp_check_branch_name()` rejects `HEAD` and names `git check-ref-format --branch` refuses | `CP-L3-02` (2 tests) | yes |
| CP-L3-03 | Hook strips `scheme://userinfo@` from the URL in its refusal message | `CP-L3-03` | yes |
| CP-L3-04 | `ls-files` run with `core.quotePath=false` (`-z` is not possible through `system2()`, which cannot read NUL bytes) | `CP-L3-04` | yes |
