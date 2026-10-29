# Claude Code Instructions: TempleCBE

Please refer to
[AGENTS.md](https://jkylearmstrong.github.io/TempleCBE/AGENTS.md) for
full architecture and testing details.

## Quick Reference

- **Repository:** Upstream is
  `https://github.com/jkylearmstrong/TempleCBE.git` (`origin`). Do NOT
  push to `TempleCBE-private`.
- **Workflow:** Create feature branches off `master`, test locally, push
  to `origin`, and open PRs against `master` on
  `jkylearmstrong/TempleCBE`.
- **Load Dev Package:** `Rscript --vanilla -e "pkgload::load_all('.')"`
- **Run Tests:**
  `Rscript --vanilla -e "devtools::load_all('.'); testthat::test_file('tests/testthat/test-XYZ.R')"`

## Function Naming Convention (`cbe_`)

- `cbe_*` is reserved for Temple CBE presentation and formatting only
  (themes, palettes, scales, plots, report tables, number formatters).
- Every other function is a plain verb-noun that does what the name says
  (e.g. [`nested_survival_run()`](https://jkylearmstrong.github.io/TempleCBE/reference/nested_survival_run.md),
  [`cox_single()`](https://jkylearmstrong.github.io/TempleCBE/reference/cox_single.md),
  [`checkpoint_map()`](https://jkylearmstrong.github.io/TempleCBE/reference/checkpoint_map.md),
  [`describe_columns()`](https://jkylearmstrong.github.io/TempleCBE/reference/get_dataset_info.md)).
- The former non-formatting `cbe_*` names were deprecated in 0.5.8 and
  removed in 0.7.0 (see NEWS.md and
  [`find_deprecated_calls()`](https://jkylearmstrong.github.io/TempleCBE/reference/find_deprecated_calls.md)).
