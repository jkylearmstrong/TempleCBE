# Claude Code Instructions: TempleCBE

Please refer to [AGENTS.md](AGENTS.md) for full architecture and testing details.

## Quick Reference
- **Repository:** Upstream is `https://github.com/jkylearmstrong/TempleCBE.git` (`origin`). Do NOT push to `TempleCBE-private`.
- **Workflow:** Create feature branches off `master`, test locally, push to `origin`, and open PRs against `master` on `jkylearmstrong/TempleCBE`.
- **Load Dev Package:** `Rscript --vanilla -e "pkgload::load_all('.')"`
- **Run Tests:** `Rscript --vanilla -e "devtools::load_all('.'); testthat::test_file('tests/testthat/test-XYZ.R')"`

## Function Naming Convention (`cbe_`)
- `cbe_*` is reserved for Temple CBE presentation and formatting only (themes, palettes, scales, plots, report tables, number formatters).
- Every other function is a plain verb-noun that does what the name says (e.g. `nested_survival_run()`, `cox_single()`, `checkpoint_map()`, `describe_columns()`).
- All former non-formatting `cbe_*` names are deprecated wrappers using `lifecycle::deprecate_warn("0.5.8", ...)` for backward compatibility.

