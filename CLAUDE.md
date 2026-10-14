# Claude Code Instructions: TempleCBE

Please refer to [AGENTS.md](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/AGENTS.md) for full architecture and testing details.

## Quick Reference
- **Repository:** Upstream is `https://github.com/jkylearmstrong/TempleCBE.git` (`origin`). Do NOT push to `TempleCBE-private`.
- **Workflow:** Create feature branches off `master`, test locally, push to `origin`, and open PRs against `master` on `jkylearmstrong/TempleCBE`.
- **Load Dev Package:** `Rscript --vanilla -e "pkgload::load_all('.')"`
- **Run Tests:** `Rscript --vanilla -e "devtools::load_all('.'); testthat::test_file('tests/testthat/test-XYZ.R')"`
