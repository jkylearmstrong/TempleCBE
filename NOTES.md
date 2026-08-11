# Notes

Internal working notes on where this package came from and where it's headed. For the public-facing overview, see `README.qmd`. For the full migration plan and reviewer findings this was built from, see `ARCHITECTURE_AND_MIGRATION_PLAN.md` at the GitHub root (one level up).

## Where it came from

`TempleCBE` is the public half of a split that used to be tangled together in two private repos: **`datasci`** (Temple CBE's general-purpose analysis tooling, plus protected patient data it should never have been mixed with) and **`Wolfson`** (Dr. Marla Wolfson's injury-phenotype research project, which had independently built its own general-purpose functions — `step_famd`, `glmnet_IBS`, `proc.pca`, `corr.test.all` — because there was nowhere public to put them).

The goal: pull every domain-agnostic function out of both into one clean, publicly auditable package, so `datasci` and `Wolfson` can stay private (they may touch protected data) while still being *validated* against public, testable code instead of each maintaining their own private copies nobody else can check.

## Key decisions (and why)

- **`datasci`-sourced functions were migrated and removed from `datasci`**, which now re-exports them from here (`datasci/R/reexports.R`). Zero code changes needed anywhere that called `datasci::<function>()`, because those calls always went through `datasci`'s namespace, never a local copy.
- **`Wolfson`-sourced functions (`step_famd`, `glmnet_IBS`, `proc_pca`, `corr_test_all`) were *copied*, not moved.** `Wolfson`'s own local copies are untouched, deliberately, forever (barring a future decision to resync). `Wolfson` has a hand-built compute-graph/render-plan system (`R/MakeComputeGraph.R`, `R/RenderPlan.R`) that wipes knitr caches on re-render — editing its source risks either an expensive forced recompute or, worse, silent staleness nothing would flag. This means the two copies can and will diverge over time; that's an accepted trade, not an oversight.
- **License is dual `GPL-3 | MIT`**, matching `pslongSim`, `omop-duck-db`, and `ML-PScore` — a deliberate ecosystem-wide choice, not a `datasci`/`Wolfson`-inherited default (those stay plain MIT since they're permanently private).
- **Zero real data, ever.** Every example uses `iris`/`mtcars`/inline synthetic data. `pslongSim` is the designated tool for anything that needs to look more clinically realistic than that.

## Current status (2026-08-05)

Phase 1–3 of the migration plan is executed, not just planned: every function in the relocation mapping is present, documented, and tested. Verifying what had already been built (outside this process, before this session) surfaced three real bugs that are now fixed rather than inherited silently:

- `detect_outliers`/`calculate_fences`/`flag_outliers` had lost the original MILD/EXTREME inner-vs-outer-fence distinction in an earlier simplification — restored to match the verified original contract (recovered from `datasci`'s git history).
- `min_max_norm` normalized matrices globally instead of per-column; `range_norm` was wrongly aliased to be identical to `min_max_norm` instead of combining columns into one distribution. Both fixed.
- `missmap()`'s first draft broke on mixed numeric/character input — caught by its own smoke test before shipping.

Both this package's test suite and `datasci`'s full historical test suite pass together, in one session — real evidence of behavior preservation, not just "it loads."

## Round 3 (2026-08-05): API Harmonization, Warnings Silenced, & Release Polish

- **`glmnet_IBS` Warning Fix**: Explicitly specified `cox.ties = "efron"` inside `cv.glmnet()` calls to silence the `glmnet 5.1` tie-handling default shift warning across test runs.
- **Plotting API Harmonized**: Unified `plot.features_percent_miss()` S3 method to delegate directly to `plot_features_percent_miss()`, ensuring consistent ggplot formatting, percentage scaling, and optional `top_n` feature filtering across both direct and S3 invocations.
- **Vignettes & Documentation**: Added `eda_and_missingness.Rmd` vignette covering data quality, outlier detection, and normalization. Configured `_pkgdown.yml` with structured navigation categories for online reference generation.
- **CI/CD Integration**: Configured GitHub Actions workflows (`.github/workflows/R-CMD-check.yaml` for multi-OS R CMD check and `.github/workflows/pkgdown.yaml` for automated GitHub Pages deployment).

## Downstream Maintenance & Resync Guidelines

- **`datasci` Integration**: `datasci` re-exports all migrated functions from `TempleCBE` via `datasci/R/reexports.R`. Any future bugfixes in `TempleCBE` automatically propagate to downstream `datasci` callers without modifying `datasci` code.
- **`Wolfson` Maintenance & Backport Protocol**: `Wolfson` maintains isolated local copies of `step_famd`, `glmnet_IBS`, `proc_pca`, and `corr_test_all` to avoid invalidating its cached compute graph (`R/MakeComputeGraph.R`). If core algorithm improvements or bug fixes occur in `TempleCBE`, evaluate backporting to `Wolfson` on a case-by-case basis during explicit project re-analysis cycles.
- **Testing & Quality Assurance**: Run `devtools::test()` and `devtools::check(cran = TRUE)` prior to tagging releases. All 126 unit tests pass cleanly with zero warnings or failures.
