# Developer & Agent Guide: TempleCBE

## 1. Repository Architecture & Upstream

* **Primary Repository:** [jkylearmstrong/TempleCBE](https://github.com/jkylearmstrong/TempleCBE) (Public).
* **Private Repository Status:** `TempleCBE-private` is retired and no longer used for day-to-day development. All development, branching, issue tracking, and PRs occur directly on `jkylearmstrong/TempleCBE`.
* **Git Remote:** `origin` points to `https://github.com/jkylearmstrong/TempleCBE.git`.

---

## 2. Development Workflow

1. **Branching:**
   * Always branch off the latest `origin/master`:
     ```bash
     git checkout master
     git pull origin master
     git checkout -b fix/your-feature-name
     ```
2. **Pull Requests:**
   * Push your feature branch to `origin`:
     ```bash
     git push -u origin fix/your-feature-name
     ```
   * Open a Pull Request targeting `master` on `jkylearmstrong/TempleCBE`.
3. **Continuous Integration (GitHub Actions):**
   * **Pull Requests (Full Gate):** Run the complete cross-platform test matrix (`ubuntu-latest`, `windows-latest`, `macOS-latest`, `ubuntu-24.04-arm` for aarch64 / DGX Spark, all on the R release, plus `ubuntu-latest` on `oldrel-1` and on `devel`; the `devel` job is informative and does not fail the workflow) and `test-coverage`. On Windows, vignette building is skipped during check to reduce check times from 35+ min to ~12 min.
   * **Push to Master (Sanity, Full Matrix & Deploy):** Runs the sanity check on `ubuntu-latest` and, since 0.8.1, the same cross-platform matrix as a pull request (a merge is a new commit, and it is what the images are built from), with documentation (`pkgdown`) deploying concurrently. Container images (`docker-publish`) deploy to Docker Hub once the whole `R-CMD-check` workflow has succeeded (`workflow_run`), while tagged releases (`v*`) publish multi-arch images (`linux/amd64,linux/arm64`) directly, each architecture built on a native runner and merged into one manifest.
   * **CI image:** the Linux x86-64 jobs run in `ghcr.io/jkylearmstrong/templecbe-ci`, pinned in the workflows by dated tag and digest (not `latest`). `ci-image.yml` still builds a new image every Monday; to move CI to it, replace the tag and digest in `R-CMD-check.yaml` (three places) and `test-coverage.yaml` with those of the "Inspect image" step.
   * **Denylist scan:** the `ubuntu-latest (release)` job scans the tracked files for the terms of the Actions secret `TEMPLECBE_DENYLIST_TERMS` (one per line; `scripts/ci_denylist_scan.R`). Without the secret the step only warns; set the repository variable `TEMPLECBE_REQUIRE_DENYLIST` to `true` to make a missing secret a failure for runs from this repository.
   * **Tests that need external software:** the Tier-2 PDF to DOCX tests (`TEMPLECBE_RUN_PDF_TESTS=true`, with pdf2docx and LibreOffice) run in the job `pdf-to-docx live backends`, which is not blocking yet. The Word COM backend and the live SAS tests (`TEMPLECBE_RUN_SAS_TESTS=true`) need Microsoft Word and a SAS licence, which no hosted runner has: run them locally before a release that touches `R/pdf_to_docx.R` or `R/run_sas_script.R`; CI covers them with stored SAS results (Tier 1) and mocked converters only.
   * Workflows automatically cancel superseded in-progress runs when new commits are pushed (`cancel-in-progress: true`).

---

## 3. Local Testing & Verification

* **Interactive / Dev Testing:**
  Use `pkgload` or `devtools` to load the current source tree without installing:
  ```r
  pkgload::load_all(".")
  ```
* **Running Tests:**
  ```r
  # Run a single test file:
  testthat::test_file("tests/testthat/test-your-feature.R")

  # Run the full test suite locally:
  testthat::test_local()
  ```
* **Test Execution:**
  * Tests run sequentially by default. Do not set `Config/testthat/parallel: TRUE` in `DESCRIPTION`; running testthat in parallel causes deadlocks on Windows CI because several test suites (e.g. `missforest-mtry`, `missranger-mtry`) spin up their own nested `future::multisession` / socket worker clusters.
---

## 4. Key Conventions & Safeguards

* **Known Test Warnings:**
  * Tests utilizing short, fixed known-answer test vectors set `withr::local_options(templecbe_suppress_weak_key_warning = TRUE)`. General weak-key warnings remain covered by `test-atomic_io_guards.R`.
* **Windows Reparse Points & Junctions:**
  * On Windows, R's `unlink(recursive = TRUE)` fails on directory junctions. To delete a directory that a user may have set up (a cache, checkpoint or build directory), package code calls `remove_dir_tree()` (`R/remove_dir_tree.R`), which removes links and junctions as links and never empties their targets; `remove_dir_link()` in the same file removes one link (it delegates to `cmd /c rmdir` on Windows). Tests create links with `make_dir_link()` from `tests/testthat/helper-links.R`.
* **Naming (`cbe_`):**
  * `cbe_*` **functions** are presentation and formatting only (themes, palettes, scales, plots, report tables, number formatters); every other function is a plain verb-noun. This is checked for exports.
  * S3 **class** names are an accepted exception. Classes created before the rename keep their `cbe_` name (`cbe_cox`, `cbe_cox_multi`, `cbe_km`, `cbe_cox_check`, `cbe_cox_glance`, `cbe_review_extract`, `cbe_tuning_results`, `cbe_compare_df`, `cbe_database`, `cbe_pca`, `cbe_start_stop_validation`, `cbe_completion_event`, `cbe_clean_checkpoints`, `cbe_runtime_estimate`, `cbe_tune_mixed_model`, `cbe_impute_tune`, `cbe_coef_screen` and the like): a class name is stored in saved objects and tested with `inherits()` in downstream code, so renaming one breaks objects on disk and scripts without any gain for the user. Do not rename them.
  * A **new** class does not get the prefix. Where a class has been given a plain name (`nested_survival_run`, `domain_children`, `loco_mp_coxnet`), the old name stays in the class vector behind the new one (`class = c("nested_survival_run", "cbe_nested_survival_run")`) and the methods of both stay registered.
  * Two leftovers are also kept as they are: the exported `demo_cbe_mapping()` (the name describes the demo mapping; it does not start with `cbe_`) and the internal data objects `.cbe_census_surnames` / `.cbe_first_names` in `R/sysdata.rda`.
* **Dependencies:**
  * Core dependencies are defined in `DESCRIPTION`. If running outside `renv`, use `Rscript --vanilla` to prevent uninstalled lockfile bootstrapping conflicts.
