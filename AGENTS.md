# Developer & Agent Guide: TempleCBE

## 1. Repository Architecture & Upstream

- **Primary Repository:**
  [jkylearmstrong/TempleCBE](https://github.com/jkylearmstrong/TempleCBE)
  (Public).
- **Private Repository Status:** `TempleCBE-private` is retired and no
  longer used for day-to-day development. All development, branching,
  issue tracking, and PRs occur directly on `jkylearmstrong/TempleCBE`.
- **Git Remote:** `origin` points to
  `https://github.com/jkylearmstrong/TempleCBE.git`.

------------------------------------------------------------------------

## 2. Development Workflow

1.  **Branching:**
    - Always branch off the latest `origin/master`:

      ``` bash
      git checkout master
      git pull origin master
      git checkout -b fix/your-feature-name
      ```
2.  **Pull Requests:**
    - Push your feature branch to `origin`:

      ``` bash
      git push -u origin fix/your-feature-name
      ```

    - Open a Pull Request targeting `master` on
      `jkylearmstrong/TempleCBE`.
3.  **Continuous Integration (GitHub Actions):**
    - **Pull Requests (Full Gate):** Run the complete cross-platform
      test matrix (`ubuntu-latest`, `windows-latest`, `macOS-latest`,
      `ubuntu-24.04-arm` for aarch64 / DGX Spark) and `test-coverage`.
      On Windows, vignette building is skipped during check to reduce
      check times from 35+ min to ~12 min.
    - **Push to Master (Fast Sanity & Deploy):** Runs a fast 5-minute
      sanity check on `ubuntu-latest` with documentation (`pkgdown`)
      deploying concurrently. Container images (`docker-publish`) deploy
      to Docker Hub upon successful completion of the sanity check
      (`workflow_run`), while tagged releases (`v*`) publish multi-arch
      images (`linux/amd64,linux/arm64`) directly, each architecture
      built on a native runner and merged into one manifest.
    - Workflows automatically cancel superseded in-progress runs when
      new commits are pushed (`cancel-in-progress: true`).

------------------------------------------------------------------------

## 3. Local Testing & Verification

- **Interactive / Dev Testing:** Use `pkgload` or `devtools` to load the
  current source tree without installing:

  ``` r

  pkgload::load_all(".")
  ```

- **Running Tests:**

  ``` r

  # Run a single test file:
  testthat::test_file("tests/testthat/test-your-feature.R")

  # Run the full test suite locally:
  testthat::test_local()
  ```

- **Test Execution:**

  - ## Tests run sequentially by default. Do not set `Config/testthat/parallel: TRUE` in `DESCRIPTION`; running testthat in parallel causes deadlocks on Windows CI because several test suites (e.g. `missforest-mtry`, `missranger-mtry`) spin up their own nested `future::multisession` / socket worker clusters.

## 4. Key Conventions & Safeguards

- **Known Test Warnings:**
  - Tests utilizing short, fixed known-answer test vectors set
    `withr::local_options(templecbe_suppress_weak_key_warning = TRUE)`.
    General weak-key warnings remain covered by
    `test-atomic_io_guards.R`.
- **Windows Reparse Points & Junctions:**
  - On Windows, R’s `unlink(recursive = TRUE)` fails on directory
    junctions. Use `remove_dir_link()` from
    `tests/testthat/helper-links.R`, which delegates to `cmd /c rmdir`.
- **Dependencies:**
  - Core dependencies are defined in `DESCRIPTION`. If running outside
    `renv`, use `Rscript --vanilla` to prevent uninstalled lockfile
    bootstrapping conflicts.
