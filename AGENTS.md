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
    - All GitHub Actions runners (`R-CMD-check` on Windows, macOS,
      Ubuntu, `pkgdown`, `test-coverage`) run free and unthrottled on
      `TempleCBE`.
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

- **Parallel Test Execution:**

  - `Config/testthat/parallel: TRUE` is enabled in `DESCRIPTION`.
  - `testthat` (edition 3) will execute test files concurrently across
    available CPU cores (controlled via `TESTTHAT_CPUS`).

------------------------------------------------------------------------

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
