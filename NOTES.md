# TempleCBE Development & CI/CD Notes

## Version: 0.10.0
**Date:** November 11, 2026

AGENTS.md (workflow, CI, conventions) and REVIEW.md (review levels, branch protection, releases) are the
maintained references; this file keeps the reasons behind a few decisions.

---

### 1. CI/CD Architecture (Strategy A)
* **Master Branch ("Prod Gate"):**
  - Runs `sanity-check` on `ubuntu-latest (release)` (about 13 minutes) in the pinned CI image.
  - Since 0.8.1 also runs the cross-platform matrix (it used to be skipped on `master`), so `docker-publish` builds from a commit that passed on every platform.
  - Deploys documentation site (`pkgdown`) concurrently to `gh-pages`.
  - Automatically triggers downstream container deployment upon green sanity-check (`workflow_run`):
    - `docker-publish`: Builds native `linux/amd64` Docker image with GitHub Actions layer caching (~4 minutes) and pushes directly to Docker Hub (`<user>/templecbe`). Gated strictly to `push` events from the primary repository.
* **Pull Requests & Release Tags ("Dev Full Matrix Gate"):**
  - Runs full cross-platform matrix: `windows-latest`, `macOS-latest`, `ubuntu-24.04-arm` on the R release, and `ubuntu-latest` on `oldrel-1` and `devel` (informative).
  - Runs `optional-deps-installed-tests` for multisession packages on Ubuntu and Windows.
  - Runs `test-coverage` via Codecov (tokenless OIDC integration).
  - On `windows-latest`, `rcmdcheck` passes `--no-build-vignettes` to cut Windows check time by >50% (from 35+ min to ~12 min).
* **On-Demand Manual Trigger:**
  - `workflow_dispatch` enabled in `R-CMD-check.yaml` with an optional `full_matrix` boolean.

---

### 2. Testing Safeguards & Concurrency
* **Sequential `testthat` (Edition 3):**
  - `Config/testthat/parallel: TRUE` must **never** be enabled in `DESCRIPTION`.
  - Tests that use `future::plan(future::multisession)` (e.g. `test-missforest-mtry.R`, `test-missranger-mtry.R`) spawn background R processes.
  - Running nested socket clusters inside `callr` parallel test workers on Windows runners deadlocks TCP loopback sockets.
  - `TESTTHAT_PARALLEL: "FALSE"` is strictly enforced in all CI workflows.
* **Concurrency Cancellation:**
  - The check workflows use one group per pull request number (`${{ github.workflow }}-${{ github.event.pull_request.number || github.ref }}`) with `cancel-in-progress: true`; `release.yaml` uses one group for all runs and does not cancel.
  - Older superseded runs are cancelled immediately when a new commit is pushed.
  - Job-level `timeout-minutes` prevent zombie runners.

---

### 3. Container Publishing
* **Registry:** Docker Hub (`DOCKERHUB_USERNAME` and `DOCKERHUB_TOKEN` configured in repository secrets).
* **Architectures:**
  - Master pushes build native `linux/amd64` (~4 minutes).
  - Tagged releases (`v*`) build multi-architecture (`linux/amd64,linux/arm64`).

---

### 4. Versioning and Checkpoints
* **One version helper:** `templecbe_version()` (`R/package_version.R`) is the only place R code reads the installed package version. Never hardcode a fallback version string.
* **Checkpoint fingerprints use a schema number, not the package version:** `checkpoint_schema_version()` is hashed into every checkpoint fingerprint. Bump it by hand only when a change alters what a cached checkpoint contains or means. The package version is still written to `checkpoint_meta$version` for provenance but is not hashed, so releases that do not change results keep users' cached long-running work valid.
* **Tests must not pin the package version.** Compare against `packageVersion()` / `templecbe_version()`.

### 5. Deprecation Removal Schedule
* Wrappers deprecated in 0.5.8 (`cbe_*` non-formatting names, masking helpers such as `clean_names()`, `like()`, `write_xlsx()`, `render()`) were announced for removal in 0.6.0 but were **kept** in 0.6.0. They were removed in **0.7.0**.

### 6. Release Records
* L3 release sign-offs live in `release-notes/` (excluded from the build). 0.6.0 has a retroactive record there.
