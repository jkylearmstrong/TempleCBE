# TempleCBE Development & CI/CD Notes

## Version: 0.5.2
**Date:** October 14, 2026

---

### 1. CI/CD Architecture (Strategy A)
* **Master Branch ("Prod Gate"):**
  - Runs `sanity-check` on `ubuntu-latest (release)` (~5 minutes).
  - Skips redundant 35-minute Windows and macOS checks.
  - Deploys documentation site (`pkgdown`) concurrently to `gh-pages`.
  - Automatically triggers downstream container deployment upon green sanity-check (`workflow_run`):
    - `docker-publish`: Builds native `linux/amd64` Docker image with GitHub Actions layer caching (~4 minutes) and pushes directly to Docker Hub (`<user>/templecbe`). Gated strictly to `push` events from the primary repository.
* **Pull Requests & Release Tags ("Dev Full Matrix Gate"):**
  - Runs full cross-platform matrix: `ubuntu-latest`, `windows-latest`, `macOS-latest`.
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
  - All workflows use `group: ${{ github.workflow }}-${{ github.head_ref || github.ref }}` with `cancel-in-progress: true`.
  - Older superseded runs are cancelled immediately when a new commit is pushed.
  - Job-level `timeout-minutes` prevent zombie runners.

---

### 3. Container Publishing
* **Registry:** Docker Hub (`DOCKERHUB_USERNAME` and `DOCKERHUB_TOKEN` configured in repository secrets).
* **Architectures:**
  - Master pushes build native `linux/amd64` (~4 minutes).
  - Tagged releases (`v*`) build multi-architecture (`linux/amd64,linux/arm64`).
