# Contributing to TempleCBE

Thank you for helping. The short version: open an issue first for anything bigger than a typo, make one focused pull request per change, and add a test.

## How changes are reviewed

[`REVIEW.md`](../REVIEW.md) is the full policy. In brief, review is proportionate to risk:

* **L0** docs, `NEWS.md`, tests-only changes: CI must pass.
* **L1** small fixes and helpers: the PR checklist plus one review.
* **L2** statistical computation, public API, file writers, de-identification helpers, anything that runs a subprocess or deletes: a regression test that fails before the fix and passes after.
* **L3** privacy behaviour, release/CI/Docker files, secrets, license, dependencies: security review and maintainer sign-off.

## Before you open a pull request

1. Branch from `master` and name the branch for its level and topic, for example `l2/fix-coxnet-ties`.
2. Add or update tests in `tests/testthat/`. A statistical change needs evidence beyond "it runs": a reference implementation such as `survival::coxph`, a hand-computed case, or a SAS benchmark.
3. Add an entry to `NEWS.md` under the unreleased heading, and regenerate the documentation with the roxygen2 version pinned in `DESCRIPTION` (`Config/roxygen2/version`).
4. Run `R CMD check` on a built tarball, with `NOT_CRAN=true` so that no test is skipped, and read the skipped-test count. Tests must not depend on installed tools, paths, time zones or the operating system: the Ubuntu, macOS and Windows jobs decide.
5. Keep the pull request to roughly 400 changed lines of R. Split larger work.

## What must never be committed

Real study names, person names, subject identifiers, secret keys, mapping files, or local paths. Examples and tests use synthetic data.

## Merging

`master` is protected: nothing is pushed to it by hand. The maintainer integrates accepted changes on a development branch (`dev`, see `REVIEW.md`) and releases from there, so depending on the repository setup your pull request is either merged here with a merge commit (not a squash) once every CI job on its latest commit is green, or its commits are applied on the development branch and the pull request is closed with a pointer to the release that contains them. A few scripted paths of the maintainer do push without a pull request (the release workflow and script, the site deployment, and the publication to the public repository); `REVIEW.md` lists them.

On the **public** repository the `master` branch is written by the maintainer's publish tool only: a snapshot of the private repository is pushed there directly, and pull requests are never merged there, because the next snapshot would revert them. A pull request opened on the public repository is closed with a pointer; if the change is accepted, the maintainer applies it to the private repository, and it reaches the public one with the next snapshot.
