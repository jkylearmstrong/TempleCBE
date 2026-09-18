# TempleCBE Automated Release Pipeline

This directory archives the original draft specifications from `tasks/to_do/memories/` and documents the production release mechanisms implemented for **`TempleCBE`**.

---

## Architecture Overview

The release pipeline facilitates coordinated, reproducible releases across multiple machines and agent environments.

### Traceable Datetime Versioning
To prevent collisions and enable clean Git lifecycle management across distributed developer machines and agents, versions default to incorporating a sortable UTC datetime stamp:
```text
<base_version>.<YYYY>.<MM>.<DD>.<HH>.<MM>
Example: 0.3.403.2026.09.17.11.37
```
This enables:
- Automatic generation of unique, chronological release tags.
- Simple git cleanup and filtering (`git tag -l "0.3.4.*"`).
- Exact auditability of when and where an agent release occurred.

---

## 1. Local R Deployment Tool: `scripts/deploy_release.R`

The script [`scripts/deploy_release.R`](../../scripts/deploy_release.R) can be sourced interactively in R/RStudio or executed from the command line.

### Interactive R Usage:
```r
source("scripts/deploy_release.R")

# Default: auto-generates datetime version tag (0.3.403.YYYY.MM.DD.HH.MM), runs tests and docs, commits & tags
deploy_release()

# Dry run: previews every step without modifying files or git
deploy_release(dry_run = TRUE)

# Fast release: skip lengthy pkgdown site and R CMD check
deploy_release(build_site = FALSE, run_check = FALSE)

# Custom version tag
deploy_release(version_tag = "0.3.5")
```

### CLI / Shell Usage:
Using the repository's version-pinned R runner [`scripts/Rscript.ps1`](../../scripts/Rscript.ps1):
```powershell
# Preview release
scripts/Rscript.ps1 scripts/deploy_release.R --dry-run

# Run standard release
scripts/Rscript.ps1 scripts/deploy_release.R

# Specify base semver override
scripts/Rscript.ps1 scripts/deploy_release.R --base 0.3.5

# Full build including pkgdown and R CMD check
scripts/Rscript.ps1 scripts/deploy_release.R --build-site --run-check
```

---

## 2. GitHub Actions Workflow: `.github/workflows/release.yaml`

The workflow [`.github/workflows/release.yaml`](../../.github/workflows/release.yaml) enables automated `workflow_dispatch` releases with configurable parameters:

| Input Parameter | Default | Description |
| :--- | :--- | :--- |
| `version_tag` | `auto` | Version tag. `auto` generates `<base_ver>.YYYY.MM.DD.HH.MM` |
| `base_version` | `""` | Optional base semver override (e.g. `0.3.5`) |
| `run_tests` | `true` | Runs `devtools::test()` |
| `render_readme` | `true` | Renders `README.qmd` to `README.md` via Quarto |
| `update_description` | `true` | Synchronizes `Version:` in `DESCRIPTION` |
| `run_document` | `true` | Runs `devtools::document()` |
| `build_site` | `false` | Builds pkgdown site (disabled by default for speed; `pkgdown.yaml` builds on push) |
| `run_check` | `false` | Runs R CMD check (disabled by default for speed; `R-CMD-check.yaml` runs on push) |
| `commit_and_push` | `true` | Commits changes, tags, and pushes branch + tags to origin |

---

## Archived Original Drafts
- [`gemini-code-1789644326979.r`](gemini-code-1789644326979.r): Initial prototype R release function.
- [`gemini-code-1789644394521.yaml`](gemini-code-1789644394521.yaml): Initial prototype GitHub Actions workflow.
