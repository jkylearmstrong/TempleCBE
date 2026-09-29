# README.qmd Rewrite

Rewrite [README.qmd](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/README.qmd) from scratch — organized for a reader who knows nothing about the package, with a clear visual hierarchy that answers "What → Install → Try → Learn More" in that order.

## User Review Required

> [!IMPORTANT]
> **Quick Start example choice.** The plan uses the **multi-table clinical database** workflow as the single Quick Start example (broadest audience — everyone has messy multi-table data). If you'd prefer the **penalized Cox model** or **joint ensemble** example instead, flag it before I start.

> [!IMPORTANT]
> **Hero image.** The plan includes a regenerated AI overview image at `man/figures/overview.png`. If you'd rather skip this and use a Mermaid diagram (or nothing), let me know.

> [!IMPORTANT]
> **Gap-areas table.** The current 6-row methodology table is removed from the main flow. It reappears as a condensed bullet list under "Feature Overview." If you want to preserve the full table (e.g. in a `<details>` collapse), say so.

## Open Questions

> [!WARNING]
> **Badge accuracy.** The current Codecov badge points to `branch/master`. Is your default branch `master` or `main`? I'll match whatever it is.

> [!NOTE]
> **Vignette numbering.** The current README numbers vignettes 01–07 but only 7 `.Rmd` files exist. Is this final, or are more planned? I'll number as-is and leave room.

---

## Proposed Structure (top → bottom)

The new README has **8 sections**, roughly half the vertical length of the current one.

### Section 1 — Title Block
```
# TempleCBE <logo>
badges (R-CMD-check, Codecov, License, Version)
```
Unchanged from current. Keep logo, keep all four badges.

---

### Section 2 — What is TempleCBE? (NEW — replaces the dense opening paragraph)

**3 plain-language sentences.** No acronyms, no jargon. Goal: a reader who Googles "R clinical biostatistics package" understands what this is in 5 seconds.

Draft:

> **TempleCBE** is an R package for clinical biostatistics, survival modeling, and reproducible pipeline management. It gives you tools to wrangle multi-table trial databases, fit penalized Cox and ensemble survival models inside tidymodels, validate results against SAS, and track analytical dependencies as DAGs — all in one package.
>
> Developed at Temple University's Center for Biostatistics & Epidemiology (CBE), Lewis Katz School of Medicine.

---

### Section 3 — Overview Image (NEW)

Regenerated AI hero image embedded as:
```markdown
![TempleCBE at a glance](man/figures/overview.png)
```
Fixed version — no duplicate labels. 16:9, dark background, 6 pillars.

---

### Section 4 — Installation (MOVED UP)

Identical content to current, just repositioned to come before any code examples. `pak::pak()` primary, `remotes` secondary.

---

### Section 5 — Quick Start (REPLACES current 6-example section)

**One complete, copy-pasteable example** that shows the broadest-appeal workflow:

1. Build a 3-table clinical database list
2. Assign metadata & labels
3. Get a consolidated data dictionary
4. Export to Excel
5. Inspect entity-relationship graph

This is the current "Multi-Table Clinical Databases" example, lightly edited. All other examples are **removed from the README** — they live in their respective vignettes.

---

### Section 6 — Feature Overview (REPLACES the gap-areas table)

Six bullet points, each one line + a vignette link. No table. Scannable in 10 seconds.

Draft:

- **Multi-Table Clinical Databases** — Data dictionaries, Excel round-trips, ER graphs, variable roles, and cohort overlap Venns. → [EDA & Missingness vignette](link)
- **Penalized Cox & Survival ML** — `coxnet()` and `cv_coxnet()` with tidymodels `hardhat` engines, subject-grouped CV, counting-process `Surv(tstart, tstop, status)` support. → [Nested Survival CV vignette](link)
- **Joint Ensemble Models** — `joint_model()` blends Cox survival, binary classification, and duration regression into calibrated stacked risk scores. → [Nested Survival CV vignette](link)
- **SAS Regulatory Parity** — `cbe_compare_df()` matches SAS `PROC COMPARE`; `proc_pca()` matches `PROC PRINCOMP`; `cbe_exact2x2()` provides mid-p exact inference. → [R to SAS vignette](link) & [PCA vignette](link)
- **Python Interoperability** — Export fitted R models, survival matrices, and data dictionaries to Python via `reticulate` for SHAP, survex, and scikit-learn. → [Reticulate Export vignette](link)
- **Pipeline DAGs** — `compute_graph()` tracks script/data/report dependencies as typed DAGs with staleness-aware rendering. → [Compute Graph vignette](link)

---

### Section 7 — Vignettes (SIMPLIFIED)

Slim table: number, linked title, one-line description. No "Key Topics & Methodology" column (that detail belongs in the vignettes themselves).

| # | Vignette | Description |
|---|---|---|
| 1 | EDA & Missingness | Multi-table databases, missing sentinels, data dictionaries |
| 2 | Nested Survival CV | Penalized Cox, counting-process data, subject-grouped CV |
| 3 | Compute Graph | DAG pipelines, multi-report orchestration |
| 4 | SAS Survival Validation | Cross-validating Cox models against SAS PROC PHREG |
| 5 | R to SAS | Automated macro exports, PROC COMPARE verification |
| 6 | Reticulate Export | R ↔ Python model and data transfer |
| 7 | PCA Analysis | SAS PROC PRINCOMP parity, scree/biplots |

Plus the `vignette()` install snippet.

---

### Section 8 — Ecosystem, Authors, License (UNCHANGED)

Keep all three sub-sections exactly as they are today. They're already concise.

---

## Summary of Structural Changes

| Current README Section | Action | New Section |
|---|---|---|
| Title + badges | **Keep** | §1 Title Block |
| Dense 2-paragraph intro | **Replace** → 3 plain sentences | §2 What is TempleCBE? |
| *(none)* | **Add** hero image | §3 Overview Image |
| Gap-areas table (6 rows) | **Replace** → 6 bullet points | §6 Feature Overview |
| Installation | **Keep**, move up | §4 Installation |
| 6 code showcases | **Replace** → 1 Quick Start | §5 Quick Start |
| Vignettes table | **Simplify** | §7 Vignettes |
| Ecosystem | **Keep** | §8 Ecosystem |
| Authors | **Keep** | §8 Authors |
| License | **Keep** | §8 License |

## Files Changed

### [MODIFY] [README.qmd](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/README.qmd)

Full rewrite of content. YAML frontmatter (`format: gfm`) unchanged. Estimated final length: ~120 lines (down from 236).

### [NEW] `man/figures/overview.png`

Regenerated AI hero image (16:9, ~1400×788px). Saved alongside the existing `man/figures/logo.png`.

### [REGENERATE] `README.md`

After editing `README.qmd`, re-render via `quarto render README.qmd` to produce the GitHub-facing `README.md`.

---

## Verification Plan

### Automated
```bash
quarto render README.qmd
```
Confirm `README.md` is produced without errors and all markdown links resolve.

### Manual
- Visual check of `README.md` rendering on GitHub (badge images, logo, overview image, vignette links)
- Confirm the Quick Start code block is copy-pasteable into a clean R session with `TempleCBE` installed
