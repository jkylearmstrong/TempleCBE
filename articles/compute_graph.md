# Visualizing Computational Pipeline Dependencies

## Why track a pipeline as a graph

A multi-report analysis pipeline is a dependency graph whether or not
anyone draws it:

1.  raw data feeds an EDA report,

2.  an EDA report feeds downstream computations

3.  imputed data feeds a dozen downstream reports

Once that graph exists only in the analyst’s head, two things get hard
to answer: “if I change this upstream file, what needs to be
re-rendered?” and “can a new collaborator see the whole pipeline at a
glance?”

`compute_graph.R` answers both by representing every file in the
pipeline (raw data, rendering scripts, derived artifacts) as an S4
object with a `stage`, an `artifact_role`, and links to what it depends
on and what it produces, then building an `igraph`/`tidygraph` graph
from those links. It is deliberately format-agnostic — a node is a path
on disk, whether that means an `.R` script, a `.qmd` report, an `.xlsx`
extract, or an `.rds` cache.

## Comparison with existing R architecture (`targets` & friends)

The R ecosystem has established workflow automation frameworks, most
notably [targets](https://books.ropensci.org/targets/) (and its
predecessor `drake`), along with GNU Make wrappers and `workflowr`.
Understanding how
[`TempleCBE::compute_graph`](https://jkylearmstrong.github.io/TempleCBE/reference/compute_graph.md)
compares and contrasts with these architectures clarifies when and why
to use it.

### How `targets` works

`targets` (rOpenSci, developed by Will Landau) is the gold standard in R
for **function-oriented, computation-driven pipelines**. Its design
philosophy centers on pure R functions and expressions:

- **Computational units**: The user defines steps via
  `tar_target(name, command)` inside a central `_targets.R` script.
- **Dependency discovery**: `targets` inspects the Abstract Syntax Tree
  (AST) of user functions to automatically detect which targets and
  functions feed into others.
- **Caching & staleness**: Cryptographic hashes of functions,
  expressions, and in-memory objects detect staleness. If a function’s
  code or input data changes, only downstream targets are invalidated.
- **Execution engine**: Running `tar_make()` executes the pipeline,
  stores serialized R objects in an internal binary cache
  (`_targets/objects/`), and skips up-to-date targets. It supports
  high-performance computing (HPC) clusters via `crew`, `clustermq`, or
  `future`.

`targets` is extraordinarily powerful for long-running statistical
computations, simulation studies, and reproducible research where
intensive R calculations need to be cached and parallelized.

### How `TempleCBE` differs

`TempleCBE`’s compute graph architecture is fundamentally **file-system
artifact-oriented, metadata-rich, and non-intrusive**. Rather than
acting as a heavy execution engine for R expressions, it serves as a
lightweight, human-interpretable dependency auditor and visual
communication layer:

1.  **File-system nodes vs. In-memory objects**: In `targets`, a target
    is usually an R variable or intermediate data frame stored in a
    private cache. In `TempleCBE`, every node is a tangible file on disk
    (`FilePath`), inspectable by anyone using standard file browsers or
    command-line tools.
2.  **Zero-rewrite adoption vs. Functional refactoring**: Adopting
    `targets` demands modularizing all analysis scripts into pure
    functions and writing a central `_targets.R` configuration. In
    contrast, `TempleCBE` requires zero source code changes: you wrap
    existing project scripts, templates, raw data files, and outputs
    with a lightweight metadata manifest without touching a single line
    of existing analysis code.
3.  **Language & format agnosticism**: While `targets` can track
    external files via `format = "file"` or Quarto reports via
    `tar_quarto()`, it is primarily an R execution engine. `TempleCBE`
    treats all files equally: a raw `.xlsx` from a clinical coordinator,
    a `.sas` data step, a `.py` preprocessing script, a `.qmd` Quarto
    document, and a rendered `.pdf` or `.docx` deliverable are all
    first-class nodes.
4.  **Domain-aware clinical semantics**: `targets` nodes carry
    computational statuses (e.g. `uptodate`, `queued`, `running`).
    `TempleCBE` nodes carry domain-meaningful metadata: workflow
    lifecycle milestones (`stage`), semantic analytical roles
    (`artifact_role`: raw data, derived baseline data, imputed data,
    helper scripts, deliverable reports), modification timestamps
    (`mtime`), and plain-language descriptions.
5.  **Decoupled execution vs. Monolithic runner**: `targets` tightly
    couples dependency analysis with execution via `tar_make()`.
    `TempleCBE` decouples staleness planning
    ([`get_render_plan()`](https://jkylearmstrong.github.io/TempleCBE/reference/get_render_plan.md))
    from execution: it answers *what* is stale and *in what topological
    order* it must be rendered, leaving execution mechanics entirely up
    to the analyst (e.g. Quarto CLI, RStudio, GNU Make, or custom batch
    scripts).
6.  **Multi-tier stakeholder communication**: While `targets` provides
    developer diagnostics (`tar_visnetwork()`), `TempleCBE` provides
    rich, publication-ready and client-ready deliverables: zoomable HTML
    widgets with rich tooltips, neighborhood ego-subgraphs, custom
    Sugiyama-layout static PNGs, and collapsed stage-level diagrams.

### Feature comparison matrix

| Dimension | `targets` | [`TempleCBE::compute_graph`](https://jkylearmstrong.github.io/TempleCBE/reference/compute_graph.md) |
|:---|:---|:---|
| **Primary purpose** | Computation caching & pipeline execution engine | File dependency auditing, staleness planning & stakeholder visualization |
| **Node representation** | In-memory R objects / target expressions (cached in `_targets/`) | Explicit files on disk (`FilePath`, `FileUses`, `FileOutputs`) |
| **Invasiveness / barrier** | High: requires refactoring code into pure R functions in `_targets.R` | Low / Zero: non-intrusive metadata wrapper around existing files |
| **Language support** | R-centric (supports external files/tools via extensions) | Language- and format-agnostic (R, Quarto, Python, SAS, Excel, PDF) |
| **Domain semantics** | Computational status (hash, runtime, memory, skipped) | Clinical/study taxonomy (`stage`, `artifact_role`, descriptions) |
| **Staleness detection** | AST code hashing & data content hashes | File-system modification timestamps (`mtime`) |
| **Execution model** | Monolithic runner (`tar_make()`) handles execution and storage | Decoupled: [`get_render_plan()`](https://jkylearmstrong.github.io/TempleCBE/reference/get_render_plan.md) returns topological order for custom execution |
| **Relational analytics** | Internal query tools (`tar_network()`, `tar_manifest()`) | Native bridge to `igraph`, [`tidygraph::tbl_graph`](https://tidygraph.data-imaginist.com/reference/tbl_graph.html), and `dplyr` verbs |
| **Stakeholder views** | Developer-oriented target network | Zoomable HTML (`visNetwork`), PNG (`ggraph`), stage collapse, ego subgraphs |

------------------------------------------------------------------------

## Specific gaps addressed by `TempleCBE`

In biostatistical consulting centers, academic medical centers, and
clinical research organizations, data science workflows face constraints
rarely accommodated by pure functional workflow engines. `TempleCBE` was
engineered to address five specific operational gaps:

### 1. The “Retrofit & Legacy Pipeline” gap (Zero-rewrite adoption)

Most clinical and epidemiologic studies are already underway when
dependency tracking becomes urgent. Teams have dozens of pre-existing
Quarto/RMarkdown scripts, data cleaning scripts, and statistical models
written by different analysts over months or years. Rewriting an ongoing
multi-thousand-line project into pure R functions to satisfy `targets`
is often impossible due to locked regulatory protocols, looming sponsor
deadlines, or prohibitive refactoring costs. `TempleCBE` can be layered
over an existing project in under an hour by declaring file
relationships in a standalone manifest, without modifying existing
source code.

### 2. The “Heterogeneous Stack” gap (Multi-language clinical environments)

Real-world clinical pipelines rarely live entirely within R. A typical
study might ingest raw Excel spreadsheets from hospital clinical
research coordinators (CRCs), run legacy SAS macros for CDISC data
derivation, invoke Python for deep-learning image feature extraction,
render Quarto `.qmd` statistical reports, and produce `.pdf` or `.docx`
deliverables for Institutional Review Boards (IRBs) and PIs. Because
`TempleCBE` operates strictly on file paths and modification times, it
treats every component across languages and software suites as a
first-class citizen.

### 3. The “Semantic Data Lineage & Governance” gap

In regulated clinical trials and collaborative research, knowing that a
target is stale is not enough — auditors, biostatisticians, and clinical
monitors need to know *what kind* of asset it is and *where* it sits in
the data lifecycle. `TempleCBE` formally tracks semantic roles: -
`raw_data`: Immutable source data (e.g. initial registry export). -
`derived_data`: Standardized baseline dataset created from raw inputs. -
`enhanced_data`: Statistically augmented data (e.g. multiple
imputation). - `intermediate_data`: Stage-internal calculations or
fitted model caches. - `report_source` & `deliverable_report`:
Executable documents and their deliverables. - `helper_script` &
`reference`: Child templates, macros, or lookup tables.

This taxonomy provides transparent data provenance and auditability out
of the box.

### 4. The “Multilevel Stakeholder Communication” gap

An execution DAG with 200 granular function targets is invaluable to the
software developer, but overwhelming and unintelligible to a clinical
investigator, surgeon, or regulatory auditor. `TempleCBE` bridges this
communication divide through multi-tiered visualization: - **For
analysts**: Interactive `visNetwork` HTML widgets with rich tooltips
detailing file paths, modification dates, roles, and staleness
warnings. - **For study sections**: Focused ego-subgraphs
([`export_subgraph()`](https://jkylearmstrong.github.io/TempleCBE/reference/export_subgraph.md))
that isolate the neighborhood around a single deliverable or analytical
stage. - **For protocol deliverables & manuscripts**: Sized-to-fit
static figures
([`print_pipeline()`](https://jkylearmstrong.github.io/TempleCBE/reference/print_pipeline.md))
with intelligent Sugiyama layout adjustments to prevent overlapping text
labels. - **For executive sponsors**: Stage-level collapsed diagrams
([`collapse_by_stage()`](https://jkylearmstrong.github.io/TempleCBE/reference/collapse_by_stage.md))
that condense a 90-file pipeline into a clear 4-node study milestone
overview.

### 5. The “Decoupled Execution & Inspection” gap

Many institutional workflows prohibit autonomous pipeline runners from
executing scripts directly — whether due to strict HPC batch submission
policies, specialized Quarto rendering parameters, interactive review
requirements, or containerized runtimes. `TempleCBE` isolates staleness
detection from code execution:
[`get_render_plan()`](https://jkylearmstrong.github.io/TempleCBE/reference/get_render_plan.md)
identifies out-of-date reports and computes their topological render
order, allowing the user, Makefile, or CI/CD runner to trigger the
execution using whatever environment or CLI tools they prefer.

------------------------------------------------------------------------

``` r

library(TempleCBE)
```

## Building a small pipeline

The classes are:

| Class | Adds | Meaning |
|----|----|----|
| `FilePath` | — | Any file: name, path, stage, role, description |
| `FileUses` | `dependencies` | A file that depends on upstream `FilePath` objects |
| `FileOutputs` | `output` | A file (typically a rendering script) that also produces downstream files |

We build a tiny two-node pipeline out of files that ship with the
package itself: a helper `.qmd` and a report `.qmd` that sources it.
[`create_qmd_renderer()`](https://jkylearmstrong.github.io/TempleCBE/reference/create_qmd_renderer.md)
is a convenience wrapper around
[`FileOutputs()`](https://jkylearmstrong.github.io/TempleCBE/reference/FileOutputs.md)
for the common case of “this `.qmd` renders to a PDF of the same name.”

``` r

pipeline_config(
  study_name = "Example Pipeline",
  stage_labels = c(setup = "Setup", report = "Reporting"),
  stage_colors = list(setup = "#DDEBF7", report = "#D4EFDF")
)
#> $study_name
#> [1] "Example Pipeline"
#> 
#> $stage_labels
#>       setup      report 
#>     "Setup" "Reporting" 
#> 
#> $stage_colors
#> $stage_colors$setup
#> [1] "#DDEBF7"
#> 
#> $stage_colors$report
#> [1] "#D4EFDF"

helper <- FilePath(
  name = "t_test_child",
  path = system.file("templates", "t_test_child.qmd", package = "TempleCBE"),
  renders = FALSE,
  stage = "setup",
  artifact_role = "helper_script"
)

report <- create_qmd_renderer(
  name = "t_test_example",
  path = system.file("templates", "t_test_example.qmd", package = "TempleCBE"),
  deps = list(helper),
  file_stage = "report",
  description = "Worked example of a one/two-sample t-test report"
)

pipeline <- list(helper, report)
report
#> S4 object of class: FileOutputs 
#>   Name:        t_test_example 
#>   Path:        /home/runner/.cache/R/renv/library/TempleCBE-357df843/linux-ubuntu-noble/R-4.6/x86_64-pc-linux-gnu/TempleCBE/templates/t_test_example.qmd 
#>   Stage:       report 
#>   Role:        report_source 
#>   Renders:     TRUE 
#>   Modified:    2026-09-15 22:00:59 
#>   Description: Worked example of a one/two-sample t-test report 
#>   Dependencies:
#>     - t_test_child
#>   Outputs:    
#>     - t_test_example PDF
```

Each `FilePath` object inspects the file on disk at construction time,
so `mtime` and `file_ext` are already populated — that is what makes
staleness detection possible later.

## Visualizing the graph

[`visualize_pipeline()`](https://jkylearmstrong.github.io/TempleCBE/reference/visualize_pipeline.md)
returns a
[`DiagrammeR::grViz()`](https://rich-iannone.github.io/DiagrammeR/reference/grViz.html)
object (or, with `extract_graph_code = TRUE`, the raw DOT source) for a
quick interactive look, e.g. in the RStudio Viewer:

``` r

visualize_pipeline(pipeline)
```

For a document deliverable,
[`print_pipeline()`](https://jkylearmstrong.github.io/TempleCBE/reference/print_pipeline.md)
lays the same graph out with `igraph`/`ggraph` and saves it as a
sized-to-fit PNG:

``` r

png_path <- "pipeline_graph.png"
print_pipeline(pipeline, png_path)
knitr::include_graphics(png_path)
```

![](pipeline_graph.png)

## Staleness and render order

Because every node carries its own `mtime`,
[`get_render_plan()`](https://jkylearmstrong.github.io/TempleCBE/reference/get_render_plan.md)
can compare each output’s modification time against its sources and
return only the reports that are actually out of date — topologically
sorted, so upstream reports always come before the downstream reports
that depend on them:

``` r

plan <- get_render_plan(pipeline)
names(plan)
#> NULL
```

[`pipeline_summary()`](https://jkylearmstrong.github.io/TempleCBE/reference/pipeline_summary.md)
gives the same information as an at-a-glance table — node/edge counts,
staleness, and a stage-by-role breakdown — useful as a standing status
check without re-deriving the graph by hand:

``` r

pipeline_summary(pipeline)
#> ========================================================================
#>                       Computational Pipeline Summary                    
#> ========================================================================
#> Total Nodes: 3 | Total Edges: 2 | Stale Nodes: 0 (All Up-To-Date)
#> 
#> Artifact Roles:
#>   - deliverable_report        :  1
#>   - helper_script             :  1
#>   - report_source             :  1
#> 
#> Pipeline Stages Breakdown:
#>   stage Reports Data_Files Helpers Total
#>  report       2          0       0     2
#>   setup       0          0       1     1
#> ========================================================================
```

## Interoperating with tidygraph

[`as_pipeline_graph()`](https://jkylearmstrong.github.io/TempleCBE/reference/as_pipeline_graph.md)
converts the object list to a
[`tidygraph::tbl_graph`](https://tidygraph.data-imaginist.com/reference/tbl_graph.html),
which brings ordinary dplyr verbs to the graph’s node and edge tables:

``` r

library(tidygraph)
library(dplyr)

as_pipeline_graph(pipeline) |>
  activate(nodes) |>
  as_tibble() |>
  select(name, stage, artifact_role, renders)
#> # A tibble: 3 × 4
#>   name               stage  artifact_role      renders
#>   <chr>              <chr>  <chr>              <lgl>  
#> 1 t_test_child       setup  helper_script      FALSE  
#> 2 t_test_example     report report_source      TRUE   
#> 3 t_test_example PDF report deliverable_report FALSE
```

## Sharing an interactive version

For a collaborator who wants to pan, zoom, and hover over individual
nodes rather than read a static image,
[`export_interactive_pipeline()`](https://jkylearmstrong.github.io/TempleCBE/reference/export_interactive_pipeline.md)
writes a self-contained HTML file (via `visNetwork`/`htmlwidgets`) with
the same staleness coloring and per-node tooltips:

``` r

export_interactive_pipeline(pipeline, file = "pipeline.html")
```

For pipelines large enough that the full per-file graph stops being
legible as one image,
[`collapse_by_stage()`](https://jkylearmstrong.github.io/TempleCBE/reference/collapse_by_stage.md)
reduces it to one node per `stage` first:

``` r

full_graph <- as_igraph(pipeline)
collapsed <- collapse_by_stage(full_graph)
igraph::vcount(full_graph)
#> [1] 3
igraph::vcount(collapsed)
#> [1] 2
```

## How others can use this technology

The `TempleCBE` compute graph is intentionally modular and can be
integrated into any data science or biostatistical workflow. Here are
five concrete patterns for adopting this technology in your own
projects:

### Pattern 1: Non-intrusive auditing of an existing multi-stage project

You can audit an existing repository without changing any analytical
code. Create a single file (e.g. `_pipeline.R` or `build_graph.R`) that
defines your files and their dependencies:

``` r

library(TempleCBE)

# 1. Configure study branding and stage definitions
pipeline_config(
  study_name = "Cardiology Clinical Trial Phase II",
  stage_labels = c(
    "01_intake"      = "Data Intake & Cleaning",
    "02_eda"         = "Exploratory Analysis",
    "03_imputation"  = "Missing Data Imputation",
    "04_modeling"    = "Survival & Cox Models",
    "05_deliverable" = "Final Regulatory Reports"
  ),
  stage_colors = list(
    "01_intake"      = "#EBF5FB",
    "02_eda"         = "#E8F8F5",
    "03_imputation"  = "#FEF9E7",
    "04_modeling"    = "#F4ECF7",
    "05_deliverable" = "#EAECEE"
  )
)

# 2. Define data inputs and multi-language scripts
raw_data <- FilePath(
  name = "patient_registry_xlsx",
  path = "data/raw/patient_registry_2024.xlsx",
  stage = "01_intake",
  artifact_role = "raw_data",
  description = "De-identified patient registry export from clinical site"
)

# A Python or SAS script that outputs an RDS
clean_script <- FileOutputs(
  name = "clean_data_py",
  path = "scripts/01_clean_data.py",
  dependencies = list(raw_data),
  output = list(FilePath(
    name = "cleaned_cohort_rds",
    path = "data/derived/cleaned_cohort.rds",
    stage = "01_intake",
    artifact_role = "derived_data"
  )),
  stage = "01_intake",
  artifact_role = "helper_script",
  description = "Python preprocessing script standardizing clinical biomarkers"
)

# 3. Define Quarto reports
eda_report <- create_qmd_renderer(
  name = "02_eda_report",
  path = "reports/02_eda_summary.qmd",
  deps = clean_script@output,
  file_stage = "02_eda",
  output_format = "pdf",
  description = "Primary missingness and distribution auditing deliverable"
)

# 4. Assemble the pipeline and inspect
study_pipeline <- list(raw_data, clean_script, eda_report)
pipeline_summary(study_pipeline)
```

You can also use
[`list_stage_data_artifacts()`](https://jkylearmstrong.github.io/TempleCBE/reference/list_stage_data_artifacts.md)
to scan directories and catalogue derived data files automatically:

``` r

# Inspect all data files in an imputation output folder
stage_artifacts <- list_stage_data_artifacts(
  stage_dir = "data/derived/imputation",
  stage = "03_imputation"
)
```

### Pattern 2: Delivering interactive DAGs to clinical collaborators

Non-technical investigators and sponsors frequently want to know where a
specific number or figure in a final report came from. You can export a
self-contained HTML file with rich interactive cards and search bars:

``` r

# Export full interactive HTML for stakeholder distribution
export_interactive_pipeline(
  study_pipeline,
  file = "deliverables/pipeline_lineage.html",
  title = "Study Data Lineage & Report Dependencies"
)

# Or export an ego-subgraph isolating a single report and its direct inputs
export_subgraph(
  study_pipeline,
  focal_node = "02_eda_report",
  order = 1,
  file = "deliverables/eda_subgraph.html"
)
```

### Pattern 3: Automated staleness-driven re-rendering

Instead of manually checking whether data files have changed or
re-rendering all reports from scratch, use
[`get_render_plan()`](https://jkylearmstrong.github.io/TempleCBE/reference/get_render_plan.md)
to drive selective rendering in a build script or CI/CD workflow:

``` r

# Detect out-of-date reports in topological dependency order
render_plan <- get_render_plan(study_pipeline)

if (length(render_plan) > 0) {
  for (report_obj in render_plan) {
    message("Rendering out-of-date report: ", report_obj@path)
    quarto::quarto_render(input = report_obj@path)
  }
} else {
  message("All reports are up-to-date with their underlying data.")
}
```

### Pattern 4: Relational pipeline governance with `tidygraph` and `dplyr`

Because
[`as_pipeline_graph()`](https://jkylearmstrong.github.io/TempleCBE/reference/as_pipeline_graph.md)
converts the pipeline directly to a
[`tidygraph::tbl_graph`](https://tidygraph.data-imaginist.com/reference/tbl_graph.html),
you can audit and query your workflow using standard tidyverse grammar:

``` r

library(tidygraph)
library(dplyr)

tg <- as_pipeline_graph(study_pipeline)

# List all reports that depend on a specific raw data file
upstream_data <- "patient_registry_xlsx"
downstream_reports <- tg |>
  activate(nodes) |>
  mutate(dist = node_distance_from(which(name == upstream_data), mode = "out")) |>
  filter(is.finite(dist), artifact_role == "deliverable_report") |>
  as_tibble()

# Audit unrendered or orphaned scripts across stages
orphans <- tg |>
  activate(nodes) |>
  filter(node_is_isolated()) |>
  as_tibble()
```

### Pattern 5: Multi-analyst pipeline composition

In large studies where different biostatisticians manage different
domains (e.g. one analyst leads clinical safety, another leads genomics,
and a third leads pharmacokinetics), each analyst can maintain their own
sub-pipeline. Use
[`join_pipelines()`](https://jkylearmstrong.github.io/TempleCBE/reference/join_pipelines.md)
to combine them into an overarching study graph without duplicating
shared nodes:

``` r

# Merge clinical and genomics pipelines
full_study_graph <- join_pipelines(clinical_subpipeline, genomics_subpipeline)

# Generate executive summary of combined study
pipeline_summary(full_study_graph)
```

------------------------------------------------------------------------

## Choosing between `targets` and `TempleCBE` (or combining both)

`targets` and `TempleCBE` solve fundamentally different problems and can
be chosen based on project needs:

### When to choose `targets`

- Your project is written predominantly in R functions.
- You have compute-intensive steps (e.g. fitting 50,000 MCMC models,
  large bootstrap resamples, complex machine learning cross-validation)
  where function-level caching saves hours of runtime.
- You need distributed computation across a high-performance computing
  cluster (SLURM, SGE, AWS) via `crew`.

### When to choose `TempleCBE`

- You are working in an existing project with established scripts and
  reports that cannot be refactored into pure functions.
- Your project spans multiple languages and tools (Excel, SAS, Python,
  Quarto, R).
- You need explicit clinical governance, tracking artifact roles
  (`raw_data`, `enhanced_data`, `deliverable_report`) and data
  provenance.
- You need to communicate the pipeline to clinical investigators,
  clients, or regulatory bodies via interactive HTML widgets or
  publication-quality figures.
- You want decoupled staleness detection without ceding execution
  control to an autonomous framework.

### Combining both in a hybrid architecture

`targets` and `TempleCBE` are not mutually exclusive. A common, highly
effective pattern in clinical data science is a **two-layer hybrid
architecture**:

1.  **Low-level compute layer (`targets`)**: Heavy statistical
    simulation, model tuning, or imputation calculations are managed by
    `targets` inside an `analysis/models/` subfolder, writing final
    analytical datasets or tables to disk
    (e.g. `data/imputed_data.rds`).
2.  **High-level governance layer (`TempleCBE`)**: `TempleCBE` tracks
    the overall study graph from raw intake data, through the
    `targets`-generated artifacts, to the final Quarto reports and
    deliverable PDFs, providing executive summaries, staleness planning,
    and stakeholder visualizations.
