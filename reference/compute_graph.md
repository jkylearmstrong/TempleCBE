# Computational Pipeline Dependency Graphs

S4 classes and helper functions for tracking file-level dependencies in
an analysis pipeline (raw data, rendering scripts, derived artifacts)
and for visualizing, summarizing, and topologically sorting the
resulting dependency graph. Language-, format-, and project-agnostic: a
node is any file on disk (R, Python, SAS, Quarto, spreadsheets, ...),
identified by path rather than tied to a specific tool.

## Details

The graph engine (igraph/tidygraph) is a hard dependency. Three
rendering backends are optional (Suggests) and only required by the
functions that use them: static PNG export
([`print_pipeline`](https://jkylearmstrong.github.io/TempleCBE/reference/print_pipeline.md))
needs ggraph; the interactive Graphviz view
([`visualize_pipeline`](https://jkylearmstrong.github.io/TempleCBE/reference/visualize_pipeline.md))
needs DiagrammeR; and the interactive HTML export
([`visualize_pipeline_interactive`](https://jkylearmstrong.github.io/TempleCBE/reference/visualize_pipeline_interactive.md),
[`export_interactive_pipeline`](https://jkylearmstrong.github.io/TempleCBE/reference/export_interactive_pipeline.md),
[`export_subgraph`](https://jkylearmstrong.github.io/TempleCBE/reference/export_subgraph.md))
needs visNetwork and htmlwidgets.
