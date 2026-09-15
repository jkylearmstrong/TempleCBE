# Print the dependency graph as a PNG image

Renders via igraph/ggraph with a layered (Sugiyama) DAG layout and
canvas sizing adapted to the graph's own node density, rather than the
previous Graphviz/DiagrammeR renderer, which laid large graphs out
extremely wide and flat (e.g. the full compute graph rendered at
9019x1140px, with text too small to read and the arrows between boxes
impossible to follow).

## Usage

``` r
print_pipeline(
  objects,
  path,
  drop_stages = character(0),
  drop_edges_to = character(0),
  title = NULL,
  base_font_size = 9,
  label_wrap_width = 18,
  min_width_in = 8,
  min_height_in = 3,
  max_width_in = 40,
  width_per_node_in = 2.1,
  dpi = 200
)
```

## Arguments

- objects:

  The full list of FilePath, FileUses, and FileOutputs objects, or an
  already-built igraph object (see as_igraph()).

- path:

  The path where the PNG image will be saved.

- drop_stages:

  Character vector of \`stage\` values to omit entirely (node and its
  edges). Use "output_report" on a crowded overview graph to hide the
  "\<X\> PDF"/"\<X\> DOCX" leaf nodes, which add little beyond what
  their parent QMD node already conveys.

- drop_edges_to:

  Character vector of node names whose \*incoming\* edges should be
  omitted, while keeping the node itself visible. Use this for a node
  like an introduction/summary report that every other report is wired
  to depend on purely to force it to render last, not because of a real
  analytical dependency – dropping just the edges avoids a dense fan-in
  that conveys no real relationship.

- title:

  Optional title drawn on the plot itself. Leave NULL when the image is
  embedded with its own Quarto fig-cap, to avoid a duplicate- looking
  caption.
