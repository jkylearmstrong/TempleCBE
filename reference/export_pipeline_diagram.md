# Export Pipeline Diagram to Static Vector or Raster Format

Exports a computational pipeline dependency graph to a static file (SVG,
PDF, or PNG) formatted and sized appropriately for reports and Word
documents (e.g. 6.5 x 9.0 inches for a standard portrait Word page with
1-inch margins).

## Usage

``` r
export_pipeline_diagram(
  all_objects,
  file,
  format = c("auto", "svg", "pdf", "png"),
  width = 6.5,
  height = 9,
  orientation = c("TB", "LR", "BT", "RL"),
  font_size = 10,
  node_size = NULL,
  stage_colors = NULL
)
```

## Arguments

- all_objects:

  A list of pipeline objects (\`FilePath\`, \`FileUses\`,
  \`FileOutputs\`).

- file:

  Character path to output file.

- format:

  Target format: \`"auto"\` (inferred from \`file\` extension),
  \`"svg"\`, \`"pdf"\`, or \`"png"\`.

- width:

  Page width in inches (default 6.5, fitting standard letter with 1-inch
  margins).

- height:

  Page height in inches (default 9.0).

- orientation:

  Layout orientation: \`"TB"\` (top-to-bottom, default) or \`"LR"\`
  (left-to-right).

- font_size:

  Font size in points (default 10 for print readability).

- node_size:

  Optional numeric vector of node dimensions \`c(width, height)\`.

- stage_colors:

  Optional named list of stage colors.

## Value

Invisibly returns the output \`file\` path.
