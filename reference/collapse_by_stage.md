# Collapse an igraph pipeline graph to one node per pipeline \`stage\`

The full per-file compute graph (~90+ nodes) is the right level of
detail for the interactive HTML, where a reader can zoom and pan – but
it cannot be made legible as a single static image for the PDF/DOCX
deliverable at any reasonable page size. This produces a bird's-eye
version instead: one node per \`stage\`, with an edge between two stages
whenever any file in the first stage feeds any file in the second
(parallel edges collapsed).

## Usage

``` r
collapse_by_stage(g, stage_labels = NULL)
```

## Arguments

- g:

  An igraph object (see as_igraph()).

- stage_labels:

  Optional named character vector mapping stage identifiers to display
  labels.

## Value

An igraph object with one node per stage.
