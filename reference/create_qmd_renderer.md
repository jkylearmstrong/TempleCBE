# Create a FileOutputs object for a rendering QMD or R Markdown document

Create a FileOutputs object for a rendering QMD or R Markdown document

## Usage

``` r
create_qmd_renderer(
  name,
  path,
  deps = list(),
  file_stage = NA_character_,
  output_format = getOption("pipeline.default_formats", "pdf"),
  description = NA_character_
)
```

## Arguments

- name:

  The nickname for the file.

- path:

  The full path to the .qmd or .Rmd file.

- deps:

  A list of dependency objects (e.g., list(og_DATA)).

- file_stage:

  The 'stage' for this file.

- output_format:

  Output format(s): e.g. "pdf", "html", `c("pdf", "docx")`, or `"yaml"`
  to automatically extract the output formats declared in the document's
  YAML frontmatter. Defaults to
  `getOption("pipeline.default_formats", "pdf")`. Each output file gets
  the extension its format writes, which is not always the format's
  name: `revealjs`, `ioslides`, `slidy` and `html_document` write
  `.html`, `beamer` and `typst` write `.pdf`, `gfm` and
  `github_document` write `.md`. A format added by an extension
  (`<extension>-<base format>`, e.g. `temple-html`) writes its base
  format's file. For a format whose extension is not known the format
  name is used, with a warning.

- description:

  A brief summary of the document.

## Value

A FileOutputs object.
