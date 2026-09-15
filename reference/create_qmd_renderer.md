# Create a FileOutputs object for a rendering QMD

Create a FileOutputs object for a rendering QMD

## Usage

``` r
create_qmd_renderer(
  name,
  path,
  deps = list(),
  file_stage = NA_character_,
  output_format = "pdf",
  description = NA_character_
)
```

## Arguments

- name:

  The nickname for the file.

- path:

  The full path to the .qmd file.

- deps:

  A list of dependency objects (e.g., list(og_DATA)).

- file_stage:

  The 'stage' for this file.

- output_format:

  "pdf" or "html". Default is "pdf".

- description:

  A brief summary of the document.

## Value

A FileOutputs object.
