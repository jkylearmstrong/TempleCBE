# Render Quarto Documents to Multiple Formats With Timing

Renders one or more Quarto (`.qmd`) documents to multiple output formats
(by default both PDF and DOCX) sequentially or in parallel, recording
execution duration for each document/format combination. Useful for
workflows where reviewers need a Word (`.docx`) file for track
changes/edits while publication and archival copies need PDF.

## Usage

``` r
render_me(
  path,
  formats = c("pdf", "docx"),
  pattern = "\\.qmd$",
  workers = NULL
)
```

## Arguments

- path:

  Character string specifying a path to a `.qmd` file, or a directory
  containing `.qmd` files.

- formats:

  Character vector of output formats to generate. Defaults to
  `c("pdf", "docx")`.

- pattern:

  Optional regular expression to filter files when `path` is a
  directory. Defaults to `"\.qmd$"`.

- workers:

  Integer specifying the number of multisession parallel workers when
  rendering multiple documents. Defaults to `NULL`, which caps at
  available cores minus one. Ignored (rendering falls back to
  sequential) when the future/furrr packages are not installed.

## Value

A data frame containing:

- file:

  Base filename of the rendered document

- output:

  Output format (e.g., "pdf", "docx")

- duration:

  Execution time in seconds

- status:

  "success" or error condition message

## Examples

``` r
if (FALSE) { # \dontrun{
# Render a single report to both PDF and DOCX
render_me(here::here("analysis", "report.qmd"))

# Render all QMD reports in a directory in parallel
render_me("analysis/")
} # }
```
