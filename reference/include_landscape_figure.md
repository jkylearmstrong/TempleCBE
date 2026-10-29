# Include a Figure on a Landscape Page (PDF Only)

For a Quarto or R Markdown chunk with \`results = "asis"\`. When the
document is rendered to PDF the figure is placed on its own landscape
page; in HTML, Word and other formats it is a normal figure and no LaTeX
is written, so nothing leaks into the output.

## Usage

``` r
include_landscape_figure(
  path,
  caption = NULL,
  label = NULL,
  width = "100%",
  environment = c("landscape", "blandscape"),
  format = NULL
)
```

## Arguments

- path:

  Path to the image file, relative to the document.

- caption:

  Optional caption text. Square brackets in it are escaped.

- label:

  Optional cross-reference id (for Quarto use a \`"fig-"\` prefix, for
  example \`"fig-flow"\`).

- width:

  Image width as a percentage or length (default \`"100%"\`).

- environment:

  \`"landscape"\` (default) or \`"blandscape"\`; see PDF requirements.

- format:

  Output format override: \`"latex"\`, \`"html"\`, \`"docx"\` or
  \`"other"\`. By default it is read from knitr.

## Value

A string of class \`knit_asis\` (printed by knitr as raw markdown); with
\`format = NULL\` outside knitr the format is \`"other"\`.

## PDF requirements

The default \`environment = "landscape"\` uses the \`landscape\`
environment of the LaTeX package pdflscape, so the document must load
that package (in Quarto, under \`header-includes\`). With \`environment
= "blandscape"\` the figure is wrapped in a begin/end macro pair named
blandscape and elandscape that your own preamble defines. The function
does not add packages to your document.

## Examples

``` r
cat(include_landscape_figure("fig/flow.png", caption = "Flow", label = "fig-flow", format = "latex"))
#> 
#> 
#> ```{=latex}
#> \begin{landscape}
#> ```
#> 
#> ![Flow](fig/flow.png){#fig-flow width="100%"}
#> 
#> ```{=latex}
#> \end{landscape}
#> ```
#> 
cat(include_landscape_figure("fig/flow.png", caption = "Flow", format = "html"))
#> 
#> 
#> ![Flow](fig/flow.png){width="100%"}
#> 
```
