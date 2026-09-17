# Extract Output Formats From Document YAML Front Matter

Inspects the YAML header of a Quarto (`.qmd`) or R Markdown (`.Rmd`)
file and returns all declared output formats (e.g. `"html"`, `"pdf"`,
`"docx"`, `"gfm"`).

## Usage

``` r
extract_yaml_formats(file_path)
```

## Arguments

- file_path:

  Path to the `.qmd` or `.Rmd` document.

## Value

Character vector of lowercase format names, or empty character vector if
none found.
