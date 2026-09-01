# Scaffold a New Report From a Template

Copies a bundled report template (and its supporting bibliography/title
files) into `location`.

## Usage

``` r
create_report(
  location = getwd(),
  template_name = "t_test_example",
  child = TRUE,
  type = ".qmd",
  include_bib = TRUE,
  include_tex = TRUE
)
```

## Arguments

- location:

  Directory to create the report in (default
  [`getwd()`](https://rdrr.io/r/base/getwd.html)).

- template_name:

  One of `"t_test_example"` (default) or `"example"`.

- child:

  Logical (default `TRUE`); also copy the child-document template.

- type:

  One of `".qmd"` (default) or `".Rmd"`.

- include_bib:

  Logical (default `TRUE`); also copy the `.bib` file.

- include_tex:

  Logical (default `TRUE`); also copy the title `.tex` file.

## Value

A list indicating whether each file was created.

## Examples

``` r
if (FALSE) { # \dontrun{
create_report(here::here("analysis"))
} # }
```
