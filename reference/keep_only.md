# Keep Only Specified Objects in an Environment

Removes every object in the caller's environment except the ones named
in `vector`. Prompts for confirmation in interactive sessions unless
`.dontask = TRUE`; proceeds without prompting in non-interactive
sessions (scripts, `R CMD check`, knitr rendering), since
[`readline()`](https://rdrr.io/r/base/readline.html) would otherwise
hang there.

## Usage

``` r
keep_only(vector, .dontask = FALSE)
```

## Arguments

- vector:

  Character vector of object names to keep.

- .dontask:

  Logical (default `FALSE`); skip the confirmation prompt.

## Value

Invisibly, `NULL`.

## Examples

``` r
e <- new.env()
local({a <- 1; b <- 2; keep_only("a", .dontask = TRUE)}, envir = e)
#> Removing objects:
#>   b
ls(e)
#> [1] "a"
```
