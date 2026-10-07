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

## Details

`vector` must be a character vector of object names, so the names are
quoted: `keep_only(c("a", "b"))`, not `keep_only(c(a, b))`. Every name
has to be an object of the calling environment itself (not of one above
it); otherwise the function stops before it removes anything. A misspelt
name used to pass unnoticed and remove every object, the one that was
meant to be kept included.

Called from inside a function, `keep_only()` works on that function's
own environment, so the function's arguments are removed too unless they
are named in `vector`.

## Examples

``` r
e <- new.env()
local({a <- 1; b <- 2; keep_only("a", .dontask = TRUE)}, envir = e)
#> Removing objects:
#>   b
ls(e)
#> [1] "a"
```
