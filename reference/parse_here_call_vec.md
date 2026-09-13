# Resolve \`here::here()\` Calls Found in Code Text

For each string, finds the first \`here::here(...)\` call and evaluates
it with its quoted string arguments, giving the path the call builds in
the current project. Calls whose arguments are not all string literals
resolve from the literals alone.

## Usage

``` r
parse_here_call_vec(x)
```

## Arguments

- x:

  Character vector of code lines.

## Value

A character vector the length of \`x\`: the resolved path, or \`NA\`
where a line has no \`here::here()\` call with quoted arguments.

## See also

\[scan_data_io()\], \[find_code()\]

## Examples

``` r
parse_here_call_vec(c("x <- readRDS(here::here('data', 'x.rds'))", "no call"))
#> [1] "/home/runner/work/TempleCBE/TempleCBE/data/x.rds"
#> [2] NA                                                
```
