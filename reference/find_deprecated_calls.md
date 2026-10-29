# Find and Update Deprecated Calls Across Project Code

\`find_deprecated_calls()\` inspects code files and reports all
occurrences of deprecated TempleCBE functions removed in TempleCBE 0.7.0
(this tool finds them in your own code).

\`update_deprecated_calls()\` previews or applies in-place renames to
update both package-qualified calls (\`TempleCBE::old()\`,
\`TempleCBE:::old()\`) and unqualified calls (\`old()\`). Only code is
read: string literals and comments are ignored, and in \`.Rmd\`/\`.qmd\`
files only the R chunks. Method calls (\`obj\$render()\`) are ignored.

## Usage

``` r
find_deprecated_calls(path, exclude_dirs = NULL)

update_deprecated_calls(path, dry_run = TRUE, exclude_dirs = NULL)
```

## Arguments

- path:

  Path to a source file or directory of project scripts.

- exclude_dirs:

  Optional character vector of directory names (for example
  \`c("ARCHIVE", "renv", "doc")\`) to skip when \`path\` is a directory;
  a file is skipped if any folder on its path below \`path\` has one of
  these names.

- dry_run:

  Logical; if \`TRUE\` (default), only previews the proposed renames
  without modifying files. If \`FALSE\`, rewrites files in place.

## Value

A \`tibble\` with columns:

- file:

  Path to the file where the deprecated call was detected.

- line:

  1-based line number.

- old_name:

  Name of the deprecated function.

- replacement:

  Canonical replacement function name.

- call_type:

  \`"qualified"\` (\`TempleCBE::old()\`), \`"unqualified"\` (\`old()\`),
  or \`"ambiguous"\`: an unqualified \`clean_names()\`, \`render()\`,
  \`like()\` or \`write_xlsx()\` in a file that uses TempleCBE. Other
  packages export these names too, so \`update_deprecated_calls()\`
  never rewrites them.

- context:

  The original source code line.

## Details

Scans source files (\`.R\`, \`.Rmd\`, \`.qmd\`) in a project directory
or script for calls to deprecated TempleCBE functions and replaces them
with their canonical names.

## Examples

``` r
# \donttest{
tmp <- tempfile(fileext = ".R")
writeLines(c(
  "x <- TempleCBE::sd.error(c(1, 2, 3))",
  "y <- keep_only(c('a', 'b'))"
), tmp)
find_deprecated_calls(tmp)
#> # A tibble: 2 × 6
#>   file                               line old_name replacement call_type context
#>   <chr>                             <int> <chr>    <chr>       <chr>     <chr>  
#> 1 /tmp/RtmppxIjyF/file2188eaf0fdc.R     1 sd.error std_error   qualified x <- T…
#> 2 /tmp/RtmppxIjyF/file2188eaf0fdc.R     2 keep_on… rm_all_exc… unqualif… y <- k…
update_deprecated_calls(tmp, dry_run = TRUE)
#> Dry run: Found 2 deprecated call(s) across 1 file(s). Set dry_run = FALSE to apply replacements.
#> # A tibble: 2 × 6
#>   file                               line old_name replacement call_type context
#>   <chr>                             <int> <chr>    <chr>       <chr>     <chr>  
#> 1 /tmp/RtmppxIjyF/file2188eaf0fdc.R     1 sd.error std_error   qualified x <- T…
#> 2 /tmp/RtmppxIjyF/file2188eaf0fdc.R     2 keep_on… rm_all_exc… unqualif… y <- k…
update_deprecated_calls(tmp, dry_run = FALSE)
#>   - file2188eaf0fdc.R: 2 call(s) updated
#> Updated 2 deprecated call(s) across 1 file(s).
readLines(tmp)
#> [1] "x <- TempleCBE::std_error(c(1, 2, 3))"
#> [2] "y <- rm_all_except(c('a', 'b'))"      
unlink(tmp)
# }
```
