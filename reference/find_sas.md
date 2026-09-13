# Locate a SAS Executable

Checks, in order: \`getOption("templecbe.sas")\`, the \`SAS_EXE\`
environment variable, \`sas\` on the \`PATH\`, and the default SAS 9
install locations (\`SASHome/SASFoundation/\<version\>\` under
\`C:/Program Files\` or \`C:/Program Files (x86)\` on Windows; under
\`/usr/local/SASHome\`, \`/opt/sas/SASHome\`, or \`/opt/SASHome\`
elsewhere), preferring the highest installed version.

## Usage

``` r
find_sas()
```

## Value

The path to the SAS executable, or \`NULL\` if none is found.

## See also

\[run_sas_script()\]

## Examples

``` r
find_sas()
#> NULL
```
