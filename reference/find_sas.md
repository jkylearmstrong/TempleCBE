# Locate a SAS Executable

Checks, in order: \`getOption("templecbe.sas")\`, the \`SAS_EXE\`
environment variable, the \`SASROOT\` environment variable
(\`sas.exe\`), \`sas\` on the \`PATH\`, and default SAS install
locations (\`SASHome/SASFoundation/\<version\>\` under \`C:/Program
Files\`, \`C:/Program Files (x86)\`, \`C:\`, \`D:/Program Files\`, or
\`D:\` on Windows; under \`/usr/local/SASHome\`, \`/opt/sas/SASHome\`,
or \`/opt/SASHome\` elsewhere), preferring the highest installed
version.

## Usage

``` r
find_sas()
```

## Value

The path to the SAS executable, or \`NULL\` if none is found.

## See also

\[run_sas_script()\], \[cbe_sas_macro_path()\], \[cbe_sas_macro_dir()\]

## Examples

``` r
find_sas()
#> NULL
```
