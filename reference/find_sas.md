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

sas_available()
```

## Value

`find_sas()`: the path to the SAS executable, or `NULL` if none is
found. `sas_available()`: `TRUE` or `FALSE`.

## Details

`sas_available()` is `!is.null(find_sas())`. Guard code that needs SAS
with it (for example `eval = TempleCBE::sas_available()` on a vignette
chunk), so that code runs only on machines that have SAS.

## Package tests

The package's tests never need SAS: they compare R with reference values
that SAS produced once, stored in `tests/testthat/reference/`. A second
set of tests runs the real SAS programs and compares their listings with
those references. These are opt-in, so that a machine that happens to
have SAS does not start slow jobs by accident. Set the environment
variable `TEMPLECBE_RUN_SAS_TESTS=true` (and make SAS findable as above)
to run them. The same holds for real PDF to DOCX conversions, which need
`TEMPLECBE_RUN_PDF_TESTS=true`; see
[`find_python()`](https://jkylearmstrong.github.io/TempleCBE/reference/find_python.md).

## See also

\[run_sas_script()\], \[cbe_sas_macro_path()\], \[cbe_sas_macro_dir()\]

## Examples

``` r
find_sas()
#> NULL
sas_available()
#> [1] FALSE
```
