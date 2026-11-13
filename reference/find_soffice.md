# Locate a LibreOffice Headless Binary

Honours `getOption("templecbe.soffice")` and the `TEMPLECBE_SOFFICE`
environment variable before searching the `PATH` and default install
locations.

## Usage

``` r
find_soffice()
```

## Value

Path to `soffice`/`libreoffice`, or `NULL`.

## See also

\[check_docx_toolchain()\]
