# Locate a Python Interpreter That Can Import a Module

Search order (first verified hit wins):

1.  `getOption("templecbe.python")`

2.  `Sys.getenv("TEMPLECBE_PYTHON")`

3.  `Sys.getenv("RETICULATE_PYTHON")` and, only when reticulate has
    already started Python in this session,
    [`reticulate::py_exe()`](https://rstudio.github.io/reticulate/reference/py_exe.html)

4.  `Sys.which("python3")`, `Sys.which("python")`

5.  Platform-specific well-known locations

## Usage

``` r
find_python(verify = TRUE, module = "pdf2docx")
```

## Arguments

- verify:

  If `TRUE`, require `import <module>` to succeed.

- module:

  Name of the Python module an interpreter must be able to import when
  `verify = TRUE`: a plain, possibly dotted, module name. Defaults to
  `"pdf2docx"`.

## Value

Path to a usable interpreter, or `NULL`.

## Details

Each candidate is verified by actually running it, so Windows App
Execution Alias stubs (which resolve on PATH but do nothing) are
rejected. A probe that takes longer than 60 seconds (running
`--version`, or importing `module`) counts as a failure, so one hung
interpreter cannot block the search. When `verify = TRUE` the candidate
must additionally be able to `import` `module`. Candidates are probed in
a separate process: this function never starts Python inside R, and
never starts reticulate's Python, so it is safe to use as a guard before
code that would (for example
`eval = !is.null(find_python(module = "numpy"))` on a vignette chunk).
Install the pinned Python requirements for PDF conversion with
`pip install -r` on
`system.file("python", "requirements.txt", package = "TempleCBE")`.

The tests that run a real PDF to DOCX conversion (Python, LibreOffice or
Word) are opt-in: set the environment variable
`TEMPLECBE_RUN_PDF_TESTS=true` to run them. See also
[`find_sas()`](https://jkylearmstrong.github.io/TempleCBE/reference/find_sas.md)
for `TEMPLECBE_RUN_SAS_TESTS`.

## See also

\[check_docx_toolchain()\]
