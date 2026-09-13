# Locate a Python Interpreter That Can Import pdf2docx

Search order (first verified hit wins):

1.  `getOption("templecbe.python")`

2.  `Sys.getenv("TEMPLECBE_PYTHON")`

3.  `Sys.getenv("RETICULATE_PYTHON")` and
    [`reticulate::py_exe()`](https://rstudio.github.io/reticulate/reference/py_exe.html)

4.  `Sys.which("python3")`, `Sys.which("python")`

5.  Platform-specific well-known locations

## Usage

``` r
find_python(verify = TRUE)
```

## Arguments

- verify:

  If `TRUE`, require `import pdf2docx` to succeed.

## Value

Path to a usable interpreter, or `NULL`.

## Details

Each candidate is verified by actually running it, so Windows App
Execution Alias stubs (which resolve on PATH but do nothing) are
rejected. When `verify = TRUE` the candidate must additionally be able
to `import pdf2docx`. Install the pinned Python requirements with
`pip install -r` on
`system.file("python", "requirements.txt", package = "TempleCBE")`.

## See also

\[check_docx_toolchain()\]
