# Run a SAS Program in Batch Mode

Runs a \`.sas\` file with the SAS executable, writing its log and
listing to separate folders next to the program (created if needed).

## Usage

``` r
run_sas_script(
  path,
  sas_path = find_sas(),
  log_dir = file.path(dirname(path), "logs"),
  list_dir = file.path(dirname(path), "list"),
  timeout = 0
)
```

## Arguments

- path:

  Path to the \`.sas\` program.

- sas_path:

  Path to the SAS executable. Defaults to \[find_sas()\].

- log_dir:

  Folder for the \`.log\` file. Defaults to \`logs/\` beside \`path\`.

- list_dir:

  Folder for the \`.lst\` listing. Defaults to \`list/\` beside
  \`path\`.

- timeout:

  Seconds SAS may run before it is stopped, or \`0\` (the default) for
  no limit. A run that is stopped is an error that names the log, which
  holds what SAS had written by then. Only the SAS process that was
  started is stopped.

## Value

The exit status of the SAS process (0 on success, 1 for warnings, 2 for
errors). A run that is stopped by \`timeout\` is an error.

## See also

\[find_sas()\], \[cbe_sas_macro_path()\]

## Examples

``` r
if (FALSE) { # \dontrun{
programs <- list.files("validation", pattern = "\\.sas$", full.names = TRUE)
vapply(programs, run_sas_script, integer(1))
} # }
```
