# Guard Against Inactive renv or Lockfile Disagreements

Checks whether an \`renv.lock\` exists in the project root or ancestors
and warns if \`renv\` is inactive (which commonly occurs when Quarto
renders from a subfolder, skipping the project-root \`.Rprofile\`), or
if any currently loaded package versions disagree with the lockfile
definitions.

## Usage

``` r
check_renv_guard(project_root = NULL)
```

## Arguments

- project_root:

  Optional project root directory. Defaults to \[here::here()\] if
  available, or the current working directory.

## Value

Logical \`TRUE\` if clean or not using renv, \`FALSE\` if a warning was
emitted.
