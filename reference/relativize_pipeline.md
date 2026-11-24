# Portable Pipeline Paths: Relative to a Project Root, and Back

A pipeline built with \`here::here(...)\` paths stores absolute, machine
specific paths (\`S:/Clinical Science/...\` on Windows, \`/home/...\` on
Linux), so a saved pipeline (an \`all_file_objects.rds\`) found none of
its files on the other machine and had to be rebuilt.
\`relativize_pipeline()\` rewrites every path under \`root\` relative to
it; \`relocate_pipeline()\` puts relative paths back under \`root\`
(and, with \`from\`, moves absolute paths from an old root to the new
one), then refreshes the timestamps from disk, so \[get_render_plan()\]
and \[render_pipeline()\] see the files on this machine.
\[save_pipeline()\] and \[load_pipeline()\] do both when given \`root\`.

## Usage

``` r
relativize_pipeline(x, root = here::here())

relocate_pipeline(x, root = here::here(), from = NULL, refresh = TRUE)
```

## Arguments

- x:

  A pipeline object (\[FilePath\], \[FileUses\], \[FileOutputs\]) or a
  list of them. Dependencies and outputs are rewritten too.

- root:

  The project root; default \`here::here()\`.

- from:

  Optional character vector of old project roots: absolute paths under
  one of them are moved under \`root\`.

- refresh:

  Re-read each file's modification time from disk afterwards (default
  \`TRUE\`; see \[refresh_pipeline()\]).

## Value

\`x\`, with rewritten paths.

## Details

Paths outside \`root\` are left as they are. Separators are compared as
\`/\`, and a Windows drive path is matched without regard to case.

## See also

\[save_pipeline()\], \[load_pipeline()\], \[refresh_pipeline()\]

## Examples

``` r
f <- FilePath("data", "S:/proj/data/raw.xlsx")
rel <- relativize_pipeline(f, root = "S:/proj")
rel@path
#> [1] "data/raw.xlsx"
relocate_pipeline(rel, root = "/home/me/proj", refresh = FALSE)@path
#> [1] "/home/me/proj/data/raw.xlsx"
relocate_pipeline(f, root = "/home/me/proj", from = "S:/proj", refresh = FALSE)@path
#> [1] "/home/me/proj/data/raw.xlsx"
```
