# Export a Fitted Model, Survival Matrix, or Data Dictionary to Python

Writes a fitted TempleCBE model (or a data frame / survival matrix /
data dictionary) to a directory of plain files a Python session can read
with no R runtime: CSV tables, a JSON manifest and model metadata, and a
small Python loader script (\`\<name\>\_load.py\`) that reads them back
with the standard library, or with \`pandas\` when it is installed. This
is the package function behind README gap area 4 ("Python & Reticulate
Cross-Language ML"): before it, the only Python-facing exports were
\[find_python()\] (a locator for the PDF converter),
\`adapter_tidymodels_survex()\` and the DALEX summarizers, and the
reticulate vignette showed only raw user code calling Python from R.

## Usage

``` r
export_to_python(
  object,
  path,
  name = "templecbe_export",
  eval_time = NULL,
  new_data = NULL,
  penalty = NULL,
  dictionary = NULL,
  format = c("files", "pickle"),
  overwrite = FALSE
)

# S3 method for class 'python_export'
print(x, ...)
```

## Arguments

- object:

  The object to export: a data frame, a matrix, a \`coxnet_model\`, a
  \`cv_coxnet\`, a \`joint_model\`, or a named list.

- path:

  Directory the export is written to. Created if needed.

- name:

  File-name stem for the artifacts (default \`"templecbe_export"\`).
  Must be a single string without a path separator.

- eval_time, new_data:

  Passed to \[predict()\] for \`coxnet_model\` / \`cv_coxnet\` /
  \`joint_model\` objects to also export a survival matrix. Both must be
  given together; otherwise no survival matrix is written. \`eval_time\`
  must be finite, non-negative, sorted and free of duplicates.

- penalty:

  Penalty passed to \[generics::tidy()\] / \[predict()\] for
  \`coxnet_model\` objects. Defaults to the model's own penalty.

- dictionary:

  Optional data frame (a column dictionary) to export alongside
  \`object\`, as \`dictionary.csv\` and \`dictionary.json\`.

- format:

  \`"files"\` (default) or \`"pickle"\` (additionally save a
  \`reticulate\` pickle; see the Reticulate pickle format section).

- overwrite:

  If \`FALSE\` (default), refuse to write into a directory that already
  holds an export with the same \`name\`.

- x:

  The object to print (the result of the function).

- ...:

  Not used.

## Value

Invisibly, a list of class \`python_export\` with \`path\`, \`name\`,
\`object_class\`, \`format\` and the \`files\` written (absolute paths).

## What is exported

\* A data frame or matrix: \`\<name\>\_data.csv\`. \* A \`coxnet_model\`
(\[coxnet()\]): \`coefficients.csv\` (the \[generics::tidy()\]
coefficients at \`penalty\`), \`model.json\` (penalty, mixture,
predictor names, outcome type), and, when \`new_data\` and \`eval_time\`
are given, \`survival.csv\` (one row per subject, one column per
evaluation time). \* A \`cv_coxnet\` object: coefficients at
\`"lambda.min"\`, \`metrics.csv\` (the cross-validated metrics, when
present), and \`model.json\`. \* A \`joint_model\`: the
\[generics::tidy()\] coefficients and \`model.json\` (component model
classes); a survival matrix is added when \`new_data\` and \`eval_time\`
are given, from the \`.pred_survival\` list-column of
\[predict.joint_model()\]. \* A named list: each data-frame element as
\`\<name\>\_\<element\>.csv\` and the whole list as \`\<name\>.json\`.
\* A data dictionary passed as \`dictionary\` (a data frame, for example
the \`matched\` table of \[reconcile_column_dictionary()\]):
\`dictionary.csv\` and \`dictionary.json\`.

Every export also writes \`manifest.json\` (package and R versions,
object class, file list, creation time) and \`\<name\>\_load.py\`. The
loader exposes \`load_coefficients()\`, \`load_survival()\`,
\`load_dictionary()\` and \`load_manifest()\`, each returning \`pandas\`
DataFrames when \`pandas\` is importable and plain lists of dicts
otherwise, and \`load_data()\` only when a data file was actually
exported (a data frame or matrix object): model exports write no
\`\<name\>\_data.csv\`, so their loaders do not define it instead of
raising \`FileNotFoundError\`.

## Reticulate pickle format

With \`format = "pickle"\`, the same payload is additionally saved as
\`\<name\>.pkl\` with \[reticulate::py_save_object()\], which needs the
reticulate package (a \`Suggests\` dependency, not installed
everywhere). The plain files are always written, so a pickle failure
never loses the export; when \`reticulate\` is absent the function stops
with a clear message instead of writing a partial pickle. A pickle is
Python-version sensitive and opaque to other tools, so prefer the
default \`"files"\` format for artifacts that must be inspected,
versioned, or read outside Python.

## See also

\[find_python()\], \[reconcile_column_dictionary()\],
\[adapter_tidymodels_survex()\]

## Examples

``` r
# \donttest{
out <- export_to_python(head(mtcars), path = tempfile("pyexp_"), name = "demo")
out$files
#> [1] "/tmp/RtmpMhtxuZ/pyexp_22e65229ad0/demo_data.csv"     
#> [2] "/tmp/RtmpMhtxuZ/pyexp_22e65229ad0/demo_load.py"      
#> [3] "/tmp/RtmpMhtxuZ/pyexp_22e65229ad0/demo_manifest.json"
# The loader is plain Python; read it back without reticulate:
cat(readLines(file.path(out$path, "demo_load.py")), sep = "\n")
#> """Loader for the TempleCBE Python export "demo".
#> 
#> Reads the CSV/JSON artifacts written by TempleCBE::export_to_python()
#> from the directory this file lives in. pandas is used when available;
#> otherwise each loader returns a plain list of dicts.
#> """
#> 
#> import csv
#> import json
#> from pathlib import Path
#> 
#> _HERE = Path(__file__).resolve().parent
#> _STEM = "demo"
#> 
#> try:
#>     import pandas as _pd
#> except ImportError:  # pragma: no cover - pandas is optional
#>     _pd = None
#> 
#> 
#> def _read_csv(filename):
#>     path = _HERE / filename
#>     if _pd is not None:
#>         return _pd.read_csv(path)
#>     with path.open(newline="") as fh:
#>         return list(csv.DictReader(fh))
#> 
#> 
#> def _read_json(filename):
#>     with (_HERE / filename).open() as fh:
#>         return json.load(fh)
#> 
#> 
#> def load_data():
#>     """The exported data frame (<stem>_data.csv)."""
#>     return _read_csv(f"{_STEM}_data.csv")
#> 
#> 
#> def load_coefficients():
#>     """Fitted-model coefficients (<stem>_coefficients.csv)."""
#>     return _read_csv(f"{_STEM}_coefficients.csv")
#> 
#> 
#> def load_survival():
#>     """Survival probabilities (<stem>_survival.csv)."""
#>     return _read_csv(f"{_STEM}_survival.csv")
#> 
#> 
#> def load_dictionary():
#>     """The exported data dictionary (<stem>_dictionary.csv)."""
#>     return _read_csv(f"{_STEM}_dictionary.csv")
#> 
#> 
#> def load_manifest():
#>     """Export metadata (<stem>_manifest.json)."""
#>     return _read_json(f"{_STEM}_manifest.json")
#> 
# }
```
