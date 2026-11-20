# Convert Raw Variable Names to Human-Readable Display Labels

Converts snake_case, camelCase, or dot-delimited variable names into
clean, title-cased labels suitable for tables and figures. Preserves
common clinical acronyms (e.g. BMI, ID, IBS, HR, SD, IQR, CBE, RNA,
DNA).

## Usage

``` r
pretty_name(
  x,
  acronyms = c("BMI", "ID", "IBS", "HR", "SD", "IQR", "CBE", "RNA", "DNA", "CV", "AUC",
    "ROC", "IPCW"),
  dict = NULL,
  form = c("short", "full", "unit")
)
```

## Arguments

- x:

  Character vector of variable names.

- acronyms:

  Optional character vector of terms to keep uppercase.

- dict:

  Optional label dictionary: a data frame with a term column (\`term\`,
  \`variable\`, \`name\`, ...) and any of \`short\`, \`full\` and
  \`unit\` columns, or a named character vector (\`c(raw_name =
  "Label")\`). Names found in the dictionary take its label; the others
  fall back to the automatic title-cased name, so a partial dictionary
  is fine.

- form:

  Which dictionary label to return: \`"short"\` (default; for crowded
  figures, falling back to the full label), \`"full"\` (for tables and
  prose, falling back to the short label) or \`"unit"\` (the full label
  followed by the unit in parentheses, when the dictionary has one and
  the label does not already end with it). Ignored without \`dict\`.

## Value

Character vector of formatted display labels.

## See also

\[tbl_abbreviations_glossary()\], \[cbe_labels()\]

## Examples

``` r
pretty_name(c("patient_bmi", "follow_up_time", "hr_ratio_sd"))
#> [1] "Patient BMI"    "Follow Up Time" "HR Ratio SD"   
dict <- data.frame(term = "pao2", short = "PaO2", full = "Arterial oxygen pressure", unit = "mmHg")
pretty_name(c("pao2", "heart_rate"), dict = dict, form = "unit")
#> [1] "Arterial oxygen pressure (mmHg)" "Heart Rate"                     
```
