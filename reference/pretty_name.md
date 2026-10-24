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
    "ROC", "IPCW")
)
```

## Arguments

- x:

  Character vector of variable names.

- acronyms:

  Optional character vector of terms to keep uppercase.

## Value

Character vector of formatted display labels.

## Examples

``` r
pretty_name(c("patient_bmi", "follow_up_time", "hr_ratio_sd"))
#> [1] "Patient BMI"    "Follow Up Time" "HR Ratio SD"   
```
