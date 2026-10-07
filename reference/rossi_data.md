# Rossi Recidivism Dataset

Loads the classic Rossi recidivism study dataset used in biostatistical
benchmarks for survival models with time-varying coefficients and
covariates (e.g., JSS Vol 61, Code 01). The study tracks 432 convicts
released from Maryland state prisons over a 52-week follow-up period.

## Usage

``` r
rossi_data(format = c("csv", "sas"))
```

## Source

Rossi, P. H., Berk, R. A., & Lenihan, K. J. (1980). \*Money, work, and
crime: Some experimental results\*. Academic Press.

## Arguments

- format:

  Either \`"csv"\` (default, reading \`inst/extdata/rossi.csv\`) or
  \`"sas"\` (reading the native \`inst/extdata/recid.sas7bdat\` via
  \[haven::read_sas()\]).

## Value

A tibble with 432 rows containing recidivism follow-up data:

- week:

  Follow-up time until arrest or censoring (1–52 weeks).

- arrest:

  Event indicator (1 = arrested, 0 = censored/did not re-offend).

- fin:

  Financial aid treatment (1 = received financial aid, 0 = control).

- age:

  Age at release from prison in years.

- race:

  Race indicator (1 = Black, 0 = other).

- wexp:

  Prior full-time work experience (1 = yes, 0 = no).

- mar:

  Marital status (1 = married, 0 = unmarried).

- paro:

  Release on parole (1 = yes, 0 = no).

- prio:

  Number of prior convictions.

- educ:

  Education level coded as 2 (grades 2-5) through 6 (some college).

- emp1–emp52:

  Weekly employment status indicator across the 52 weeks.

## See also

\[cbe_sas_macro_path()\], \[run_sas_script()\]

## Examples

``` r
df <- rossi_data()
head(df[, 1:10])
#> # A tibble: 6 × 10
#>    week arrest   fin   age  race  wexp   mar  paro  prio  educ
#>   <int>  <int> <int> <int> <int> <int> <int> <int> <int> <int>
#> 1    20      1     0    27     1     0     0     1     3     3
#> 2    17      1     0    18     1     0     0     1     8     4
#> 3    25      1     0    19     0     1     0     1    13     3
#> 4    52      0     1    23     1     1     1     1     1     5
#> 5    52      0     0    19     0     1     0     1     3     3
#> 6    52      0     0    24     1     1     0     0     2     4
```
