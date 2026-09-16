# Convert Wide Tumor Data to Counting Process (Start/Stop) Format

Replicates the exact SAS DATA step transformation from SAS/STAT User's
Guide (Example 85.7: "Tumor1" dataset). For each subject, intervals (T1,
T2\] are constructed across observation times where the time-dependent
covariate `NPap` changes, with `Status` equal to `Dead` only on the
terminal interval and 0 elsewhere.

## Usage

``` r
tumor_long(data = NULL)
```

## Source

SAS/STAT User's Guide, Example 85.7: Time-Dependent Repeated
Measurements of a Covariate.
<https://support.sas.com/documentation/cdl/en/statug/68162/HTML/default/statug_phreg_examples07.htm>

## Arguments

- data:

  A data frame structured like
  [`tumor_wide()`](https://jkylearmstrong.github.io/TempleCBE/reference/tumor_wide.md).
  If `NULL`,
  [`tumor_wide()`](https://jkylearmstrong.github.io/TempleCBE/reference/tumor_wide.md)
  is used by default.

## Value

A tibble with columns:

- ID:

  Subject ID.

- Time:

  Original death or censoring time.

- Dead:

  Original death status.

- Dose:

  Dose level.

- T1:

  Start time of the risk interval.

- T2:

  Stop time of the risk interval.

- NPap:

  Number of papillomas active during interval (T1, T2\].

- Status:

  Event status at T2 (1 = event, 0 = censored/continuing).
