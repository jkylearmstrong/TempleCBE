# Internal SAS Benchmark Tumor Dataset (Wide Format)

Transcribes the 45-animal tumor-promoting agent study from the SAS/STAT
User's Guide (Example 85.7 / 91.7: "Time-Dependent Repeated Measurements
of a Covariate"). In this study, 45 rodents were exposed to a carcinogen
and randomized to three dose levels of a tumor-promoting agent (1.0,
2.5, 10.0). The number of papillomas was observed repeatedly across 15
observation times (weeks 27, 34, 37, 41, 43, 45, 46, 47, 49, 50, 51, 53,
65, 67, 71).

## Usage

``` r
tumor_wide()
```

## Source

SAS/STAT User's Guide, Example 85.7: Time-Dependent Repeated
Measurements of a Covariate.
<https://support.sas.com/documentation/cdl/en/statug/68162/HTML/default/statug_phreg_examples07.htm>

## Value

A tibble with 45 rows and 19 columns:

- ID:

  Subject ID (1–45).

- Time:

  Event or censoring time in weeks.

- Dead:

  Censoring status (1 = dead, 0 = censored).

- Dose:

  Dose level of the promoting agent (1.0, 2.5, or 10.0).

- P1–P15:

  Number of papillomas counted at observation weeks 27, 34, 37, 41, 43,
  45, 46, 47, 49, 50, 51, 53, 65, 67, 71. Missing (NA) beyond death or
  study termination.
