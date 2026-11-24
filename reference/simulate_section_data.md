# Simulate Synthetic Section Data From a Schema Mapping

Generates synthetic data honoring the column roles declared in a
validated mapping table. If the `pslongSim` package is available, it is
leveraged for baseline subject scaffolding; otherwise, an internal
robust generator produces demographic and longitudinal grids.

## Usage

``` r
simulate_section_data(
  mapping,
  index,
  n_subjects = 20,
  seed = 1,
  missing_rate = 0,
  time_levels = NULL
)
```

## Arguments

- mapping:

  Validated column mapping table; see
  [`validate_column_mapping`](https://jkylearmstrong.github.io/TempleCBE/reference/validate_column_mapping.md).

- index:

  Character string identifying the section/domain in `mapping$INDEX` to
  simulate.

- n_subjects:

  Number of synthetic subjects to generate (default: 20).

- seed:

  Random seed for reproducibility (default: 1).

- missing_rate:

  Probability in `[0, 1]` that any non-ID, non-time cell is set to NA.

- time_levels:

  Optional character vector of levels for the section's `Time_var`
  column (default: `c("T0", "T1", "T2", "T3")`).

## Value

A tibble shaped according to the standardized `new` columns for the
specified section.
