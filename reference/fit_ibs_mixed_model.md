# Fit a Mixed-Effects Model for Integrated Brier Score Against Alpha

Fits a linear mixed-effects model (or fixed-effects regression fallback)
of Integrated Brier Score (IBS) or \`log10(IBS)\` against \`alpha\`,
accounting for grouping across resampling splits
(\`inner_resamples_splits\`). Returns the fitted model, tidy
coefficients, a formatted equation string, and model predictions along
the alpha spectrum.

## Usage

``` r
fit_ibs_mixed_model(data, log_ibs = TRUE)
```

## Arguments

- data:

  A data frame containing tuning results from \[tune_alpha_by_split()\].
  Must contain columns \`alpha\`, \`IBS\`, and
  \`inner_resamples_splits\`.

- log_ibs:

  Logical. When \`TRUE\` (default), models \`log10(IBS)\`. When
  \`FALSE\`, models raw \`IBS\`.

## Value

A list with class \`"cbe_tune_mixed_model"\` containing:

- model:

  The fitted \`merMod\` or \`lm\` model object

- equation:

  A character string with the fitted equation (e.g. \`"log10(IBS) =
  -0.8712 + 0.0450 \* alpha"\`)

- coefficients:

  A tibble with \`term\`, \`estimate\`, and standard error

- fitted:

  A tibble of predicted values across the alpha continuum

- data:

  The deduplicated per-split fit data used for modeling

## Details

This dynamic fitting eliminates hard-coded regression coefficients in
reports.
