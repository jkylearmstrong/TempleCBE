# Extract Survival Outcome Components and Predictors

Helper utility that molds a survival formula into clean components,
through the same hardhat formula blueprint used by
[`coxnet`](https://jkylearmstrong.github.io/TempleCBE/reference/coxnet.md)
(one-hot dummy encoding, no intercept), supporting both 2-parameter
`Surv(time, status)` and 3-parameter start/stop
`Surv(tstart, tstop, status)` counting process structures.

## Usage

``` r
extract_surv_components(data, outcome, subject_id = NULL)
```

## Arguments

- data:

  A data frame.

- outcome:

  A survival formula or Surv expression.

- subject_id:

  Optional subject identifier column name; dropped from \`data\` before
  molding, so it is never treated as a predictor.

## Value

A list with elements `surv_obj`, `time`, `start`, `status`, `type`,
`predictors` (a numeric tibble, one-hot encoded), `pred_names`,
`raw_predictors` (the original predictor columns, before encoding, which
is what the models' [`predict()`](https://rdrr.io/r/stats/predict.html)
methods and the explainers take), `formula`, `data` (the molded,
\`subject_id\`-free data frame), and `blueprint` (the hardhat blueprint
used, for
[`forge`](https://hardhat.tidymodels.org/reference/forge.html)).
