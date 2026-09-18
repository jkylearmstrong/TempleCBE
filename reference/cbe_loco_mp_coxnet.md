# Leave-One-Covariate-Out Inference with MiniPatch Ensembles (LOCO-MP) for Cox Models

Implements the LOCO-MP statistical inference framework (Gan, Zheng, &
Allen 2022) for penalized Cox proportional hazards regression. Combines
random minipatch ensembling (subsampling both observations/subjects and
covariates) with out-of-bag Leave-One-Covariate-Out evaluation to
produce distribution-free feature importance estimates, standard errors,
asymptotic z-tests, and confidence intervals for survival models.

## Usage

``` r
cbe_loco_mp_coxnet(
  formula = NULL,
  data = NULL,
  x = NULL,
  y = NULL,
  B = 100,
  n_ratio = 0.7,
  m_ratio = 0.5,
  penalty = NULL,
  mixture = 1,
  eval_time = NULL,
  subject_id = NULL,
  alpha = 0.05,
  p_adjust = c("bonferroni", "BH", "fdr", "holm", "none"),
  trunc = 0.05,
  parallel = FALSE,
  seed = NULL,
  ...
)
```

## Arguments

- formula:

  A formula with a
  [`Surv()`](https://rdrr.io/pkg/survival/man/Surv.html) outcome, e.g.
  `Surv(time, status) ~ x1 + x2`.

- data:

  A data frame containing the variables in the model.

- x:

  An optional predictor matrix (used if `formula` and `data` are
  `NULL`).

- y:

  An optional `Surv` outcome object (used if `formula` and `data` are
  `NULL`).

- B:

  Number of minipatches to generate. Default is 100.

- n_ratio:

  Subsampling fraction for subjects/observations without replacement.
  Default is 0.7.

- m_ratio:

  Subsampling fraction for predictors without replacement. Default is
  0.5.

- penalty:

  Penalty parameter for `coxnet`. If `NULL`, defaults to 0
  (unpenalized).

- mixture:

  Elastic-net mixing parameter (\\\alpha \in \[0, 1\]\\). Default is 1
  (lasso).

- eval_time:

  Numeric vector of evaluation time points for IPCW Brier loss
  integration. If `NULL`, defaults to deciles of event times.

- subject_id:

  Optional character column name identifying subjects for clustered /
  counting-process start/stop data.

- alpha:

  Significance level for confidence intervals. Default is 0.05.

- p_adjust:

  Multiple testing correction method for p-values. Options include
  `"bonferroni"`, `"BH"`, `"fdr"`, `"holm"`, or `"none"`. Default is
  `"bonferroni"`.

- trunc:

  Lower truncation threshold for Kaplan-Meier censoring weights. Default
  is 0.05.

- parallel:

  Logical; if `TRUE` and `future` is available, runs patches in
  parallel.

- seed:

  Optional random seed for reproducible subsampling.

- ...:

  Additional arguments passed to
  [`coxnet()`](https://jkylearmstrong.github.io/TempleCBE/reference/coxnet.md).

## Value

An S3 object of class `"cbe_loco_mp_coxnet"` containing:

- results:

  A tibble with columns `term`, `importance`, `std_error`, `statistic`,
  `p_value`, `p_adjusted`, `conf_low`, `conf_high`.

- eval_time:

  Evaluation time grid used for IPCW loss integration.

- B:

  Number of successfully fitted minipatches.

- n_ratio, m_ratio:

  Subsampling ratios used.

- alpha:

  Significance level.

- p_adjust:

  Multiple testing adjustment method.

## See also

\[nested_cv_coxnet()\], \[cv_coxnet()\], \[coxnet()\]

## Examples

``` r
# \donttest{
if (requireNamespace("glmnet", quietly = TRUE) && requireNamespace("survival", quietly = TRUE)) {
  set.seed(42)
  n <- 60
  df <- data.frame(
    time = stats::rexp(n, 0.1) + 0.1,
    status = stats::rbinom(n, 1, 0.6),
    x1 = stats::rnorm(n),
    x2 = stats::rnorm(n),
    x3 = stats::rnorm(n)
  )
  fit <- cbe_loco_mp_coxnet(survival::Surv(time, status) ~ x1 + x2 + x3, data = df, B = 20)
  fit
  generics::tidy(fit)
}
#> # A tibble: 3 × 8
#>   term  importance std_error statistic p_value conf_low conf_high p_adjusted
#>   <chr>      <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl>      <dbl>
#> 1 x3       0.00667    0.0191     0.349   0.364  -0.0308    0.0442      1    
#> 2 x2       0.00659    0.0126     0.523   0.300  -0.0181    0.0313      0.901
#> 3 x1      -0.00452    0.0124    -0.364   0.642  -0.0288    0.0198      1    
# }
```
