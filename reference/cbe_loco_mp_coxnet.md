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
  covariates = c("path", "baseline"),
  ...
)

loco_mp_coxnet(
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
  covariates = c("path", "baseline"),
  ...
)
```

## Arguments

- formula:

  A formula with a
  [`Surv()`](https://rdrr.io/pkg/survival/man/Surv.html) outcome, e.g.
  `Surv(time, status) ~ x1 + x2`, or a \[recipes::recipe()\] whose
  outcome is a `Surv` column. A formula may not use \`strata()\` or
  \`offset()\`; see \[coxnet()\].

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

  Penalty parameter for `coxnet`. If `NULL`, each minipatch is predicted
  at the smallest penalty on its own glmnet path, an almost unpenalized
  fit.

- mixture:

  Elastic-net mixing parameter (\\\alpha \in \[0, 1\]\\). Default is 1
  (lasso).

- eval_time:

  Numeric vector of evaluation time points for IPCW Brier loss
  integration. If `NULL`, defaults to deciles of event times.

- subject_id:

  Optional character column name identifying subjects for clustered /
  counting-process start/stop data. With a recipe, a single column with
  role `"id"` is used when `NULL`, as in \[cv_coxnet()\].

- alpha:

  Significance level: the two-sided confidence intervals have level
  `1 - alpha`, and
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  calls a predictor significant when its adjusted one-sided p-value is
  below `alpha`. Default is 0.05.

- p_adjust:

  Multiple testing correction method for the one-sided p-values. Options
  include `"bonferroni"`, `"BH"`, `"fdr"`, `"holm"`, or `"none"`.
  Default is `"bonferroni"`.

- trunc:

  Lower truncation threshold for Kaplan-Meier censoring weights. Default
  is 0.05.

- parallel:

  Logical; if `TRUE` and `future` is available, runs patches in
  parallel.

- seed:

  Optional random seed for reproducible subsampling. The caller's RNG
  state is restored on exit.

- covariates:

  `"path"` (default) or `"baseline"`: how the out-of-bag survival curve
  of a subject with start/stop rows uses the covariates, as in
  [`cv_coxnet`](https://jkylearmstrong.github.io/TempleCBE/reference/cv_coxnet.md).
  With `"path"` the cumulative hazard is integrated along the subject's
  covariate path, so no value from after an evaluation time predicts
  survival at it; `"baseline"` uses the first interval's covariates. The
  two agree for right-censored data.

- ...:

  Additional arguments passed to
  [`coxnet()`](https://jkylearmstrong.github.io/TempleCBE/reference/coxnet.md),
  and so to
  [`glmnet`](https://glmnet.stanford.edu/reference/glmnet.html), such as
  `cox.ties`, `standardize`, or `penalty.factor`. Not `weights` or
  `offset`.

## Value

An S3 object of class `"cbe_loco_mp_coxnet"` containing:

- results:

  A tibble with columns `term`, `importance`, `std_error`, `statistic`,
  `p_value` (one-sided, for `importance > 0`), `p_adjusted`, `conf_low`,
  `conf_high` (a two-sided interval at level `1 - alpha`); see Details.

- eval_time:

  Evaluation time grid used for IPCW loss integration.

- B:

  Number of successfully fitted minipatches; a warning says when some
  failed.

- n_ratio, m_ratio:

  Subsampling ratios used.

- alpha:

  Significance level.

- p_adjust:

  Multiple testing adjustment method.

- covariates:

  How start/stop covariates were used; see `covariates`.

## Details

\*\*Tests and intervals.\*\* For each predictor, \`importance\` is the
mean, over the subjects that were out of bag in patches both with and
without it, of the excess IPCW integrated Brier loss when it is left
out. \`p_value\` is from a *one-sided* z test of \`importance \> 0\`,
the upper tail of the standard normal at \`statistic\`, and
\`p_adjusted\` adjusts those one-sided p-values with \`p_adjust\`.
\`conf_low\` and \`conf_high\` are a *two-sided* interval at level \`1 -
alpha\`, not adjusted for multiple testing. The p-values and the
interval therefore answer different questions: the interval excludes 0
when the unadjusted one-sided p-value is below \`alpha / 2\`, whereas
\`p_adjusted \< alpha\` is a one-sided, adjusted criterion. (With
\`p_adjust = "none"\`, a predictor whose \`p_value\` is between \`alpha
/ 2\` and \`alpha\` passes the test but has an interval that still
contains 0.) \[autoplot()\]\[autoplot.cbe_loco_mp_coxnet\] colours a
predictor "Significant" by \`p_adjusted \< alpha\`, that is, by the
one-sided adjusted test, not by whether its interval excludes 0.

\*\*Data.\*\* The predictors must be numeric, complete, and finite, and
there must be at least 3 of them: each minipatch keeps 2 and leaves at
least 1 out. This is checked once before any patch is fit. A minipatch
that fails anyway (for example, one whose sampled subjects have no
events) is dropped with a warning that gives the count and the first
error; \`B\` in the result is the number that succeeded.

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
