# The \`coxnet\` Engine for \`parsnip::proportional_hazards()\`

Registers \[coxnet()\] as an engine of
\[parsnip::proportional_hazards()\] in \`"censored regression"\` mode,
so penalized Cox models fit by TempleCBE work anywhere a parsnip model
does: \[parsnip::fit()\], workflows, tune, workflowsets, and stacks.
Unlike censored's \`"glmnet"\` engine, it also accepts counting-process
outcomes, \`Surv(start, stop, event)\`, for time-varying covariates.

## Usage

``` r
coxnet_train(
  formula,
  data,
  penalty = NULL,
  mixture = NULL,
  weights = NULL,
  ...
)

predict_coxnet_linear_pred(object, new_data, penalty = NULL)

predict_coxnet_time(object, new_data, penalty = NULL)

predict_coxnet_survival(object, new_data, eval_time, penalty = NULL)
```

## Arguments

- formula:

  A formula with a \[survival::Surv()\] outcome.

- data:

  A data frame containing the variables in \`formula\`.

- penalty:

  The penalty, glmnet's \`lambda\`: a single non-negative number.

- mixture:

  Elastic-net mixing parameter, glmnet's \`alpha\`. \`NULL\` means 1.

- weights:

  Not supported; must be \`NULL\`.

- ...:

  Further arguments passed to \[glmnet::glmnet()\].

- object:

  A parsnip \`model_fit\` using the coxnet engine.

- new_data:

  A data frame of new predictors.

- eval_time:

  Times at which to predict survival.

## Value

For \`coxnet_train()\`, a \[coxnet()\] model. This is the function
parsnip calls to fit the engine; call \[coxnet()\] directly outside
parsnip.

## Details

The engine is registered when parsnip is loaded, whichever package is
loaded first; nothing needs to be called.

Use it like any other engine:

“\`r spec \<- parsnip::proportional_hazards(penalty = 0.05, mixture =
0.5) \|\> parsnip::set_engine("coxnet") fit \<- parsnip::fit(spec,
survival::Surv(time, status) ~ ., data = lung) predict(fit, lung, type =
"linear_pred") predict(fit, lung, type = "survival", eval_time = c(180,
365)) “\`

\* \`penalty\` is required (it is glmnet's \`lambda\`); \`mixture\`
defaults to 1, the lasso. Both can be \[tune::tune()\]d. Each candidate
is fit separately; the coxnet engine doesn't use glmnet's path to
evaluate a grid of penalties from one fit, so tuning \`penalty\` alone
costs one fit per value. \* Engine arguments given to
\[parsnip::set_engine()\] are passed on to \[glmnet::glmnet()\], such as
\`nlambda\` or \`cox.ties\`. \* Predictions: \`"linear_pred"\` (larger
means longer survival, as in censored), \`"survival"\`, which needs
\`eval_time\`, and \`"time"\`, the restricted mean survival time, which
static metrics such as \[yardstick::concordance_survival()\] use. \*
Case weights aren't supported, and neither is \`strata()\` in the
formula. \* \[generics::tidy()\] on the parsnip fit returns the
coefficients.

## See also

\[coxnet()\], \[cv_coxnet()\], \[parsnip::proportional_hazards()\],
\`censored::proportional_hazards\` engines

## Examples

``` r
if (requireNamespace("parsnip", quietly = TRUE) &&
    requireNamespace("glmnet", quietly = TRUE) &&
    requireNamespace("survival", quietly = TRUE)) {
  lung <- stats::na.omit(survival::lung[, c("time", "status", "age", "sex", "ph.ecog")])
  spec <- parsnip::set_engine(
    parsnip::proportional_hazards(penalty = 0.05, mixture = 0.5),
    "coxnet"
  )
  fit <- parsnip::fit(spec, survival::Surv(time, status) ~ ., data = lung)
  predict(fit, lung[1:3, ], type = "survival", eval_time = c(180, 365))
}
#> # A tibble: 3 × 1
#>   .pred           
#>   <list>          
#> 1 <tibble [2 × 2]>
#> 2 <tibble [2 × 2]>
#> 3 <tibble [2 × 2]>
```
