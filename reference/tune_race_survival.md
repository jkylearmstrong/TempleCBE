# Adaptive Racing Tuning for Survival Workflows and Workflow Sets

Tunes hyperparameters of a survival
[`workflow`](https://workflows.tidymodels.org/reference/workflow.html)
or
[`workflow_set`](https://workflowsets.tidymodels.org/reference/workflow_set.html)
using finetune racing algorithms
([`tune_race_anova`](https://finetune.tidymodels.org/reference/tune_race_anova.html)
or
[`tune_race_win_loss`](https://finetune.tidymodels.org/reference/tune_race_win_loss.html)).
When given a `workflow_set`, execution is seamlessly mapped across all
constituent workflows via
[`workflow_map`](https://workflowsets.tidymodels.org/reference/workflow_map.html).

## Usage

``` r
tune_race_survival(
  object,
  resamples,
  fn = c("tune_race_anova", "tune_race_win_loss"),
  grid = 20,
  metrics = NULL,
  eval_time = NULL,
  control = NULL,
  seed = 1503,
  ...
)

cbe_tune_race_survival(
  object,
  resamples,
  fn = c("tune_race_anova", "tune_race_win_loss"),
  grid = 20,
  metrics = NULL,
  eval_time = NULL,
  control = NULL,
  seed = 1503,
  ...
)
```

## Arguments

- object:

  A
  [`workflow`](https://workflows.tidymodels.org/reference/workflow.html)
  or
  [`workflow_set`](https://workflowsets.tidymodels.org/reference/workflow_set.html)
  specifying the preprocessor and survival model.

- resamples:

  An `rset` resampling object (e.g. from
  [`vfold_cv`](https://rsample.tidymodels.org/reference/vfold_cv.html)
  or
  [`group_vfold_cv`](https://rsample.tidymodels.org/reference/group_vfold_cv.html)).

- fn:

  Character string specifying the racing function: `"tune_race_anova"`
  (default) or `"tune_race_win_loss"` (which needs BradleyTerry2).

- grid:

  Integer number of candidate tuning parameter combinations (default
  20), or an explicit parameter grid data frame.

- metrics:

  A
  [`metric_set`](https://yardstick.tidymodels.org/reference/metric_set.html)
  of survival metrics. If `NULL` (default), defaults to Integrated Brier
  Score and Concordance.

- eval_time:

  Numeric vector of evaluation time points for dynamic survival metrics.
  If `NULL` (default), the deciles of the observed event times in the
  first resample's data are used, as in
  [`cv_coxnet`](https://jkylearmstrong.github.io/TempleCBE/reference/cv_coxnet.md)
  and
  [`nested_cv_coxnet`](https://jkylearmstrong.github.io/TempleCBE/reference/nested_cv_coxnet.md);
  tune itself requires at least two evaluation times for the integrated
  metrics. The outcome is read from the workflow's formula or recipe
  (for a `workflow_set`, from its first workflow); pass `eval_time` for
  any other preprocessor.

- control:

  A
  [`control_race`](https://finetune.tidymodels.org/reference/control_race.html)
  object. Defaults to
  [`control_race_survival()`](https://jkylearmstrong.github.io/TempleCBE/reference/control_race_survival.md).

- seed:

  Optional random seed integer for reproducible candidate generation and
  fold processing.

- ...:

  Additional arguments passed to
  [`tune_race_anova`](https://finetune.tidymodels.org/reference/tune_race_anova.html),
  [`tune_race_win_loss`](https://finetune.tidymodels.org/reference/tune_race_win_loss.html),
  or
  [`workflow_map`](https://workflowsets.tidymodels.org/reference/workflow_map.html).

## Value

If `object` is a workflow, a `tune_results` / `race_results` object. If
`object` is a workflow set, a `workflow_set` object containing tuning
results.

## Details

Default evaluation metrics are
[`brier_survival_integrated`](https://yardstick.tidymodels.org/reference/brier_survival_integrated.html)
and
[`concordance_survival`](https://yardstick.tidymodels.org/reference/concordance_survival.html).

## See also

[`control_race_survival`](https://jkylearmstrong.github.io/TempleCBE/reference/control_race_survival.md),
[`tune_race_anova`](https://finetune.tidymodels.org/reference/tune_race_anova.html),
[`workflow_map`](https://workflowsets.tidymodels.org/reference/workflow_map.html)

## Examples

``` r
if (FALSE) { # \dontrun{
if (requireNamespace("finetune", quietly = TRUE) &&
    requireNamespace("survival", quietly = TRUE) &&
    requireNamespace("parsnip", quietly = TRUE) &&
    requireNamespace("workflows", quietly = TRUE) &&
    requireNamespace("rsample", quietly = TRUE)) {
  lung <- stats::na.omit(survival::lung[, c("time", "status", "age", "sex")])
  spec <- parsnip::set_engine(
    parsnip::proportional_hazards(penalty = tune::tune(), mixture = 0.5),
    "coxnet"
  ) |> parsnip::set_mode("censored regression")
  wflow <- workflows::workflow() |>
    workflows::add_model(spec) |>
    workflows::add_formula(survival::Surv(time, status) ~ age + sex)
  folds <- rsample::vfold_cv(lung, v = 4)
  res <- tune_race_survival(wflow, resamples = folds, grid = 10, eval_time = c(180, 365))
  tune::collect_metrics(res)
}
} # }
```
