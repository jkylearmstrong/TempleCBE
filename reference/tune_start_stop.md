# Resample and Tune Workflows on Start/Stop Survival Data

Counterparts of \[tune::fit_resamples()\], \[tune::tune_grid()\] and
\[finetune::tune_race_anova()\] for workflows whose outcome is
\`Surv(start, stop, event)\`. Those functions cannot score start/stop
data: the yardstick survival metrics need a right-censored truth, and
tune scores each start/stop row as if it were a subject. Here each
resample's assessment rows are collapsed to one row per subject with
\[collapse_start_stop_predictions()\] before the metrics are computed,
so every subject counts once and its survival follows its covariate
path.

## Usage

``` r
fit_resamples_start_stop(
  object,
  resamples,
  subject_id,
  metrics = NULL,
  eval_time = NULL,
  covariates = c("path", "baseline"),
  recurrent = c("error", "first"),
  trunc = 0.05,
  check_subject_overlap = TRUE,
  save_pred = FALSE,
  parallel = FALSE
)

tune_grid_start_stop(
  object,
  resamples,
  subject_id,
  grid = 10,
  metrics = NULL,
  eval_time = NULL,
  covariates = c("path", "baseline"),
  recurrent = c("error", "first"),
  trunc = 0.05,
  check_subject_overlap = TRUE,
  save_pred = FALSE,
  parallel = FALSE
)

tune_race_start_stop(
  object,
  resamples,
  subject_id,
  grid = 10,
  metrics = NULL,
  eval_time = NULL,
  covariates = c("path", "baseline"),
  recurrent = c("error", "first"),
  trunc = 0.05,
  check_subject_overlap = TRUE,
  save_pred = FALSE,
  parallel = FALSE,
  control = NULL
)
```

## Arguments

- object:

  A \[workflows::workflow()\] with a proportional-hazards model and a
  \`Surv()\` outcome.

- resamples:

  An \`rset\`, grouped by subject.

- subject_id:

  Name of the subject identifier column in the data.

- metrics:

  A \[yardstick::metric_set()\] of survival metrics; defaults to the
  integrated Brier score and concordance. The first metric is the one
  \`select_best()\` and racing use by default.

- eval_time:

  Evaluation times; defaults to deciles of the subjects' event times in
  the full data.

- covariates, recurrent, trunc:

  As in \[collapse_start_stop_predictions()\].

- check_subject_overlap:

  If \`TRUE\` (default), stop when a resample puts a subject in both its
  analysis and its assessment set.

- save_pred:

  If \`TRUE\`, keep each resample's subject-level predictions in
  \`.predictions\`, for \`tune::collect_predictions()\`.

- parallel:

  If \`TRUE\`, score resamples with \[furrr::future_map()\] (the burn-in
  resamples, when racing); set a \[future::plan()\] first.

- grid:

  A data frame of candidate parameter values (columns named by the
  tuning parameters' ids), or a number of candidates for a space-filling
  design from \[dials::grid_space_filling()\].

- control:

  For \`tune_race_start_stop()\`: a list or a
  \[finetune::control_race()\] object, of which \`burn_in\` (default 3),
  \`alpha\` (default 0.05) and \`randomize\` (default \`TRUE\`) are
  used.

## Value

A \`tune_results\` object (class \`start_stop_tune_results\`) with
\`splits\`, \`id\`, \`.metrics\`, \`.notes\` and, with \`save_pred =
TRUE\`, \`.predictions\`.

## Details

\*\*Models.\*\* The workflow's model must be
\[parsnip::proportional_hazards()\] with the \`"survival"\` (coxph) or
\`"glmnet"\` engine (from censored); both fit start/stop data, and
scoring uses their linear predictor. Other censored-regression models
either refuse start/stop outcomes or treat each row as a subject. With
the glmnet engine, candidates that differ only in \`penalty\` share one
fit per resample, as glmnet fits the whole path. Its \`cox.ties\` is set
to \`getOption("templecbe.cox_ties", "breslow")\` unless the engine sets
it, e.g. \`set_engine("glmnet", cox.ties = "efron")\`, which also
silences glmnet 5.1's warning about the default tie handling. The pin is
applied to an internal copy of the workflow, not to the workflow
supplied: the workflow stored in the result
(\`tune::extract_workflow(res)\`) carries the pin, so pass that workflow
— not the workflow supplied here — to \`tune::finalize_workflow()\` when
refitting the tuned model. Finalizing the supplied workflow refits
without the pin, and so with glmnet's default tie handling.

\*\*Resamples.\*\* Group them by subject, e.g.
\[rsample::group_vfold_cv()\] with \`group = subject_id\`, or
\[subject_vfold_cv()\], which also stratifies the folds by each
subject's event status. If a subject is in both the analysis and the
assessment set of a resample, these functions stop before fitting
(\`check_subject_overlap = FALSE\` skips the check). Out-of-bag sets of
\[rsample::group_bootstraps()\] pass.

\*\*Racing.\*\* \`tune_race_start_stop()\` races the candidates with the
rule of \[finetune::tune_race_anova()\]: after \`burn_in\` resamples,
the first metric (at the first \`eval_time\` for a time-dependent
metric) is modelled as \`.estimate ~ candidate + resample\`, and
candidates whose \`1 - alpha\` confidence interval for the difference
from the current best is entirely on the worse side are dropped.
Eliminated candidates keep the metrics of the resamples they were scored
on, and the attribute \`race\` lists every candidate with
\`n_resamples\` and \`eliminated\`; \`show_best()\` and
\`select_best()\` use the survivors, which are the candidates scored on
every resample.

## See also

\[collapse_start_stop_predictions()\], \[cv_coxnet()\],
\[nested_cv_coxnet()\]

## Examples

``` r
# \donttest{
if (requireNamespace("censored", quietly = TRUE) &&
    requireNamespace("workflows", quietly = TRUE) &&
    requireNamespace("tune", quietly = TRUE) &&
    requireNamespace("rsample", quietly = TRUE)) {
  set.seed(1)
  long <- do.call(rbind, lapply(1:90, function(i) {
    k <- sample(1:3, 1)
    stops <- cumsum(stats::runif(k, 2, 6))
    risk <- stats::rnorm(1)
    data.frame(subject_id = i, tstart = c(0, utils::head(stops, -1)), tstop = stops,
               status = c(rep(0, k - 1), stats::rbinom(1, 1, stats::plogis(risk))),
               x1 = risk + stats::rnorm(k, sd = 0.2), x2 = stats::rnorm(k))
  }))
  spec <- parsnip::proportional_hazards(penalty = tune::tune(), mixture = 1) |>
    parsnip::set_engine("glmnet") |>
    parsnip::set_mode("censored regression")
  wflow <- workflows::workflow(survival::Surv(tstart, tstop, status) ~ x1 + x2, spec)
  folds <- rsample::group_vfold_cv(long, group = subject_id, v = 5)
  res <- tune_grid_start_stop(
    wflow, folds, subject_id = "subject_id",
    grid = data.frame(penalty = 10^seq(-3, -0.5, length.out = 8))
  )
  tune::show_best(res, metric = "brier_survival_integrated")
}
#> # A tibble: 5 × 8
#>   penalty .metric              .estimator .eval_time  mean     n std_err .config
#>     <dbl> <chr>                <chr>           <dbl> <dbl> <int>   <dbl> <chr>  
#> 1 0.0268  brier_survival_inte… standard           NA 0.164     5  0.0290 pre0_m…
#> 2 0.0118  brier_survival_inte… standard           NA 0.164     5  0.0292 pre0_m…
#> 3 0.00518 brier_survival_inte… standard           NA 0.164     5  0.0293 pre0_m…
#> 4 0.001   brier_survival_inte… standard           NA 0.164     5  0.0292 pre0_m…
#> 5 0.00228 brier_survival_inte… standard           NA 0.164     5  0.0292 pre0_m…
# }
```
