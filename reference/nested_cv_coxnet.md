# Nested Cross-Validation of a Penalized Cox Model

Estimates how well the whole tuning procedure of \[cv_coxnet()\]
generalizes. For each outer split of an \[rsample::nested_cv()\] object:
\`mixture\` and \`penalty\` are chosen by \[cv_coxnet()\] on that
split's inner resamples; the model is refit on the outer analysis set
with those settings; and the refit is scored on the outer assessment
set, which played no part in tuning.

## Usage

``` r
nested_cv_coxnet(
  object,
  preprocessor,
  subject_id = NULL,
  group = NULL,
  rule = c("min", "1se"),
  mixture = 1,
  penalty = NULL,
  metrics = NULL,
  eval_time = NULL,
  metric = NULL,
  covariates = c("path", "baseline"),
  trunc = 0.05,
  recurrent = c("error", "first", "gap"),
  parallel = FALSE,
  importance = c("none", "loco_mp"),
  loco_mp_B = 30L,
  check_subject_overlap = TRUE,
  tune_method = c("grid", "race_anova", "race_win_loss"),
  control = NULL,
  checkpoint_dir = NULL,
  shard = NULL,
  checkpoint_mismatch = c("recompute", "overwrite", "error"),
  on_complete = getOption("templecbe.on_complete", NULL),
  ...
)

# S3 method for class 'nested_cv_coxnet'
collect_metrics(x, ..., summarize = TRUE)
```

## Arguments

- object:

  An \[rsample::nested_cv()\] object.

- preprocessor:

  A formula with a \`Surv()\` outcome, or a \[recipes::recipe()\] whose
  outcome is a \`Surv\` column.

- subject_id, group:

  Column names, as in \[cv_coxnet()\].

- rule:

  Use \`"min"\` (\`lambda.min\`) or \`"1se"\` (\`lambda.1se\`) from each
  inner cross-validation.

- mixture:

  Numeric vector of elastic-net mixing values to try.

- penalty:

  Optional decreasing penalty path; by default glmnet's path for each
  \`mixture\`.

- metrics:

  A \[yardstick::metric_set()\] of survival metrics. Defaults to the
  integrated Brier score, concordance, time-specific Brier score, and
  time-dependent ROC AUC. In gap mode only \`concordance_survival\`,
  \`brier_survival_integrated\` and \`brier_survival\` are available.

- eval_time:

  Evaluation times, shared by all splits. Defaults to deciles of the
  event times in the full data.

- metric:

  Name of the metric used to choose settings; the first metric in
  \`metrics\` by default.

- covariates:

  \`"path"\` or \`"baseline"\`; see Details.

- trunc:

  Lower bound for the censoring probability in the Graf weights.

- recurrent:

  How recurrent (start/stop) data are scored, as in \[cv_coxnet()\]:
  \`"error"\` (default), \`"first"\`, or \`"gap"\` (gap-time concordance
  and gap-time Brier scores; needs \`tune_method = "grid"\` or
  \`"race_anova"\`).

- parallel:

  If \`TRUE\`, fit folds with \[furrr::future_map()\]; set a
  \[future::plan()\] first.

- importance:

  Method for evaluating feature importance on the outer analysis sets.
  Options are \`"none"\` (default) or \`"loco_mp"\`
  (Leave-One-Covariate-Out with MiniPatch ensembles).
  \[loco_mp_coxnet()\] is run on each outer analysis set at the
  \`mixture\` and penalty chosen there, with the same glmnet arguments
  (see \`...\`). It draws its minipatches by subject, so \`group\` does
  not apply to it, and a warning says so when \`group\` is set.

- loco_mp_B:

  Number of minipatch ensembles drawn by \[loco_mp_coxnet()\] when
  \`importance = "loco_mp"\` (default \`30\`).

- check_subject_overlap:

  If \`TRUE\` (the default), stop when an outer split, or an inner
  resample, puts the same subject (or, when \`group\` is set, the same
  group) in both its analysis and its assessment set; see Details and
  \[cv_coxnet()\]. \`FALSE\` skips the check.

- tune_method:

  Method for tuning hyperparameters across the inner resamples:
  \`"grid"\` (default, standard \[cv_coxnet()\] grid search),
  \`"race_anova"\` (ANOVA racing), or \`"race_win_loss"\`
  (\[finetune::tune_race_win_loss()\] racing; needs BradleyTerry2). See
  "Racing" below.

- control:

  Racing settings for \`tune_method = "race_anova"\` or
  \`"race_win_loss"\`: a list or a \[finetune::control_race()\] object.
  See \[cv_coxnet()\] for the fields the start/stop race uses.

- checkpoint_dir:

  Optional directory path where per-split checkpoints are stored.

- shard:

  Optional shard specification (e.g. \`c(1, 2)\`) for distributed
  computation.

- checkpoint_mismatch:

  Action on mismatch: \`"recompute"\`, \`"overwrite"\`, or \`"error"\`.

- on_complete:

  Optional callback \`function(result)\` invoked when \`cv_coxnet()\`
  finishes or errors (defaults to \`getOption("templecbe.on_complete",
  NULL)\`).

- ...:

  Further arguments passed to \[glmnet::glmnet()\], such as
  \`cox.ties\`, \`standardize\`, \`penalty.factor\`, or \`nlambda\`.
  With \`importance = "loco_mp"\` they are passed to
  \[loco_mp_coxnet()\] as well, so the minipatch models are fit like the
  tuned ones; arguments that \[loco_mp_coxnet()\] sets itself, such as
  \`B\`, are not forwarded.

- x:

  A \`nested_cv_coxnet\` object.

- summarize:

  For \`collect_metrics()\`: \`TRUE\` for the mean and standard error
  over outer splits, \`FALSE\` for each split's metrics.

## Value

A tibble, of class \`nested_cv_coxnet\`, with one row per outer split:
\`id\`, the chosen \`mixture\` and \`penalty\`, \`.metrics\` (outer
assessment-set metrics), \`.coefs\` (coefficients of the refit),
\`.fits\` (the refit itself, a \[coxnet()\] model at the split's chosen
\`mixture\` and \`penalty\`), and \`.inner\` (the inner
cross-validation's summarized metrics). If \`importance = "loco_mp"\`,
includes \`.importance\` with tidy LOCO-MP statistical inference. When
\`tune_method\` races, includes \`.race\`: a one-row tibble per outer
split with \`n_candidates\`, \`n_eliminated\`, \`n_survivors\`,
\`n_resamples\` (inner resamples) and \`candidates\` (one row per
candidate with \`mixture\` and \`penalty\` — the finetune path lists
only the tuned parameters — \`n_resamples\`, the inner resamples it was
scored on, and \`eliminated\`). When a race produced no candidates, the
row is a zero-count row (\`n_candidates = 0\` with an empty
\`candidates\` table), never \`NULL\`, so \`.race\[\[i\]\]\` is always a
one-row tibble. On counting-process data only \`race_anova\` is
available (see "Racing"); \`race_win_loss\` is for right-censored data
only. The \`.fits\` column is what \[predict_event_risk()\] (and through
it \[calibration_from_model()\] and \[decision_curve_from_model()\])
predicts from: a nested result has no single final model, so the risk at
a horizon is one minus the mean of the outer splits' predicted survival
at that horizon, each split at its own chosen \`penalty\`.
\`tune::collect_metrics()\` averages \`.metrics\` over outer splits;
\[tidy_nested_cv_coxnet()\] and \[summarize_loco_mp()\] unnest and
summarize \`.coefs\` and \`.importance\`, and
\[tidy_outer_coefficients()\], \[summarize_outer_importance()\] and
\[plot_outer_importance()\] accept the result directly.
\[subject_nested_cv()\] builds \`object\` with grouped folds stratified
by subject event status.

## Details

Build \`object\` with grouped resampling at both levels, e.g.
\`nested_cv(data, outside = group_vfold_cv(group = subject_id), inside =
group_vfold_cv(group = subject_id))\`, so a subject's start/stop rows
stay together. This is checked: if a subject (or, when \`group\` is set,
a group) is in both the analysis and the assessment set of an outer
split or of any inner resample, \`nested_cv_coxnet()\` stops before
fitting anything, because the model would be scored on subjects it was
fit on. Out-of-bag assessment sets of bootstraps pass.
\`check_subject_overlap = FALSE\` skips the check, for overlap that is
deliberate.

The outer assessment set is scored as in \[cv_coxnet()\]: a subject with
a missing predictor after preprocessing, such as a factor level the
outer analysis set never saw, is left out with a warning, and an outer
split with no subject left is an error.

The outer metrics describe the procedure, not one final model: the
chosen settings can differ between outer splits. To fit a final model,
run \[cv_coxnet()\] on all the data.

## Nested CV vs Random-Alpha Design

It is essential to select the appropriate validation and tuning workflow
for your project goal:

- **Nested Cross-Validation (`nested_cv_coxnet`)**: Designed for
  *unbiased generalization performance estimation* of the end-to-end
  modeling pipeline. Hyperparameters (`mixture` and `penalty`) are
  selected on inner resamples (e.g., using \[rsample::group_vfold_cv()\]
  or \[rsample::group_bootstraps()\]), and the refit model is scored on
  completely held-out outer assessment subjects. The chosen parameters
  may differ across outer splits, reflecting pipeline sensitivity and
  guarding against optimization bias.

- **Random-Alpha Exploration (`tune_over_alpha` /
  `summarize_tune_results`)**: Designed for *exploratory hyperparameter
  tuning and model selection* across a continuous spectrum of \\\alpha
  \in \[0, 1\]\\. This workflow is ideal for comparing penalty
  properties (Ridge vs Lasso vs Elastic Net), profiling coefficient
  shrinkage paths across resamples, and choosing a single final model
  specification for reporting or deployment.

## Racing

With \`tune_method = "race_anova"\`, how the inner resamples are raced
depends on the outcome:

- **Start/stop data**, \`Surv(start, stop, event)\`, and gap mode: the
  race runs inside \[cv_coxnet()\] on glmnet directly (see "Racing" in
  \[cv_coxnet()\]). It uses finetune's elimination rule but scores at
  the subject level, like the grid search, so \`metric\`, \`rule\`,
  \`penalty\`, \`covariates\` and \`recurrent\` apply as they do for
  \`"grid"\`. \[finetune::tune_race_anova()\] cannot be used here: the
  yardstick survival metrics need a right-censored truth, and tune
  scores each start/stop row as if it were a subject.

- **Right-censored data**, \`Surv(time, status)\`: the race is
  \[finetune::tune_race_anova()\] on a parsnip
  \`proportional_hazards()\` workflow, through \[tune_race_survival()\],
  selecting by \`metric\` (default \`"brier_survival_integrated"\`).

\`"race_win_loss"\` is available for right-censored data only.

## See also

\[cv_coxnet()\], \[coxnet()\], \[loco_mp_coxnet()\],
\[tune_race_survival()\], \[subject_nested_cv()\]

## Examples

``` r
# \donttest{
if (requireNamespace("glmnet", quietly = TRUE) &&
    requireNamespace("survival", quietly = TRUE) &&
    requireNamespace("rsample", quietly = TRUE) &&
    requireNamespace("yardstick", quietly = TRUE)) {
  set.seed(1)
  long <- do.call(rbind, lapply(1:90, function(i) {
    k <- sample(1:3, 1)
    stops <- cumsum(stats::runif(k, 2, 6))
    risk <- stats::rnorm(1)
    data.frame(subject_id = i, tstart = c(0, utils::head(stops, -1)), tstop = stops,
               status = c(rep(0, k - 1), stats::rbinom(1, 1, stats::plogis(risk))),
               x1 = risk + stats::rnorm(k, sd = 0.2), x2 = stats::rnorm(k), x3 = stats::rnorm(k))
  }))
  folds <- rsample::nested_cv(
    long,
    outside = rsample::group_vfold_cv(group = subject_id, v = 3),
    inside = rsample::group_vfold_cv(group = subject_id, v = 3)
  )
  res <- nested_cv_coxnet(
    folds, survival::Surv(tstart, tstop, status) ~ x1 + x2 + x3,
    subject_id = "subject_id", mixture = c(0.5, 1),
    metrics = yardstick::metric_set(yardstick::brier_survival_integrated,
                                    yardstick::concordance_survival),
    nlambda = 20, cox.ties = "breslow"
  )
  res
}
#> # A tibble: 3 × 7
#>   id        mixture penalty .metrics         .coefs   .fits      .inner  
#>   <chr>       <dbl>   <dbl> <list>           <list>   <list>     <list>  
#> 1 Resample1     0.5  0.0193 <tibble [2 × 4]> <tibble> <cxnt_mdl> <tibble>
#> 2 Resample2     1    0.0628 <tibble [2 × 4]> <tibble> <cxnt_mdl> <tibble>
#> 3 Resample3     1    0.0447 <tibble [2 × 4]> <tibble> <cxnt_mdl> <tibble>
# }
```
