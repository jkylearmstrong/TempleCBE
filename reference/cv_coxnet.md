# Cross-Validate a Penalized Cox Model With yardstick Survival Metrics

A tidymodels counterpart to \[glmnet::cv.glmnet()\] for right-censored
and start/stop survival data. Every combination of \`mixture\` and
\`penalty\` is fit on each analysis set and scored on the assessment set
with a yardstick survival metric set, and the best settings are chosen
by one metric: the integrated Brier score by default.

## Usage

``` r
cv_coxnet(x, ...)

# Default S3 method
cv_coxnet(x, ...)

# S3 method for class 'data.frame'
cv_coxnet(
  x,
  y,
  subject_id = NULL,
  group = NULL,
  mixture = 1,
  penalty = NULL,
  resamples = NULL,
  v = 10,
  metrics = NULL,
  eval_time = NULL,
  metric = NULL,
  covariates = c("path", "baseline"),
  trunc = 0.05,
  recurrent = c("error", "first", "gap"),
  parallel = FALSE,
  parallel_over = c("resamples", "everything"),
  check_subject_overlap = TRUE,
  checkpoint_dir = NULL,
  shard = NULL,
  checkpoint_mismatch = c("recompute", "overwrite", "error"),
  seed = NULL,
  tune_method = c("grid", "race_anova"),
  control = NULL,
  on_complete = getOption("templecbe.on_complete", NULL),
  ...
)

# S3 method for class 'matrix'
cv_coxnet(
  x,
  y,
  subject_id = NULL,
  group = NULL,
  mixture = 1,
  penalty = NULL,
  resamples = NULL,
  v = 10,
  metrics = NULL,
  eval_time = NULL,
  metric = NULL,
  covariates = c("path", "baseline"),
  trunc = 0.05,
  recurrent = c("error", "first", "gap"),
  parallel = FALSE,
  parallel_over = c("resamples", "everything"),
  check_subject_overlap = TRUE,
  checkpoint_dir = NULL,
  shard = NULL,
  checkpoint_mismatch = c("recompute", "overwrite", "error"),
  seed = NULL,
  tune_method = c("grid", "race_anova"),
  control = NULL,
  on_complete = getOption("templecbe.on_complete", NULL),
  ...
)

# S3 method for class 'formula'
cv_coxnet(
  formula,
  data,
  subject_id = NULL,
  group = NULL,
  mixture = 1,
  penalty = NULL,
  resamples = NULL,
  v = 10,
  metrics = NULL,
  eval_time = NULL,
  metric = NULL,
  covariates = c("path", "baseline"),
  trunc = 0.05,
  recurrent = c("error", "first", "gap"),
  parallel = FALSE,
  parallel_over = c("resamples", "everything"),
  check_subject_overlap = TRUE,
  checkpoint_dir = NULL,
  shard = NULL,
  checkpoint_mismatch = c("recompute", "overwrite", "error"),
  seed = NULL,
  tune_method = c("grid", "race_anova"),
  control = NULL,
  on_complete = getOption("templecbe.on_complete", NULL),
  ...
)

# S3 method for class 'recipe'
cv_coxnet(
  x,
  data,
  subject_id = NULL,
  group = NULL,
  mixture = 1,
  penalty = NULL,
  resamples = NULL,
  v = 10,
  metrics = NULL,
  eval_time = NULL,
  metric = NULL,
  covariates = c("path", "baseline"),
  trunc = 0.05,
  recurrent = c("error", "first", "gap"),
  parallel = FALSE,
  parallel_over = c("resamples", "everything"),
  check_subject_overlap = TRUE,
  checkpoint_dir = NULL,
  shard = NULL,
  checkpoint_mismatch = c("recompute", "overwrite", "error"),
  seed = NULL,
  tune_method = c("grid", "race_anova"),
  control = NULL,
  on_complete = getOption("templecbe.on_complete", NULL),
  ...
)
```

## Arguments

- x:

  A data frame or matrix of predictors, or a \[recipes::recipe()\] whose
  outcome is a single \`Surv\` column.

- ...:

  Further arguments passed to \[glmnet::glmnet()\], such as \`cox.ties\`
  or \`nlambda\`.

- y:

  For the data frame and matrix methods, a \[survival::Surv()\] outcome
  with one element per row of \`x\`.

- subject_id:

  Subject identifier: a column name for the formula and recipe methods
  (for a recipe, a single column with role \`"id"\` is used when
  \`NULL\`), or a vector for the data frame and matrix methods. Required
  for start/stop data; for right-censored data, each row is a subject
  when \`NULL\`.

- group:

  Optional grouping for folds, in the same form as \`subject_id\`.
  Defaults to \`subject_id\`.

- mixture:

  Numeric vector of elastic-net mixing values to try.

- penalty:

  Optional decreasing penalty path; by default glmnet's path for each
  \`mixture\`.

- resamples:

  An \`rset\` of \`data\`, such as \[rsample::group_vfold_cv()\].
  Overrides \`v\` and \`group\`. No subject (or group) may be in both
  the analysis and the assessment set of a split; see Details and
  \`check_subject_overlap\`.

- v:

  Number of folds when \`resamples\` is \`NULL\`.

- metrics:

  A \[yardstick::metric_set()\] of survival metrics. Defaults to the
  integrated Brier score, concordance, time-specific Brier score, and
  time-dependent ROC AUC. In gap mode only \`concordance_survival\`,
  \`brier_survival_integrated\` and \`brier_survival\` are available.

- eval_time:

  Evaluation times. Defaults to deciles (10th to 90th percentile) of the
  observed event times.

- metric:

  Name of the metric used to choose settings; the first metric in
  \`metrics\` by default.

- covariates:

  \`"path"\` or \`"baseline"\`; see Details.

- trunc:

  Lower bound for the censoring probability in the Graf weights.

- recurrent:

  How recurrent (start/stop) data are scored: \`"error"\` (default; a
  subject with an event before its last interval stops the fit),
  \`"first"\` (score the time to the first event only), or \`"gap"\`
  (score the whole recurrent process by gap; see Details). In gap mode
  \`metrics\` may contain \`concordance_survival\` (the default there),
  \`brier_survival_integrated\` and \`brier_survival\`, and
  \`eval_time\` sets the gap times the Brier score is integrated over
  (with concordance only, it is used only for the refit model's
  predictions, not for scoring).

- parallel:

  If \`TRUE\`, fit folds with \[furrr::future_map()\]; set a
  \[future::plan()\] first.

- parallel_over:

  How to parallelize when \`parallel\` is \`TRUE\`: \`"resamples"\`
  (default) or \`"everything"\`, as in \[glmnet_IBS()\].

- check_subject_overlap:

  If \`TRUE\` (the default), stop when a supplied \`resamples\` puts the
  same subject (or, when \`group\` is set, the same group) in both the
  analysis and the assessment set of a split; see Details. \`FALSE\`
  skips the check. Folds that \`cv_coxnet()\` builds itself are never
  checked, as they are grouped by subject already.

- checkpoint_dir:

  Optional directory path where per-resample \`.rds\` checkpoint files
  are stored. If \`NULL\` (default), executes without caching.

- shard:

  Optional integer vector \`c(i, n)\` specifying that this execution
  evaluates shard \`i\` of \`n\` resample folds.

- checkpoint_mismatch:

  Action when a checkpoint fingerprint mismatches: \`"recompute"\`
  (default), \`"overwrite"\`, or \`"error"\`.

- seed:

  Optional integer seed for reproducible fold generation when
  \`resamples\` is \`NULL\`. When \`checkpoint_dir\` is supplied and
  \`resamples\` is \`NULL\`, defaults to \`1L\` to ensure
  cross-validation fold partitions are identical across calls, enabling
  instant checkpoint reuse.

- tune_method:

  \`"grid"\` (default) scores every candidate on every resample;
  \`"race_anova"\` races them; see Details.

- control:

  Racing settings, used when \`tune_method = "race_anova"\`: a list or a
  \[finetune::control_race()\] object, of which \`burn_in\` (default
  \`3\`, at most the number of resamples minus one), \`alpha\` (default
  \`0.05\`) and \`randomize\` (default \`TRUE\`, shuffle the order of
  the resamples) are used.

- on_complete:

  Optional callback \`function(result)\` invoked when \`cv_coxnet()\`
  finishes or errors (defaults to \`getOption("templecbe.on_complete",
  NULL)\`).

- formula:

  A formula with a \`Surv()\` outcome, as in \[coxnet()\].

- data:

  A data frame with the outcome, predictors, \`subject_id\`, and
  \`group\` columns.

## Value

A \`cv_coxnet\` object, with: \* \`metrics\`: mean, \`n\`, and
\`std_err\` of each metric by \`mixture\`, \`penalty\`, and
\`.eval_time\`; \* \`fold_metrics\`: every fold's metrics; \*
\`best_by_mixture\`: \`lambda_min\` and \`lambda_1se\` for each
\`mixture\`; \* \`mixture\`, \`lambda_min\`, \`lambda_1se\`, \`metric\`,
\`direction\`; \* \`fit\`: a \[coxnet()\] model fit to all of \`data\`
at the chosen \`mixture\`, with \`lambda_min\` as its default penalty;
\* \`eval_time\`, \`resamples\`, \`covariates\`, and \`recurrent\` (how
recurrent data were scored: \`"error"\`, \`"first"\`, or \`"gap"\`); \*
\`tune_method\`, and \`race\`: with racing, one row per candidate
(\`mixture\`, \`penalty\`, \`n_resamples\`, \`eliminated\`), else
\`NULL\`.

Use \`predict()\`, \`tidy()\`, \`autoplot()\`, and
\`tune::collect_metrics()\` on it.

## Details

\*\*Folds.\*\* Unless \`resamples\` is given, folds come from
\[rsample::group_vfold_cv()\] grouped by \`group\` (default:
\`subject_id\`), so a subject's start/stop rows are never split between
analysis and assessment sets, as they would be by \`cv.glmnet()\`'s
row-level folds. Group by a coarser unit, such as study site, for
leave-sites-out cross-validation; each subject must then belong to one
group.

\*\*Supplied resamples.\*\* With \`resamples\`, every split is checked
before anything is scored: if a subject (or, when \`group\` is set, a
group) is in both the analysis and the assessment set of a split,
\`cv_coxnet()\` stops, because the model would be scored on subjects it
was fit on. That is what a row-level \[rsample::vfold_cv()\] does to
start/stop data. Group the folds by the subject instead, with
\[rsample::group_vfold_cv()\] or \[rsample::group_bootstraps()\]; the
out-of-bag assessment set of a bootstrap never overlaps its analysis
set. \`check_subject_overlap = FALSE\` switches the check off, for
overlap that is deliberate.

\*\*Fitting.\*\* For each \`mixture\`, one penalty path is computed on
all of \`data\` (as \`cv.glmnet()\` does) and every fold is fit along
it. With a recipe, the recipe is prepped on each analysis set, so
preprocessing is learned inside the fold.

\*\*Predictions.\*\* Survival for each assessment-set subject is
computed from the Breslow baseline hazard of the analysis set. With
\`covariates = "path"\`, the cumulative hazard is integrated over the
subject's start/stop covariate path, carrying each interval's values
forward to the next interval and past the last one; with \`"baseline"\`,
only the covariates of the first interval are used, which uses no
information from after time 0. The static concordance metric uses
predicted survival at the last \`eval_time\`.

\*\*Scoring.\*\* Start/stop rows are collapsed to one row per subject
(\[surv_subject_truth()\]), weighted for censoring with Graf weights
from the analysis set (\[graf_weights()\]), and passed to \`metrics\`.
An assessment-set subject with a missing predictor after preprocessing,
such as a factor level the analysis set never saw, cannot be predicted:
it is left out at every penalty, so all penalties are scored on the same
subjects, with one warning per resample that gives the count. A resample
with no subject left is skipped. Recurrent events (an event before a
subject's last interval) stop with an error unless \`recurrent\` says
otherwise: \`"first"\` scores the time to first event
(\[surv_subject_truth()\]), and \`"gap"\` scores the whole recurrent
process by gap (see \[surv_gap_truth()\]). Gap-time concordance is
Harrell's C over gaps, using each interval's linear predictor as its
risk: it treats a subject's gaps as independent observations and needs
no censoring weights. The gap-time Brier score (integrated,
\`brier_survival_integrated\`, or at one time, \`brier_survival\`)
scores each gap by its conditional gap-time survival under the fitted
Andersen-Gill model – the Breslow baseline hazard integrated over the
gap's own window, times the gap's linear predictor – with gap-level
inverse-probability-of-censoring weights that condition on the gap's
entry time (see \[predict_gap_survival()\] and \[gap_graf_weights()\]).
Gaps are event-to-event: a subject's rows up to and including an event
form one gap, and the baseline hazard is integrated piecewise over the
gap's rows, so splitting a gap's rows does not change the score. The
censoring distribution \`G\` is the reverse Kaplan-Meier on the training
subjects' full follow-up – each subject's last interval
(\[surv_subject_truth()\] with \`recurrent = "last"\`), so a subject's
censoring time counts whenever follow-up ends in censoring, not only
when it ends at the first event. The Brier score is integrated over
\`eval_time\` the way \[yardstick::brier_survival_integrated()\]
integrates it, and averaged over gaps, so like concordance it describes
gaps, not subjects. A gap entering late can run past the last training
event time, where the Breslow baseline is flat, so its predicted
survival is too high over the rest of the window; the effect is bounded
by the 90th-percentile default \`eval_time\` and the capped censoring
weights, and the simulation shows no bias. Gap mode defaults to
concordance only; pass \`metrics\` to add the Brier scores, and
\`roc_auc_survival\` is not available there.

\*\*Selection.\*\* \`lambda.min\` is the penalty, across all \`mixture\`
values, with the best mean \`metric\` over folds (for time-specific
metrics, at the first \`eval_time\`). \`lambda.1se\` is the largest
penalty, at the same mixture, whose mean is within one standard error of
that best mean.

\*\*Racing.\*\* With \`tune_method = "race_anova"\`, the candidates
(every \`mixture\` and penalty) are raced over the resamples the way
\[finetune::tune_race_anova()\] races a tuning grid, but on glmnet
directly, so it works for start/stop data and in gap mode, which
finetune cannot score. The first \`burn_in\` resamples score every
candidate. Before each later resample, the selection \`metric\` is
modelled as \`.estimate ~ candidate + resample\` and a candidate is
dropped when the \`1 - alpha\` confidence interval of its difference
from the current best is entirely on the worse side; the next resamples
score only the survivors, and a mixture with no surviving penalty is no
longer fit. Settings are chosen among the survivors, so \`lambda.1se\`
is the largest surviving penalty within one standard error. \`metrics\`
and \`fold_metrics\` keep the eliminated candidates, with their smaller
\`n\`; \`race\` lists every candidate with the number of resamples it
was scored on and whether it was eliminated. Racing runs the resamples
after the burn-in one at a time and cannot be checkpointed.

## See also

\[coxnet()\], \[nested_cv_coxnet()\], \[surv_subject_truth()\]

## Examples

``` r
# \donttest{
if (requireNamespace("glmnet", quietly = TRUE) &&
    requireNamespace("survival", quietly = TRUE) &&
    requireNamespace("rsample", quietly = TRUE) &&
    requireNamespace("yardstick", quietly = TRUE)) {
  set.seed(1)
  # Start/stop data: up to 3 visits per subject, events only at the last one
  long <- do.call(rbind, lapply(1:80, function(i) {
    k <- sample(1:3, 1)
    stops <- cumsum(stats::runif(k, 2, 6))
    risk <- stats::rnorm(1)
    data.frame(subject_id = i, tstart = c(0, utils::head(stops, -1)), tstop = stops,
               status = c(rep(0, k - 1), stats::rbinom(1, 1, stats::plogis(risk))),
               x1 = risk + stats::rnorm(k, sd = 0.2), x2 = stats::rnorm(k), x3 = stats::rnorm(k))
  }))
  cv <- cv_coxnet(
    survival::Surv(tstart, tstop, status) ~ x1 + x2 + x3,
    data = long, subject_id = "subject_id", mixture = c(0.5, 1),
    v = 5, nlambda = 20, cox.ties = "breslow"
  )
  cv
  generics::tidy(cv, penalty = "lambda.1se")
}
#> # A tibble: 3 × 3
#>   term  estimate penalty
#>   <chr>    <dbl>   <dbl>
#> 1 x1       0.333   0.105
#> 2 x2       0       0.105
#> 3 x3       0       0.105
# }
```
