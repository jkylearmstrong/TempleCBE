# Run an End-to-End Nested Cross-Validation Survival Pipeline (E1)

Executes the complete multi-stage nested survival analysis workflow in a
single call:

1.  Pre-flight data validation via \[validate_start_stop()\] (stopping
    or warning on structural anomalies or absence of censoring).

2.  Grouped nested resampling fold construction via
    \[rsample::nested_cv()\] and \[rsample::group_vfold_cv()\].

3.  Inner hyperparameter tuning across outer folds via
    \[tune_alpha_by_split()\] with per-task fingerprinted checkpoints.

4.  Candidate model selection via \[select_candidate_models()\].

5.  Outer-fold model scoring via \[score_outer()\] with fingerprinted
    checkpoints.

6.  Predictor importance and selection stability summarization across
    outer folds via \[summarize_outer_importance()\].

7.  Censoring weight sensitivity comparison via
    \[compare_censoring_weights()\] (automatically skipping the
    duplicate fit when the training data has no censored subjects).

## Usage

``` r
nested_survival_run(
  data,
  recipe = NULL,
  id = "id",
  start = "tstart",
  stop = "tstop",
  status = "status",
  domain = "default",
  run_dir = NULL,
  shard = NULL,
  collect = FALSE,
  feature_names = NULL,
  formulas = NULL,
  outer_v = 5L,
  inner_v = 5L,
  inner_bootstraps = NULL,
  nested_folds = NULL,
  alphas = NULL,
  num_alpha_values = 6L,
  num_fixed = min(6L, as.integer(num_alpha_values)),
  ntile = 0.25,
  top_n = NULL,
  select_by = c("row", "alpha"),
  internal_folds = 5L,
  censoring_weights = c("ipcw", "none"),
  coefficients = c("all", "nonzero"),
  compare_censoring = TRUE,
  no_censoring_action = c("warn", "stop", "ignore"),
  error_on_invalid = TRUE,
  checkpoint_mismatch = c("recompute", "overwrite", "error"),
  chunk_size = NULL,
  seed = 42L,
  on_complete = getOption("templecbe.on_complete", NULL),
  progress = FALSE,
  ...
)
```

## Arguments

- data:

  A data frame containing counting-process \`(start, stop\]\` or
  right-censored survival intervals.

- recipe:

  Optional \[recipes::recipe()\] object specifying preprocessing. When
  \`NULL\`, a default recipe is built on \`data\` assigning \`id\`,
  \`start\`, \`stop\`, and \`status\` the \`"id variable"\` role.

- id, start, stop, status:

  Column names (as strings or bare symbols) for subject identifier,
  interval start time, interval stop time, and event status indicator
  (\`0\` = censored, \`1\` = event). Defaults to \`"id"\`, \`"tstart"\`,
  \`"tstop"\`, and \`"status"\`.

- domain:

  Character label identifying the clinical or modeling domain (default
  \`"default"\`).

- run_dir:

  Optional directory path where fingerprinted checkpoints, stage
  artifacts (\`nested_survival_run.rds\`), and \`render_manifest.json\`
  are stored.

- shard:

  Optional integer vector \`c(i, n)\` specifying that this machine
  should evaluate shard \`i\` of \`n\` into \`run_dir\`.

- collect:

  Logical (default \`FALSE\`). When \`TRUE\` and \`run_dir\` contains
  completed sharded checkpoints, merges them with
  \[collect_checkpoints()\] before computing candidate selection, outer
  importance, and summaries.

- feature_names:

  Optional character vector of candidate predictor column names.
  Defaults to all predictor variables in \`recipe\` (or all columns of
  \`data\` other than \`id\`, \`start\`, \`stop\`, and \`status\`).

- formulas:

  Optional character vector or list of candidate right-hand-side
  formulas. Defaults to a single full formula combining
  \`feature_names\`.

- outer_v:

  Number of outer cross-validation folds (default \`5L\`).

- inner_v:

  Number of inner cross-validation folds (default \`5L\`).

- inner_bootstraps:

  Optional integer number of grouped inner bootstrap resamples (when
  non-\`NULL\`, uses \[rsample::group_bootstraps()\] instead of
  \[rsample::group_vfold_cv()\] for the inner resamples).

- nested_folds:

  Optional pre-constructed \[rsample::nested_cv()\] object. Overrides
  \`outer_v\`, \`inner_v\`, and \`inner_bootstraps\` when supplied.

- alphas:

  Optional numeric vector of elastic-net mixing values in \\\[0, 1\]\\
  for inner tuning.

- num_alpha_values:

  Number of alpha values to generate when \`alphas\` is \`NULL\`
  (default \`6L\`).

- num_fixed:

  Number of evenly spaced fixed alpha values when \`alphas\` is \`NULL\`
  (default \`min(6L, num_alpha_values)\`).

- ntile:

  Quantile cutoff in \`(0, 1\]\` passed to \[select_candidate_models()\]
  (default \`0.25\`).

- top_n:

  Optional integer number of top alphas to select in
  \[select_candidate_models()\] (overrides \`ntile\` when non-\`NULL\`).

- select_by:

  Selection granularity for \[select_candidate_models()\]: \`"row"\`
  (default) or \`"alpha"\`.

- internal_folds:

  Number of internal folds passed to \[glmnet_IBS()\] (default \`5L\`).

- censoring_weights:

  Primary censoring weight scheme for tuning and outer scoring:
  \`"ipcw"\` (default) or \`"none"\`.

- coefficients:

  Which coefficients to store in \[score_outer()\]: \`"all"\` (default)
  or \`"nonzero"\`.

- compare_censoring:

  Logical (default \`TRUE\`). Run \[compare_censoring_weights()\] on the
  candidate grid across outer folds.

- no_censoring_action:

  Action when \[validate_start_stop()\] detects zero censored subjects
  (\`n_censored == 0\`): \`"warn"\` (default), \`"stop"\`, or
  \`"ignore"\`.

- error_on_invalid:

  Logical (default \`TRUE\`). Stop immediately if
  \[validate_start_stop()\] detects structural interval errors.

- checkpoint_mismatch:

  Action when an existing checkpoint in \`run_dir\` has a mismatched
  fingerprint: \`"recompute"\` (default), \`"overwrite"\`, or
  \`"error"\`.

- chunk_size:

  Optional integer number of tasks per worker chunk passed to
  \[tune_alpha_by_split()\] and \[score_outer()\] (default \`NULL\`).

- seed:

  Integer RNG seed for reproducible fold construction, tuning, and outer
  scoring (default \`42L\`).

- on_complete:

  Optional callback \`function(result)\` invoked when the run completes
  or errors (defaults to \`getOption("templecbe.on_complete", NULL)\`).

- progress:

  Logical (default \`FALSE\`). Display progress bars.

- ...:

  Additional arguments passed to \[glmnet_IBS()\] (e.g. \`cox.ties\`,
  \`rule\`, \`metric\`, \`tune_engine\`, \`eval_time\`).

## Value

An S3 object of class \`cbe_nested_survival_run\` containing:

- \`domain\`:

  Domain identifier string.

- \`validation\`:

  Result of \[validate_start_stop()\].

- \`nested_folds\`:

  The \[rsample::nested_cv()\] object.

- \`tune_results\`:

  Tibble of inner tuning results across outer folds.

- \`candidates\`:

  Tibble of candidate models from \[select_candidate_models()\].

- \`outer_scores\`:

  Tibble of outer-fold model evaluations from \[score_outer()\].

- \`outer_coefficients\`:

  Long tibble of outer-fold coefficients from
  \[tidy_outer_coefficients()\].

- \`importance\`:

  Predictor importance summary tibble from
  \[summarize_outer_importance()\].

- \`censoring_comparison\`:

  Tibble from \[compare_censoring_weights()\] (when \`compare_censoring
  = TRUE\`).

- \`run_dir\`:

  Normalized path to \`run_dir\` (or \`NULL\`).

- \`fingerprint\`:

  SHA-256 fingerprint of the run configuration and data.

- \`seed\`:

  RNG seed used.

- \`seed_scheme\`:

  Seed generation scheme version (\`"v0.5.6"\`).

- \`shard\`:

  Shard specification (\`NULL\` or \`c(i, n)\`).

- \`status\`:

  \`"completed"\` or \`"shard_completed"\`.

## Details

When \`run_dir\` is provided, every stage writes fingerprinted
checkpoints and a run manifest (\`render_manifest.json\`) into
\`run_dir\` and resumes from disk on rerun without recomputing completed
tasks. When \`shard = c(i, n)\` is supplied, only shard \`i\` of \`n\`
is evaluated; once all shards finish, rerun with \`shard = NULL\` (or
\`collect = TRUE\`) to merge checkpoints and finalize the run object.

## See also

\[validate_start_stop()\], \[tune_alpha_by_split()\],
\[select_candidate_models()\], \[score_outer()\],
\[summarize_outer_importance()\], \[compare_censoring_weights()\],
\[collect_checkpoints()\], \[repro_appendix()\]
