# Compare Model Performance Under Unweighted and IPCW Censoring Weights

Evaluates candidate models on an outer resampling split under both
\`"none"\` (unweighted legacy scoring) and \`"ipcw"\` (proper inverse
probability of censoring weighted scoring), returning a tidy
side-by-side comparison table of integrated Brier scores (IBS) and model
ranks, along with rank agreement statistics (Spearman rank correlation
and Kendall's tau).

## Usage

``` r
compare_censoring_weights(
  object,
  grid,
  recipe = NULL,
  ...,
  checkpoint_dir = NULL,
  shard = NULL,
  checkpoint_mismatch = c("recompute", "overwrite", "error"),
  seed = NULL,
  on_complete = getOption("templecbe.on_complete", NULL),
  progress = FALSE
)
```

## Arguments

- object:

  An \`rsplit\`, \`nested_cv\`, list of \`rsplit\`s, or a precomputed
  data frame scored under \`censoring_weights = "none"\`.

- grid:

  A candidate model grid (data frame with \`alpha\` and optional
  \`formula\`), or a precomputed data frame scored under
  \`censoring_weights = "ipcw"\` when \`object\` is already a scored
  data frame.

- recipe:

  Preprocessing recipe passed to \[score_outer()\].

- ...:

  Additional arguments passed to \[glmnet_IBS()\] through
  \[score_outer()\].

- checkpoint_dir:

  Optional directory to save fingerprinted checkpoints for outer scoring
  passes.

- shard:

  Optional integer vector \`c(i, n)\` for sharded execution.

- checkpoint_mismatch:

  Action when a checkpoint fingerprint mismatches: \`"recompute"\`
  (default), \`"overwrite"\`, or \`"error"\`.

- seed:

  Optional integer seed for reproducible draws and fitting.

- on_complete:

  Optional callback \`function(result)\` invoked when the run finishes
  or errors (defaults to \`getOption("templecbe.on_complete", NULL)\`).

- progress:

  Logical (default \`FALSE\`). Display progress indication.

## Value

A tibble with columns:

- outer_fold:

  Outer fold identifier (if present)

- model:

  Model identifier

- alpha:

  Elastic net mixing parameter

- formula:

  Formula specification (if present)

- ibs_none:

  Integrated Brier score under \`censoring_weights = "none"\`

- ibs_ipcw:

  Integrated Brier score under \`censoring_weights = "ipcw"\`

- rank_none:

  Model rank under unweighted IBS (1 = best / lowest IBS)

- rank_ipcw:

  Model rank under IPCW IBS (1 = best / lowest IBS)

- rank_diff:

  \`rank_ipcw - rank_none\`

The returned object has attributes \`"rank_agreement"\` (containing
\`spearman\`

A tibble with columns \`outer_fold\` (if grouped), \`model\`, \`alpha\`,
\`formula\`, \`ibs_none\`, and \`ibs_ipcw\`, plus attributes
\`"rank_agreement"\` (a list with rank correlation metrics across
candidates, \`spearman\` and \`kendall\`), \`"single_fit_no_censoring"\`
(\`TRUE\` when the duplicate fit was skipped), and
\`"n_score_outer_runs"\`.

## Details

When the training data across splits contains no censored subjects, this
is detected before model fitting so \[score_outer()\] is run only once
and both \`ibs_none\` and \`ibs_ipcw\` are populated from that single
fit.

Alternatively, accepts two precomputed tables of model evaluations (one
under \`"none"\` and one under \`"ipcw"\`) and merges them into the
comparison table.

## See also

\[score_outer()\], \[glmnet_IBS()\], \[select_candidate_models()\]
