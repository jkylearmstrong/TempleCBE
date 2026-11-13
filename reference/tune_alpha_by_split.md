# Tune Over \`alpha\` for Every Split of a Resample

Evaluates candidate \`(split, alpha)\` (or \`(split, formula)\`)
combinations as a single flat task grid across all available
\[future::plan()\] workers and row-binds the successful fits.

## Usage

``` r
tune_alpha_by_split(
  object,
  ...,
  num_alpha_values = 10,
  num_fixed = 6,
  alphas = NULL,
  formulas = NULL,
  progress = FALSE,
  seed = NULL,
  calibrate = TRUE,
  checkpoint_dir = NULL,
  checkpoint_mismatch = c("recompute", "overwrite", "error"),
  shard = NULL,
  chunk_size = NULL,
  on_complete = getOption("templecbe.on_complete", NULL),
  .fn_name = "tune_alpha_by_split"
)
```

## Arguments

- object:

  A resampling object with a \`splits\` column (e.g. one element of the
  \`inner_resamples\` column of \[rsample::nested_cv()\]).

- ...:

  Arguments passed to \[glmnet_IBS()\] (\`recipe\`, \`feature_names\`,
  \`time_data\`, \`id_col\`, \`cox.ties\`, ...).

- num_alpha_values:

  Total number of \`alpha\` values in the grid.

- num_fixed:

  Number of evenly spaced values from 0 to 1 in the grid.

- alphas:

  Optional numeric vector of \`alpha\` values to use instead of the
  generated grid; in formula mode, one per formula.

- formulas:

  Optional character vector of \`+\`-separated feature sets, or a list
  of one-sided formulas, each fit as a separate model (see
  \[glmnet_IBS()\]'s \`formula\`).

- progress:

  Show a progress bar.

- seed:

  Optional integer seed for reproducible random draws across splits.

- calibrate:

  Logical (default TRUE); whether to benchmark baseline serial
  execution. Set to FALSE to skip serial benchmarking and dispatch tasks
  directly to parallel workers.

- checkpoint_dir:

  Optional directory path to save checkpoint files. Finished
  splits/tasks are loaded from disk on rerun when their fingerprint
  matches, skipping recomputation.

- checkpoint_mismatch:

  Action when an existing checkpoint file's fingerprint does not match
  the current inputs/settings: \`"recompute"\` (default; warns and
  recomputes), \`"overwrite"\` (overwrites mismatched files in place and
  removes stale checkpoints from previous runs), or \`"error"\` (stops
  immediately).

- shard:

  Optional integer vector \`c(i, n)\` specifying that this call should
  process shard \`i\` of \`n\` across machines into \`checkpoint_dir\`.
  Use \[collect_checkpoints()\] to merge all shards once finished.

- chunk_size:

  Optional positive integer passed to \[furrr::furrr_options()\]
  controlling the number of tasks batched per worker invocation.
  Defaults to \`NULL\` (one chunk per worker so each worker receives all
  of its assigned tasks in a single batch).

- on_complete:

  Optional callback \`function(result)\` (or option
  \`templecbe.on_complete\`) invoked when the run finishes or errors.

- .fn_name:

  Internal caller function name for logging and checkpointing metadata.

## Value

A tibble of \[glmnet_IBS()\] results with an \`inner_resamples_splits\`
column giving the split's identifier.

## Nested Parallelism, Benchmarking & Physical Cores

When tuning penalized survival models across nested resampling
structures, computation scales multiplicatively: \\\text{outer folds}
\times \text{inner splits} \times \text{alphas} \times \text{internal CV
folds}\\. \`tune_alpha_by_split()\` flattens \`inner splits x alphas\`
into a single task pool (e.g. 7 splits x 10 alphas = 70 tasks), sends
the shared dataset, recipe, and split definitions once per worker, and
pre-preps/bakes each split once per worker rather than repeating recipe
preprocessing for every \`alpha\`.

\*\*Physical Cores vs. Hyper-Threading:\*\* When configuring a parallel
backend (e.g. \`future::plan(future::multisession, workers = ...)\`), it
is strongly recommended to set worker count to the number of
\*physical\* CPU cores (\[parallel::detectCores(logical = FALSE)\]),
leaving 1–2 cores for the coordinator process and OS I/O, rather than
using all hyper-threaded logical cores. Because CPU-intensive coordinate
descent and Breslow hazard calculations saturate CPU execution units and
L1/L2 caches, two worker processes executing on siblings of the same
physical core contend for the same ALUs, FPUs, and memory bandwidth,
which frequently doubles per-task compute latency inside concurrent
workers (e.g., tasks taking ~2.2 s serially taking ~5.0 s inside 14
concurrent workers on 8 physical cores). Sizing workers to physical
cores yields optimal wall-clock speedup and reduces IPC contention. Set
`future::plan(multisession, workers = 7)` before calling while keeping
inner model evaluations sequential (`parallel = FALSE` in
[`glmnet_IBS()`](https://jkylearmstrong.github.io/TempleCBE/reference/glmnet_IBS.md)).
Use \`chunk_size\` to control how many tasks are batched per worker
message.

## See also

\[glmnet_IBS()\], \[tune_over_alpha()\], \[collect_checkpoints()\],
\[clean_checkpoints()\], \[nested_cv_coxnet()\]
