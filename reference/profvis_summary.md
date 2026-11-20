# Summarize a \`profvis\` Profile

Tabulates the samples of a \[profvis::profvis()\] profile by function:
memory, memory increments, call frequency, stack depth, and memory over
time.

## Usage

``` r
profvis_summary(prof)
```

## Arguments

- prof:

  A \`profvis\` object.

## Value

A named list of tibbles:

- \`memory_by_function\`:

  Summed \`memalloc\`, share, calls, and memory per call, by function.

- \`memory_increment_by_function\`:

  Summed \`meminc\`, share, calls, and increment per call, by function.

- \`calls_by_function\`:

  Samples per function and share.

- \`deepest_calls\`:

  Each function's deepest sample, deepest first.

- \`memory_over_time\`:

  Summed \`memalloc\` per sample time.

## Details

Memory columns sum the sampled \`memalloc\`/\`meminc\` values, so they
rank functions by how much memory was allocated while they were on the
stack; they are not exact allocation totals.

## Examples

``` r
if (FALSE) { # \dontrun{
prof <- profvis::profvis(for (i in 1:50) sort(runif(1e5)))
profvis_summary(prof)$memory_by_function
} # }
```
