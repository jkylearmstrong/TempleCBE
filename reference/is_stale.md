# Check if an Output File is Stale

Stale when the output is missing, or older than its producer script, one
of the producer's dependencies, or anything those dependencies read
(through helper scripts); and, when \`all_objects\` is given, when any
dependency is itself the output of a stale producer, however far up the
pipeline and whether or not that producer renders a document. Without
\`all_objects\` only the direct sources are compared.

## Usage

``` r
is_stale(
  output_file,
  producer_script,
  all_objects = NULL,
  seen = character(),
  cache = NULL
)
```

## Arguments

- output_file:

  A \[FilePath\] object representing the output file to check.

- producer_script:

  A \[FileOutputs\] object representing the producer.

- all_objects:

  Optional list of pipeline objects to check upstream staleness.

- seen:

  Internal character vector of visited node names to prevent cycles.

- cache:

  Internal environment caching staleness decisions.

## Value

Logical \`TRUE\` if stale, \`FALSE\` otherwise.
