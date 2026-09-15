# Procedural Synthetic Last Name Generator

Generates realistic, pronounceable synthetic surnames using phonetic
onsets, vowels, and common surname endings (e.g., \`-berg\`, \`-ton\`,
\`-man\`, \`-wood\`, \`-ford\`). Supports strict exclusion lists (e.g.
to guarantee real investigator surnames are never generated) and
reproducible seed-based sampling without mutating the global RNG state.

## Usage

``` r
generate_last_names(
  n = 10,
  seed = NULL,
  exclude = character(),
  max_tries = 100L
)
```

## Arguments

- n:

  Integer, number of unique names to generate (default 10).

- seed:

  Optional integer, random seed for reproducibility.

- exclude:

  Character vector of surnames (case-insensitive) that must not be
  generated.

- max_tries:

  Integer, maximum attempts multiplier before throwing an error if
  distinct names cannot be generated.

## Value

A character vector of length \`n\` containing synthetic surnames.

## Examples

``` r
# Generate 5 random last names
generate_last_names(5)
#> [1] "Peninen"       "Nozoun"        "Proakowieberg" "Gaiprais"     
#> [5] "Freavoland"   

# Reproducible generation with exclusions
generate_last_names(3, seed = 1, exclude = c("Madison", "Davison"))
#> [1] "Fajeford"   "Nesin"      "Draimousen"
```
