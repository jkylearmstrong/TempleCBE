# Generate Random PI Names or Tokens

Generates random synthetic principal investigator (PI) names, full
names, or cryptographic pseudonym tokens (\`PI\_...\`).

## Usage

``` r
generate_pi_names(
  n = 1,
  format = c("synthetic", "token", "full_name"),
  prefix = "PI_",
  n_chars = 16,
  rename_fn = NULL,
  seed = NULL,
  exclude = character(),
  allow_collisions = FALSE
)
```

## Arguments

- n:

  Integer, number of names to generate (default 1).

- format:

  Character, either \`"synthetic"\` (realistic surnames),
  \`"full_name"\` (synthetic first and last name), or \`"token"\`
  (cryptographic hash tokens). Aliases \`"surname"\` and \`"full"\` are
  also accepted.

- prefix:

  Character, prefix when format is \`"token"\` (default \`"PI\_"\`).

- n_chars:

  Integer, number of hex characters from the hash digest when \`format =
  "token"\` (default 16).

- rename_fn:

  Optional function to rename or format generated tokens/names.

- seed:

  Optional integer, random seed for reproducibility.

- exclude:

  Character vector of surnames or names (case-insensitive) to omit.

- allow_collisions:

  Logical, default `FALSE`. If `TRUE`, allows short tokens with
  `n_chars < 6`.

## Value

A character vector of length \`n\` (or a single character string if \`n
= 1\`).

## Examples

``` r
generate_pi_names(1)
#> [1] "Rose"
generate_pi_names(3, format = "full_name")
#> [1] "Bella Medina"  "Kathryn Burns" "Kathryn Henry"
generate_pi_names(3, format = "token")
#> [1] "PI_4787ada7ae31f149" "PI_7885f84948c8e82e" "PI_6e4483fe51469ddd"
generate_pi_names(3, format = "token", n_chars = 8)
#> [1] "PI_b681f63a" "PI_33eac1fa" "PI_a0e4660d"
```
