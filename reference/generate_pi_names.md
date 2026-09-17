# Generate Random PI Names or Tokens

Generates random synthetic principal investigator (PI) names or
cryptographic pseudonym tokens (\`PI\_...\`).

## Usage

``` r
generate_pi_names(
  n = 1,
  format = c("synthetic", "token"),
  prefix = "PI_",
  n_chars = 16,
  rename_fn = NULL,
  seed = NULL
)
```

## Arguments

- n:

  Integer, number of names to generate (default 1).

- format:

  Character, either \`"synthetic"\` (realistic surnames) or \`"token"\`
  (cryptographic hash tokens).

- prefix:

  Character, prefix when format is \`"token"\` (default \`"PI\_"\`).

- n_chars:

  Integer, number of hex characters from the hash digest when \`format =
  "token"\` (default 16).

- rename_fn:

  Optional function to rename or format generated tokens/names.

- seed:

  Optional integer, random seed for reproducibility.

## Value

A character vector of length \`n\` (or a single character string if \`n
= 1\`).

## Examples

``` r
generate_pi_names(1)
#> [1] "Noubiland"
generate_pi_names(3, format = "token")
#> [1] "PI_dc1f69c2cfd0990e" "PI_fb6d277914c85584" "PI_4b80a914bc323e46"
generate_pi_names(3, format = "token", n_chars = 8)
#> [1] "PI_f59d4d69" "PI_49fe124f" "PI_0246cb6e"
```
