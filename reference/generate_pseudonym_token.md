# Generate Pseudonym Tokens (Cryptographic Hash)

Generates secure cryptographic hash pseudonym tokens. When \`name\` is
supplied, computes deterministic keyed HMAC-SHA256 tokens. When \`name\`
is \`NULL\`, generates random cryptographic hash tokens (or reproducible
hash tokens when \`seed\` is provided). Supports custom prefix,
configurable character length (e.g. for short tokens), and custom
renaming/formatting functions.

## Usage

``` r
generate_pseudonym_token(
  name = NULL,
  n = 1,
  prefix = "PI_",
  key = Sys.getenv("TEMPLECBE_SECRET_KEY", unset = ""),
  n_chars = 16,
  rename_fn = NULL,
  seed = NULL
)
```

## Arguments

- name:

  Optional character vector of names to tokenize. If provided, computes
  a deterministic HMAC-SHA256 token for each non-NA name.

- n:

  Integer, number of tokens to generate when \`name\` is \`NULL\`
  (default 1).

- prefix:

  Character string, prefix prepended to each token (default \`"PI\_"\`).

- key:

  Character string, secret key / pepper for HMAC tokenization. Defaults
  to the environment variable \`Sys.getenv("TEMPLECBE_SECRET_KEY", unset
  = "")\`.

- n_chars:

  Integer, number of hex characters from the hash digest to include
  (default 8). Can be shortened (e.g. 4 or 6) to produce compact/short
  tokens.

- rename_fn:

  Optional function to transform or rename tokens (e.g., \`tolower\`,
  \`toupper\`, or a custom function such as \`function(tok)
  paste0("INV-", substr(tok, 4, 8))\`).

- seed:

  Optional integer, random seed for reproducible random token generation
  when \`name\` is \`NULL\`.

## Value

Character vector of pseudonym tokens.

## Examples

``` r
# Random cryptographic hash tokens
generate_pseudonym_token(n = 3)
#> [1] "PI_c72f879fc3d56834" "PI_732741454e236a76" "PI_a7eb18346a21a7c7"

# Short hash tokens (4 hex chars)
generate_pseudonym_token(n = 3, n_chars = 4)
#> [1] "PI_2b34" "PI_4f41" "PI_c7cf"

# Custom renaming function
generate_pseudonym_token(n = 2, rename_fn = tolower)
#> [1] "pi_d3afc93d00fee795" "pi_ad2361d109462a53"

# Stateless deterministic token with key
generate_pseudonym_token("Franklin", key = "study_salt")
#> [1] "PI_74a3a4218fa7aa9c"

# Vectorized tokenization
generate_pseudonym_token(c("Smith", "Jones"), key = "study_salt")
#> [1] "PI_e89748fe2a7c0fd9" "PI_c9caaa891b723bb5"
```
