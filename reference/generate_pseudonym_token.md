# Generate Pseudonym Tokens (Cryptographic Hash)

Generates cryptographic hash pseudonym tokens. When `name` is supplied,
computes deterministic keyed HMAC-SHA256 tokens, which require a secret
key (see
[`pi_anonymizer`](https://jkylearmstrong.github.io/TempleCBE/reference/pi_anonymizer.md)).
When `name` is `NULL`, generates random tokens (or reproducible,
*non-secret* tokens when `seed` is provided). Supports a custom prefix,
a configurable length (e.g. for short tokens), and custom
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
  seed = NULL,
  allow_default_key = FALSE,
  allow_collisions = FALSE
)
```

## Arguments

- name:

  Optional character vector of names to tokenize. If provided, computes
  a deterministic HMAC-SHA256 token for each non-NA name; this needs a
  secret key.

- n:

  Integer, number of tokens to generate when `name` is `NULL` (default
  1).

- prefix:

  Character string, prefix prepended to each token (default `"PI_"`).

- key:

  Character string, the secret key for HMAC tokenization, used only when
  `name` is supplied. Defaults to the environment variable
  `TEMPLECBE_SECRET_KEY`. An empty key counts as no key: the call stops
  unless `allow_default_key = TRUE`. Prefer the environment variable
  over writing the key into a script.

- n_chars:

  Integer, number of hex characters from the hash digest to include
  (default 16). Can be shortened (e.g. 4 or 8) to produce compact
  tokens, at the cost of more collisions between different names.

- rename_fn:

  Optional function to transform or rename tokens (e.g., `tolower`,
  `toupper`, or a custom function such as
  `function(tok) paste0("INV-", substr(tok, 4, 8))`).

- seed:

  Optional integer, random seed for reproducible random token generation
  when `name` is `NULL`. Seeded tokens are derived from the seed alone,
  so anyone who knows it can regenerate them; do not use them where
  secrecy matters.

- allow_default_key:

  Logical, default `FALSE`. If `TRUE` and no key is available, use the
  package's public built-in key instead of stopping, and warn that the
  tokens are NOT secret. This only reproduces output made without a key
  by earlier versions; do not use it for data you intend to protect.

- allow_collisions:

  Logical, default `FALSE`. If `TRUE`, suppresses errors when
  `n_chars < 6` or when distinct input names map to identical pseudonym
  tokens.

## Value

Character vector of pseudonym tokens.

## Details

Tokens replace investigator names only; nothing else in a dataset is
de-identified (see the scope notes in
[`pi_anonymizer`](https://jkylearmstrong.github.io/TempleCBE/reference/pi_anonymizer.md)).

## See also

[`pi_anonymizer`](https://jkylearmstrong.github.io/TempleCBE/reference/pi_anonymizer.md)
for scope and key handling.

## Examples

``` r
# Random tokens need no key
generate_pseudonym_token(n = 3)
#> [1] "PI_a72661486b982f60" "PI_c0da1d1845a7bb82" "PI_df1b3796cdde10a2"

# Short tokens (8 hex chars)
generate_pseudonym_token(n = 3, n_chars = 8)
#> [1] "PI_e4f7ea7b" "PI_2d95d350" "PI_ed3f4755"

# Custom renaming function
generate_pseudonym_token(n = 2, rename_fn = tolower)
#> [1] "pi_544054fb778362a5" "pi_6fc2bfc4d41b5220"

# Deterministic tokens need a secret key. In real use keep it in the
# TEMPLECBE_SECRET_KEY environment variable (see ?pi_anonymizer), never in a
# script you commit. Here a throwaway value is set for this example only.
if (requireNamespace("withr", quietly = TRUE)) {
  withr::with_envvar(c(TEMPLECBE_SECRET_KEY = "example-only-not-a-real-key"), {
    print(generate_pseudonym_token("Franklin"))
    print(generate_pseudonym_token(c("Smith", "Jones")))
  })
}
#> [1] "PI_2713ba3e1fcb5c01"
#> [1] "PI_9db709bc851f5123" "PI_81bac4a3024fcdb2"
```
