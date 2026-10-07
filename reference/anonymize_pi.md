# Pseudonymize Investigator Names (Vectorized)

Replaces investigator names with pseudonyms. Supports both single scalar
names and character vectors (e.g. data frame columns like
`df$investigator`). Offers stateless keyed-HMAC hashing as well as a
stateful mapping table.

## Usage

``` r
anonymize_pi(
  name,
  method = c("hmac_token", "hmac_surname", "token", "synthetic"),
  key = Sys.getenv("TEMPLECBE_SECRET_KEY", unset = ""),
  secrets_path = NULL,
  exclude = character(),
  prefix = "PI_",
  n_chars = 16,
  rename_fn = NULL,
  seed = NULL,
  allow_default_key = FALSE,
  allow_collisions = FALSE,
  normalize = FALSE
)
```

## Arguments

- name:

  Character vector of investigator names or surnames to mask.

- method:

  Character string specifying the approach: `"hmac_token"`,
  `"hmac_surname"`, `"token"`, or `"synthetic"`.

- key:

  Character string, the secret key for the HMAC methods (and for keyed
  tokens with `method = "token"`). Defaults to the environment variable
  `TEMPLECBE_SECRET_KEY`. An empty key counts as no key. Prefer the
  environment variable over writing the key into a script.

- secrets_path:

  Path to the confidential mapping file when `method = "token"`.
  Defaults to
  `tools::R_user_dir("TempleCBE", which = "data")/pi_mapping.json`. It
  must be outside any Git repository.

- exclude:

  Character vector of names that must never be generated (passed to
  [`generate_last_names`](https://jkylearmstrong.github.io/TempleCBE/reference/generate_last_names.md)
  when method is `"hmac_surname"` or `"synthetic"`).

- prefix:

  Character string, prefix for tokens (default `"PI_"`).

- n_chars:

  Integer, number of hex characters for tokens (default 16). Can be
  shortened for compact tokens; very short tokens (e.g. 4) collide, so
  different investigators may share one.

- rename_fn:

  Optional function to rename or format generated tokens/pseudonyms.

- seed:

  Optional integer, random seed when method is `"synthetic"`.

- allow_default_key:

  Logical, default `FALSE`. If `TRUE` and no key is available for
  `"hmac_token"` or `"hmac_surname"`, use the package's public built-in
  key instead of stopping, and warn that the output is NOT secret. This
  only reproduces output made without a key by earlier versions; do not
  use it for data you intend to protect.

- allow_collisions:

  Logical, default `FALSE`. If `TRUE`, suppresses collision
  errors/warnings when distinct names produce identical tokens or
  surnames.

- normalize:

  Logical, default `FALSE`. If `TRUE`, trims leading/trailing whitespace
  and converts names to lowercase before hashing.

## Value

Character vector of pseudonyms matching the length of `name`; `NA`
inputs stay `NA`.

## Details

**Scope.** Only investigator names are replaced. Dates, record or
subject numbers, free text, and other identifiers are left untouched, so
this is not a general de-identification tool; see
[`pi_anonymizer`](https://jkylearmstrong.github.io/TempleCBE/reference/pi_anonymizer.md)
for the limits.

**Key required for the deterministic methods.** `"hmac_token"` and
`"hmac_surname"` stop with an error unless a secret key is available,
from the `key` argument or the `TEMPLECBE_SECRET_KEY` environment
variable. Without a secret key anyone can recompute the pseudonyms from
a list of names. Set the variable in your user `.Renviron` rather than
writing the key into a script that may be committed. Passing
`allow_default_key = TRUE` restores the earlier fallback to a public
built-in key, with a warning that the output is NOT secret. The check
happens before the data is looked at, so an all-`NA` input does not skip
it.

Methods:

- `"hmac_token"` (default): Stateless deterministic token (e.g.,
  `PI_5c1ebfd9a0b3c4d2`) computed via
  `openssl::sha256(name, key = key)`. Needs a key. No mapping file is
  written.

- `"hmac_surname"`: Stateless deterministic procedural surname generated
  via a keyed HMAC seed and
  [`generate_last_names`](https://jkylearmstrong.github.io/TempleCBE/reference/generate_last_names.md).
  Needs a key. Consistent across runs without disk state; different
  names can share a surname.

- `"token"`: Stateful tokens persisted to a user-level JSON mapping file
  (`tools::R_user_dir("TempleCBE", "data")/pi_mapping.json`), so a name
  keeps its token between calls. Works without a key: new names then get
  random tokens, which exist only in the mapping file. If a key is set,
  tokens are the keyed HMAC of the name instead. The mapping file lists
  real names next to their tokens and is confidential. The function
  refuses to create or write it inside a Git repository tree, to prevent
  accidental commits.

- `"synthetic"`: Random procedural surnames via
  [`generate_last_names`](https://jkylearmstrong.github.io/TempleCBE/reference/generate_last_names.md).
  Needs no key; every element gets a new random surname, so repeated
  names are not mapped consistently.

## See also

[`pi_anonymizer`](https://jkylearmstrong.github.io/TempleCBE/reference/pi_anonymizer.md)
for scope, limits and key setup.

## Examples

``` r
# The key comes from the TEMPLECBE_SECRET_KEY environment variable. In real use
# keep it in your user ~/.Renviron (see ?pi_anonymizer), never in a script you
# commit. Here a throwaway value is set for this example only.
if (requireNamespace("withr", quietly = TRUE)) {
  withr::with_envvar(c(TEMPLECBE_SECRET_KEY = "example-only-not-a-real-key"), {
    # Vectorized deterministic tokens (default method = "hmac_token")
    print(anonymize_pi(c("Franklin", "Taylor", "Patel")))

    # Short tokens (8 characters)
    print(anonymize_pi(c("Franklin", "Taylor"), n_chars = 8))

    # Custom renaming function
    print(anonymize_pi(c("Franklin", "Taylor"), rename_fn = tolower))

    # Vectorized realistic pseudonyms
    print(anonymize_pi(c("Franklin", "Taylor"), method = "hmac_surname"))
  })
}
#> [1] "PI_2713ba3e1fcb5c01" "PI_383a552e8b600d2f" "PI_b0ed77cfcb0fdb62"
#> [1] "PI_2713ba3e" "PI_383a552e"
#> [1] "pi_2713ba3e1fcb5c01" "pi_383a552e8b600d2f"
#> [1] "Plefraison"  "Jiefreaford"

# Random tokens kept in a mapping file need no key. Here the file goes in a
# temporary directory; in real use leave secrets_path at its default.
mapping <- file.path(tempdir(), "pi_mapping_example.json")
anonymize_pi(c("Franklin", "Taylor", "Franklin"), method = "token", secrets_path = mapping)
#> [1] "PI_51af47eb115e77c1" "PI_9f5475e8c0eef2a9" "PI_51af47eb115e77c1"
unlink(mapping)
```
