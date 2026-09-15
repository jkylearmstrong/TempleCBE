# Anonymize Investigator Names (Vectorized)

Anonymizes investigator names into pseudonyms. Supports both single
scalar names and character vectors (e.g. data frame columns like
\`df\$investigator\`). Offers both stateless keyed-HMAC hashing and
stateful mapping table persistence.

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
  seed = NULL
)
```

## Arguments

- name:

  Character vector of investigator names or surnames to mask.

- method:

  Character string specifying anonymization approach: \`"hmac_token"\`,
  \`"hmac_surname"\`, \`"token"\`, or \`"synthetic"\`.

- key:

  Character string, secret key / pepper for HMAC methods. Defaults to
  \`Sys.getenv("TEMPLECBE_SECRET_KEY", unset = "")\`.

- secrets_path:

  Path to confidential mapping file when \`method = "token"\`. Defaults
  to \`tools::R_user_dir("TempleCBE", which = "data")/pi_mapping.json\`.

- exclude:

  Character vector of names that must never be generated (passed to
  \[generate_last_names()\] when method is \`"hmac_surname"\` or
  \`"synthetic"\`).

- prefix:

  Character string, prefix for tokens (default \`"PI\_"\`).

- n_chars:

  Integer, number of hex characters for tokens (default 8). Can be
  shortened for short names.

- rename_fn:

  Optional function to rename or format generated tokens/pseudonyms.

- seed:

  Optional integer, random seed when method is \`"synthetic"\`.

## Value

Character vector of anonymized tokens or pseudonyms, matching the length
of \`name\`.

## Details

Methods:

- \`"hmac_token"\` (default): Stateless deterministic token (e.g.,
  \`PI_5c1ebfd9\`) computed via \`openssl::sha256(name, key = key)\`. No
  mapping file needed on disk.

- \`"hmac_surname"\`: Stateless deterministic procedural surname
  generated via keyed HMAC seed and \[generate_last_names()\].
  Consistent across runs without disk state.

- \`"token"\`: Stateful cryptographic tokens persisted to a user-level
  JSON mapping file (\`tools::R_user_dir("TempleCBE",
  "data")/pi_mapping.json\`). Explicitly refuses to write into the Git
  repository tree to prevent accidental commits of PHI.

- \`"synthetic"\`: Random procedural surnames via
  \[generate_last_names()\].

## Examples

``` r
# Vectorized token mapping (default method = "hmac_token")
anonymize_pi(c("Franklin", "Taylor", "Patel"), key = "study_salt")
#> [1] "PI_74a3a4218fa7aa9c" "PI_55cad5aeb6812bac" "PI_c4c136722bee9305"

# Short tokens (4 characters)
anonymize_pi(c("Franklin", "Taylor"), n_chars = 4, key = "study_salt")
#> [1] "PI_74a3" "PI_55ca"

# Custom renaming function
anonymize_pi(c("Franklin", "Taylor"), rename_fn = tolower, key = "study_salt")
#> [1] "pi_74a3a4218fa7aa9c" "pi_55cad5aeb6812bac"

# Vectorized realistic pseudonyms
anonymize_pi(c("Franklin", "Taylor"), method = "hmac_surname", key = "study_salt")
#> [1] "Glasujoll" "Staroford"
```
