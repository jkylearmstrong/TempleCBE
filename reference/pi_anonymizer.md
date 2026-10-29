# Investigator Name Pseudonymization Helpers

Helpers that generate synthetic surnames and pseudonym tokens, and
replace investigator (PI) names with them in reports and shared output.
They accept a single name or a character vector, such as a data frame
column.

## Scope and limits

These functions pseudonymize **investigator names only**. They do not
find, remove, or alter dates, record or subject numbers, locations, free
text, or any other identifier, and they are not a general
de-identification tool. Replacing investigator names does not by itself
make a dataset safe to share.

Pseudonyms are not guaranteed to be unique: two different names can map
to the same surname (`method = "hmac_surname"`), and short tokens (a
small `n_chars`) collide more often.

## Secret key

The deterministic methods (`"hmac_token"` and `"hmac_surname"`) are only
as private as their key. Anyone who knows the key, or who can guess it,
can recompute every pseudonym from a list of candidate names. The
functions therefore stop unless a key is supplied, either through the
`key` argument or, preferably, the `TEMPLECBE_SECRET_KEY` environment
variable. Keep the key out of scripts and repositories: put it in your
user `.Renviron` (open it with
[`usethis::edit_r_environ()`](https://usethis.r-lib.org/reference/edit.html))
and restart R:

    TEMPLECBE_SECRET_KEY=paste-a-long-random-string-here

A random key can be made with
`paste(as.character(openssl::rand_bytes(32)), collapse = "")`. Use the
same key every time to get the same pseudonyms; a lost key cannot be
recovered, and a changed key changes every pseudonym.

For a quick, non-private run you can pass `allow_default_key = TRUE`,
which falls back to a public built-in key and warns that the output is
NOT secret. Methods `"token"` (random tokens kept in a private mapping
file) and `"synthetic"` need no key.

## See also

[`anonymize_pi`](https://jkylearmstrong.github.io/TempleCBE/reference/anonymize_pi.md),
[`generate_pseudonym_token`](https://jkylearmstrong.github.io/TempleCBE/reference/generate_pseudonym_token.md),
[`generate_pi_names`](https://jkylearmstrong.github.io/TempleCBE/reference/generate_pi_names.md),
[`generate_last_names`](https://jkylearmstrong.github.io/TempleCBE/reference/generate_last_names.md)
