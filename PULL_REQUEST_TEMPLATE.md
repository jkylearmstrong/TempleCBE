# NA

## What and why

## Review level (see REVIEW.md)

L0 docs / tests only

L1 small fix or helper

L2 statistics, public API, file writer, de-identification,
subprocess/git/delete

L3 privacy behaviour, release/CI/Docker, secrets, license, dependencies

## Checklist

A test fails without this change and passes with it

`NEWS.md` has an entry; `man/` and `NAMESPACE` are regenerated with the
pinned roxygen2

`R CMD check` (built tarball, `NOT_CRAN=true`) was run; skipped tests
were read, not ignored

Exports compared before and after; a removed export is noted in
`NEWS.md`

Statistical change: reference check or SAS benchmark recorded below (or
“not re-run”)

No real study names, person names, identifiers, keys or local paths

## Evidence
