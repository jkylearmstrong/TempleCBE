# Guard a Public Remote Against Accidental Direct Push

Records the URL(s) of the public remote in git config
(`cleanpublish.publicurl`) and makes sure the `pre-push` hook is
installed. The hook refuses a `git push` whose push URL is one of them,
after git has applied its own rewriting (`url.<base>.insteadOf`,
`pushInsteadOf`) and the URLs have been normalised: case, a trailing
`.git` or slash, a scheme, a user name, `host:path` and backslash
notation do not matter, and a local directory counts by its physical
path as well. Nothing else is resolved (a symbolic link on a remote
host, a changed DNS name), and a URL of another spelling is not the same
URL.

## Usage

``` r
guard_public_remote(public_url, repo_root = NULL, allow_unmatched = FALSE)
```

## Arguments

- public_url:

  The name of a configured remote (its fetch and push URLs, as
  `git remote get-url --all` and `--push --all` report them, are
  stored), or a URL. A URL must match the push URL of at least one
  configured remote after normalising, so that a fence that guards
  nothing is an error, unless `allow_unmatched = TRUE`. It must not
  contain whitespace, which the hook cannot compare word by word.

- repo_root:

  Path to the git repository root. Defaults to the git toplevel of the
  working directory (an error when that is not in a repository).

- allow_unmatched:

  Logical (default `FALSE`). Set `TRUE` to fence a URL that no
  configured remote has yet, for a remote that will be added later.

## Value

The normalized fenced URL(s), invisibly.

## Details

The one push that goes through is the one
[`clean_publish()`](https://jkylearmstrong.github.io/TempleCBE/reference/clean_publish.md)
makes: it sets the environment variable `TEMPLECBE_PUBLISHING=1`, and
with that variable the hook lets through the clean commits
[`clean_publish()`](https://jkylearmstrong.github.io/TempleCBE/reference/clean_publish.md)
recorded, and nothing else. The variable bypasses the fence only: any
other ref, the private branch for one, is still refused.

After writing, the function checks the result. It runs the hook by hand,
as git does, for every configured remote and for every fenced URL, with
`TEMPLECBE_PUBLISHING` unset, and stops with an error when the hook is
missing, is not executable, or does not refuse a push to a remote the
fence matches: a fence that is not confirmed is not left to guard
nothing in silence.

**What the guard does not stop.** It is a hook of command-line `git`. It
does not stop `git push --no-verify`, a push made by a client that does
not run hooks (the gert package, which usethis uses, is one), a push
after `core.hooksPath` has been pointed elsewhere or the hook has been
deleted or made non-executable, and a push to the URL in a spelling that
is not one of the fenced ones, for example an explicit URL that names
the same repository through another host name. It checks pushes, not
reads. The safest procedure is to publish from a throwaway clone and to
keep no pushable public remote in the clone you work in.

## Examples

``` r
if (FALSE) { # \dontrun{
# the remote of the public repository, by name ...
guard_public_remote("public")
# ... or by URL, which must be the push URL of a configured remote
guard_public_remote("https://github.com/OWNER/NEW-PUBLIC-REPO.git")
# a URL for a remote that is added after this call
guard_public_remote("https://github.com/OWNER/NEW-PUBLIC-REPO.git", allow_unmatched = TRUE)
} # }
```
