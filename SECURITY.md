# Security policy

TempleCBE includes helpers that pseudonymize identifiers
([`anonymize_pi()`](https://jkylearmstrong.github.io/TempleCBE/reference/anonymize_pi.md)
and friends), read and write files, and run external tools (git, SAS,
Python, Quarto). A flaw in any of them can expose data or damage files,
so please report problems privately rather than in a public issue.

## Reporting a vulnerability

- Use GitHub’s **Report a vulnerability** button on the repository’s
  *Security* tab (private vulnerability reporting), or
- email the maintainer, J Kyle Armstrong, at
  <j.kyle.armstrong@temple.edu>.

Please include the TempleCBE version (`packageVersion("TempleCBE")`), a
minimal reproduction using synthetic data, and what you expected to
happen. Never send real study data, identifiers, keys, or mapping files.

You can expect an acknowledgement within a week. Fixes for confirmed
problems are released as a patch version and noted in `NEWS.md`.

## Scope

In scope: the R package, the scripts under `scripts/`, and the GitHub
workflows in this repository. A secret key for the pseudonymization
helpers is your responsibility: store it outside any repository (for
example in `~/.Renviron`) and never commit it or a mapping file.

## Supported versions

Only the latest release receives security fixes.
