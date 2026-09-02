<#
.SYNOPSIS
Wrapper that runs Rscript.exe under the R version this project's renv.lock
actually targets, instead of whatever R the Windows Registry/PATH resolves
`Rscript`/`R` to.

.DESCRIPTION
This machine (and others this project runs on) can have multiple R versions
installed side by side. Neither Git Bash nor PowerShell reliably resolves a
bare `Rscript`/`R` command to the R version this project's renv library
actually targets: Git Bash has no `Rscript`/`R` on PATH at all, and
PowerShell's `Rscript`/`R` resolve to `C:\Program Files\R\bin\Rscript.bat`, a
shared version-selector launcher that reflects whichever R was last
registered as "current" system-wide -- not necessarily the version this
project's renv.lock pins (confirmed directly: that launcher currently
resolves to R 4.6.0 while renv.lock targets R 4.6.1). Typing the
version-specific path out by hand every time
(`C:\Program Files\R\R-4.6.1\bin\x64\Rscript.exe`) is easy to mistype or
fall back to a bare command out of habit -- which silently runs against the
wrong R version's (likely empty or stale) renv library instead of erroring.

Unlike the sibling `Wolfson` project (which has an explicit QUARTO_R in
.Renviron to read), TempleCBE's DESCRIPTION only requires R >= 4.1.0, not
one exact version -- so this script derives the target version directly
from renv.lock's own recorded "R"."Version" field, which is the actual
source of truth for what the restored renv library was built against, and
constructs the standard per-version install path from it. If that version
gets bumped in a future renv snapshot, this script picks it up automatically
with no separate value to keep in sync.

.EXAMPLE
scripts/Rscript.ps1 -e "renv::restore()"

.EXAMPLE
scripts/Rscript.ps1 -e "devtools::test()"
#>

# Deliberately no declared param() block: Rscript's own flags (like `-e`) collide
# with PowerShell's common parameter abbreviations (`-e` -> `-ErrorAction`), which
# a declared [Parameter(ValueFromRemainingArguments)] fails to bind before it ever
# reaches this script. Reading the raw, unparsed $args instead sidesteps that.

$repoRoot = Split-Path -Parent $PSScriptRoot
$renvLockPath = Join-Path $repoRoot "renv.lock"

if (-not (Test-Path $renvLockPath)) {
    Write-Error "Could not find renv.lock at $renvLockPath"
    exit 1
}

$renvLock = Get-Content $renvLockPath -Raw | ConvertFrom-Json
$rVersion = $renvLock.R.Version

if (-not $rVersion) {
    Write-Error "Could not read R.Version from $renvLockPath"
    exit 1
}

$rscriptExePath = "C:\Program Files\R\R-$rVersion\bin\x64\Rscript.exe"

if (-not (Test-Path $rscriptExePath)) {
    Write-Error "renv.lock targets R $rVersion but couldn't find it at $rscriptExePath`nInstall R $rVersion, or check renv.lock's R.Version if this project has moved to a different R version."
    exit 1
}

& $rscriptExePath @args
exit $LASTEXITCODE
