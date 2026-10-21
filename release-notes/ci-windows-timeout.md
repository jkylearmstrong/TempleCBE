# Review record: R-CMD-check Windows timeout 45 -> 60 minutes

Status: **signed off by maintainer** (2026-10-21).

## Change
`.github/workflows/R-CMD-check.yaml`, job `cross-platform-check`: `timeout-minutes: 45` -> `60`. No other line changes.

## Why
The Windows job of this matrix takes 44m12s on unmodified `master` code (PR #14, tests only), against the 45-minute limit. Adding a few slow tests (PR #16) made it exceed the limit, and GitHub cancelled the job ("The operation was canceled"); no test failed. The limit therefore blocks any change that adds tests on Windows.

## Risk
A hung job now costs up to 15 more minutes of runner time. The job runs only for pull requests, version tags and manual full-matrix runs. Permissions, secrets, triggers and the steps are unchanged.

## Maintainer sign-off
Maintainer: J. Kyle Armstrong, PhD  Date: 2026-10-21  Decision: accept
