# Joint Model of Recurrent Readmission and Death (Draft)

Fits readmission and death together instead of treating death as
censoring of the readmission process. Two sub-models are fit on the same
counting-process data:

1.  **Readmission**: an Andersen-Gill recurrent-event Cox model
    (\[recurrent_cox()\], \`model = "ag"\`, or \`"frailty"\` with
    \`frailty = TRUE\`) for the rate of readmissions.

2.  **Death**: a Cox model for the time to death, with the subject's
    readmission history as a time-varying covariate
    (\`n_prior_readmissions\`, the number of readmissions before the
    interval), so the death hazard depends on the recurrent process.
    With \`frailty = TRUE\` it adds a gamma frailty for the subject.

## Usage

``` r
joint_readmission_death(
  data,
  readmission_predictors,
  death_predictors,
  id = "id",
  start = "tstart",
  stop = "tstop",
  status = "status",
  death = "death",
  frailty = FALSE,
  conf_level = 0.95
)
```

## Arguments

- data:

  A data frame of counting-process intervals, as for
  \[recurrent_cox()\].

- readmission_predictors:

  Covariates of the readmission model: a character vector of column
  names, or a one-sided formula such as \`~ age + sex\`. In this draft
  they must be numeric columns (see \`predict()\`).

- death_predictors:

  Covariates of the death model: a character vector of column names, or
  a one-sided formula. In this draft they must be numeric columns (see
  \`predict()\`).

- id, start, stop, status, death:

  Column names (strings) of the subject identifier, the interval start
  and stop, the readmission indicator (0/1), and the death indicator
  (0/1; see Details).

- frailty:

  Logical (default \`FALSE\`). With \`TRUE\`, both sub-models get a
  gamma frailty for the subject.

- conf_level:

  Confidence level of the hazard ratios (default \`0.95\`).

## Value

An object of class \`joint_readmission_death\`: a list with
\`readmission\` (the \[recurrent_cox()\] fit), \`death_fit\` (the
\[survival::coxph()\] death fit, fitted with \`model = TRUE\`, so the
stored model frame makes it usable with standard generics such as
\[survival::basehaz()\] and \[survival::survfit()\]), \`terms\` (a
combined tibble with \`process\`, \`term\`, \`estimate\`, \`std_error\`,
\`hr\`, \`conf_low\`, \`conf_high\`, \`statistic\` and \`p_value\`),
\`death_terms\` (the death sub-model's terms), \`n_subjects\`,
\`n_intervals\`, \`n_readmissions\`, \`n_deaths\`, \`frailty\`,
\`frailty_variance\` (named \`readmission\` and \`death\`, \`NA\` unless
\`frailty = TRUE\`), and \`conf_level\`. \`tidy()\` returns \`terms\`.

## Details

\*\*Draft status – this departs from the package's standing
assumption.\*\* \`NEWS.md\` 0.8.0 ("Not done, and why") explicitly
declined joint models of readmission and death, with the reason that
death ends follow-up (censoring), the usual assumption of the
Andersen-Gill and PWP models, which is informative when death is common;
that is also stated in the help of \[recurrent_intervals()\]. This
function is a draft extension built at the maintainer's request, and it
does \*\*not\*\* replace that assumption elsewhere in the package:
\[recurrent_cox()\], \[cv_coxnet()\] and the other recurrent tools still
treat death as censoring.

\*\*What the draft does and does not do.\*\* The two processes are
linked by the \`n_prior_readmissions\` covariate in the death model, not
by a single shared frailty estimated jointly: with \`frailty = TRUE\`
each sub-model gets its own gamma frailty (one for readmissions, one for
death), and the two frailty variances are reported separately. A fully
joint likelihood with one shared frailty (as in
\`frailtypack::frailtyPenal()\`) is follow-up work and is not
implemented here; it would also add a dependency the package does not
currently have. Death-survival predictions are conditional on a fixed
number of prior readmissions (see \`predict()\`): they do not integrate
over the readmission process.

\*\*Death in the data.\*\* \`death\` marks the subject's death at the
stop time of an interval: \`1\` on the subject's last interval when they
died there, \`0\` elsewhere. A subject who is readmitted and later dies
gets a final interval ending at death (\`status = 0\`, \`death = 1\`); a
row must not carry both a readmission and a death, and no rows may
follow a death. Subjects who do not die are censored for death at the
end of follow-up.

\*\*Frailty with competing risks.\*\* With \`frailty = TRUE\`, this is
also the draft's frailty-with-competing-risks support: the readmission
and death processes – which compete, in that death ends the readmission
process – each carry a subject frailty. As above, the frailties are
fitted separately per process, not shared; a shared frailty across the
competing processes is follow-up work.

## See also

\[recurrent_cox()\], \[recurrent_intervals()\], \[joint_model()\],
\[cv_coxnet()\]

## Examples

``` r
if (requireNamespace("survival", quietly = TRUE)) {
  set.seed(1)
  n <- 150
  subj <- data.frame(id = seq_len(n), age = rnorm(n), end = 120)
  ev <- do.call(rbind, lapply(seq_len(n), function(i) {
    k <- rpois(1, exp(0.3 * subj$age[i]) * 1.2)
    if (k == 0) return(NULL)
    data.frame(id = i, time = sort(runif(k, 1, 120)))
  }))
  long <- recurrent_intervals(ev, subj)
  long$age <- subj$age[long$id]
  # Deaths: about a quarter of subjects, at the end of follow-up
  long$death <- 0L
  died <- rbinom(n, 1, plogis(-1 + 0.5 * subj$age))
  for (i in which(died == 1)) {
    idx <- which(long$id == i)
    long$death[idx[length(idx)]] <- 1L
  }
  fit <- joint_readmission_death(long, ~ age, ~ age)
  fit
  generics::tidy(fit)
}
#> # A tibble: 3 × 9
#>   process    term  estimate std_error    hr conf_low conf_high statistic p_value
#>   <chr>      <chr>    <dbl>     <dbl> <dbl>    <dbl>     <dbl>     <dbl>   <dbl>
#> 1 readmissi… age      0.227    0.0798  1.25    1.07       1.47      2.84 0.00448
#> 2 death      age      0.433    0.178   1.54    1.09       2.19      2.44 0.0148 
#> 3 death      n_pr…    0.118    0.117   1.12    0.894      1.42      1.00 0.316  
```
