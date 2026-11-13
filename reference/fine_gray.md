# Fine-Gray Subdistribution Hazard Model, Fitted for Prediction

Fits the Fine-Gray (1999) proportional subdistribution hazards model of
one cause with the other causes treated as competing events, using
\`cmprsk::crr()\`, and keeps the fit so \[predict.fine_gray_model()\]
can return the predicted cumulative incidence function (CIF). It is the
fitted-model counterpart of \[tbl_fine_gray_multi()\], which returns a
coefficient table only: the design matrix, factor levels and event codes
are stored here, so new data are encoded the same way as the training
data.

## Usage

``` r
fine_gray(data, time_col, event_col, formula, failcode = 1, cencode = 0, ...)

# S3 method for class 'fine_gray_model'
print(x, ...)
```

## Arguments

- data:

  A data frame, one row per subject.

- time_col:

  Follow-up time.

- event_col:

  Event type, as in \[tbl_cumulative_incidence()\]: a censoring code,
  then one code per cause.

- formula:

  One-sided formula of the predictors, for example \`~ age + arm +
  sex\`.

- failcode:

  Code of the cause of interest (default 1).

- cencode:

  Code for censoring (default 0).

- ...:

  Passed to \`cmprsk::crr()\` (for example \`maxiter\`). Prediction
  works whether \`variance\` is \`TRUE\` or \`FALSE\`: \`crr()\` stores
  the baseline hazard either way.

- x:

  The object to print (the result of the function).

## Value

An object of class \`fine_gray_model\` with the \`crr\` fit (\`\$fit\`),
the design matrix (\`\$x\`), the model terms and factor levels
(\`\$terms\`, \`\$xlevels\`), the event codes (\`\$failcode\`,
\`\$cencode\`), and \`n\` / \`n_event\` (rows used / events of the cause
of interest) / \`n_dropped\` (rows dropped for missing values; a warning
reports the count when it is not zero).

## Details

The CIF for covariates \`x\` is \`1 - exp(-exp(x'b) \* H0(t))\`, where
\`H0\` is the baseline cumulative subdistribution hazard, evaluated by
\`cmprsk::predict.crr()\` at the event times of the cause of interest. A
competing event keeps a subject in the risk set, which is what \`crr()\`
does; the CIF is therefore the probability of the cause of interest by
time \`t\` in the presence of the competing causes, not a Kaplan-Meier
risk.

## See also

\[predict.fine_gray_model()\], \[predict_fine_gray()\],
\[tbl_fine_gray_multi()\], \[tbl_fine_gray_single()\],
\[tbl_cumulative_incidence()\]

## Examples

``` r
if (requireNamespace("cmprsk", quietly = TRUE)) {
  set.seed(1)
  d <- data.frame(time = rexp(150, 0.1), type = sample(0:2, 150, TRUE, prob = c(0.3, 0.4, 0.3)),
                  age = rnorm(150, 60, 8), arm = rep(c("A", "B"), 75))
  fit <- fine_gray(d, "time", "type", ~ age + arm)
  fit
  predict(fit, d[1:3, ])
}
#> # A tibble: 3 × 1
#>   .pred            
#>   <list>           
#> 1 <tibble [69 × 2]>
#> 2 <tibble [69 × 2]>
#> 3 <tibble [69 × 2]>
```
