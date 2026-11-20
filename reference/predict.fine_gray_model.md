# Predict the Cumulative Incidence Function from a Fine-Gray Model

Predicted cumulative incidence of the cause of interest for new subjects
under a \[fine_gray()\] fit, at the event times of that cause (or at
\`times\`). The CIF is a step function: a requested time takes the value
at the latest event time at or before it, and 0 before the first event
time.

## Usage

``` r
# S3 method for class 'fine_gray_model'
predict(object, new_data = NULL, times = NULL, ...)
```

## Arguments

- object:

  A \[fine_gray()\] fit.

- new_data:

  A data frame of new predictors. \`NULL\` (the default) predicts on the
  training data.

- times:

  Times to predict the CIF at. \`NULL\` (the default) is every event
  time of the cause of interest in the training data.

- ...:

  Not used.

## Value

A tibble with one row per row of \`new_data\`: \`.pred\`, a list-column
of tibbles with \`.eval_time\` and \`.pred_cif\` (the predicted
cumulative incidence of the cause of interest by that time). Rows of
\`new_data\` with a missing predictor are kept, with an all-\`NA\` CIF,
so the output always aligns positionally with \`new_data\`.

## See also

\[fine_gray()\], \[predict_fine_gray()\], \[tbl_cumulative_incidence()\]
