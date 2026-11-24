# Make TempleCBE Models Smaller With butcher

Methods for \[butcher::axe_call()\], \[butcher::axe_env()\],
\[butcher::axe_data()\] and \[butcher::axe_rsample_data()\] for
\[coxnet()\] models, \[cv_coxnet()\] results and \[joint_model()\] fits,
so that \[butcher::butcher()\] shrinks them as it does other models.
They are used when butcher is installed, including on a parsnip fit or a
fitted workflow with the \`coxnet\` engine.

## Usage

``` r
# S3 method for class 'coxnet_model'
axe_call(x, verbose = FALSE, ...)

# S3 method for class 'coxnet_model'
axe_env(x, verbose = FALSE, ...)

# S3 method for class 'coxnet_model'
axe_data(x, verbose = FALSE, ...)

# S3 method for class 'cv_coxnet'
axe_call(x, verbose = FALSE, ...)

# S3 method for class 'cv_coxnet'
axe_env(x, verbose = FALSE, ...)

# S3 method for class 'cv_coxnet'
axe_data(x, verbose = FALSE, ...)

# S3 method for class 'cv_coxnet'
axe_rsample_data(x, verbose = FALSE, ...)

# S3 method for class 'joint_model'
axe_call(x, verbose = FALSE, ...)

# S3 method for class 'joint_model'
axe_env(x, verbose = FALSE, ...)

# S3 method for class 'joint_model'
axe_data(x, verbose = FALSE, ...)
```

## Arguments

- x:

  A \`coxnet_model\`, \`cv_coxnet\` or \`joint_model\`.

- verbose:

  Passed to butcher: print the memory released.

- ...:

  Passed on to the methods of the inner objects.

## Value

\`x\` with the parts above removed, with a \`butchered\_\*\` class added
as butcher does.

## Details

What each removes, and what still works afterwards:

\* \`axe_call()\`: the call stored by glmnet (\`print()\` of the inner
glmnet fit shows a placeholder). Predictions are unchanged. \*
\`axe_env()\`: the environments captured by the recipe steps of a recipe
preprocessor (as butcher does for recipes) and, for a \`joint_model\`,
the environment of its stored outcome formula, which is the frame the
formula was written in (when that is a function's frame, saving the
model saved every object in it). Predictions are unchanged. \*
\`axe_data()\`: for a \`coxnet_model\`, the training design matrix and
outcome. Survival (\`type = "survival"\`) and restricted mean (\`type =
"time"\`) predictions need the Breslow baseline hazard, which is
computed from them; it is stored first at the model's default penalty,
so predictions at that penalty are unchanged, and predictions at another
penalty stop with an error. Linear predictors and \`tidy()\` work at any
penalty. A model fit without a default penalty can then only predict
linear predictors. For a \`cv_coxnet\` this applies to its refit model,
whose default penalty is \`lambda.min\`; for a \`joint_model\`, to its
Cox component, and the stored training data (\`components\$data\`,
\`components\$predictors\`, \`components\$raw_predictors\`) is removed
too, so \`predict()\` and the explainers need \`new_data\` / \`data\`
passed. \* \`axe_rsample_data()\`: the \`resamples\` a \`cv_coxnet\`
keeps, which hold the data once per resample when saved. Its metrics,
\`predict()\`, \`tidy()\`, \`collect_metrics()\`, \`autoplot()\` and
\`print()\` do not use them.

## Examples

``` r
# \donttest{
if (requireNamespace("butcher", quietly = TRUE) &&
    requireNamespace("glmnet", quietly = TRUE)) {
  lung <- na.omit(survival::lung[, c("time", "status", "age", "sex", "ph.ecog")])
  fit <- coxnet(survival::Surv(time, status) ~ ., data = lung, penalty = 0.01)
  small <- butcher::butcher(fit)
  predict(small, lung[1:3, ], type = "survival", eval_time = c(100, 300))
}
#> # A tibble: 3 × 1
#>   .pred           
#>   <list>          
#> 1 <tibble [2 × 2]>
#> 2 <tibble [2 × 2]>
#> 3 <tibble [2 × 2]>
# }
```
