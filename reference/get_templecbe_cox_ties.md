# Cox Tie-Handling Configuration and Quiet Scopes

\`glmnet\` 5.0 and 5.1 introduced transitional warnings and changed the
default Cox tie-handling method from \`"breslow"\` to \`"efron"\`. To
ensure reproducible results across \`glmnet\` package updates, TempleCBE
pins tie-handling via the package option \`templecbe.cox_ties\`
(defaulting to \`"breslow"\`).

## Usage

``` r
get_templecbe_cox_ties()

with_quiet_cox_ties(expr)

apply_workflow_cox_ties(object)
```

## Arguments

- expr:

  An expression to evaluate within a quiet tie-handling scope.

- object:

  A \`workflow\` or \`model_spec\` to inspect and configure with pinned
  Cox tie handling.

## Value

\* \`get_templecbe_cox_ties()\` returns the active tie-handling string
(\`"breslow"\` or \`"efron"\`). \* \`with_quiet_cox_ties()\` returns the
result of evaluating \`expr\`. \* \`apply_workflow_cox_ties()\` returns
the modified \`workflow\` or \`model_spec\`.

## Details

Manages tie-handling methods for Cox models (via \`glmnet\` and
\`parsnip\`) consistently across TempleCBE tuning and evaluation
wrappers.

## Examples

``` r
get_templecbe_cox_ties()
#> [1] "breslow"
```
