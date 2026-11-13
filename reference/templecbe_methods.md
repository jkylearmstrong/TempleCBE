# Print, Summary, Tidy and Recipe-Step Methods for TempleCBE Classes

S3 methods of the classes that TempleCBE functions return, for generics
of base R, generics, dplyr and recipes. They are called through the
generic (\`print(x)\`, \`summary(x)\`, \`tidy(x)\`, \`x\$name\`, ...),
not by their full name. The help page of the function that creates each
class describes the object; this page records the methods that have no
page of their own.

## Usage

``` r
# S3 method for class 'cbe_tuning_results'
summary(object, ...)

# S3 method for class 'cbe_tuning_results'
tidy(x, ...)

# S3 method for class 'render_parity'
print(x, ...)

# S3 method for class 'balance_smd'
print(x, ...)

# S3 method for class 'boot_lmer'
print(x, ...)

# S3 method for class 'cbe_deliverable_diff'
print(x, ...)

# S3 method for class 'cbe_compare_database'
print(x, ...)

# S3 method for class 'cbe_compare_df'
print(x, max_print = 10, ...)

# S3 method for class 'cbe_compare_df'
summary(object, ...)

# S3 method for class 'cbe_compare_df'
tidy(x, ...)

# S3 method for class 'cbe_database_relationships'
plot(x, ...)

# S3 method for class 'cbe_key_integrity'
print(x, ...)

# S3 method for class 'docx_review_extract'
print(x, ...)

# S3 method for class 'cbe_review_extract'
print(x, ...)

# S3 method for class 'cbe_column_reconciliation'
print(x, ...)

# S3 method for class 'templecbe_run_comparison'
print(x, ...)

# S3 method for class 'templecbe_tuning_comparison'
print(x, ...)

# S3 method for class 'cox_time_varying'
print(x, ...)

# S3 method for class 'coxnet_model'
print(x, ...)

# S3 method for class 'cv_coxnet'
print(x, ...)

# S3 method for class 'cbe_database'
intersect(x, y, node_by = NULL, edge_by = c("from", "to"), ...)

# S3 method for class 'cbe_database'
setdiff(x, y, node_by = NULL, edge_by = c("from", "to"), ...)

# S3 method for class 'cbe_database'
union(x, y, node_by = NULL, edge_by = c("from", "to"), ...)

# S3 method for class 'joint_readmission_death'
print(x, ...)

# S3 method for class 'loco_mp_coxnet'
print(x, ...)

# S3 method for class 'cbe_loco_mp_coxnet'
print(x, ...)

# S3 method for class 'cbe_loco_mp_coxnet'
tidy(x, ...)

# S3 method for class 'cbe_loco_mp_coxnet'
autoplot(object, ...)

# S3 method for class 'nested_survival_run'
print(x, ...)

# S3 method for class 'cbe_nested_survival_run'
print(x, ...)

# S3 method for class 'summary.cbe_nested_survival_run'
print(x, ...)

# S3 method for class 'cbe_repro_appendix'
x$name

# S3 method for class 'cbe_repro_appendix'
print(x, ...)

# S3 method for class 'domain_children'
x$name

# S3 method for class 'cbe_domain_children'
x$name

# S3 method for class 'domain_children'
print(x, ...)

# S3 method for class 'cbe_domain_children'
print(x, ...)

# S3 method for class 'domain_children'
summary(object, ...)

# S3 method for class 'cbe_domain_children'
summary(object, ...)

# S3 method for class 'domain_children'
tidy(x, ...)

# S3 method for class 'cbe_domain_children'
tidy(x, ...)

# S3 method for class 'recurrent_cox'
print(x, ...)

# S3 method for class 'step_famd'
prep(x, training, info = NULL, ...)

# S3 method for class 'step_famd'
bake(object, new_data, ...)

# S3 method for class 'step_famd'
print(x, width = max(20, options()$width - 29), ...)

# S3 method for class 'step_famd'
tunable(x, ...)

# S3 method for class 'step_famd'
required_pkgs(x, ...)

# S3 method for class 'step_lencode_coxnet'
prep(x, training, info = NULL, ...)

# S3 method for class 'step_lencode_coxnet'
bake(object, new_data, ...)

# S3 method for class 'step_lencode_coxnet'
print(x, width = max(20, options()$width - 30), ...)

# S3 method for class 'step_lencode_coxnet'
tidy(x, ...)

# S3 method for class 'step_lencode_joint_model'
prep(x, training, info = NULL, ...)

# S3 method for class 'step_lencode_joint_model'
bake(object, new_data, ...)

# S3 method for class 'step_lencode_joint_model'
print(x, width = max(20, options()$width - 30), ...)

# S3 method for class 'step_lencode_joint_model'
tidy(x, ...)

# S3 method for class 'censoring_km'
print(x, ...)

# S3 method for class 'cbe_start_stop_validation'
print(x, ...)

# S3 method for class 'cbe_completion_event'
x$name

# S3 method for class 'cbe_clean_checkpoints'
print(x, ...)

# S3 method for class 'cbe_runtime_estimate'
print(x, ...)
```

## Arguments

- ...:

  Passed to other methods, or not used.

- x, object:

  An object of the class named after the dot in the method.

- max_print:

  Largest number of differences to print for each table.

- y:

  A second object of the same class as \`x\`.

- node_by, edge_by:

  Columns that identify a node and an edge when two databases are
  combined. \`node_by = NULL\` uses the node name.

- name:

  Name of the element to extract.

- training:

  The training data from which a step is estimated.

- info:

  The variable table of the recipe (name, type, role, source).

- new_data:

  A data frame to which a trained step is applied.

- width:

  Width of the printed line.

## Value

\`print()\` methods return \`x\` invisibly. \`summary()\` and \`tidy()\`
methods return a tibble or a list of tibbles. \`\$\` returns the
element. The set operations return a database. \`prep()\` returns the
trained step, \`bake()\` the transformed data, \`tunable()\` a tibble of
tunable arguments and \`required_pkgs()\` a character vector of package
names.

## Details

\* \`print()\` methods write a short description of the object to the
console and return it invisibly. \* \`summary()\` and \`tidy()\` methods
return a tibble (or a list of tibbles) with the content of the object.
\* \`\$\` methods give access to elements under their current names, and
under former names with a deprecation warning. \* \`union()\`,
\`intersect()\` and \`setdiff()\` for a database (see \[as_database()\])
combine the nodes and the edges of two databases. \* \`prep()\`,
\`bake()\`, \`tidy()\`, \`tunable()\` and \`required_pkgs()\` are the
methods that recipes calls for the steps \[step_famd()\],
\[step_lencode_coxnet()\] and \[step_lencode_joint_model()\].
