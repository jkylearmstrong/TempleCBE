# Reconcile a Data Frame's Column Names With a Column Dictionary

Maps the columns of \`data\` onto a dictionary of source names (for
example the upper-case or mixed-case variable names of a SAS export) and
target names (the standardized R names), and reports whether the mapping
is complete and one to one. It is meant for the check that comes before
a rename: it does not guess, and it does not rename anything that is
ambiguous.

## Usage

``` r
reconcile_column_dictionary(
  data,
  dictionary,
  source_col = "source",
  target_col = "target",
  required = NULL,
  rename = c("if_valid", "always", "never")
)
```

## Arguments

- data:

  A data frame.

- dictionary:

  A data frame with one row per variable.

- source_col, target_col:

  Names of the columns of \`dictionary\` that hold the source names (as
  they appear in \`data\`) and the target names.

- required:

  Character vector of target names that must be mapped for the result to
  be valid. \`NULL\` (the default) requires every target in the
  dictionary.

- rename:

  When is \`\$data\` filled in with the renamed data frame?
  \`"if_valid"\` (the default), \`"always"\` (the unambiguous matches
  are renamed, whatever else is wrong) or \`"never"\`.

## Value

An object of class \`cbe_column_reconciliation\`, a list with \*
\`data\`: the data frame with the matched columns renamed, or \`NULL\`
(see \`rename\`); \* \`matched\`: tibble of the confirmed matches
(\`raw_name\`, \`clean_name\`, \`target_name\`, \`match_tier\`, the tier
being \`"exact"\`, \`"case_insensitive"\` or \`"normalized"\`); \*
\`unmapped_data\`: columns of \`data\` that matched nothing; \*
\`unmapped_dict\`: source names of the dictionary that matched no
column; \* \`conflicts\`: tibble (\`type\`, \`raw_name\`,
\`source_name\`, \`target_name\`, \`detail\`); \* \`is_valid\`: \`TRUE\`
if there is no conflict and every required target is mapped.

## Details

Each column of \`data\` is matched to the dictionary's source names in
three tiers, and the first tier that finds anything decides: 1.
\*\*Exact\*\*: \`col == source\`. 2. \*\*Case-insensitive\*\*:
\`tolower(col) == tolower(source)\`. 3. \*\*Normalized\*\*: both cleaned
by \`janitor::make_clean_names()\` (the cleaning that
\[clean_names_keep_labels()\] applies: symbols and spaces become \`\_\`,
lower case, repeated \`\_\` collapsed), one name at a time, so that two
names that clean to the same string are seen as a collision and not
renamed apart.

A match is a \*\*conflict\*\*, and is not used, when \* a column matches
more than one dictionary entry in its tier (\`"ambiguous_column"\`); \*
a dictionary entry is matched by more than one column
(\`"shared_source"\`), for instance \`Bruise_Area\` and \`bruise_area\`
both present; \* two matched entries have the same target
(\`"shared_target"\`), or the dictionary lists a source twice with
different targets (\`"duplicate_source"\`); \* the target is already the
name of another column of \`data\` that is not itself being renamed
(\`"target_in_data"\`).

## See also

\[clean_names_keep_labels()\], \[compare_df()\]

## Examples

``` r
dat <- data.frame(Bruise_Area = 1:3, CCS_CB_caud_ceph = 4:6, extra = 7:9)
dict <- data.frame(
  source = c("bruise_area", "CCS CB caud ceph", "age"),
  target = c("bruise_area", "ccs_cb_caud_ceph", "age")
)
res <- reconcile_column_dictionary(dat, dict)
res$matched
#> # A tibble: 2 × 4
#>   raw_name         clean_name       target_name      match_tier      
#>   <chr>            <chr>            <chr>            <chr>           
#> 1 Bruise_Area      bruise_area      bruise_area      case_insensitive
#> 2 CCS_CB_caud_ceph ccs_cb_caud_ceph ccs_cb_caud_ceph normalized      
res$unmapped_data
#> [1] "extra"
res$unmapped_dict
#> [1] "age"
res$is_valid
#> [1] FALSE
```
