# Racing Methods and Controls for Survival Workflows

Provides an interface to finetune racing methods
([`tune_race_anova`](https://finetune.tidymodels.org/reference/tune_race_anova.html),
[`tune_race_win_loss`](https://finetune.tidymodels.org/reference/tune_race_win_loss.html),
and
[`control_race`](https://finetune.tidymodels.org/reference/control_race.html))
tailored for Tidymodels survival workflows, workflow sets, and nested
cross-validation.

## Details

Racing evaluates candidate parameter configurations across resample
folds sequentially, eliminating unpromising candidates using ANOVA
models or Bradley-Terry win-fraction models before evaluating all folds
on all grid points. This significantly accelerates hyperparameter
optimization for survival models such as
[`coxnet`](https://jkylearmstrong.github.io/TempleCBE/reference/coxnet.md)
and multi-model workflow sets.
