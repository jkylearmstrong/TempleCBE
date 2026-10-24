test_that("sample_candidate_formulas_adaptive learns size and terms from winners", {
  prior <- data.frame(
    formula = c("y ~ a + b", "y ~ a + c", "y ~ a + b + c", "y ~ d", "y ~ d + e", "y ~ e"),
    IBS = c(0.10, 0.11, 0.12, 0.20, 0.22, 0.25)
  )
  out <- sample_candidate_formulas_adaptive(prior, top_quantile = 0.5, n_formulas = 3, seed = 1)
  ad <- attr(out, "adaptive")
  expect_setequal(ad$pool, c("a", "b", "c"))
  expect_true(all(out$n_terms >= ad$min_size & out$n_terms <= ad$max_size))
  expect_true(all(vapply(out$formula, function(f) identical(deparse(f[[2]]), "y"), logical(1))))

  eff <- data.frame(term = c("a", "b", "c"), estimate = c(1.5, 0.8, 1.2))
  out2 <- sample_candidate_formulas_adaptive(prior, top_quantile = 0.5, n_formulas = 1, effects = eff, seed = 1)
  expect_false("b" %in% attr(out2, "adaptive")$pool)

  expect_error(sample_candidate_formulas_adaptive(prior, metric = "nope"), "not in")
  expect_error(sample_candidate_formulas_adaptive(prior, top_quantile = 0), "top_quantile")
})

test_that("tbl_table_one builds an overall, per-stratum and p-value table", {
  skip_if_not_installed("gtsummary")
  d <- mtcars
  d$cyl <- factor(d$cyl)
  tb <- tbl_table_one(d, strata = "cyl", vars = c("mpg", "hp", "am"), non_normal_vars = "hp")
  expect_s3_class(tb, "gtsummary")
  df <- gtsummary::as_tibble(tb, col_labels = FALSE)
  expect_true(any(grepl("p.value", names(df))))
  expect_true(any(grepl("\\[", unlist(df[df[[1]] == "hp" | grepl("hp", df[[1]]), ]))))
  expect_error(tbl_table_one(d, strata = "zzz"), "strata")
  expect_s3_class(tbl_table_one(d, vars = c("mpg", "am")), "gtsummary")
})

test_that("plot_imputed_vs_observed draws per fold", {
  set.seed(1)
  d <- data.frame(Fold = rep(c("F1", "F2"), each = 30), observed = rnorm(60))
  d$imputed <- d$observed + rnorm(60, 0, 0.5)
  d$imputed[1] <- NA
  p <- plot_imputed_vs_observed(d, type = "density")
  expect_s3_class(p, "ggplot")
  expect_equal(nrow(p$data), 2 * 59)
  expect_s3_class(plot_imputed_vs_observed(d, type = "boxplot"), "ggplot")
  expect_error(plot_imputed_vs_observed(d, observed_col = "zzz"), "not in")
  skip_if_not_installed("patchwork")
  expect_s3_class(plot_imputed_vs_observed(d), "patchwork")
})

test_that("plot_comissingness_matrix finds columns that drop out together", {
  d <- data.frame(
    a = c(1, NA, 3, NA, 5, 6, 7, NA), b = c(1, NA, 3, NA, 5, 6, 7, NA),
    c = c(NA, 2, 3, 4, 5, NA, 7, 8), full = 1:8, empty = NA_real_
  )
  p <- plot_comissingness_matrix(d, k = 2)
  expect_s3_class(p, "ggplot")
  expect_equal(attr(p, "dropped"), c("full", "empty"))
  expect_equal(p$data$r[p$data$var1 == "a" & p$data$var2 == "b"], 1)
  expect_error(plot_comissingness_matrix(d, vars = c("a", "full")), "Fewer than two")
})

test_that("plot_marginal_effect_contrast supports both layouts", {
  res <- rbind(glm_single(mtcars, "mpg", "wt"), glm_single(mtcars, "mpg", "hp"))
  p1 <- plot_marginal_effect_contrast(res)
  expect_s3_class(p1, "ggplot")
  expect_equal(nrow(p1$data), 4)
  p2 <- plot_marginal_effect_contrast(res, layout = "ratio")
  expect_equal(nrow(p2$data), 2)
  rng <- diff(range(mtcars$wt))
  expect_equal(p2$data$ratio[grepl("wt", p2$data$label)], rng, tolerance = 1e-6)
  expect_error(plot_marginal_effect_contrast(data.frame(x = 1)), "not in")
})

test_that("render_parameterized_study routes each parameter set to its folder", {
  src <- withr::local_tempdir()
  writeLines("---\ntitle: a\nparams:\n  v: 1\n---\n", file.path(src, "a.qmd"))
  writeLines("x", file.path(src, "_inc.qmd"))
  out <- withr::local_tempdir()
  t1 <- file.path(out, "raw")
  t2 <- file.path(out, "imp")
  plan <- render_parameterized_study(src, c(t1, t2), list(list(v = 1), list(v = 2)), dry_run = TRUE)
  expect_equal(nrow(plan), 2)
  expect_setequal(plan$target_dir, c(t1, t2))
  expect_true(all(basename(plan$document) == "a.qmd"))
  expect_error(render_parameterized_study(src, c(t1, t2), list(list(v = 1))), "one parameter set")
  expect_error(render_parameterized_study(src, c(t1, t1), list(list(v = 1), list(v = 2))), "different")
  # undeclared parameter: reported, not fatal
  bad <- render_parameterized_study(src, t1, list(list(zzz = 1)), dry_run = TRUE)
  expect_equal(bad$status, "failed")
})

test_that("invalidate_downstream_checkpoints removes only downstream checkpoints", {
  raw <- FilePath("raw", "data/raw.csv")
  clean_rds <- FilePath("clean_rds", "clean.rds")
  clean <- FileOutputs("clean", "01_clean.qmd", dependencies = list(raw), output = list(clean_rds))
  model <- FileOutputs("model", "02_model.qmd", dependencies = list(clean_rds),
                       output = list(FilePath("model_rds", "model.rds")))
  other <- FileOutputs("other", "03_other.qmd", dependencies = list(raw),
                       output = list(FilePath("other_rds", "other.rds")))
  g <- list(raw, clean, model, other)
  ck <- withr::local_tempdir()
  files <- c("clean.rds", "model.rds", "model_fold1.rds", "model2.rds", "other.rds")
  file.create(file.path(ck, files))

  dry <- invalidate_downstream_checkpoints(g, "clean", ck, dry_run = TRUE)
  expect_true(all(file.exists(file.path(ck, files))))
  expect_setequal(dry$file, c("clean.rds", "model.rds", "model_fold1.rds"))

  res <- invalidate_downstream_checkpoints(g, "clean", ck)
  expect_true(all(res$removed))
  expect_false(file.exists(file.path(ck, "model.rds")))
  expect_false(file.exists(file.path(ck, "model_fold1.rds")))
  expect_true(all(file.exists(file.path(ck, c("model2.rds", "other.rds")))))
  file.create(file.path(ck, c("clean.rds", "model.rds")))

  # a changed raw file reaches everything built from it, by path too
  res2 <- invalidate_downstream_checkpoints(g, "data/raw.csv", ck)
  expect_setequal(res2$file, c("clean.rds", "model.rds", "other.rds"))
  expect_error(invalidate_downstream_checkpoints(g, "nope", ck), "not a node")
  expect_equal(nrow(invalidate_downstream_checkpoints(g, "clean", file.path(ck, "missing"))), 0)
})

test_that("expand_child_template expands one child per row", {
  skip_if_not_installed("knitr")
  tmp <- withr::local_tempfile(fileext = ".qmd")
  writeLines("## {{label}}\n\nFeature `{{feature}}`.", tmp)
  grid <- data.frame(feature = c("age", "bmi"), label = c("Age", "BMI"))
  txt <- expand_child_template(tmp, grid, expand_only = TRUE)
  expect_match(txt, "## Age", fixed = TRUE)
  expect_match(txt, "Feature `bmi`", fixed = TRUE)
  knit <- expand_child_template(tmp, grid)
  expect_match(paste(knit, collapse = "\n"), "## BMI", fixed = TRUE)
  expect_error(expand_child_template(tmp, grid[0, ]), "at least one row")
  expect_error(expand_child_template("nope.qmd", grid), "existing file")
})

test_that("input checks and edge branches of the 0.7.4 helpers", {
  prior <- data.frame(
    f = c("y ~ a + b", "y ~ a + c", "y ~ d"), s = c(0.9, 0.8, 0.2)
  )
  # higher-is-better metric, formula objects, required term, coef scale
  out <- sample_candidate_formulas_adaptive(
    prior, metric = "s", formula_col = "f", lower_is_better = FALSE, top_quantile = 0.7,
    required = "a", effects = data.frame(term = "b", estimate = -1), effect_scale = "coef",
    n_formulas = 1, seed = 2
  )
  expect_true(all(vapply(out$terms, function(t) "a" %in% t, logical(1))))
  expect_false("b" %in% attr(out, "adaptive")$pool)
  prior$f <- lapply(prior$f, stats::as.formula)
  expect_s3_class(sample_candidate_formulas_adaptive(prior, metric = "s", formula_col = "f", n_formulas = 1, seed = 1), "tbl_df")
  expect_error(sample_candidate_formulas_adaptive(1), "data frame")
  expect_error(sample_candidate_formulas_adaptive(prior, metric = "s", formula_col = "f", effects = data.frame(a = 1)), "term")
  expect_warning(
    sample_candidate_formulas_adaptive(prior, metric = "s", formula_col = "f", top_quantile = 1, n_formulas = 1,
                                       effects = data.frame(term = c("a", "b", "c", "d"), estimate = 0.5)),
    "unfavourable"
  )
  bad <- data.frame(f = NA_character_, s = NA_real_)
  expect_error(sample_candidate_formulas_adaptive(bad, metric = "s", formula_col = "f"), "No row")

  skip_if_not_installed("gtsummary")
  d <- mtcars
  d$cyl <- factor(d$cyl)
  d$gear <- factor(d$gear)
  expect_error(tbl_table_one(1), "data frame")
  expect_error(tbl_table_one(d, vars = "zzz"), "not in")
  expect_error(tbl_table_one(d[d$cyl == "4", ], strata = "cyl"), "two levels")
  expect_s3_class(tbl_table_one(d, strata = "am", vars = c("mpg", "hp", "gear"), non_normal_vars = "hp", overall = FALSE), "gtsummary")
  expect_s3_class(tbl_table_one(d, strata = "cyl", vars = "gear", test = FALSE), "gtsummary")

  p <- data.frame(Fold = "F1", observed = 1:5, imputed = c(1.1, 2, 2.9, 4.2, 5))
  expect_s3_class(plot_imputed_vs_observed(p, type = "boxplot", title = "t"), "ggplot")
  expect_error(plot_imputed_vs_observed(1), "data frame")
  expect_error(plot_imputed_vs_observed(transform(p, imputed = "a")), "numeric")
  expect_error(plot_imputed_vs_observed(transform(p, imputed = NA_real_), type = "density"), "No row")
  skip_if_not_installed("patchwork")
  expect_s3_class(plot_imputed_vs_observed(p, title = "t"), "patchwork")

  m <- data.frame(a = c(1, NA, 3, NA, 5, 6), b = c(1, NA, 3, NA, 5, NA), c = c(NA, 2, 3, 4, 5, 6))
  expect_error(plot_comissingness_matrix(1), "data frame")
  expect_error(plot_comissingness_matrix(m, vars = "zzz"), "not in")
  expect_s3_class(plot_comissingness_matrix(m, cluster = FALSE, k = 2, label = FALSE), "ggplot")
  expect_s3_class(plot_comissingness_matrix(m[c("a", "b")]), "ggplot")

  res <- rbind(glm_single(mtcars, "mpg", "wt"), glm_single(mtcars, "mpg", "hp"))
  expect_error(plot_marginal_effect_contrast(1), "data frame")
  expect_error(plot_marginal_effect_contrast(res[0, ]), "No raw")
  expect_error(plot_marginal_effect_contrast(res[res$scale == "raw", ], layout = "ratio"), "both")
  zero <- res
  zero$estimate[zero$scale == "raw" & zero$feature == "wt"] <- 0
  expect_s3_class(plot_marginal_effect_contrast(zero, layout = "ratio"), "ggplot")
})

test_that("render_parameterized_study, invalidate and expand reject bad input", {
  src <- withr::local_tempdir()
  expect_error(render_parameterized_study("nope", "a", list(list())), "existing folder")
  expect_error(render_parameterized_study(src, character(), list()), "target_dirs")
  expect_error(render_parameterized_study(src, "a", list(list())), "No document")
  writeLines("---\ntitle: a\nparams:\n  v: 1\n---\n", file.path(src, "a.qmd"))
  expect_error(render_parameterized_study(src, "a", list(1)), "named list")
  expect_error(render_parameterized_study(src, src, list(list(v = 1))), "source folder")
  expect_error(
    render_parameterized_study(src, withr::local_tempdir(), list(list(zzz = 1)), on_error = "stop", dry_run = TRUE),
    "zzz"
  )

  g <- list(FilePath("raw", "raw.csv"))
  expect_error(invalidate_downstream_checkpoints(list(), "raw"), "non-empty")
  expect_error(invalidate_downstream_checkpoints(g, c("a", "b")), "one node")
  expect_equal(nrow(invalidate_downstream_checkpoints(g, "raw", withr::local_tempdir())), 0)
  prod <- FileOutputs("p", "p.qmd", dependencies = g, output = list(FilePath("o", "o.rds")))
  ck <- withr::local_tempdir()
  dir.create(file.path(ck, "p"))
  file.create(file.path(ck, "p", "x.rds"))
  r <- invalidate_downstream_checkpoints(list(g[[1]], prod), "p", ck, include_self = TRUE)
  expect_equal(r$file, "p/x.rds")

  skip_if_not_installed("knitr")
  tmp <- withr::local_tempfile(fileext = ".qmd")
  writeLines("{{x}}", tmp)
  expect_error(expand_child_template(tmp, 1), "data frame")
  expect_error(expand_child_template(tmp, data.frame(x = 1), delim = "{"), "delim")
  expect_equal(expand_child_template(tmp, list(x = c("a", "b")), expand_only = TRUE, sep = "|"), "a|b")
})
