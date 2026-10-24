preds <- c("age", "sex", "bmi", "lactate", "log(crp)", "age:sex")

test_that("sample_candidate_formulas() returns distinct formulas inside the size limits", {
  cand <- sample_candidate_formulas(preds, response = "Surv(time, status)", n = 20, min_size = 2, max_size = 4, seed = 1)
  expect_named(cand, c("id", "formula", "terms", "n_terms"))
  expect_equal(nrow(cand), 20L)
  expect_true(all(cand$n_terms >= 2L & cand$n_terms <= 4L))
  expect_equal(cand$n_terms, vapply(cand$terms, length, integer(1)))
  keys <- vapply(cand$terms, function(t) paste(sort(t), collapse = "|"), character(1))
  expect_false(anyDuplicated(keys) > 0)
  expect_true(all(vapply(cand$formula, inherits, logical(1), "formula")))
  expect_equal(deparse(cand$formula[[1]][[2]]), "Surv(time, status)")
})

test_that("formulas are one-sided without a response and use the given environment", {
  e <- new.env()
  cand <- sample_candidate_formulas(preds, n = 3, seed = 1, env = e)
  expect_length(cand$formula[[1]], 2L)
  expect_identical(environment(cand$formula[[1]]), e)
})

test_that("the same seed gives the same formulas and the caller's RNG state is untouched", {
  a <- sample_candidate_formulas(preds, n = 10, seed = 42)
  b <- sample_candidate_formulas(preds, n = 10, seed = 42)
  expect_equal(a$terms, b$terms)
  set.seed(7)
  before <- runif(1)
  set.seed(7)
  invisible(sample_candidate_formulas(preds, n = 10, seed = 42))
  expect_equal(runif(1), before)
  expect_false(identical(a$terms, sample_candidate_formulas(preds, n = 10, seed = 43)$terms))
})

test_that("required terms are in every formula and count toward the size", {
  cand <- sample_candidate_formulas(preds, required = c("age", "sex"), n = 10, min_size = 3, max_size = 3, seed = 3)
  expect_true(all(vapply(cand$terms, function(t) all(c("age", "sex") %in% t), logical(1))))
  expect_true(all(cand$n_terms == 3L))
  # a required term that is not in `predictors` is added
  cand2 <- sample_candidate_formulas(c("bmi", "lactate"), required = "age", n = 3, seed = 3)
  expect_true(all(vapply(cand2$terms, function(t) "age" %in% t, logical(1))))
  expect_equal(cand2$terms[[1]][1], "age")
})

test_that("interactions need their variables when hierarchy = TRUE", {
  cand <- sample_candidate_formulas(preds, n = 25, seed = 5)
  has_int <- vapply(cand$terms, function(t) "age:sex" %in% t, logical(1))
  expect_true(any(has_int))
  expect_true(all(vapply(cand$terms[has_int], function(t) all(c("age", "sex") %in% t), logical(1))))
  free <- sample_candidate_formulas(preds, n = 40, hierarchy = FALSE, seed = 5)
  bad <- vapply(free$terms, function(t) "age:sex" %in% t && !all(c("age", "sex") %in% t), logical(1))
  expect_true(any(bad))
})

test_that("exclude keeps listed terms apart", {
  cand <- sample_candidate_formulas(c("bmi", "weight", "age", "sex"), exclude = list(c("bmi", "weight")), n = 30, seed = 1)
  expect_false(any(vapply(cand$terms, function(t) all(c("bmi", "weight") %in% t), logical(1))))
  expect_error(sample_candidate_formulas(c("bmi", "weight"), required = c("bmi", "weight"),
                                         exclude = list(c("bmi", "weight"))), "required.*constraint")
})

test_that("weights steer the sampling; a zero weight keeps a term out", {
  w <- c(age = 1, sex = 1, bmi = 0, lactate = 1, `log(crp)` = 1, `age:sex` = 1)
  cand <- sample_candidate_formulas(preds, n = 30, max_size = 3, weights = w, seed = 9)
  expect_false(any(vapply(cand$terms, function(t) "bmi" %in% t, logical(1))))
  heavy <- sample_candidate_formulas(c("a", "b", "c", "d", "e", "f"), n = 40, min_size = 1, max_size = 1,
                                     weights = c(100, 1, 1, 1, 1, 1), seed = 2, unique = FALSE)
  expect_gt(mean(vapply(heavy$terms, function(t) t == "a", logical(1))), 0.5)
  expect_error(sample_candidate_formulas(preds, weights = c(1, 2)), "one value per")
  expect_error(sample_candidate_formulas(preds, weights = rep(-1, 6)), "non-negative")
})

test_that("fewer distinct formulas than requested warns and returns what exists", {
  expect_warning(cand <- sample_candidate_formulas(c("a", "b"), n = 20, seed = 1), "Only 3 of 20")
  expect_equal(nrow(cand), 3L)
})

test_that("invalid input errors clearly", {
  expect_error(sample_candidate_formulas(character()), "non-empty")
  expect_error(sample_candidate_formulas(c("a", "a")), "repeated")
  expect_error(sample_candidate_formulas(preds, min_size = 5, max_size = 2), "No formula fits")
  expect_error(sample_candidate_formulas(preds, n = 0), "positive")
  expect_error(sample_candidate_formulas(preds, response = c("x", "y")), "one string")
  expect_error(sample_candidate_formulas(preds, exclude = c("a", "b")), "list of character")
})

# ---- calculate_feature_weights ------------------------------------------------

test_that("score-based weights are the score shares of the formulas that contain the feature", {
  cand <- list(c("a", "b"), c("a"), c("c"))
  w <- calculate_feature_weights(cand, scores = c(0.8, 0.6, 0.4))
  # shifted scores 0.4, 0.2, 0 -> shares 2/3, 1/3, 0
  expect_equal(w$feature, c("a", "b", "c"))
  expect_equal(w$weight, c(1, 2 / 3, 0))
  expect_equal(w$n_formulas, c(2L, 1L, 1L))
  expect_equal(w$rank, 1:3)
})

test_that("inclusion weights ignore the scores", {
  cand <- list(c("a", "b"), c("a"), c("c"), c("a", "c"))
  w <- calculate_feature_weights(cand, method = "inclusion")
  expect_equal(w$weight[w$feature == "a"], 3 / 4)
  expect_equal(w$weight[w$feature == "b"], 1 / 4)
})

test_that("higher_is_better = FALSE flips the ranking, equal scores give equal shares", {
  cand <- list(c("a"), c("b"))
  expect_equal(calculate_feature_weights(cand, c(0.1, 0.3), higher_is_better = FALSE)$feature, c("a", "b"))
  eq <- calculate_feature_weights(cand, c(0.5, 0.5))
  expect_equal(eq$weight, c(0.5, 0.5))
})

test_that("normalize rescales the weights", {
  cand <- list(c("a", "b"), c("a"), c("c"))
  expect_equal(sum(calculate_feature_weights(cand, c(3, 2, 1), normalize = "sum")$weight), 1)
  expect_equal(max(calculate_feature_weights(cand, c(3, 2, 1), normalize = "max")$weight), 1)
})

test_that("transformed terms and interactions are counted per variable or per term", {
  cand <- list(c("log(x)", "x:z"), c("poly(x, 2)"), c("z"))
  v <- calculate_feature_weights(cand, method = "inclusion")
  expect_equal(v$feature, c("x", "z"))
  expect_equal(v$n_formulas, c(2L, 2L)) # x once per formula even with two terms; z in formulas 1 and 3
  t <- calculate_feature_weights(cand, method = "inclusion", level = "term")
  expect_setequal(t$feature, c("log(x)", "x:z", "poly(x, 2)", "z"))
})

test_that("it accepts sample_candidate_formulas() output and formulas", {
  cand <- sample_candidate_formulas(c("a", "b", "c"), n = 5, seed = 1)
  sc <- seq_len(nrow(cand)) / 10
  expect_equal(calculate_feature_weights(cand, sc), calculate_feature_weights(cand$formula, sc))
})

test_that("calculate_feature_weights() validates scores", {
  cand <- list("a", "b")
  expect_error(calculate_feature_weights(cand), "scores. is needed")
  expect_error(calculate_feature_weights(cand, 1), "one value per candidate")
  expect_warning(w <- calculate_feature_weights(cand, c(1, NA)), "dropped")
  expect_equal(w$feature, "a")
  expect_error(calculate_feature_weights(cand, c(NA_real_, NA_real_)), "No candidate has a score")
  expect_error(calculate_feature_weights(1:3, 1:3), "must be a result")
})
