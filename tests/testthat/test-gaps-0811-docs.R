# Fresh-review items 134/135 (docs/exports/namespace/CI): the pkgdown reference
# index must list the internal `templecbe_methods` topic page explicitly and must
# not list any topic twice.
test_that("pkgdown reference index lists templecbe_methods once and has no duplicates", {
  testthat::skip_if_not_installed("yaml")
  pkgdown_yml <- testthat::test_path("../../_pkgdown.yml")
  testthat::skip_if_not(file.exists(pkgdown_yml), "no _pkgdown.yml at the package root")
  cfg <- yaml::read_yaml(pkgdown_yml)
  testthat::expect_true(is.list(cfg$reference))

  ref_topics <- unlist(lapply(cfg$reference, function(section) section$contents))
  # Item 134: the 58 S3 methods live only on the internal `templecbe_methods`
  # topic page, so pkgdown only indexes it when it is listed explicitly.
  testthat::expect_true("templecbe_methods" %in% ref_topics)
  # Item 135: every reference topic listed exactly once (the duplicated
  # "PCA & Dimensionality Reduction" section and the second
  # `reexports` / `extract_yaml_formats` entries were removed).
  dup <- unique(ref_topics[duplicated(ref_topics)])
  testthat::expect_length(dup, 0)
  testthat::expect_equal(sum(ref_topics == "pca_loadings"), 1L)
  testthat::expect_equal(sum(ref_topics == "reexports"), 1L)
  testthat::expect_equal(sum(ref_topics == "extract_yaml_formats"), 1L)

  # `compute_graph` legitimately appears once under `articles:` (the vignette)
  # and once under `reference:` (the help topic); that pair is not a duplicate.
  article_topics <- unlist(lapply(cfg$articles, function(section) section$contents))
  testthat::expect_equal(sum(article_topics == "compute_graph"), 1L)
  testthat::expect_equal(sum(ref_topics == "compute_graph"), 1L)
})
