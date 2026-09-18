test_that("cbe_find_shared_keys and cbe_database_relationships detect links and cardinality", {
  patients <- data.frame(
    id = 1:5,
    age = c(50, 55, 60, 65, 70)
  )

  vitals <- data.frame(
    id = c(1, 1, 2, 2, 3, 3, 6),
    visit = c(1, 2, 1, 2, 1, 2, 1),
    sbp = c(120, 125, 130, 135, 140, 145, 110)
  )

  db <- list(patients = patients, vitals = vitals)

  # Check shared keys
  shared <- cbe_find_shared_keys(db)
  expect_equal(shared$column, "id")
  expect_equal(shared$n_tables, 2)

  # Check relationships
  rel <- cbe_database_relationships(db)
  expect_equal(nrow(rel), 1)
  expect_equal(rel$from_table, "patients")
  expect_equal(rel$to_table, "vitals")
  expect_equal(rel$cardinality, "1:Many")
  expect_equal(rel$shared_keys, 3) # IDs 1, 2, 3

  # Check integrity without compare
  integ <- cbe_check_key_integrity(db, id_col = "id", master_dataset = "patients")
  expect_s3_class(integ, "cbe_key_integrity")
  expect_equal(nrow(integ), 2)
  vitals_integ <- integ |> dplyr::filter(dataset_name == "vitals")
  expect_equal(vitals_integ$orphan_ids_count, 1) # ID 6 is not in patients

  # Check integrity with compare = TRUE on overlapping variables
  patients2 <- data.frame(id = 1:5, age = c(50, 55, 60, 65, 70), sex = c("M", "F", "M", "F", "M"))
  demo_audit <- data.frame(id = 1:5, age = c(50, 55, 62, 65, 70), sex = c("M", "F", "M", "F", "M"))
  db2 <- list(master = patients2, audit = demo_audit)

  integ_cmp <- cbe_check_key_integrity(db2, id_col = "id", master_dataset = "master", compare = TRUE)
  cmps <- attr(integ_cmp, "comparisons")
  expect_false(is.null(cmps))
  expect_false(is.null(cmps$audit))
  expect_equal(cmps$audit$summary$n_diff[cmps$audit$summary$variable == "age"], 1) # ID 3 age 60 vs 62

  # Check cbe_compare_df direct list call
  cmp_direct <- cbe_compare_df(db2, c("master", "audit"), by = "id")
  expect_s3_class(cmp_direct, "cbe_compare_df")
  expect_equal(cmp_direct$meta$base_name, "master")
  expect_equal(cmp_direct$meta$compare_name, "audit")

  # Check graph conversions
  tg <- as_tbl_graph(rel)
  expect_s3_class(tg, "tbl_graph")
  expect_equal(igraph::vcount(tg), 2)
  expect_equal(igraph::ecount(tg), 1)

  ig <- as_igraph(rel)
  expect_s3_class(ig, "igraph")

  # Check cbe_database_venn
  p_venn <- cbe_database_venn(db, id_col = "id")
  expect_s3_class(p_venn, "ggplot")

  # Check autoplot on cbe_key_integrity
  p_integ <- ggplot2::autoplot(integ)
  expect_s3_class(p_integ, "ggplot")

  # Check autoplot on cbe_compare_df
  p_obs <- ggplot2::autoplot(cmp_direct, type = "observations")
  expect_s3_class(p_obs, "ggplot")

  p_vars <- ggplot2::autoplot(cmp_direct, type = "variables")
  expect_s3_class(p_vars, "ggplot")

  p_diff <- ggplot2::autoplot(cmp_direct, type = "discrepancies")
  expect_s3_class(p_diff, "ggplot")
})



