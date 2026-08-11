test_that("R_names cleans column names and preserves originals as labels", {
  df <- tibble::tibble(
    `name with spaces` = 1:3,
    `special * characters` = c("a", "b", "c")
  )
  res <- R_names(df)

  expect_equal(colnames(res), janitor::clean_names(df) |> colnames())
  labels <- labelled::var_label(res)
  expect_equal(unname(unlist(labels)), c("name with spaces", "special * characters"))
})
