test_that("as_database() converts igraph/tbl_graph and visNetwork objects", {
  edges_df <- data.frame(
    from = c("a", "b", "c"),
    to = c("b", "c", "a"),
    weight = c(1, 2, 3)
  )
  nodes_df <- data.frame(name = c("a", "b", "c"), label = c("A", "B", "C"))

  g <- igraph::graph_from_data_frame(edges_df, vertices = nodes_df, directed = TRUE)

  db <- as_database(g)
  expect_type(db, "list")
  expect_named(db, c("nodes", "edges"))
  expect_s3_class(db$nodes, "tbl_df")
  expect_s3_class(db$edges, "tbl_df")
  expect_equal(nrow(db$nodes), 3)
  expect_equal(nrow(db$edges), 3)
  expect_setequal(db$nodes$label, c("A", "B", "C"))
  expect_setequal(db$edges$weight, c(1, 2, 3))

  # tbl_graph dispatches through the igraph method via class inheritance
  tg <- tidygraph::as_tbl_graph(g)
  db_tg <- as_database(tg)
  expect_equal(nrow(db_tg$nodes), 3)
  expect_equal(nrow(db_tg$edges), 3)

  # visNetwork htmlwidgets store their node/edge tables under $x
  vn <- structure(
    list(x = list(nodes = nodes_df, edges = edges_df)),
    class = c("visNetwork", "htmlwidget")
  )
  db_vn <- as_database(vn)
  expect_equal(db_vn$nodes, tibble::as_tibble(nodes_df))
  expect_equal(db_vn$edges, tibble::as_tibble(edges_df))

  # Unsupported class errors informatively rather than silently misbehaving
  expect_error(as_database(list(1, 2)), "No `as_database\\(\\)` method")
})
