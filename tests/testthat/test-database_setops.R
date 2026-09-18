test_that("database_intersect()/database_subtract() match nodes/edges and handle differing key columns", {
  db_x <- list(
    nodes = tibble::tibble(name = c("a", "b", "c")),
    edges = tibble::tibble(from = c("a", "b"), to = c("b", "c"))
  )
  db_y <- list(
    nodes = tibble::tibble(name = c("a", "b")),
    edges = tibble::tibble(from = "a", to = "b")
  )

  di <- database_intersect(db_x, db_y)
  expect_equal(nrow(di$nodes), 2)
  expect_equal(nrow(di$edges), 1)
  expect_setequal(di$nodes$name, c("a", "b"))

  ds <- database_subtract(db_x, db_y)
  expect_equal(nrow(ds$nodes), 1)
  expect_equal(ds$nodes$name, "c")

  # Intersect + subtract partition nodes exactly, but NOT necessarily edges:
  # edge b->c doesn't literally match one of db_y's edges (so it can't join
  # the intersect result), yet its endpoint `b` IS one of db_y's nodes (so
  # `b` -- and, by the well-formedness constraint, any edge touching it --
  # drops out of the subtract result too). It belongs to neither side.
  expect_equal(nrow(di$nodes) + nrow(ds$nodes), nrow(db_x$nodes))
  expect_equal(nrow(ds$edges), 0)
  expect_lt(nrow(di$edges) + nrow(ds$edges), nrow(db_x$edges))

  # Different node-key column names: auto-detected via first column ...
  db_x2 <- list(
    nodes = tibble::tibble(id = c("a", "b", "c")),
    edges = tibble::tibble(from = c("a", "b"), to = c("b", "c"))
  )
  db_y2 <- list(
    nodes = tibble::tibble(name = c("a", "b")),
    edges = tibble::tibble(from = "a", to = "b")
  )
  di2 <- database_intersect(db_x2, db_y2)
  expect_setequal(di2$nodes$id, c("a", "b"))

  # ... or specified explicitly (dplyr::*_join `by` convention) -- same result
  di3 <- database_intersect(db_x2, db_y2, node_by = c(id = "name"))
  expect_equal(di2, di3)
})

test_that("database_to_igraph() builds a well-formed igraph from a database", {
  db <- list(
    nodes = tibble::tibble(name = c("a", "b", "c")),
    edges = tibble::tibble(from = c("a", "b"), to = c("b", "c"))
  )
  g <- database_to_igraph(db)
  expect_s3_class(g, "igraph")
  expect_equal(igraph::vcount(g), 3)
  expect_equal(igraph::ecount(g), 2)

  g_undirected <- database_to_igraph(db, directed = FALSE)
  expect_false(igraph::is_directed(g_undirected))
})
