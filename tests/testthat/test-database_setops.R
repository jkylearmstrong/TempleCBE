test_that("dplyr::intersect()/dplyr::setdiff() on cbe_database match nodes/edges and handle differing key columns", {
  db_x <- as_database(list(
    nodes = tibble::tibble(name = c("a", "b", "c")),
    edges = tibble::tibble(from = c("a", "b"), to = c("b", "c"))
  ))
  db_y <- as_database(list(
    nodes = tibble::tibble(name = c("a", "b")),
    edges = tibble::tibble(from = "a", to = "b")
  ))

  di <- dplyr::intersect(db_x, db_y)
  expect_s3_class(di, "cbe_database")
  expect_equal(nrow(di$nodes), 2)
  expect_equal(nrow(di$edges), 1)
  expect_setequal(di$nodes$name, c("a", "b"))

  ds <- dplyr::setdiff(db_x, db_y)
  expect_equal(nrow(ds$nodes), 1)
  expect_equal(ds$nodes$name, "c")

  # Intersect + setdiff partition nodes exactly, but NOT necessarily edges:
  # edge b->c doesn't literally match one of db_y's edges (so it can't join
  # the intersect result), yet its endpoint `b` IS one of db_y's nodes (so
  # `b` -- and, by the well-formedness constraint, any edge touching it --
  # drops out of the setdiff result too). It belongs to neither side.
  expect_equal(nrow(di$nodes) + nrow(ds$nodes), nrow(db_x$nodes))
  expect_equal(nrow(ds$edges), 0)
  expect_lt(nrow(di$edges) + nrow(ds$edges), nrow(db_x$edges))

  # Different node-key column names: auto-detected via first column ...
  db_x2 <- as_database(list(
    nodes = tibble::tibble(id = c("a", "b", "c")),
    edges = tibble::tibble(from = c("a", "b"), to = c("b", "c"))
  ))
  db_y2 <- as_database(list(
    nodes = tibble::tibble(name = c("a", "b")),
    edges = tibble::tibble(from = "a", to = "b")
  ))
  di2 <- dplyr::intersect(db_x2, db_y2)
  expect_setequal(di2$nodes$id, c("a", "b"))

  # ... or specified explicitly (dplyr::*_join `by` convention) -- same result
  di3 <- dplyr::intersect(db_x2, db_y2, node_by = c(id = "name"))
  expect_equal(di2, di3)
})

test_that("dplyr::union() on cbe_database coalesces overlapping attributes instead of dropping them", {
  snapshot_today <- as_database(list(
    nodes = data.frame(
      name = c("a", "b"), stage = c("analysis", NA), mtime = c(2, NA),
      stringsAsFactors = FALSE
    ),
    edges = data.frame(from = character(0), to = character(0))
  ))
  snapshot_yesterday <- as_database(list(
    nodes = data.frame(
      name = c("a", "c"), stage = c("eda", "report"), mtime = c(1, 3),
      stringsAsFactors = FALSE
    ),
    edges = data.frame(from = character(0), to = character(0))
  ))

  u <- dplyr::union(snapshot_today, snapshot_yesterday)
  expect_s3_class(u, "cbe_database")
  expect_setequal(u$nodes$name, c("a", "b", "c"))

  # Node "a": today's non-NA stage/mtime win over yesterday's
  a_row <- u$nodes[u$nodes$name == "a", ]
  expect_equal(a_row$stage, "analysis")
  expect_equal(a_row$mtime, 2)

  # Node "b": only in today's snapshot, values pass through as-is
  b_row <- u$nodes[u$nodes$name == "b", ]
  expect_true(is.na(b_row$stage))

  # Node "c": only in yesterday's snapshot -- its data isn't dropped
  c_row <- u$nodes[u$nodes$name == "c", ]
  expect_equal(c_row$stage, "report")
  expect_equal(c_row$mtime, 3)
})

test_that("database_to_igraph() builds a well-formed igraph from a database", {
  db <- as_database(list(
    nodes = tibble::tibble(name = c("a", "b", "c")),
    edges = tibble::tibble(from = c("a", "b"), to = c("b", "c"))
  ))
  g <- database_to_igraph(db)
  expect_s3_class(g, "igraph")
  expect_equal(igraph::vcount(g), 3)
  expect_equal(igraph::ecount(g), 2)

  g_undirected <- database_to_igraph(db, directed = FALSE)
  expect_false(igraph::is_directed(g_undirected))
})
