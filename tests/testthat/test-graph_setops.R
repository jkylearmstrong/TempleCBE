test_that("graph_union() coalesces overlapping node/edge attributes", {
  g1 <- igraph::graph_from_data_frame(
    data.frame(from = "a", to = "b"),
    vertices = data.frame(name = c("a", "b"), stage = c("eda", NA))
  )
  g2 <- igraph::graph_from_data_frame(
    data.frame(from = "b", to = "c"),
    vertices = data.frame(name = c("b", "c"), stage = c("analysis", "report"))
  )

  gu <- graph_union(g1, g2)
  expect_s3_class(gu, "igraph")
  expect_equal(igraph::vcount(gu), 3)
  expect_equal(igraph::ecount(gu), 2)

  db <- as_database(gu)
  expect_equal(db$nodes$stage[db$nodes$name == "a"], "eda")
  # node "b": g1's NA stage is filled in by g2's "analysis"
  expect_equal(db$nodes$stage[db$nodes$name == "b"], "analysis")
  expect_equal(db$nodes$stage[db$nodes$name == "c"], "report")
})

test_that("graph_intersect()/graph_subtract() work across graph types", {
  g1 <- igraph::graph_from_data_frame(
    data.frame(from = c("a", "b", "c"), to = c("b", "c", "d")),
    vertices = data.frame(name = c("a", "b", "c", "d"))
  )
  g2 <- igraph::graph_from_data_frame(
    data.frame(from = "a", to = "b"),
    vertices = data.frame(name = c("a", "b"))
  )

  gi <- graph_intersect(g1, g2)
  expect_s3_class(gi, "igraph")
  expect_equal(igraph::vcount(gi), 2)
  expect_equal(igraph::ecount(gi), 1)
  expect_setequal(igraph::V(gi)$name, c("a", "b"))

  gs <- graph_subtract(g1, g2)
  expect_equal(igraph::vcount(gs), 2) # c, d
  expect_equal(igraph::ecount(gs), 1) # c->d (b->c dropped: b no longer a node)
  expect_setequal(igraph::V(gs)$name, c("c", "d"))

  # tbl_graph inputs work via as_database()'s igraph-inheritance dispatch
  gi_tg <- graph_intersect(tidygraph::as_tbl_graph(g1), tidygraph::as_tbl_graph(g2))
  expect_equal(igraph::vcount(gi_tg), 2)

  # No overlap at all -> empty graph, not an error
  g3 <- igraph::graph_from_data_frame(
    data.frame(from = "z", to = "w"),
    vertices = data.frame(name = c("z", "w"))
  )
  gi_empty <- graph_intersect(g1, g3)
  expect_equal(igraph::vcount(gi_empty), 0)
  expect_equal(igraph::ecount(gi_empty), 0)

  # visNetwork widgets (different node-key column name: "id" not "name")
  vn1 <- structure(
    list(x = list(
      nodes = data.frame(id = c("a", "b", "c")),
      edges = data.frame(from = c("a", "b"), to = c("b", "c"))
    )),
    class = c("visNetwork", "htmlwidget")
  )
  vn2 <- structure(
    list(x = list(
      nodes = data.frame(id = c("a", "b")),
      edges = data.frame(from = "a", to = "b")
    )),
    class = c("visNetwork", "htmlwidget")
  )
  gi_vn <- graph_intersect(vn1, vn2)
  expect_equal(igraph::vcount(gi_vn), 2)
  expect_equal(igraph::ecount(gi_vn), 1)
})
