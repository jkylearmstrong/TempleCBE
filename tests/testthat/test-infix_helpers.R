test_that("infix operators function as expected", {
  expect_true(1 %notin% c(2, 3, 4))
  expect_false(1 %notin% c(1, 2, 3))
  expect_true(1 %!in% c(2, 3, 4))
  expect_false(1 %!in% c(1, 2, 3))
  
  expect_true("hello world" %like% "world")
  expect_true("HELLO WORLD" %ilike% "world")
  expect_true("a.b" %flike% ".")
  expect_true("abc123xyz" %plike% "\\d+")
  expect_false("abcxyz" %plike% "\\d+")
})
