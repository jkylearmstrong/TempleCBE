fake_profvis <- function() {
  list(x = list(message = list(prof = data.frame(
    time = c(1, 1, 2, 2, 3),
    depth = c(1, 2, 1, 2, 1),
    label = c("f", "g", "f", "g", "f"),
    memalloc = c(10, 20, 15, 25, 30),
    meminc = c(0, 10, -5, 10, 15)
  ))))
}

test_that("profvis_summary tabulates samples by function and time", {
  s <- profvis_summary(fake_profvis())
  expect_named(s, c(
    "memory_by_function", "memory_increment_by_function", "calls_by_function",
    "deepest_calls", "memory_over_time"
  ))

  expect_equal(s$memory_by_function$label, c("f", "g"))
  expect_equal(s$memory_by_function$total_mem, c(55, 45))
  expect_equal(s$memory_by_function$pct, c(55, 45))
  expect_equal(s$memory_increment_by_function$total_meminc, c(20, 10))
  expect_equal(s$calls_by_function$times_called, c(3, 2))
  expect_equal(s$deepest_calls$label[1], "g")
  expect_equal(s$memory_over_time$total_mem, c(30, 40, 30))
})

test_that("profvis_summary rejects non-profvis input", {
  expect_error(profvis_summary(list()), "must be a profvis object")
})
