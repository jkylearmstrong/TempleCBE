fake_plan <- function() {
  list(
    FileOutputs("clean", "clean.qmd", renders = TRUE),
    FileOutputs("model", "model.qmd", renders = TRUE),
    FileOutputs("report", "report.qmd", renders = TRUE)
  )
}

# stand-ins for the console and for the renderer; returns the render_pipeline() arguments
with_console <- function(choice, answer = "", code) {
  calls <- new.env()
  calls$args <- NULL
  testthat::local_mocked_bindings(
    .session_is_interactive = function() TRUE,
    .menu_choice = function(choices, title) choice,
    .read_answer = function(prompt) answer,
    get_render_plan = function(...) fake_plan(),
    render_pipeline = function(...) {
      calls$args <- list(...)
      data.frame(file = "x")
    },
    .package = "TempleCBE"
  )
  utils::capture.output(force(code))
  calls$args
}

test_that("a non-interactive session is refused, and nothing is rendered", {
  local_mocked_bindings(
    .session_is_interactive = function() FALSE,
    render_pipeline = function(...) stop("must not render"),
    .package = "TempleCBE"
  )
  expect_error(interactive_render_pipeline(fake_plan()), "requires an interactive R session")
})

test_that("choice 1 renders the stale documents", {
  args <- with_console(1L, code = {
    out <- suppressMessages(interactive_render_pipeline(fake_plan(), formats = "html"))
    expect_s3_class(out, "data.frame")
  })
  expect_true(args$only_stale)
  expect_identical(args$formats, "html")
  expect_null(args$from)
  expect_null(args$to)
  expect_identical(length(args$path), 3L)
})

test_that("choice 2 is a full rerun", {
  args <- with_console(2L, code = suppressMessages(interactive_render_pipeline(fake_plan())))
  expect_identical(args$rerun, "full")
  expect_null(args$only_stale)
})

test_that("choice 3 renders up to an item given by number or by name", {
  args <- with_console(3L, "2", code = suppressMessages(interactive_render_pipeline(fake_plan())))
  expect_identical(args$to, 2L)
  expect_false(args$only_stale)
  expect_null(args$from)

  args <- with_console(3L, "report", code = suppressMessages(interactive_render_pipeline(fake_plan())))
  expect_identical(args$to, 3L)
})

test_that("choice 4 renders a single item", {
  args <- with_console(4L, " model ", code = suppressMessages(interactive_render_pipeline(fake_plan())))
  expect_identical(args$from, 2L)
  expect_identical(args$to, 2L)
})

test_that("cancelling, or closing the menu, renders nothing", {
  for (choice in c(5L, 0L)) {
    args <- with_console(choice, code = {
      expect_message(out <- interactive_render_pipeline(fake_plan()), "nothing was rendered")
      expect_null(out)
    })
    expect_null(args)
  }
})

test_that("an item that is not in the list is an error and nothing is rendered", {
  for (answer in c("0", "4", "nope", "")) {
    args <- with_console(3L, answer, code = {
      expect_error(suppressMessages(interactive_render_pipeline(fake_plan())), "nothing was rendered")
    })
    expect_null(args)
  }
})

test_that("arguments the menu sets cannot be passed", {
  local_mocked_bindings(.session_is_interactive = function() TRUE, .package = "TempleCBE")
  expect_error(interactive_render_pipeline(fake_plan(), only_stale = FALSE, to = 1), "`only_stale`, `to`")
})

test_that("an empty pipeline says so", {
  local_mocked_bindings(
    .session_is_interactive = function() TRUE,
    get_render_plan = function(...) list(),
    .package = "TempleCBE"
  )
  expect_message(out <- interactive_render_pipeline(list()), "no document to render")
  expect_null(out)
})
