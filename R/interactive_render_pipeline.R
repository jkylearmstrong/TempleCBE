#' Choose Interactively What `render_pipeline()` Renders
#'
#' A menu in front of [render_pipeline()] for an interactive R session: it lists the
#' documents of the pipeline in render order ([get_render_plan()]) and asks whether to
#' render the stale ones, everything again, everything up to one document, or one document
#' only. For a script or any non-interactive session use [render_pipeline()] itself.
#'
#' The choices:
#' 1. **Render all stale documents**: `only_stale = TRUE`.
#' 2. **Render all (force rerun)**: `rerun = "full"`, i.e. `only_stale = FALSE` and
#'    `cache = "refresh"`.
#' 3. **Render up to an item**: asks for a number or a name from the list, then `to = item`.
#' 4. **Render a single item**: asks for a number or a name, then `from = item, to = item`.
#' 5. **Cancel**: nothing is rendered.
#'
#' Closing the menu without a choice cancels too. A number or name that is not in the list
#' is an error; nothing is rendered.
#'
#' @param all_file_objects The list of `FilePath`, `FileUses` and `FileOutputs` objects
#'   that [render_pipeline()] takes as `path`.
#' @param ... Further arguments for [render_pipeline()] (`formats`, `workers`, `on_error`,
#'   ...). `path`, `only_stale`, `rerun`, `from` and `to` are set by the menu and may not be
#'   given here.
#' @return The data frame that [render_pipeline()] returns, invisibly; `NULL` if cancelled.
#' @seealso [render_pipeline()], [get_render_plan()]
#' @export
#' @examples
#' \dontrun{
#' interactive_render_pipeline(all_file_objects, formats = "html", workers = 4)
#' }
interactive_render_pipeline <- function(all_file_objects, ...) {
  if (!.session_is_interactive()) {
    stop(
      "`interactive_render_pipeline()` requires an interactive R session. ",
      "Use `render_pipeline()` for non-interactive execution.",
      call. = FALSE
    )
  }
  dots <- list(...)
  reserved <- intersect(names(dots), c("path", "only_stale", "rerun", "from", "to"))
  if (length(reserved)) {
    stop("The menu sets ", paste0("`", reserved, "`", collapse = ", "),
         "; do not pass ", if (length(reserved) > 1L) "them" else "it", ".", call. = FALSE)
  }

  plan <- get_render_plan(all_file_objects, only_stale = FALSE)
  if (!length(plan)) {
    message("The pipeline has no document to render.")
    return(invisible(NULL))
  }
  items <- vapply(plan, function(x) x@name, character(1))
  cat("Render plan, in render order:\n")
  cat(sprintf("  %2d. %s\n", seq_along(items), items), sep = "")

  choices <- c(
    "Render all stale documents",
    "Render all (force rerun)",
    "Render up to an item",
    "Render a single item",
    "Cancel"
  )
  pick <- .menu_choice(choices, title = "What should be rendered?")
  if (pick == 0L || pick == 5L) {
    message("Cancelled; nothing was rendered.")
    return(invisible(NULL))
  }

  args <- switch(
    as.character(pick),
    "1" = list(only_stale = TRUE),
    "2" = list(rerun = "full"),
    "3" = list(only_stale = FALSE, to = .ask_item(items, "Render up to which item (number or name)? ")),
    "4" = {
      item <- .ask_item(items, "Render which item (number or name)? ")
      list(only_stale = FALSE, from = item, to = item)
    }
  )
  invisible(do.call("render_pipeline", c(list(path = all_file_objects), args, dots)))
}

# thin wrappers so that tests can stand in for the console
.session_is_interactive <- function() interactive()

.menu_choice <- function(choices, title) utils::menu(choices, title = title)

.read_answer <- function(prompt) readline(prompt)

# the position of an answer that is a number in 1:length(items) or one of the names in `items`
.ask_item <- function(items, prompt) {
  answer <- trimws(.read_answer(prompt))
  if (!nzchar(answer)) {
    stop("No item given; nothing was rendered.", call. = FALSE)
  }
  if (grepl("^[0-9]+$", answer)) {
    i <- as.integer(answer)
    if (i < 1L || i > length(items)) {
      stop("No item number ", i, " (1 to ", length(items), "); nothing was rendered.", call. = FALSE)
    }
    return(i)
  }
  i <- match(answer, items)
  if (is.na(i)) {
    stop("No item named '", answer, "'; nothing was rendered.", call. = FALSE)
  }
  i
}
