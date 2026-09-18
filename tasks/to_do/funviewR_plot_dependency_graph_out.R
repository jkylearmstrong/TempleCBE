setup_dev <- function() {
  # renv::use() rebuilds every dependency (and reinstalls TempleCBE from
  # GitHub) into a fresh sandbox on every run -- 2-3 minutes each time.
  # p_load_gh() alone just checks the regular user library and attaches
  # what's already there (~4s once packages are installed once).
  pacman::p_load_gh(c("funviewR", "pacman", "jkylearmstrong/TempleCBE"))
}

.tic <- Sys.time()
setup_dev()
cat(sprintf("[timing] setup_dev(): %.1fs\n", as.numeric(Sys.time() - .tic, units = "secs")))

library('funviewR')

# plot_dependency_graph() itself is the expensive step (parses every .R file
# under R/, ~56s for this package) -- cache it so repeated runs of everything
# downstream (excel export, round-trip checks) don't re-pay that cost. Delete
# the .rds to force a refresh after changing files under R/.
cache_path <- here::here('tasks', 'to_do', 'funviewR_plot_dependency_graph.rds')

.tic <- Sys.time()
if (file.exists(cache_path)) {
  cached <- readRDS(cache_path)
  funviewR_plot_dependency_graph <- cached$graph
  dep_info <- cached$dep_info
  cat("[timing] loaded from cache\n")
} else {
  funviewR_plot_dependency_graph <- funviewR::plot_dependency_graph(here::here('R'))

  # plot_dependency_graph() discards `duplicates` and flattens each function's
  # args/returns/source file/doc line into one HTML tooltip string per node --
  # neither survives into $x$nodes/$x$edges. Recompute the underlying analysis
  # directly to recover them as their own columns/sheet.
  dep_info <- funviewR::analyze_internal_dependencies_multi(
    funviewR::get_r_files(here::here('R'))
  )

  saveRDS(
    list(graph = funviewR_plot_dependency_graph, dep_info = dep_info),
    cache_path
  )
}
cat(sprintf("[timing] graph+dep_info ready: %.1fs\n", as.numeric(Sys.time() - .tic, units = "secs")))

.tic <- Sys.time()

# One pass over all 22.5k code lines to find each function's definition line,
# instead of a fresh grep() over the whole vector per function (441 x O(lines)
# -> O(lines) once). That per-function grep() was the actual bottleneck here
# (~75s for 441 functions), not the args/returns/body parsing beside it.
def_line_matches <- regmatches(
  dep_info$all_code_lines,
  regexec("^([[:alnum:]._]+)\\s*(?:<-|=)\\s*function", dep_info$all_code_lines, perl = TRUE)
)
def_names_by_line <- vapply(
  def_line_matches,
  function(m) if (length(m) >= 2) m[2] else NA_character_,
  character(1)
)
# Named-vector single-name indexing (`lookup[fname]`) returns the FIRST
# matching element, matching the original `grep(...)[1]` (first occurrence).
def_line_lookup <- stats::setNames(seq_along(def_names_by_line), def_names_by_line)
def_line_lookup <- def_line_lookup[!is.na(names(def_line_lookup))]

functions_tbl <- purrr::map_dfr(names(dep_info$function_file_map), function(fname) {
  src_file <- dep_info$function_file_map[[fname]]
  args_str <- NA_character_
  returns_str <- NA_character_
  description <- NA_character_

  if (exists(fname, envir = dep_info$env)) {
    fn <- get(fname, envir = dep_info$env)
    args_str <- tryCatch(
      gsub("^function", "", deparse(args(fn))[1]),
      error = function(e) NA_character_
    )

    body_exprs <- as.list(body(fn))
    return_call <- Filter(
      function(x) is.call(x) && identical(x[[1]], as.name("return")),
      body_exprs
    )
    returns_str <- if (length(return_call) > 0) {
      # deparse() can return a multi-line character vector for a long
      # expression; collapsing to one line keeps each function to exactly
      # one row (see the multi-row bug this caused, noted above).
      paste(deparse(return_call[[1]][[2]]), collapse = " ")
    } else if (length(body_exprs) > 0) {
      paste(deparse(utils::tail(body_exprs, 1)[[1]]), collapse = " ")
    } else {
      NA_character_
    }

    def_line <- unname(def_line_lookup[fname])
    doc_line <- if (!is.na(def_line)) def_line - 1 else NA
    description <- if (
      !is.na(doc_line) && doc_line > 0 &&
        grepl("^\\s*#'", dep_info$all_code_lines[doc_line])
    ) {
      sub("^\\s*#'\\s*", "", dep_info$all_code_lines[doc_line])
    } else {
      NA_character_
    }
  }

  tibble::tibble(
    id = fname,
    source_file = src_file,
    args = args_str,
    returns = returns_str,
    description = description
  )
})
cat(sprintf("[timing] functions_tbl built: %.1fs\n", as.numeric(Sys.time() - .tic, units = "secs")))

duplicates_tbl <- if (length(dep_info$duplicates) > 0) {
  purrr::map_dfr(names(dep_info$duplicates), function(fname) {
    tibble::tibble(
      id = fname,
      source_files = paste(dep_info$duplicates[[fname]], collapse = "; ")
    )
  })
} else {
  tibble::tibble(id = character(), source_files = character())
}

xlsx_path <- here::here('tasks', 'to_do', 'funviewR_plot_dependency_graph.xlsx')

write_workbook(
  list(
    nodes = funviewR_plot_dependency_graph$x$nodes,
    edges = funviewR_plot_dependency_graph$x$edges,
    functions = functions_tbl,
    duplicates = duplicates_tbl
  ),
  path = xlsx_path
)

# Round-trip check: excel -> data_frame.list -> graph_object. `nodes`'s `id`
# column matches `edges`'s `from`/`to`, which is exactly what
# igraph::graph_from_data_frame() wants for its `vertices`/`d` arguments.
db_from_excel <- read_database_from_excel(xlsx_path)

# Excel has no native Inf: distance = Inf (unreachable nodes) round-trips as
# the literal text "Inf", which drags the whole column to character. R's
# as.numeric() parses "Inf" back to Inf correctly, so this is lossless.
db_from_excel$nodes$distance <- as.numeric(db_from_excel$nodes$distance)

g_from_excel <- igraph::graph_from_data_frame(
  d = db_from_excel$edges,
  vertices = db_from_excel$nodes,
  directed = TRUE
)

tg_from_excel <- tidygraph::as_tbl_graph(g_from_excel)

cat(sprintf(
  "Round-trip graph: %d nodes, %d edges (original had %d nodes, %d edges)\n",
  igraph::vcount(g_from_excel),
  igraph::ecount(g_from_excel),
  nrow(funviewR_plot_dependency_graph$x$nodes),
  nrow(funviewR_plot_dependency_graph$x$edges)
))

# Attribute fidelity check: does every original nodes/edges column survive
# the excel round-trip with the same values (after Excel's own type coercion)?
check_attr_fidelity <- function(original_df, round_tripped_df, label) {
  original_df <- original_df[order(original_df[[1]]), , drop = FALSE]
  round_tripped_df <- round_tripped_df[order(round_tripped_df[[1]]), , drop = FALSE]

  for (col in names(original_df)) {
    orig_vals <- original_df[[col]]
    rt_vals <- round_tripped_df[[col]]

    if (is.null(rt_vals)) {
      cat(sprintf("[%s] MISSING after round-trip: %s\n", label, col))
      next
    }

    class_changed <- !identical(class(orig_vals), class(rt_vals))

    # NA-safe comparison after coercing both to character (side-steps
    # class differences that don't actually change the represented value).
    same <- ifelse(
      is.na(orig_vals) & is.na(rt_vals), TRUE,
      ifelse(
        is.na(orig_vals) | is.na(rt_vals), FALSE,
        as.character(orig_vals) == as.character(rt_vals)
      )
    )
    mismatches <- sum(!same)

    if (class_changed || mismatches > 0) {
      cat(sprintf(
        "[%s] %-14s class %s -> %s | value mismatches: %d / %d\n",
        label, col,
        paste(class(orig_vals), collapse = "/"),
        paste(class(rt_vals), collapse = "/"),
        mismatches, length(orig_vals)
      ))
    } else {
      cat(sprintf("[%s] %-14s OK (class %s)\n", label, col, paste(class(orig_vals), collapse = "/")))
    }
  }
}

cat("\n-- Node attribute fidelity --\n")
check_attr_fidelity(funviewR_plot_dependency_graph$x$nodes, db_from_excel$nodes, "nodes")

cat("\n-- Edge attribute fidelity --\n")
check_attr_fidelity(funviewR_plot_dependency_graph$x$edges, db_from_excel$edges, "edges")