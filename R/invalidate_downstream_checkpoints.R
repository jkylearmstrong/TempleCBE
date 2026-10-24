#' Delete the Checkpoints Downstream of a Changed Node
#'
#' When one document or data file of a pipeline changes, only what depends on it needs to be
#' recomputed. This finds every node reachable from `modified_node` along the dependency edges of
#' the pipeline (`graph`, the list of [FilePath], [FileUses] and [FileOutputs] objects), and deletes
#' the checkpoint files in `checkpoint_dir` that belong to those nodes. Files the modified node produces
#' are downstream and go too; checkpoints named after the modified node itself are kept unless
#' `include_self = TRUE`; checkpoints of unrelated nodes are never touched.
#'
#' A checkpoint belongs to a node when its path (relative to `checkpoint_dir`, without extension)
#' starts with the node's `name` or with the file name stem of its `path`, either as a file
#' (`<stem>.rds`, `<stem>_<key>.rds`) or inside a folder of that name (`<stem>/...`); the match is on whole
#' words, so `fit` does not match `fit2.rds`. Files a node declares as outputs that lie inside
#' `checkpoint_dir` belong to it as well.
#'
#' @param graph A list of pipeline objects ([FilePath], [FileUses], [FileOutputs]).
#' @param modified_node Name or path of the node that changed.
#' @param checkpoint_dir Folder with the checkpoint files.
#' @param include_self Also remove the modified node's own checkpoints (default `FALSE`).
#' @param pattern Regular expression for files that count as checkpoints (default `"\\.rds$"`).
#' @param dry_run List what would be removed without removing it.
#' @return A tibble with `node` and `file` (relative to `checkpoint_dir`) and `removed`, invisibly.
#' @seealso [checkpoint_map()], [clean_checkpoints()], [get_render_plan()]
#' @export
#' @examples
#' raw <- FilePath("raw", "data/raw.csv")
#' clean <- FileOutputs("clean", "01_clean.qmd", dependencies = list(raw),
#'                      output = list(FilePath("clean_rds", "clean.rds")))
#' model <- FileOutputs("model", "02_model.qmd", dependencies = list(FilePath("clean_rds", "clean.rds")),
#'                      output = list(FilePath("model_rds", "model.rds")))
#' ck <- file.path(tempdir(), "ck_demo"); dir.create(ck, showWarnings = FALSE)
#' file.create(file.path(ck, c("clean.rds", "model.rds", "other.rds")))
#' invalidate_downstream_checkpoints(list(raw, clean, model), "clean", ck, dry_run = TRUE)
invalidate_downstream_checkpoints <- function(graph,
                                              modified_node,
                                              checkpoint_dir = "checkpoints/",
                                              include_self = FALSE,
                                              pattern = "\\.rds$",
                                              dry_run = FALSE) {
  if (!is.list(graph) || length(graph) == 0L || !all(vapply(graph, function(x) is(x, "FilePath"), logical(1)))) {
    stop("`graph` must be a non-empty list of FilePath/FileUses/FileOutputs objects.", call. = FALSE)
  }
  if (!is.character(modified_node) || length(modified_node) != 1L || is.na(modified_node)) {
    stop("`modified_node` must be one node name or path.", call. = FALSE)
  }
  norm <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)

  # every distinct node, with the names it depends on
  nodes <- list()
  add <- function(x) if (is.null(nodes[[x@name]])) nodes[[x@name]] <<- x
  deps <- list()
  for (x in graph) {
    add(x)
    if (is(x, "FileUses")) {
      for (d in x@dependencies) add(d)
    }
  }
  # edge: dependency -> producing node, and producing node -> its outputs
  edges <- list()
  link <- function(from, to) edges[[from]] <<- unique(c(edges[[from]], to))
  for (x in graph) {
    if (is(x, "FileUses")) for (d in x@dependencies) link(d@name, x@name)
    if (is(x, "FileOutputs")) for (o in x@output) { add(o); link(x@name, o@name) }
  }

  start <- names(nodes)[vapply(nodes, function(n) n@name == modified_node || norm(n@path) == norm(modified_node), logical(1))]
  if (length(start) == 0L) stop("`modified_node` is not a node of `graph`: ", modified_node, ".", call. = FALSE)

  seen <- character()
  queue <- start
  while (length(queue)) {
    cur <- queue[1L]
    queue <- queue[-1L]
    for (nx in setdiff(edges[[cur]], c(seen, start))) {
      seen <- c(seen, nx)
      queue <- c(queue, nx)
    }
  }
  affected <- if (isTRUE(include_self)) union(start, seen) else seen

  empty <- tibble::tibble(node = character(), file = character(), removed = logical())
  if (!dir.exists(checkpoint_dir)) return(invisible(empty))
  files <- list.files(checkpoint_dir, pattern = pattern, recursive = TRUE, ignore.case = TRUE)
  if (length(files) == 0L || length(affected) == 0L) return(invisible(empty))

  esc <- function(s) gsub("([.|()\\^{}+$*?\\[\\]\\\\])", "\\\\\\1", s)
  hits <- list()
  for (nm in affected) {
    n <- nodes[[nm]]
    stems <- unique(c(n@name, tools::file_path_sans_ext(basename(n@path))))
    rx <- paste0("^(", paste(esc(stems), collapse = "|"), ")([^A-Za-z0-9]|$)")
    stem_hit <- grepl(rx, files) | grepl(paste0("^(", paste(esc(stems), collapse = "|"), ")/"), files)
    out_hit <- rep(FALSE, length(files))
    if (is(n, "FileOutputs")) {
      for (o in n@output) out_hit <- out_hit | (norm(file.path(checkpoint_dir, files)) == norm(o@path))
    }
    out_hit <- out_hit | (norm(file.path(checkpoint_dir, files)) == norm(n@path))
    sel <- files[stem_hit | out_hit]
    if (length(sel)) hits[[nm]] <- tibble::tibble(node = nm, file = sel)
  }
  res <- dplyr::distinct(dplyr::bind_rows(c(list(empty[c("node", "file")]), hits)), .data$file, .keep_all = TRUE)
  res$removed <- FALSE
  if (!dry_run && nrow(res)) {
    res$removed <- file.remove(file.path(checkpoint_dir, res$file))
  }
  invisible(res)
}
