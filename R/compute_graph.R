#' Computational Pipeline Dependency Graphs
#'
#' S4 classes and helper functions for tracking file-level dependencies in an
#' analysis pipeline (raw data, rendering scripts, derived artifacts) and for
#' visualizing, summarizing, and topologically sorting the resulting
#' dependency graph. Language-, format-, and project-agnostic: a node is any
#' file on disk (R, Python, SAS, Quarto, spreadsheets, ...), identified by
#' path rather than tied to a specific tool.
#'
#' The graph engine (\pkg{igraph}/\pkg{tidygraph}) is a hard dependency.
#' Three rendering backends are optional (Suggests) and only required by the
#' functions that use them: static PNG export (\code{\link{print_pipeline}})
#' needs \pkg{ggraph}; the interactive Graphviz view
#' (\code{\link{visualize_pipeline}}) needs \pkg{DiagrammeR}; and the
#' interactive HTML export (\code{\link{visualize_pipeline_interactive}},
#' \code{\link{export_interactive_pipeline}}, \code{\link{export_subgraph}})
#' needs \pkg{visNetwork} and \pkg{htmlwidgets}.
#'
#' @importFrom methods setClass setGeneric setMethod new callNextMethod is .hasSlot
#' @name compute_graph
NULL


# Pipeline Global Configuration -----------------

#' Get or set global pipeline configuration options
#'
#' Decouples project-specific parameters (study name, stage display labels,
#' stage color palettes) from core pipeline mechanics and classes.
#'
#' @param study_name Character string for the main study title.
#' @param stage_labels Named character vector mapping raw stage identifiers to human-readable labels.
#' @param stage_colors Named list or character vector mapping stage identifiers to hex colors.
#' @return A list containing the current configuration options.
#' @export
pipeline_config <- function(
  study_name = NULL,
  stage_labels = NULL,
  stage_colors = NULL
) {
  if (!is.null(study_name)) {
    options(pipeline.study_name = study_name)
  }
  if (!is.null(stage_labels)) {
    options(pipeline.stage_labels = stage_labels)
  }
  if (!is.null(stage_colors)) {
    options(pipeline.stage_colors = stage_colors)
  }

  list(
    study_name = getOption("pipeline.study_name", "Computational Pipeline"),
    stage_labels = getOption("pipeline.stage_labels", list()),
    stage_colors = getOption("pipeline.stage_colors", list())
  )
}


# Classes -----------------

# FilePath -----------------
## file paths are unique identifiers for files and are used to track changes in the file system
### a file path is a general r, python, sas, quarto, etc agnostic way to refer to a file
### it is not tied to any specific software, but rather to the file system itself
### `name` is a user-supplied human readable name for the file, used to refer to the file in other parts of the pipeline
### `path` is the absolute path to the file on the file system
### `mtime` is the modification time of the file on the file system
### `file_ext` is the file extension of the file
### `renders` is a logical indicating whether the file is a rendered file (i.e. a file that is produced by a rendering process)
### `stage` is the stage of the file in the pipeline
### `artifact_role` is the role of the file in the pipeline
### `description` is a brief description of the file

#' FilePath S4 Class
#'
#' @description
#' File paths are unique identifiers for files and are used to track changes in the file system.
#' A file path is a general R, Python, SAS, Quarto, shell, etc. agnostic way to refer to a file.
#' It is not tied to any specific software or programming language, but rather to the file system itself.
#'
#' @details
#' Represents an individual file node in a computational pipeline dependency graph.
#' A \code{FilePath} object encapsulates file-level metadata and state: tracking modification
#' timestamps (\code{mtime}), file extensions (\code{file_ext}), workflow lifecycle milestones
#' (\code{stage}), semantic pipeline roles (\code{artifact_role}), and rendering status (\code{renders}).
#'
#' Upon instantiation, the object automatically inspects the target file on disk: if the file
#' exists at \code{path}, \code{mtime} is populated from \code{file.info()$mtime} and \code{file_ext}
#' is extracted via \code{tools::file_ext()}.
#'
#' @slot name Character. A user-supplied, human-readable name for the file, used to refer to the
#'   file across other parts of the pipeline and dependency graph.
#' @slot path Character. The absolute or project-relative path to the file on the file system.
#' @slot mtime POSIXct. The modification time of the file on the file system. Automatically
#'   populated from disk during object initialization if the file exists.
#' @slot file_ext Character. The file extension of the file (e.g., \code{"xlsx"}, \code{"rds"},
#'   \code{"qmd"}). Automatically populated from the file path during object initialization.
#' @slot renders Logical. A logical indicating whether the file is a rendered file (i.e. a file
#'   that is produced by a rendering process) or a rendering source script. Defaults to \code{FALSE}.
#' @slot stage Character. The workflow lifecycle stage of the file in the pipeline (e.g.,
#'   \code{"01_EDA"}, \code{"02_Analysis_Original"}, \code{"03_Imputation"}).
#' @slot artifact_role Character. The semantic role of the file in the pipeline. Standard roles include:
#'   \itemize{
#'     \item \code{"raw_data"}: Original immutable input data (e.g. source spreadsheets).
#'     \item \code{"derived_data"}: Primary processed baseline dataset generated from raw inputs.
#'     \item \code{"enhanced_data"}: Imputed or augmented analytical dataset.
#'     \item \code{"intermediate_data"}: Stage-internal calculation artifact or model.
#'     \item \code{"report_source"}: Executable source document (e.g., Quarto \code{.qmd}).
#'     \item \code{"deliverable_report"}: Rendered deliverable (e.g., \code{.pdf}, \code{.html}).
#'     \item \code{"helper_script"}: Child or modular helper script invoked by parent reports.
#'     \item \code{"reference"}: Lookup tables, crosswalks, or project configuration files.
#'   }
#' @slot description Character. A brief human-readable description of the file's contents or analytical purpose.
#'
#' @seealso \code{\linkS4class{FileUses}}, \code{\linkS4class{FileOutputs}}, \code{\link{FilePath}}, \code{\link{create_qmd_renderer}}
#'
#' @name FilePath-class
#' @rdname FilePath-class
#' @exportClass FilePath
#' @export
setClass(
  "FilePath",
  slots = c(
    name = "character",
    path = "character",
    mtime = "POSIXct",
    file_ext = "character",
    renders = "logical",
    stage = "character",
    artifact_role = "character",
    description = "character"
  ),
  prototype = list(
    name = NA_character_,
    path = NA_character_,
    mtime = as.POSIXct(NA),
    file_ext = NA_character_,
    renders = FALSE,
    stage = NA_character_,
    artifact_role = NA_character_,
    description = NA_character_
  )
)

#' Initialize a FilePath object
#'
#' Automatically inspects the file path on disk to populate modification time (\code{mtime})
#' and file extension (\code{file_ext}) if the file exists.
#'
#' @param .Object The FilePath object being initialized.
#' @param ... Additional slot arguments passed to \code{\link[methods]{callNextMethod}}.
#' @return An initialized \code{FilePath} object.
#' @export
setMethod(
  "initialize",
  "FilePath",
  function(.Object, ...) {
    .Object <- callNextMethod()
    if (!is.na(.Object@path) && file.exists(.Object@path)) {
      .Object@mtime <- file.info(.Object@path)$mtime
      .Object@file_ext <- tools::file_ext(.Object@path)
    } else {
      .Object@mtime <- as.POSIXct(NA)
      .Object@file_ext <- NA_character_
    }
    return(.Object)
  }
)

#' Construct a FilePath object
#'
#' User-friendly constructor function for instantiating a \code{\linkS4class{FilePath}} object.
#'
#' @param name Character. User-supplied human-readable name for the file.
#' @param path Character. Absolute or relative file system path.
#' @param renders Logical. Indicates whether the file is a rendered file or rendering script. Defaults to \code{FALSE}.
#' @param stage Character. Pipeline lifecycle stage.
#' @param artifact_role Character. Pipeline role of the artifact.
#' @param description Character. Brief description of the file.
#' @return A new \code{\linkS4class{FilePath}} object.
#' @export
FilePath <- function(name,
                     path,
                     renders = FALSE,
                     stage = NA_character_,
                     artifact_role = NA_character_,
                     description = NA_character_) {
  new(
    "FilePath",
    name = name,
    path = path,
    renders = renders,
    stage = stage,
    artifact_role = artifact_role,
    description = description
  )
}


# FileUses -----------------
## file uses are files that depend on other files
### `dependencies` is a list of FilePath objects that this file depends on

#' FileUses S4 Class
#'
#' @description
#' Represents a pipeline file that depends on other upstream files.
#' Extends \code{\linkS4class{FilePath}} by adding upstream input dependency tracking.
#'
#' @details
#' A \code{FileUses} node represents a computational artifact whose execution or validity
#' relies upon one or more input \code{\linkS4class{FilePath}} objects listed in \code{dependencies}.
#'
#' @slot dependencies List of \code{\linkS4class{FilePath}} objects that this file depends on.
#'
#' @seealso \code{\linkS4class{FilePath}}, \code{\linkS4class{FileOutputs}}, \code{\link{FileUses}}
#'
#' @name FileUses-class
#' @rdname FileUses-class
#' @exportClass FileUses
#' @export
setClass(
  "FileUses",
  contains = "FilePath",
  slots = c(
    dependencies = "list" # list of FilePath objects
  ),
  prototype = list(
    dependencies = list()
  )
)

#' Construct a FileUses object
#'
#' User-friendly constructor function for instantiating a \code{\linkS4class{FileUses}} object.
#'
#' @param name Character. User-supplied human-readable name for the file.
#' @param path Character. Absolute or relative file system path.
#' @param dependencies List of \code{\linkS4class{FilePath}} objects that this file depends on.
#' @param renders Logical. Indicates whether the file is a rendered file or rendering script. Defaults to \code{FALSE}.
#' @param stage Character. Pipeline lifecycle stage.
#' @param artifact_role Character. Pipeline role of the artifact.
#' @param description Character. Brief description of the file.
#' @return A new \code{\linkS4class{FileUses}} object.
#' @export
FileUses <- function(name,
                     path,
                     dependencies = list(),
                     renders = FALSE,
                     stage = NA_character_,
                     artifact_role = NA_character_,
                     description = NA_character_) {
  new(
    "FileUses",
    name = name,
    path = path,
    dependencies = dependencies,
    renders = renders,
    stage = stage,
    artifact_role = artifact_role,
    description = description
  )
}


# FileOutputs -----------------
## file outputs are files that produce other files
### `output` is a list of FilePath objects that this file produces

#' FileOutputs S4 Class
#'
#' @description
#' Represents an executable or rendering pipeline file that produces downstream output artifacts.
#'
#' @details
#' Inherits from \code{\linkS4class{FileUses}} (inheriting upstream \code{dependencies}),
#' while adding a list of downstream output \code{\linkS4class{FilePath}} objects produced when
#' this file executes or renders.
#'
#' @slot output List of \code{\linkS4class{FilePath}} objects produced by this file.
#'
#' @seealso \code{\linkS4class{FilePath}}, \code{\linkS4class{FileUses}}, \code{\link{FileOutputs}}, \code{\link{create_qmd_renderer}}
#'
#' @name FileOutputs-class
#' @rdname FileOutputs-class
#' @exportClass FileOutputs
#' @export
setClass(
  "FileOutputs",
  contains = "FileUses",
  slots = c(
    output = "list" # list of FilePath objects
  ),
  prototype = list(
    output = list()
  )
)

#' Construct a FileOutputs object
#'
#' User-friendly constructor function for instantiating a \code{\linkS4class{FileOutputs}} object.
#'
#' @param name Character. User-supplied human-readable name for the file.
#' @param path Character. Absolute or relative file system path.
#' @param dependencies List of \code{\linkS4class{FilePath}} objects that this file depends on.
#' @param output List of \code{\linkS4class{FilePath}} objects produced by this file.
#' @param renders Logical. Indicates whether the file is a rendering script. Defaults to \code{TRUE}.
#' @param stage Character. Pipeline lifecycle stage.
#' @param artifact_role Character. Pipeline role of the artifact. Defaults to \code{"report_source"}.
#' @param description Character. Brief description of the file.
#' @return A new \code{\linkS4class{FileOutputs}} object.
#' @export
FileOutputs <- function(name,
                        path,
                        dependencies = list(),
                        output = list(),
                        renders = TRUE,
                        stage = NA_character_,
                        artifact_role = "report_source",
                        description = NA_character_) {
  new(
    "FileOutputs",
    name = name,
    path = path,
    dependencies = dependencies,
    output = output,
    renders = renders,
    stage = stage,
    artifact_role = artifact_role,
    description = description
  )
}


# Helper & Visualization Functions -----------------

#' Create a FileOutputs object for a rendering QMD
#'
#' @param name The nickname for the file.
#' @param path The full path to the .qmd file.
#' @param deps A list of dependency objects (e.g., list(og_DATA)).
#' @param file_stage The 'stage' for this file.
#' @param output_format "pdf" or "html". Default is "pdf".
#' @param description A brief summary of the document.
#' @return A FileOutputs object.
#' @export
create_qmd_renderer <- function(
  name,
  path,
  deps = list(),
  file_stage = NA_character_,
  output_format = "pdf",
  description = NA_character_
) {
  # 1. Define the output file path
  output_ext <- paste0(".", output_format)
  output_name_suffix <- paste0(" ", toupper(output_format))
  output_path <- stringr::str_replace(path, "\\.qmd$", output_ext)

  # 2. Create the FilePath object for the output
  output_file <- new(
    "FilePath",
    name = paste0(name, output_name_suffix),
    path = output_path,
    renders = FALSE,
    stage = file_stage,
    artifact_role = "deliverable_report"
  )

  # 3. Create the main FileOutputs object
  new(
    "FileOutputs",
    name = name,
    path = path,
    dependencies = deps,
    renders = TRUE,
    stage = file_stage,
    artifact_role = "report_source",
    description = description,
    output = list(output_file)
  )
}

#' Find the script object that produces a given file
#' @noRd
.find_producer <- function(file_to_check, all_objects) {
  producers <- Filter(function(x) is(x, "FileOutputs"), all_objects)
  for (p in producers) {
    output_names <- vapply(p@output, function(o) o@name, character(1))
    if (file_to_check@name %in% output_names) {
      return(p)
    }
  }
  return(NULL)
}

#' Check if an output file is stale
#' @noRd
.is_stale <- function(output_file, producer_script) {
  out_mtime <- output_file@mtime
  if (is.na(out_mtime)) {
    return(TRUE)
  } # Stale if missing

  source_files <- c(list(producer_script), producer_script@dependencies)
  source_mtimes <- unlist(lapply(source_files, function(f) f@mtime))
  source_mtimes <- source_mtimes[!is.na(source_mtimes)]

  if (length(source_mtimes) == 0) {
    return(FALSE)
  }

  max_source_mtime <- max(source_mtimes)
  return(out_mtime < max_source_mtime) # Stale if output is older
}

#' Visualize FilePath dependencies with stage-based coloring and staleness check
#'
#' @param all_objects The full list of FilePath, FileUses, and FileOutputs objects.
#' @param extract_graph_code Logical. If TRUE, the graph source code will be extracted and returned as a character string.
#' @param stage_colors Optional named list of stage colors. If NULL, uses pipeline_config() options.
#' @export
visualize_pipeline <- function(
  all_objects,
  extract_graph_code = FALSE,
  stage_colors = NULL
) {
  if (!extract_graph_code && !requireNamespace("DiagrammeR", quietly = TRUE)) {
    stop("Package \"DiagrammeR\" is required to render the graph. Install it, or call with `extract_graph_code = TRUE` to get the DOT source instead.", call. = FALSE)
  }
  if (is.null(stage_colors)) {
    stage_colors <- getOption("pipeline.stage_colors", list())
  }
  default_color <- "gray"
  stale_color <- "red"

  all_nodes <- list()
  for (file in all_objects) {
    all_nodes[[file@name]] <- file
    if (is(file, "FileUses")) {
      for (dep in file@dependencies) {
        all_nodes[[dep@name]] <- dep
      }
    }
    if (is(file, "FileOutputs")) {
      for (out in file@output) {
        all_nodes[[out@name]] <- out
      }
    }
  }

  graph_code <- "digraph dependencies {\n  node [shape = box, style = filled];\n"

  for (node in all_nodes) {
    node_name <- node@name
    stage <- node@stage[1]

    color <- ifelse(
      !is.na(stage) && stage %in% names(stage_colors),
      stage_colors[[stage]],
      default_color
    )

    is_stale <- FALSE
    producer <- .find_producer(node, all_objects)
    if (!is.null(producer)) {
      is_stale <- .is_stale(node, producer)
    }

    if (is_stale) {
      node_attrs <- sprintf(
        "[fillcolor = \"%s\", color = \"black\", peripheries = 2, label = \"%s (STALE)\"]",
        stale_color,
        node_name
      )
    } else {
      node_attrs <- sprintf(
        "[fillcolor = \"%s\", label = \"%s\"]",
        color,
        node_name
      )
    }

    graph_code <- paste0(
      graph_code,
      "  \"",
      node_name,
      "\" ",
      node_attrs,
      ";\n"
    )
  }

  for (file in all_objects) {
    file_name <- file@name
    if (is(file, "FileUses")) {
      for (dep in file@dependencies) {
        graph_code <- paste0(
          graph_code,
          "  \"",
          dep@name,
          "\" -> \"",
          file_name,
          "\";\n"
        )
      }
    }
    if (is(file, "FileOutputs")) {
      for (out in file@output) {
        graph_code <- paste0(
          graph_code,
          "  \"",
          file_name,
          "\" -> \"",
          out@name,
          "\";\n"
        )
      }
    }
  }

  graph_code <- paste0(graph_code, "}")

  if (extract_graph_code) {
    return(graph_code)
  }

  DiagrammeR::grViz(graph_code)
}

#' Wrap a node label to a target character width for print_pipeline()
#'
#' Many node names here are underscore_separated identifiers (e.g.
#' "imputed_DATA_XLSX") rather than natural-language phrases, and base R's
#' strwrap() only breaks on whitespace -- it would leave such a name as one
#' long unbroken line, wider than the space print_pipeline() budgeted for it
#' and prone to overlapping its neighbors. Break after underscores too, then
#' strwrap() each resulting piece in case it still contains spaces.
#' @noRd
wrap_pipeline_label <- function(label, width) {
  segments <- strsplit(label, "(?<=_)", perl = TRUE)[[1]]
  lines <- character(0)
  current <- ""
  for (seg in segments) {
    candidate <- paste0(current, seg)
    if (nchar(candidate) > width && nzchar(current)) {
      lines <- c(lines, current)
      current <- seg
    } else {
      current <- candidate
    }
  }
  if (nzchar(current)) {
    lines <- c(lines, current)
  }
  lines <- unlist(lapply(lines, strwrap, width = width))
  paste(lines, collapse = "\n")
}

#' Print the dependency graph as a PNG image
#'
#' Renders via igraph/ggraph with a layered (Sugiyama) DAG layout and canvas
#' sizing adapted to the graph's own node density, rather than the previous
#' Graphviz/DiagrammeR renderer, which laid large graphs out extremely wide
#' and flat (e.g. the full compute graph rendered at 9019x1140px, with text
#' too small to read and the arrows between boxes impossible to follow).
#'
#' @param objects The full list of FilePath, FileUses, and FileOutputs
#'   objects, or an already-built igraph object (see as_igraph()).
#' @param path The path where the PNG image will be saved.
#' @param drop_stages Character vector of `stage` values to omit entirely
#'   (node and its edges). Use "output_report" on a crowded overview graph
#'   to hide the "<X> PDF"/"<X> DOCX" leaf nodes, which add little beyond
#'   what their parent QMD node already conveys.
#' @param drop_edges_to Character vector of node names whose *incoming*
#'   edges should be omitted, while keeping the node itself visible. Use
#'   this for a node like an introduction/summary report that every other
#'   report is wired to depend on purely to force it to render last, not
#'   because of a real analytical dependency -- dropping just the edges
#'   avoids a dense fan-in that conveys no real relationship.
#' @param title Optional title drawn on the plot itself. Leave NULL when the
#'   image is embedded with its own Quarto fig-cap, to avoid a duplicate-
#'   looking caption.
#' @export
print_pipeline <- function(
  objects,
  path,
  drop_stages = character(0),
  drop_edges_to = character(0),
  title = NULL,
  base_font_size = 9,
  label_wrap_width = 18,
  min_width_in = 8,
  min_height_in = 3,
  max_width_in = 40,
  width_per_node_in = 2.1,
  dpi = 200
) {
  if (!requireNamespace("ggraph", quietly = TRUE)) {
    stop("Package \"ggraph\" is required to render the graph as a PNG.", call. = FALSE)
  }
  g <- if (is(objects, "igraph")) objects else as_igraph(objects)

  if (length(drop_stages) > 0) {
    keep <- igraph::V(g)[!(igraph::V(g)$stage %in% drop_stages)]
    g <- igraph::induced_subgraph(g, keep)
  }

  if (length(drop_edges_to) > 0) {
    edge_ends <- igraph::ends(g, igraph::E(g), names = TRUE)
    eids_to_drop <- igraph::E(g)[edge_ends[, 2] %in% drop_edges_to]
    g <- igraph::delete_edges(g, eids_to_drop)
  }

  if (igraph::vcount(g) == 0) {
    warning("Empty graph after filtering, skipping: ", path)
    return(invisible(NULL))
  }

  layout <- ggraph::create_layout(g, layout = "sugiyama")

  # Sugiyama assigns each node an integer layer (layout$y, rank/depth from
  # sources) and a position within that layer (layout$x). Size the canvas
  # from those directly -- height from the number of layers, width from how
  # tightly packed the closest pair of nodes in any single layer actually is
  # -- so the image stays close to a readable rectangle instead of the
  # extreme wide-and-flat shape a raw coordinate bounding box can produce.
  #
  # Width used to scale off "nodes in the widest layer" alone, which assumes
  # Sugiyama spaces every layer's nodes evenly -- it doesn't. A layer can end
  # up with a couple of nodes compressed close together (to reduce edge
  # crossings elsewhere) even when the graph's widest layer has plenty of
  # nodes spread comfortably; sizing off node *count* alone then either
  # leaves that compressed pair overlapping, or (if bumped up to fix it)
  # wastefully inflates every other, already-well-spaced layer along with it.
  # Measure the actual tightest gap instead, and size width so that gap gets
  # enough inches, however many total nodes exist elsewhere.
  layer_counts <- table(round(layout$y))
  n_layers <- length(layer_counts)
  max_per_layer <- max(layer_counts)

  min_gap_units <- layout |>
    split(round(layout$y)) |>
    vapply(
      function(d) {
        xs <- sort(unique(d$x))
        if (length(xs) < 2) NA_real_ else min(diff(xs))
      },
      numeric(1)
    ) |>
    (\(v) if (all(is.na(v))) 1 else min(v, na.rm = TRUE))()
  if (!is.finite(min_gap_units) || min_gap_units <= 0) {
    min_gap_units <- 1
  }

  x_range_units <- diff(range(layout$x))
  if (!is.finite(x_range_units) || x_range_units <= 0) {
    x_range_units <- 1
  }

  layout$label_wrapped <- vapply(
    layout$label,
    wrap_pipeline_label,
    character(1),
    width = label_wrap_width
  )

  width_in <- min(
    max(min_width_in, (x_range_units / min_gap_units) * width_per_node_in),
    max_width_in
  )

  # A layer's height budget needs to fit however many lines its tallest
  # wrapped label takes, not just a flat per-layer constant -- otherwise a
  # long label (e.g. one that wraps to 6 lines) gets clipped at the bottom of
  # the image regardless of how many layers the graph has. Use the longest
  # wrapped label anywhere in the graph so every layer gets enough room.
  max_label_lines <- max(
    vapply(strsplit(layout$label_wrapped, "\n"), length, integer(1)),
    1
  )
  height_per_layer_in <- max(1.1, 0.4 + max_label_lines * 0.22)
  height_in <- max(min_height_in, n_layers * height_per_layer_in)

  # ggplot2's axis `expand` is a *fraction of the coordinate range*, but the
  # edge margin a boundary node's label box needs is a fixed *absolute* size
  # (roughly half its own height/width) -- so the same 8% that comfortably
  # clears a 60-node graph's edge leaves almost no room on a 3-layer graph,
  # where that 8% might be a fraction of an inch. Scale the fraction inversely
  # with layer/node count so the resulting absolute margin stays roughly
  # constant regardless of how big the graph is.
  y_expand_mult <- max(0.08, 1 / (2 * n_layers))
  x_expand_mult <- max(0.08, 1 / (2 * max_per_layer))

  p <- ggraph::ggraph(layout) +
    ggraph::geom_edge_diagonal(
      colour = "grey55",
      width = 0.4,
      alpha = 0.55,
      arrow = grid::arrow(length = grid::unit(2.2, "mm"), type = "closed"),
      end_cap = ggraph::rectangle(3.4, 1, "cm", "cm"),
      start_cap = ggraph::rectangle(3.4, 1, "cm", "cm")
    ) +
    ggraph::geom_node_label(
      ggplot2::aes(label = label_wrapped, fill = I(color)),
      colour = "grey10",
      size = base_font_size / ggplot2::.pt,
      label.padding = grid::unit(0.18, "lines")
    ) +
    ggraph::theme_graph(base_family = "sans") +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(mult = x_expand_mult)
    ) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = y_expand_mult)
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme(
      plot.margin = ggplot2::margin(10, 14, 10, 14),
      legend.position = "none"
    )

  if (!is.null(title)) {
    p <- p +
      ggplot2::labs(title = title) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(
          size = base_font_size + 4,
          face = "bold"
        )
      )
  }

  out_dir <- dirname(path)
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }

  ggplot2::ggsave(
    path,
    p,
    width = width_in,
    height = height_in,
    dpi = dpi,
    limitsize = FALSE,
    bg = "white"
  )
  message(
    "Saved: ",
    path,
    sprintf(
      " (%.1fin x %.1fin, %d layers, %d nodes)",
      width_in,
      height_in,
      n_layers,
      igraph::vcount(g)
    )
  )
  invisible(path)
}


#' Convert to an enriched igraph graph with rich metadata
#'
#' @param all_objects list of FilePath, FileUses, and FileOutputs objects.
#' @param stage_colors Optional named list of stage colors. If NULL, uses pipeline_config() options.
#' @return an igraph object containing complete node and edge attributes
#' @export
as_igraph <- function(all_objects, stage_colors = NULL) {
  # Collect all unique nodes
  all_nodes <- list()
  for (file in all_objects) {
    all_nodes[[file@name]] <- file
    if (is(file, "FileUses")) {
      for (dep in file@dependencies) {
        all_nodes[[dep@name]] <- dep
      }
    }
    if (is(file, "FileOutputs")) {
      for (out in file@output) {
        all_nodes[[out@name]] <- out
      }
    }
  }

  node_names <- names(all_nodes)
  n_nodes <- length(all_nodes)

  if (is.null(stage_colors)) {
    stage_colors <- getOption("pipeline.stage_colors", list())
  }
  default_color <- "#EAECEE"
  stale_color <- "#FADBD8"

  stages <- character(n_nodes)
  artifact_roles <- character(n_nodes)
  colors <- character(n_nodes)
  stale <- logical(n_nodes)
  paths <- character(n_nodes)
  descriptions <- character(n_nodes)
  renders <- logical(n_nodes)
  shapes <- character(n_nodes)
  mtimes <- character(n_nodes)
  titles <- character(n_nodes)

  i <- 1
  for (node in all_nodes) {
    node_name <- node@name
    stg <- if (length(node@stage) > 0 && !is.na(node@stage[1])) {
      node@stage[1]
    } else {
      "Other"
    }
    stages[i] <- stg

    role_val <- if (.hasSlot(node, "artifact_role") && length(node@artifact_role) > 0 && !is.na(node@artifact_role[1])) {
      node@artifact_role[1]
    } else if (.hasSlot(node, "stage") && !is.na(node@stage[1]) && node@stage[1] %in% c("raw_data", "derived_data", "enhanced_data", "output_report")) {
      node@stage[1]
    } else {
      "unspecified"
    }
    artifact_roles[i] <- role_val

    path_val <- if (length(node@path) > 0 && !is.na(node@path[1])) {
      node@path[1]
    } else {
      ""
    }
    paths[i] <- path_val

    desc_val <- if (
      length(node@description) > 0 && !is.na(node@description[1])
    ) {
      node@description[1]
    } else {
      ""
    }
    descriptions[i] <- desc_val

    rndr_val <- if (length(node@renders) > 0 && isTRUE(node@renders[1])) {
      TRUE
    } else {
      FALSE
    }
    renders[i] <- rndr_val

    mtime_val <- if (length(node@mtime) > 0 && !is.na(node@mtime[1])) {
      as.character(node@mtime[1])
    } else {
      ""
    }
    mtimes[i] <- mtime_val

    # Staleness check
    producer <- .find_producer(node, all_objects)
    is_stale_node <- !is.null(producer) && .is_stale(node, producer)
    stale[i] <- is_stale_node

    # Color
    if (is_stale_node) {
      colors[i] <- stale_color
    } else if (stg %in% names(stage_colors)) {
      colors[i] <- stage_colors[[stg]]
    } else {
      colors[i] <- default_color
    }

    # Shape: database for raw data, ellipse for datasets, box for reports/code
    ext <- tolower(tools::file_ext(path_val))
    if (role_val == "raw_data" || stg == "raw_data") {
      shapes[i] <- "database"
    } else if (
      role_val %in%
        c("derived_data", "enhanced_data", "intermediate_data") ||
        ext %in% c("rds", "xlsx", "csv", "rdata")
    ) {
      shapes[i] <- "ellipse"
    } else {
      shapes[i] <- "box"
    }

    # Rich HTML Tooltip Card
    status_html <- if (is_stale_node) {
      "<span style='color: #C0392B; font-weight: bold;'>STALE (Needs Re-render)</span>"
    } else {
      "<span style='color: #27AE60; font-weight: bold;'>Up-to-date</span>"
    }

    desc_html <- if (nzchar(desc_val)) {
      paste0(
        "<div style='background: #F8F9F9; border-left: 3px solid #2980B9; padding: 6px 8px; margin: 6px 0; color: #2C3E50; font-size: 12px;'>",
        desc_val,
        "</div>"
      )
    } else {
      ""
    }

    mtime_html <- if (nzchar(mtime_val)) {
      paste0(
        "<div style='margin-top: 3px;'><b>Modified:</b> <span style='color: #566573;'>",
        mtime_val,
        "</span></div>"
      )
    } else {
      ""
    }

    file_basename <- if (nzchar(path_val)) {
      basename(path_val)
    } else {
      "(virtual node)"
    }

    titles[i] <- paste0(
      "<div style='font-family: -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, Helvetica, Arial, sans-serif; font-size: 13px; max-width: 340px; padding: 4px;'>",
      "<div style='font-weight: 700; font-size: 14px; color: #1B2631; border-bottom: 2px solid #BDC3C7; padding-bottom: 4px; margin-bottom: 6px;'>",
      node_name,
      "</div>",
      desc_html,
      "<div style='display: grid; gap: 2px; font-size: 12px;'>",
      "<div><b>Stage:</b> <span style='background: #EAECEE; padding: 1px 5px; border-radius: 3px;'>",
      stg,
      "</span></div>",
      "<div><b>Role:</b> <span style='background: #E8F8F5; padding: 1px 5px; border-radius: 3px; color: #117A65;'>",
      role_val,
      "</span></div>",
      "<div><b>File:</b> <code style='color: #884EA0;'>",
      file_basename,
      "</code></div>",
      "<div><b>Status:</b> ",
      status_html,
      "</div>",
      mtime_html,
      "</div></div>"
    )

    i <- i + 1
  }

  # Build Edge List
  edges_from <- character()
  edges_to <- character()
  edge_types <- character()
  edge_titles <- character()

  for (file in all_objects) {
    file_name <- file@name

    if (is(file, "FileUses")) {
      for (dep in file@dependencies) {
        edges_from <- c(edges_from, dep@name)
        edges_to <- c(edges_to, file_name)
        edge_types <- c(edge_types, "dependency")
        edge_titles <- c(
          edge_titles,
          paste0("Dependency: ", dep@name, " → ", file_name)
        )
      }
    }

    if (is(file, "FileOutputs")) {
      for (out in file@output) {
        edges_from <- c(edges_from, file_name)
        edges_to <- c(edges_to, out@name)
        edge_types <- c(edge_types, "output")
        edge_titles <- c(
          edge_titles,
          paste0("Output: ", file_name, " produces ", out@name)
        )
      }
    }
  }

  # Construct igraph
  g <- igraph::graph_from_data_frame(
    d = data.frame(
      from = edges_from,
      to = edges_to,
      type = edge_types,
      title = edge_titles,
      stringsAsFactors = FALSE
    ),
    vertices = data.frame(
      name = node_names,
      label = node_names,
      stage = stages,
      artifact_role = artifact_roles,
      color = colors,
      stale = stale,
      path = paths,
      description = descriptions,
      renders = renders,
      shape = shapes,
      mtime = mtimes,
      title = titles,
      stringsAsFactors = FALSE
    ),
    directed = TRUE
  )

  return(g)
}


#' Collapse an igraph pipeline graph to one node per pipeline `stage`
#'
#' The full per-file compute graph (~90+ nodes) is the right level of detail
#' for the interactive HTML, where a reader can zoom and pan -- but it cannot
#' be made legible as a single static image for the PDF/DOCX deliverable at
#' any reasonable page size. This produces a bird's-eye version instead: one
#' node per `stage`, with an edge between two stages whenever any file in the
#' first stage feeds any file in the second (parallel edges collapsed).
#'
#' @param g An igraph object (see as_igraph()).
#' @param stage_labels Optional named character vector mapping stage identifiers to display labels.
#' @return An igraph object with one node per stage.
#' @export
collapse_by_stage <- function(g, stage_labels = NULL) {
  if (is.null(stage_labels)) {
    stage_labels <- getOption("pipeline.stage_labels", list())
  }
  stage_order <- unique(igraph::V(g)$stage)
  mapping <- match(igraph::V(g)$stage, stage_order)

  g_collapsed <- igraph::contract(g, mapping, vertex.attr.comb = "first")
  igraph::V(g_collapsed)$name <- stage_order

  formatted_labels <- vapply(
    stage_order,
    function(stg) {
      if (stg %in% names(stage_labels)) {
        return(unname(stage_labels[[stg]]))
      }
      # Dynamic fallback: strip leading digits/underscores and title case
      clean_name <- gsub("^[0-9]+_", "", stg) |> gsub("_", " ", x = _)
      tools::toTitleCase(clean_name)
    },
    character(1)
  )

  igraph::V(g_collapsed)$label <- unname(formatted_labels)
  igraph::simplify(g_collapsed, remove.multiple = TRUE, remove.loops = TRUE)
}


#' Generate an interactive visNetwork visualization of the computation graph
#'
#' @param all_objects A list of FilePath objects or an igraph object.
#' @param direction Layout direction: "LR" (Left-to-Right, default) or "UD" (Top-to-Bottom).
#' @param hierarchical Logical, whether to use a hierarchical DAG layout. Default is TRUE.
#' @param title Character, main title -- defaults to getOption("pipeline.study_name", "Computational Pipeline").
#' @param subtitle Character, the document-specific name (e.g. "Computational
#'   Pipeline", or a subgraph's own name from export_subgraph()).
#' @return A visNetwork htmlwidget.
#' @export
visualize_pipeline_interactive <- function(
  all_objects,
  direction = "LR",
  hierarchical = TRUE,
  title = getOption("pipeline.study_name", "Computational Pipeline"),
  subtitle = "Computational Pipeline"
) {
  if (!requireNamespace("visNetwork", quietly = TRUE)) {
    stop("Package \"visNetwork\" is required for the interactive visualization.", call. = FALSE)
  }
  if (is(all_objects, "igraph")) {
    g <- all_objects
  } else {
    g <- as_igraph(all_objects)
  }

  nodes_df <- igraph::as_data_frame(g, what = "vertices")
  edges_df <- igraph::as_data_frame(g, what = "edges")

  # Clean nodes dataframe for visNetwork
  nodes_clean <- data.frame(
    id = nodes_df$name,
    label = nodes_df$name,
    group = nodes_df$stage,
    color = nodes_df$color,
    shape = ifelse(
      nodes_df$stage == "raw_data",
      "database",
      ifelse(
        grepl("\\.(rds|xlsx|csv)$", nodes_df$path, ignore.case = TRUE),
        "ellipse",
        "box"
      )
    ),
    title = nodes_df$title,
    borderWidth = ifelse(nodes_df$stale, 3, 1),
    stringsAsFactors = FALSE
  )

  # Clean edges dataframe
  edges_clean <- data.frame(
    from = edges_df$from,
    to = edges_df$to,
    arrows = "to",
    title = edges_df$title,
    stringsAsFactors = FALSE
  )

  # Build visNetwork widget
  vn <- visNetwork::visNetwork(
    nodes_clean,
    edges_clean,
    main = title,
    submain = subtitle,
    footer = "Interactive dependency graph -- use mouse wheel to zoom, drag to pan, click to highlight",
    width = "100%",
    height = "850px"
  ) |>
    visNetwork::visNodes(
      font = list(face = "Segoe UI, Arial, sans-serif", size = 12),
      shadow = list(enabled = TRUE, size = 3, color = "rgba(0,0,0,0.15)")
    ) |>
    visNetwork::visEdges(
      color = list(color = "#B0BEC5", highlight = "#1E88E5", hover = "#1E88E5"),
      smooth = list(enabled = TRUE, type = "cubicBezier", roundness = 0.5)
    ) |>
    visNetwork::visOptions(
      highlightNearest = list(
        enabled = TRUE,
        degree = list(from = 1, to = 1),
        hover = FALSE,
        algorithm = "hierarchical"
      ),
      nodesIdSelection = list(
        enabled = TRUE,
        style = "width: 280px; height: 30px; font-size: 13px; margin: 5px;"
      ),
      selectedBy = list(
        variable = "group",
        style = "width: 220px; height: 30px; font-size: 13px; margin: 5px;"
      )
    ) |>
    visNetwork::visInteraction(
      navigationButtons = TRUE,
      hover = TRUE,
      zoomView = TRUE,
      dragView = TRUE,
      multiselect = TRUE,
      keyboard = TRUE,
      tooltipDelay = 100
    )

  if (hierarchical) {
    vn <- vn |>
      visNetwork::visHierarchicalLayout(
        direction = direction,
        levelSeparation = 220,
        nodeSpacing = 70,
        treeSpacing = 120,
        sortMethod = "directed"
      )
  } else {
    vn <- vn |>
      visNetwork::visPhysics(
        solver = "forceAtlas2Based",
        forceAtlas2Based = list(
          gravitationalConstant = -40,
          centralGravity = 0.01,
          springLength = 100
        ),
        stabilization = list(iterations = 150)
      )
  }

  vn <- vn |>
    visNetwork::visEvents(
      stabilized = "function() { this.fit(); }",
      selectNode = "function(properties) { this.focus(properties.nodes[0], {scale: 1.2, animation: true}); }"
    )

  return(vn)
}


#' Export the computational graph to a self-contained interactive HTML file for stakeholders
#'
#' @param all_objects A list of FilePath objects or an igraph object.
#' @param file Path to the output HTML file. Default is here::here("R", "workflow_viz", "full_compute_graph.html").
#' @param direction "LR" (default) or "UD".
#' @param hierarchical Logical. Default TRUE.
#' @return The saved file path (invisibly).
#' @export
export_interactive_pipeline <- function(
  all_objects,
  file = here::here("R", "workflow_viz", "full_compute_graph.html"),
  direction = "LR",
  hierarchical = TRUE,
  title = getOption("pipeline.study_name", "Computational Pipeline"),
  subtitle = "Computational Pipeline"
) {
  if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
    stop("Package \"htmlwidgets\" is required to save the interactive graph.", call. = FALSE)
  }
  vn <- visualize_pipeline_interactive(
    all_objects,
    direction = direction,
    hierarchical = hierarchical,
    title = title,
    subtitle = subtitle
  )

  out_dir <- dirname(file)
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }

  message("Saving interactive computational graph to: ", file)
  htmlwidgets::saveWidget(vn, file = file, selfcontained = TRUE)

  # Clean up temporary supporting directory left by saveWidget
  files_dir <- sub("\\.html$", "_files", file)
  if (dir.exists(files_dir)) {
    unlink(files_dir, recursive = TRUE)
  }

  message("Interactive HTML graph exported successfully!")
  invisible(file)
}


#' Export a focused interactive subgraph centered around a focal report or stage
#'
#' @param all_objects List of FilePath objects or an igraph object.
#' @param focal_node Character name of the node to focus on (e.g. "EDA_Report").
#' @param stage Character name of the stage to filter by (e.g. "03_Imputation").
#' @param order Degree of neighborhood around focal_node (default 1).
#' @param file Destination HTML path.
#' @return File path invisibly.
#' @export
export_subgraph <- function(
  all_objects,
  focal_node = NULL,
  stage = NULL,
  order = 1,
  file = NULL
) {
  if (is(all_objects, "igraph")) {
    g <- all_objects
  } else {
    g <- as_igraph(all_objects)
  }

  sub_g <- g
  sub_title <- "Computational Subgraph"

  if (!is.null(focal_node) && focal_node %in% igraph::V(g)$name) {
    neighborhood_nodes <- igraph::make_ego_graph(
      g,
      order = order,
      nodes = focal_node,
      mode = "all"
    )[[1]]
    sub_g <- neighborhood_nodes
    sub_title <- paste0("Pipeline Neighborhood: ", focal_node)
  } else if (!is.null(stage)) {
    matching_nodes <- igraph::V(g)[igraph::V(g)$stage == stage]$name
    if (length(matching_nodes) > 0) {
      sub_g <- igraph::induced_subgraph(g, matching_nodes)
      sub_title <- paste0("Pipeline Stage: ", stage)
    }
  }

  if (is.null(file)) {
    clean_tag <- gsub(
      "[^A-Za-z0-9_]",
      "_",
      if (!is.null(focal_node)) focal_node else stage
    )
    file <- here::here(
      "R",
      "workflow_viz",
      paste0("subgraph_", clean_tag, ".html")
    )
  }

  export_interactive_pipeline(
    sub_g,
    file = file,
    hierarchical = FALSE,
    subtitle = sub_title
  )
}


#' Get the topologically sorted list of QMDs that need to be re-rendered
#'
#' @param all_objects The full list of FilePath, FileUses, and FileOutputs objects.
#' @return A list of FileOutputs objects (the QMDs) in the correct render order.
#' @export
get_render_plan <- function(all_objects) {
  message("Building dependency graph...")

  # 1. Create a lookup map (name -> object)
  obj_lookup <- setNames(
    all_objects,
    vapply(all_objects, \(x) x@name, character(1))
  )

  # 2. Build the edge list for the graph
  edge_list <- data.frame(from = character(), to = character())

  for (obj in all_objects) {
    obj_name <- obj@name

    # Add dependency edges (dependency -> object)
    if (is(obj, "FileUses") && length(obj@dependencies) > 0) {
      dep_names <- vapply(obj@dependencies, \(d) d@name, character(1))
      edge_list <- rbind(edge_list, data.frame(from = dep_names, to = obj_name))
    }

    # Add output edges (object -> output)
    if (is(obj, "FileOutputs") && length(obj@output) > 0) {
      out_names <- vapply(obj@output, \(o) o@name, character(1))
      edge_list <- rbind(edge_list, data.frame(from = obj_name, to = out_names))
    }
  }

  # 3. Create the graph
  g <- igraph::graph_from_data_frame(unique(edge_list), directed = TRUE)

  # Ensure all nodes are in the graph, even if they have no edges
  all_node_names <- names(obj_lookup)
  missing_nodes <- all_node_names[!all_node_names %in% igraph::V(g)$name]
  if (length(missing_nodes) > 0) {
    g <- igraph::add_vertices(g, length(missing_nodes), name = missing_nodes)
  }

  # 4. Find all *initially* stale QMDs
  renderers <- Filter(\(x) is(x, "FileOutputs") && x@renders, all_objects)
  initial_stale_qmds <- c()

  for (qmd in renderers) {
    is_qmd_stale <- FALSE
    for (out_file in qmd@output) {
      if (.is_stale(out_file, qmd)) {
        is_qmd_stale <- TRUE
        break
      }
    }
    if (is_qmd_stale) {
      initial_stale_qmds <- c(initial_stale_qmds, qmd@name)
    }
  }

  if (length(initial_stale_qmds) == 0) {
    message("✨ All files are up-to-date. Nothing to render.")
    return(list())
  }

  message(paste(
    "Found",
    length(initial_stale_qmds),
    "initially stale QMD(s):",
    paste(initial_stale_qmds, collapse = ", ")
  ))

  # 5. Propagate staleness: Find all nodes downstream of the stale ones
  all_affected_nodes <- unique(unlist(
    lapply(initial_stale_qmds, \(name) {
      names(igraph::subcomponent(g, name, mode = "out"))
    })
  ))

  # 6. Filter this list for *only* renderable QMDs
  affected_qmd_names <- Filter(
    \(name) {
      obj <- obj_lookup[[name]]
      !is.null(obj) && obj@renders
    },
    all_affected_nodes
  )

  # Combine with the initial list
  final_qmd_list <- unique(c(initial_stale_qmds, affected_qmd_names))

  if (length(final_qmd_list) == 0) {
    message("... but no QMDs need to be rendered.")
    return(list())
  }

  # 7. Get the topological sort of the *entire* graph
  sorted_all_names <- names(igraph::topo_sort(g))

  # 8. Filter the sorted list to get our final, ordered plan
  ordered_plan_names <- sorted_all_names[sorted_all_names %in% final_qmd_list]

  message(paste(
    "Render plan includes",
    length(ordered_plan_names),
    "QMD(s) in total."
  ))

  # Return the actual objects in the correct order
  return(obj_lookup[ordered_plan_names])
}


#' Check if a FileOutputs object controls datasets (outputs an rds, xlsx, etc. file)
#'
#' @param obj A FileOutputs object.
#' @return Logical.
#' @export
controls_datasets <- function(obj) {
  if (!is(obj, "FileOutputs")) {
    return(FALSE)
  }

  outputs <- obj@output
  if (length(outputs) == 0) {
    return(FALSE)
  }

  exts <- vapply(
    outputs,
    function(o) {
      if (!is.na(o@path)) tolower(tools::file_ext(o@path)) else ""
    },
    character(1)
  )

  # Check if any extension is a dataset type (rds, xlsx, csv, etc.)
  any(exts %in% c("rds", "xlsx", "csv", "rdata"))
}


## Show Methods (for printing) -----------------

#' Helper function to format slot names for printing
#' @noRd
.format_slot_name <- function(name) {
  return(sprintf("  %-12s", paste0(name, ":")))
}

#' Show method for FilePath objects
#'
#' @param object A \code{\linkS4class{FilePath}} object.
#' @rdname FilePath-class
#' @export
setMethod(
  "show",
  "FilePath",
  function(object) {
    cat("S4 object of class:", class(object), "\n")
    cat(.format_slot_name("Name"), object@name, "\n")
    cat(.format_slot_name("Path"), object@path, "\n")
    cat(.format_slot_name("Stage"), object@stage, "\n")
    cat(.format_slot_name("Role"), object@artifact_role, "\n")
    cat(.format_slot_name("Renders"), object@renders, "\n")
    cat(.format_slot_name("Modified"), as.character(object@mtime), "\n")
    if (!is.na(object@description) && nzchar(object@description)) {
      cat(.format_slot_name("Description"), object@description, "\n")
    }
  }
)

#' Show method for FileUses objects
#'
#' @param object A \code{\linkS4class{FileUses}} object.
#' @rdname FileUses-class
#' @export
setMethod(
  "show",
  "FileUses",
  function(object) {
    # Call the parent method first (for FilePath)
    callNextMethod()

    # Now, add the dependencies
    deps <- object@dependencies
    cat(.format_slot_name("Dependencies"))

    if (length(deps) > 0) {
      dep_names <- vapply(deps, \(x) x@name, character(1))
      cat("\n")
      cat(paste0("    - ", dep_names), sep = "\n")
    } else {
      cat("(none)\n")
    }
  }
)

#' Show method for FileOutputs objects
#'
#' @param object A \code{\linkS4class{FileOutputs}} object.
#' @rdname FileOutputs-class
#' @export
setMethod(
  "show",
  "FileOutputs",
  function(object) {
    # Call the parent method first (for FileUses, which in turn calls FilePath)
    callNextMethod()

    # Now, add the outputs
    outputs <- object@output
    cat(.format_slot_name("Outputs"))

    if (length(outputs) > 0) {
      out_names <- vapply(outputs, \(x) x@name, character(1))
      cat("\n")
      cat(paste0("    - ", out_names), sep = "\n")
    } else {
      cat("(none)\n")
    }
  }
)


# Tidy Graph & dplyr Pipeline Methods -----------------

#' Convert pipeline objects or igraph to a tidygraph tbl_graph
#'
#' Enables native dplyr verbs (activate, filter, mutate, group_by, summarise)
#' on computational graphs.
#'
#' @param x A list of FilePath/FileUses/FileOutputs objects, or an igraph object.
#' @param ... Additional arguments passed to tidygraph::as_tbl_graph.
#' @return A tbl_graph object.
#' @export
as_pipeline_graph <- function(x, ...) {
  if (inherits(x, "tbl_graph")) return(x)
  g <- if (inherits(x, "igraph")) x else as_igraph(x)
  tidygraph::as_tbl_graph(g, ...)
}

#' Convert pipeline list to tidygraph
#' @exportS3Method tidygraph::as_tbl_graph
as_tbl_graph.list <- function(x, ...) {
  as_pipeline_graph(x, ...)
}


# Pipeline Summary Methods -----------------

#' Produce an executive summary of the computational pipeline
#'
#' Summarizes total nodes, edges, staleness status, artifact role counts,
#' and a stage-by-role breakdown table.
#'
#' @param object A tbl_graph, igraph, or list of FilePath objects.
#' @return Invisibly returns a tibble of the stage breakdown.
#' @export
pipeline_summary <- function(object) {
  tg <- if (inherits(object, "tbl_graph")) {
    object
  } else {
    as_pipeline_graph(object)
  }

  nodes_df <- tidygraph::as_tibble(tidygraph::activate(tg, "nodes"))
  edges_df <- tidygraph::as_tibble(tidygraph::activate(tg, "edges"))

  n_nodes <- nrow(nodes_df)
  n_edges <- nrow(edges_df)
  n_stale <- if ("stale" %in% names(nodes_df)) sum(nodes_df$stale, na.rm = TRUE) else 0

  cat("========================================================================\n")
  cat("                      Computational Pipeline Summary                    \n")
  cat("========================================================================\n")
  cat(sprintf("Total Nodes: %d | Total Edges: %d | Stale Nodes: %d (%s)\n\n",
              n_nodes, n_edges, n_stale,
              if (n_stale == 0) "All Up-To-Date" else "Needs Re-render"))

  # Artifact Roles Breakdown
  cat("Artifact Roles:\n")
  role_vec <- if ("artifact_role" %in% names(nodes_df)) nodes_df$artifact_role else "unspecified"
  role_vec[is.na(role_vec) | role_vec == ""] <- "unspecified"
  role_counts <- table(role_vec)
  for (r_name in names(role_counts)) {
    cat(sprintf("  - %-25s : %2d\n", r_name, role_counts[[r_name]]))
  }
  cat("\n")

  # Pipeline Stages Breakdown
  cat("Pipeline Stages Breakdown:\n")
  stage_summary <- nodes_df |>
    dplyr::group_by(stage) |>
    dplyr::summarise(
      Reports    = sum(artifact_role %in% c("report_source", "deliverable_report"), na.rm = TRUE),
      Data_Files = sum(artifact_role %in% c("raw_data", "derived_data", "enhanced_data", "intermediate_data"), na.rm = TRUE),
      Helpers    = sum(artifact_role == "helper_script", na.rm = TRUE),
      Total      = dplyr::n(),
      .groups    = "drop"
    )

  print(as.data.frame(stage_summary), row.names = FALSE)
  cat("========================================================================\n")
  invisible(stage_summary)
}

#' @export
summarize_pipeline <- pipeline_summary

#' S3 summary method for tbl_graph objects
#' @export
summary.tbl_graph <- function(object, ...) {
  nodes_df <- tidygraph::as_tibble(tidygraph::activate(object, "nodes"))
  if ("stage" %in% names(nodes_df)) {
    return(pipeline_summary(object))
  }
  NextMethod()
}


# Pipeline Joining Methods -----------------

#' Join two computational pipeline graphs
#'
#' Relational union of two pipeline subgraphs using tidygraph::graph_join(by = "name").
#' Nodes with identical names are coalesced, and their directed dependency
#' and output edges are combined without duplicating nodes.
#'
#' @param graph_a A list of FilePath objects, igraph, or tbl_graph.
#' @param graph_b A list of FilePath objects, igraph, or tbl_graph.
#' @param ... Additional arguments passed to tidygraph::graph_join.
#' @return A merged tbl_graph object.
#' @export
join_pipelines <- function(graph_a, graph_b, ...) {
  tg_a <- if (inherits(graph_a, "tbl_graph")) graph_a else as_tbl_graph(graph_a)
  tg_b <- if (inherits(graph_b, "tbl_graph")) graph_b else as_tbl_graph(graph_b)

  merged <- tidygraph::graph_join(tg_a, tg_b, by = "name", ...)

  # Coalesce duplicated vertex attributes
  nodes_df <- tidygraph::as_tibble(tidygraph::activate(merged, "nodes"))
  for (col in c(
    "stage",
    "artifact_role",
    "path",
    "description",
    "color",
    "shape",
    "mtime",
    "renders",
    "stale",
    "title"
  )) {
    col_x <- paste0(col, ".x")
    col_y <- paste0(col, ".y")
    if (col_x %in% names(nodes_df) && col_y %in% names(nodes_df)) {
      nodes_df[[col]] <- dplyr::coalesce(nodes_df[[col_x]], nodes_df[[col_y]])
      nodes_df[[col_x]] <- NULL
      nodes_df[[col_y]] <- NULL
    }
  }

  edges_df <- tidygraph::as_tibble(tidygraph::activate(merged, "edges"))
  tidygraph::tbl_graph(nodes = nodes_df, edges = edges_df, directed = TRUE)
}


# Stage Data Artifact Inspection Helper -----------------

#' Inspect and categorize all on-disk data artifacts for a pipeline stage
#'
#' Scans a directory for data files (.rds, .xlsx, .csv) and distinguishes
#' primary pipeline handoff datasets from intermediate analysis tables.
#'
#' @param stage_dir Directory path to scan (e.g. analysis/2024_09/EDA/data).
#' @param stage Optional stage identifier to tag the artifacts with.
#' @param primary_patterns Regex pattern identifying primary handoff datasets.
#' @return A tibble of data artifacts with name, path, stage, role, size_kb, and mtime.
#' @export
list_stage_data_artifacts <- function(
  stage_dir,
  stage = NA_character_,
  primary_patterns = c(
    "^DATA\\.rds$",
    "^INDEX\\.rds$",
    "Index_Scores",
    "imputed_DATA"
  )
) {
  if (!dir.exists(stage_dir)) {
    warning("Directory does not exist: ", stage_dir)
    return(tibble::tibble())
  }

  files <- list.files(
    stage_dir,
    pattern = "\\.(rds|xlsx|csv)$",
    full.names = TRUE,
    recursive = FALSE
  )
  if (length(files) == 0) {
    return(tibble::tibble())
  }

  basenames <- basename(files)
  file_infos <- file.info(files)

  pattern <- paste(primary_patterns, collapse = "|")
  roles <- ifelse(
    grepl(pattern, basenames, ignore.case = TRUE),
    "primary_data",
    "intermediate_data"
  )

  tibble::tibble(
    file_name = basenames,
    path = files,
    stage = stage,
    artifact_role = roles,
    size_kb = round(file_infos$size / 1024, 1),
    mtime = file_infos$mtime
  )
}
