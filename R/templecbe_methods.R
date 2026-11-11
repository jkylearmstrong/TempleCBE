#' Print, Summary, Tidy and Recipe-Step Methods for TempleCBE Classes
#'
#' S3 methods of the classes that TempleCBE functions return, for generics of
#' base R, \pkg{generics}, \pkg{dplyr} and \pkg{recipes}. They are called
#' through the generic (`print(x)`, `summary(x)`, `tidy(x)`, `x$name`, ...),
#' not by their full name. The help page of the function that creates each class
#' describes the object; this page records the methods that have no page of
#' their own.
#'
#' * `print()` methods write a short description of the object to the console
#'   and return it invisibly.
#' * `summary()` and `tidy()` methods return a tibble (or a list of tibbles)
#'   with the content of the object.
#' * `$` methods give access to elements under their current names, and under
#'   former names with a deprecation warning.
#' * `union()`, `intersect()` and `setdiff()` for a database (see
#'   [as_database()]) combine the nodes and the edges of two databases.
#' * `prep()`, `bake()`, `tidy()`, `tunable()` and `required_pkgs()` are the
#'   methods that \pkg{recipes} calls for the steps [step_famd()],
#'   [step_lencode_coxnet()] and [step_lencode_joint_model()].
#'
#' @param x,object An object of the class named after the dot in the method.
#' @param name Name of the element to extract.
#' @param y A second object of the same class as `x`.
#' @param node_by,edge_by Columns that identify a node and an edge when two
#'   databases are combined. `node_by = NULL` uses the node name.
#' @param max_print Largest number of differences to print for each table.
#' @param width Width of the printed line.
#' @param new_data A data frame to which a trained step is applied.
#' @param training The training data from which a step is estimated.
#' @param info The variable table of the recipe (name, type, role, source).
#' @param ... Passed to other methods, or not used.
#'
#' @return `print()` methods return `x` invisibly. `summary()` and `tidy()`
#'   methods return a tibble or a list of tibbles. `$` returns the element.
#'   The set operations return a database. `prep()` returns the trained step,
#'   `bake()` the transformed data, `tunable()` a tibble of tunable arguments
#'   and `required_pkgs()` a character vector of package names.
#'
#' @name templecbe_methods
#' @keywords internal
NULL
