#' Summarize a `profvis` Profile
#'
#' Tabulates the samples of a [profvis::profvis()] profile by function: memory,
#' memory increments, call frequency, stack depth, and memory over time.
#'
#' Memory columns sum the sampled `memalloc`/`meminc` values, so they rank
#' functions by how much memory was allocated while they were on the stack;
#' they are not exact allocation totals.
#'
#' @param prof A `profvis` object.
#' @return A named list of tibbles:
#'   \describe{
#'     \item{`memory_by_function`}{Summed `memalloc`, share, calls, and
#'       memory per call, by function.}
#'     \item{`memory_increment_by_function`}{Summed `meminc`, share, calls,
#'       and increment per call, by function.}
#'     \item{`calls_by_function`}{Samples per function and share.}
#'     \item{`deepest_calls`}{Each function's deepest sample, deepest first.}
#'     \item{`memory_over_time`}{Summed `memalloc` per sample time.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' prof <- profvis::profvis(for (i in 1:50) sort(runif(1e5)))
#' profvis_summary(prof)$memory_by_function
#' }
profvis_summary <- function(prof) {
  samples <- prof$x$message$prof
  if (!is.data.frame(samples)) {
    stop("`prof` must be a profvis object, from profvis::profvis().", call. = FALSE)
  }
  need <- c("label", "memalloc", "meminc", "time", "depth")
  absent <- setdiff(need, names(samples))
  if (length(absent)) {
    stop(
      "The profile of `prof` has no column(s) ", paste(absent, collapse = ", "),
      "; it does not look like the result of profvis::profvis().",
      call. = FALSE
    )
  }
  # a share of a total of 0 (nothing allocated, or no sample) is 0, not 0 / 0
  share <- function(x, total) if (isTRUE(total > 0)) round(x / total * 100, 3) else rep(0, length(x))
  samples <- tibble::as_tibble(samples)
  by_label <- dplyr::group_by(samples, .data$label)

  memory <- dplyr::summarise(
    by_label,
    total_mem = sum(.data$memalloc, na.rm = TRUE),
    times_called = dplyr::n()
  )
  memory$pct <- share(memory$total_mem, sum(memory$total_mem))
  memory$mem_per_call <- memory$total_mem / memory$times_called

  increment <- dplyr::summarise(
    by_label,
    total_meminc = sum(.data$meminc, na.rm = TRUE),
    times_called = dplyr::n()
  )
  increment$pct <- share(increment$total_meminc, sum(abs(increment$total_meminc)))
  increment$meminc_per_call <- increment$total_meminc / increment$times_called

  calls <- dplyr::summarise(by_label, times_called = dplyr::n())
  calls$pct <- share(calls$times_called, sum(calls$times_called))

  deepest <- dplyr::arrange(samples, dplyr::desc(.data$depth))
  deepest <- dplyr::distinct(deepest, .data$label, .keep_all = TRUE)

  over_time <- dplyr::summarise(
    dplyr::group_by(samples, .data$time),
    total_mem = sum(.data$memalloc, na.rm = TRUE)
  )

  list(
    memory_by_function = dplyr::arrange(memory, dplyr::desc(.data$total_mem)),
    memory_increment_by_function = dplyr::arrange(increment, dplyr::desc(.data$total_meminc)),
    calls_by_function = dplyr::arrange(calls, dplyr::desc(.data$times_called)),
    deepest_calls = deepest,
    memory_over_time = dplyr::arrange(over_time, .data$time)
  )
}
