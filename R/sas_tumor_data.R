#' Internal SAS Benchmark Tumor Dataset (Wide Format)
#'
#' Transcribes the 45-animal tumor-promoting agent study from the SAS/STAT
#' User's Guide (Example 85.7 / 91.7: "Time-Dependent Repeated Measurements of a Covariate").
#' In this study, 45 rodents were exposed to a carcinogen and randomized to three dose
#' levels of a tumor-promoting agent (1.0, 2.5, 10.0). The number of papillomas was
#' observed repeatedly across 15 observation times (weeks 27, 34, 37, 41, 43, 45,
#' 46, 47, 49, 50, 51, 53, 65, 67, 71).
#'
#' @return A tibble with 45 rows and 19 columns:
#'   \describe{
#'     \item{ID}{Subject ID (1--45).}
#'     \item{Time}{Event or censoring time in weeks.}
#'     \item{Dead}{Censoring status (1 = dead, 0 = censored).}
#'     \item{Dose}{Dose level of the promoting agent (1.0, 2.5, or 10.0).}
#'     \item{P1--P15}{Number of papillomas counted at observation weeks
#'       27, 34, 37, 41, 43, 45, 46, 47, 49, 50, 51, 53, 65, 67, 71. Missing (NA)
#'       beyond death or study termination.}
#'   }
#' @source SAS/STAT User's Guide, Example 85.7: Time-Dependent Repeated Measurements
#'   of a Covariate.
#'   \url{https://support.sas.com/documentation/cdl/en/statug/68162/HTML/default/statug_phreg_examples07.htm}
#' @export
tumor_wide <- function() {
  raw_lines <- c(
    " 1 47 1  1.0  0  5  6  8 10 10 10 10",
    " 2 71 1  1.0  0  0  0  0  0  0  0  0  1  1  1  1 1 1 1",
    " 3 81 0  1.0  0  1  1  1  1  1  1  1  1  1  1  1 1 1 1",
    " 4 81 0  1.0  0  0  0  0  0  0  0  0  0  0  0  0 0 0 0",
    " 5 81 0  1.0  0  0  0  0  0  0  0  0  0  0  0  0 0 0 0",
    " 6 65 1  1.0  0  0  0  1  1  1  1  1  1  1  1  1 1",
    " 7 71 0  1.0  0  0  0  0  0  0  0  0  0  0  0  0 0 0 0",
    " 8 69 0  1.0  0  0  0  0  0  0  0  0  0  0  0  0 0 0",
    " 9 67 1  1.0  0  0  1  1  2  2  2  2  3  3  3  3 3 3",
    "10 81 0  1.0  0  0  0  0  0  0  0  0  0  0  0  0 0 0 0",
    "11 37 1  1.0  9  9  9",
    "12 81 0  1.0  0  0  0  0  0  0  0  0  0  0  0  0 0 0 0",
    "13 77 0  1.0  0  0  0  0  1  1  1  1  1  1  1  1 1 1 1",
    "14 81 0  1.0  0  0  0  0  0  0  0  0  0  0  0  0 0 0 0",
    "15 81 0  1.0  0  0  0  0  0  0  0  0  0  0  0  0 0 0 0",
    "16 54 0  2.5  0  1  1  1  2  2  2  2  2  2  2  2",
    "17 53 0  2.5  0  0  0  0  0  0  0  0  0  0  0  0",
    "18 38 0  2.5  5 13 14",
    "19 54 0  2.5  2  6  6  6  6  6  6  6  6  6  6  6",
    "20 51 1  2.5 15 15 15 16 16 17 17 17 17 17 17",
    "21 47 1  2.5 13 20 20 20 20 20 20 20",
    "22 27 1  2.5 22",
    "23 41 1  2.5  6 13 13 13",
    "24 49 1  2.5  0  3  3  3  3  3  3  3  3",
    "25 53 0  2.5  0  0  1  1  1  1  1  1  1  1  1  1",
    "26 50 1  2.5  0  0  2  3  4  6  6  6  6  6",
    "27 37 1  2.5  3 15 15",
    "28 49 1  2.5  2  3  3  3  3  4  4  4  4",
    "29 46 1  2.5  4  6  7  9  9  9  9",
    "30 48 0  2.5 15 26 26 26 26 26 26 26",
    "31 54 0 10.0 12 14 15 15 15 15 15 15 15 15 15 15",
    "32 37 1 10.0 12 16 17",
    "33 53 1 10.0  3  6  6  6  6  6  6  6  6  6  6  6",
    "34 45 1 10.0  4 12 15 20 20 20",
    "35 53 0 10.0  6 10 13 13 13 15 15 15 15 15 15 20",
    "36 49 1 10.0  0  2  2  2  2  2  2  2  2",
    "37 39 0 10.0  7  8  8",
    "38 27 1 10.0 17",
    "39 49 1 10.0  0  6  9 14 14 14 14 14 14",
    "40 43 1 10.0 14 18 20 20 20",
    "41 28 0 10.0  8",
    "42 34 1 10.0 11 18",
    "43 45 1 10.0 10 12 16 16 16 16",
    "44 37 1 10.0  0  1  1",
    "45 43 1 10.0  9 19 19 19 19"
  )

  rows <- lapply(raw_lines, function(line) {
    tokens <- strsplit(trimws(line), "\\s+")[[1]]
    id <- as.integer(tokens[1])
    time <- as.numeric(tokens[2])
    dead <- as.integer(tokens[3])
    dose <- as.numeric(tokens[4])
    p_vals <- as.numeric(tokens[5:length(tokens)])
    if (length(p_vals) < 15) {
      p_vals <- c(p_vals, rep(NA_real_, 15 - length(p_vals)))
    } else if (length(p_vals) > 15) {
      p_vals <- p_vals[1:15]
    }
    stats::setNames(c(id, time, dead, dose, p_vals), c("ID", "Time", "Dead", "Dose", paste0("P", 1:15)))
  })

  df <- tibble::as_tibble(do.call(rbind, rows))
  df$ID <- as.integer(df$ID)
  df$Dead <- as.integer(df$Dead)
  df
}

#' Convert Wide Tumor Data to Counting Process (Start/Stop) Format
#'
#' Replicates the exact SAS DATA step transformation from SAS/STAT User's Guide
#' (Example 85.7: "Tumor1" dataset). For each subject, intervals (T1, T2] are constructed
#' across observation times where the time-dependent covariate \code{NPap} changes,
#' with \code{Status} equal to \code{Dead} only on the terminal interval and 0 elsewhere.
#'
#' @param data A data frame structured like \code{tumor_wide()}. If \code{NULL},
#'   \code{tumor_wide()} is used by default.
#' @return A tibble with columns:
#'   \describe{
#'     \item{ID}{Subject ID.}
#'     \item{Time}{Original death or censoring time.}
#'     \item{Dead}{Original death status.}
#'     \item{Dose}{Dose level.}
#'     \item{T1}{Start time of the risk interval.}
#'     \item{T2}{Stop time of the risk interval.}
#'     \item{NPap}{Number of papillomas active during interval (T1, T2].}
#'     \item{Status}{Event status at T2 (1 = event, 0 = censored/continuing).}
#'   }
#' @source SAS/STAT User's Guide, Example 85.7: Time-Dependent Repeated Measurements
#'   of a Covariate.
#'   \url{https://support.sas.com/documentation/cdl/en/statug/68162/HTML/default/statug_phreg_examples07.htm}
#' @export
tumor_long <- function(data = NULL) {
  if (is.null(data)) {
    data <- tumor_wide()
  }

  tt <- c(27, 34, 37, 41, 43, 45, 46, 47, 49, 50, 51, 53, 65, 67, 71)

  out_list <- vector("list", nrow(data) * 5)
  idx <- 0

  for (r in seq_len(nrow(data))) {
    id <- data$ID[r]
    tm <- data$Time[r]
    dead <- data$Dead[r]
    dose <- data$Dose[r]
    p <- as.numeric(data[r, paste0("P", 1:15)])

    t1 <- 0
    t2 <- 0
    status <- 0

    if (tm == tt[1]) {
      t2 <- tt[1]
      npap <- p[1]
      status <- dead
      idx <- idx + 1
      out_list[[idx]] <- list(
        ID = id, Time = tm, Dead = dead, Dose = dose,
        T1 = t1, T2 = t2, NPap = npap, Status = status
      )
    } else {
      for (i in 1:14) {
        if (tt[i] == tm) {
          t2 <- tm
          npap <- p[i]
          status <- dead
          idx <- idx + 1
          out_list[[idx]] <- list(
            ID = id, Time = tm, Dead = dead, Dose = dose,
            T1 = t1, T2 = t2, NPap = npap, Status = status
          )
        } else if (tt[i] < tm) {
          p_i <- p[i]
          q_i <- p[i + 1]

          is_diff <- if (is.na(p_i) && is.na(q_i)) {
            FALSE
          } else if (is.na(p_i) || is.na(q_i)) {
            TRUE
          } else {
            p_i != q_i
          }

          if (is_diff) {
            if (is.na(q_i)) {
              t2 <- tm
            } else {
              t2 <- tt[i]
            }
            npap <- p_i
            status <- 0
            idx <- idx + 1
            out_list[[idx]] <- list(
              ID = id, Time = tm, Dead = dead, Dose = dose,
              T1 = t1, T2 = t2, NPap = npap, Status = status
            )
            t1 <- t2
          }
        }
      }
    }

    if (tm >= tt[15]) {
      t2 <- tm
      npap <- p[15]
      status <- dead
      idx <- idx + 1
      out_list[[idx]] <- list(
        ID = id, Time = tm, Dead = dead, Dose = dose,
        T1 = t1, T2 = t2, NPap = npap, Status = status
      )
    }
  }

  out_list <- out_list[seq_len(idx)]
  dplyr::bind_rows(lapply(out_list, tibble::as_tibble))
}
