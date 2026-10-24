write_header_book <- function(rows, merges = NULL) {
  skip_if_not_installed("openxlsx")
  f <- tempfile(fileext = ".xlsx")
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Data")
  for (i in seq_along(rows)) {
    for (j in seq_along(rows[[i]])) {
      v <- rows[[i]][[j]]
      if (!is.na(v)) openxlsx::writeData(wb, "Data", v, startCol = j, startRow = i, colNames = FALSE)
    }
  }
  openxlsx::saveWorkbook(wb, f, overwrite = TRUE)
  f
}

test_that("two header rows are combined, merged titles carry right, data are typed", {
  f <- write_header_book(list(
    list("Pressure", NA, NA, "Temp"),
    list("Sys", "Dia", "Mean", "C"),
    list(120, 80, 93, 36.6),
    list(130, 85, 100, 37.1)
  ))
  r <- read_positional_excel_headers(f, header_rows = 1:2)
  expect_equal(names(r), c("pressure_sys", "pressure_dia", "pressure_mean", "temp_c"))
  expect_equal(nrow(r), 2L)
  expect_equal(r$pressure_sys, c(120, 130))
  expect_true(is.numeric(r$temp_c))
  expect_equal(nrow(attr(r, "header")), 2L)
})

test_that("a blank in the last header row is not filled; columns without header text get Excel letters", {
  f <- write_header_book(list(
    list("Group", NA, "Other"),
    list("a", NA, NA),
    list(1, 2, 3)
  ))
  r <- read_positional_excel_headers(f, header_rows = 1:2, clean_names = FALSE)
  expect_equal(names(r), c("Group_a", "Group", "Other"))
  g <- write_header_book(list(list(NA, "x"), list(1, 2)))
  expect_equal(names(read_positional_excel_headers(g, header_rows = 1)), c("a", "x"))
})

test_that("duplicate names are made unique; one header row works; clean_names = FALSE keeps the text", {
  f <- write_header_book(list(list("Value", "Value", "Name (kg)"), list(1, 2, 3)))
  expect_equal(names(read_positional_excel_headers(f, header_rows = 1)), c("value", "value_2", "name_kg"))
  expect_equal(names(read_positional_excel_headers(f, header_rows = 1, clean_names = FALSE)), c("Value", "Value_1", "Name (kg)"))
})

test_that("bad input is explained", {
  f <- write_header_book(list(list("a", "b"), list(1, 2)))
  expect_error(read_positional_excel_headers("no/such.xlsx"), "existing Excel file")
  expect_error(read_positional_excel_headers(f, header_rows = c(1, 3)), "consecutive")
  expect_error(read_positional_excel_headers(f, header_rows = 0), "consecutive")
  expect_error(read_positional_excel_headers(f, header_rows = 1:5), "only 2 row")
})
