docs_root <- function() file.path(test_path(), "..", "..")

# Round-2 fresh-review docs items 30-33 (docs/exports/namespace/CI).
#
# Item 31: vignette titles and VignetteIndexEntry numbers must agree.
# Item 32: NOTES.md version must match DESCRIPTION.
# Item 30: the LIFETEST docs must describe the SAS 9.4 run as done.
# Item 33: the DESCRIPTION-scope (A7) assertions must run from a
#   built-package layout, where README.qmd is absent.

test_that("vignette titles and VignetteIndexEntry numbers agree, 01-09 once each (round-2 item 31)", {
  root <- docs_root()
  vig_dir <- file.path(root, "vignettes")
  skip_if_not(dir.exists(vig_dir), "vignettes are not in the installed package")
  files <- sort(list.files(vig_dir, pattern = "\\.Rmd$", full.names = TRUE))
  skip_if(length(files) == 0L, "no vignettes found")
  numbers <- function(path, pattern) {
    txt <- paste(readLines(path, warn = FALSE), collapse = "\n")
    m <- regmatches(txt, regexec(pattern, txt, perl = TRUE))[[1]]
    if (length(m) < 2L) NA_character_ else m[2]
  }
  title_num <- vapply(files, numbers, character(1),
                      pattern = "(?m)^title:\\s*\"([0-9]{2})\\.")
  index_num <- vapply(files, numbers, character(1),
                      pattern = "VignetteIndexEntry\\{([0-9]{2})\\.")
  expect_false(anyNA(title_num), info = "every vignette has a numbered title")
  expect_false(anyNA(index_num), info = "every vignette has a numbered index entry")
  # The nine vignettes are numbered 01-09, each exactly once.
  expect_setequal(title_num, sprintf("%02d", 1:9))
  expect_setequal(index_num, sprintf("%02d", 1:9))
  # Title and index entry agree file by file (rossi was "09."/"08.").
  expect_equal(unname(index_num), unname(title_num))
})

test_that("NOTES.md version matches DESCRIPTION (round-2 item 32)", {
  root <- docs_root()
  notes <- file.path(root, "NOTES.md")
  skip_if_not(file.exists(notes), "NOTES.md is not in the built package")
  desc_version <- read.dcf(file.path(root, "DESCRIPTION"))[, "Version"]
  lines <- readLines(notes, warn = FALSE)
  m <- regmatches(lines, regexec("^## Version:\\s*(\\S+)", lines))
  versions <- vapply(m, function(x) if (length(x) < 2L) NA_character_ else x[2], character(1))
  versions <- versions[!is.na(versions)]
  expect_length(versions, 1L)
  expect_equal(versions, unname(desc_version))
})

test_that("NEWS.md no longer claims both new vignettes title themselves 08. (round-2 item 31)", {
  root <- docs_root()
  news <- file.path(root, "NEWS.md")
  skip_if_not(file.exists(news), "NEWS.md is not in the built package")
  text <- paste(readLines(news, warn = FALSE), collapse = "\n")
  expect_false(grepl('both title themselves "08."', text, fixed = TRUE))
  expect_match(text, 'renumbered from "08." to "09."', fixed = TRUE)
})

test_that("sas_parity_boundary() LIFETEST row cites the stored SAS 9.4 run (round-2 item 30)", {
  b <- sas_parity_boundary()
  row <- b[b$proc == "PROC LIFETEST", ]
  expect_equal(nrow(row), 1L)
  expect_equal(row$status, "parity")
  expect_match(row$evidence, "inst/sas/list/benchmark_lifetest_km.lst", fixed = TRUE)
  expect_match(row$evidence, "inst/sas/logs/benchmark_lifetest_km.log", fixed = TRUE)
  expect_match(row$evidence, "SAS 9.4", fixed = TRUE)
  expect_match(row$evidence, "test-gaps-0811-w2-data.R", fixed = TRUE)
  expect_false(grepl("not yet run", row$evidence, fixed = TRUE))
  expect_false(grepl("not yet generated", row$notes, fixed = TRUE))
})

test_that("km_lifetest_quantities() Status section describes the SAS run as done (round-2 item 30)", {
  root <- docs_root()
  rd <- file.path(root, "man", "km_lifetest_quantities.Rd")
  skip_if_not(file.exists(rd), "help pages are not in the installed package")
  text <- paste(readLines(rd, warn = FALSE), collapse = "\n")
  expect_match(text, "Run once on SAS 9.4", fixed = TRUE)
  expect_match(text, "benchmark_lifetest_km.lst", fixed = TRUE)
  expect_false(grepl("no SAS reference values exist", text, fixed = TRUE))
  expect_false(grepl("The next step is to run", text, fixed = TRUE))
})

test_that("DESCRIPTION scope and README pins are checkable from a built-package layout (round-2 item 33)", {
  root <- docs_root()
  skip_if_not(file.exists(file.path(root, "DESCRIPTION")), "no DESCRIPTION at the package root")
  skip_if_not(file.exists(file.path(root, "README.md")), "no README.md at the package root")
  built <- withr::local_tempdir()
  file.copy(file.path(root, "DESCRIPTION"), built)
  file.copy(file.path(root, "README.md"), built)
  # README.qmd is .Rbuildignore-d: a built package has no copy of it.
  expect_false(file.exists(file.path(built, "README.qmd")))
  desc <- read.dcf(file.path(built, "DESCRIPTION"))
  for (word in c("SAS", "Python", "multi-table", "pipeline")) {
    expect_match(desc[, "Description"], word, fixed = TRUE)
  }
  text <- paste(readLines(file.path(built, "README.md"), warn = FALSE), collapse = "\n")
  for (pin in c("competing_risks_and_imputation", "rossi_nested_survival",
                "From render to publication", "apply_review_tracker")) {
    expect_match(text, pin, fixed = TRUE)
  }
})
