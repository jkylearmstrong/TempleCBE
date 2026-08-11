test_that("find_code works correctly on test files", {
  # Create a temporary directory for tests
  tmp_dir <- file.path(tempdir(), "test_find_code")
  dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  # Write dummy files
  file1 <- file.path(tmp_dir, "script1.R")
  writeLines(c(
    "# This is script 1",
    "foo_function <- function() {",
    "  print('hello world')",
    "  # another comment containing foo",
    "  return(TRUE)",
    "}"
  ), file1)

  file2 <- file.path(tmp_dir, "doc1.qmd")
  writeLines(c(
    "---",
    "title: 'Test QMD'",
    "---",
    "",
    "```{r}",
    "bar_value <- 42",
    "```"
  ), file2)

  # Test 1: Basic search
  res1 <- find_code(tmp_dir, "foo_function")
  expect_equal(nrow(res1), 1)
  expect_equal(res1$file[1], "script1.R")
  expect_equal(res1$line_number[1], 2)

  # Test 2: Case insensitivity
  res2 <- find_code(tmp_dir, "FOO_FUNCTION", ignore_case = TRUE)
  expect_equal(nrow(res2), 1)

  # Test 3: Exclude comments
  res3 <- find_code(tmp_dir, "foo", include_comments = FALSE)
  expect_equal(nrow(res3), 1)
  expect_match(res3$line[1], "foo_function")

  # Test 4: Include comments
  res4 <- find_code(tmp_dir, "foo", include_comments = TRUE)
  expect_equal(nrow(res4), 2) # Should match foo_function and the comment on line 4

  # Test 5: Regex search
  res5 <- find_code(tmp_dir, "bar_.* <-", regex = TRUE)
  expect_equal(nrow(res5), 1)
  expect_equal(res5$file[1], "doc1.qmd")

  # Test 6: Context lines
  res6 <- find_code(tmp_dir, "hello world", lines_before = 1, lines_after = 1)
  expect_equal(nrow(res6), 1)
  # Check if context lines are concatenated by newlines
  lines_split <- strsplit(res6$line[1], "\n")[[1]]
  expect_equal(length(lines_split), 3)
  expect_match(lines_split[1], "foo_function")
  expect_match(lines_split[2], "hello world")
  expect_match(lines_split[3], "another comment")

  # Test 7: Exclude directories
  dir.create(file.path(tmp_dir, "renv"), showWarnings = FALSE)
  writeLines("foo_function <- function() {}", file.path(tmp_dir, "renv", "ignored.R"))
  res7 <- find_code(tmp_dir, "foo_function", exclude_dirs = "renv")
  expect_equal(nrow(res7), 1) # Should not match files in renv/

  # Test 8: return_all_matches and match_extractor
  extractor <- function(block) {
    # Match any word that starts with 'foo'
    matches <- regmatches(block, gregexpr("\\bfoo\\w*", block))
    list(unlist(matches))
  }
  res8 <- find_code(tmp_dir, "foo", return_all_matches = TRUE, match_extractor = extractor)
  expect_equal(nrow(res8), 2)
  expect_equal(res8$match, c("foo_function", "foo"))
})
