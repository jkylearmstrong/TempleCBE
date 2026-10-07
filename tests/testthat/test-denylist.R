# TempleCBE is public: no tracked file may name the private downstream study or
# PI, or use its identifiers. The forbidden terms come from an untracked file
# (see helper-denylist.R), so these tests skip wherever that file is absent
# (CRAN, CI, other contributors) and the terms themselves never enter the repo.

make_denylist_fixture_repo <- function() {
  repo <- tempfile("denylist_repo_")
  dir.create(repo)
  git_output(c("init", "-q"), dir = repo)
  writeLines("nothing to see here", file.path(repo, "clean.txt"))
  writeLines("this line mentions a Secret_Marker inside", file.path(repo, "dirty.txt"))
  writeLines("harmless content", file.path(repo, "secret_marker_in_name.txt"))
  writeLines("Secret_Marker but never added to git", file.path(repo, "untracked.txt"))
  git_output(c("add", "clean.txt", "dirty.txt", "secret_marker_in_name.txt"), dir = repo)
  repo
}

write_fixture_pdf <- function(path, label) {
  grDevices::pdf(path, compress = TRUE)
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::plot.new()
  graphics::text(0.5, 0.5, label)
}

# The terms and repo root for the real scans, or skip the calling test
denylist_or_skip <- function() {
  skip_on_cran()
  skip_if(!nzchar(Sys.which("git")), "git not available")
  path <- denylist_path()
  skip_if(is.na(path), "no denylist: set TEMPLECBE_DENYLIST or create .git/info/denylist")
  terms <- read_denylist(path)
  skip_if(length(terms) == 0, "the denylist has no terms")
  root <- package_repo_root()
  skip_if(is.na(root), "not running inside a TempleCBE git checkout")
  list(terms = terms, root = root)
}

test_that("read_denylist ignores blank lines and comments", {
  path <- tempfile("denylist_")
  on.exit(unlink(path), add = TRUE)
  writeLines(c("# a comment", "", "  term_one  ", "Term_Two", "   "), path)
  expect_equal(read_denylist(path), c("term_one", "Term_Two"))
})

test_that("scan_tracked_files finds tracked names and contents, case-insensitively", {
  skip_on_cran()
  skip_if(!nzchar(Sys.which("git")), "git not available")

  repo <- make_denylist_fixture_repo()
  on.exit(unlink(repo, recursive = TRUE), add = TRUE)
  skip_if(length(git_output(c("ls-files"), dir = repo)) != 3, "could not create a git repo")

  hits <- scan_tracked_files("secret_MARKER", repo)
  expect_equal(hits, c("dirty.txt", "secret_marker_in_name.txt"))
  expect_false("untracked.txt" %in% hits)

  expect_equal(scan_tracked_files("term_that_is_absent", repo), character(0))
  expect_equal(scan_tracked_files(character(0), repo), character(0))
  # several terms: a file matching any one is reported
  expect_equal(
    scan_tracked_files(c("term_that_is_absent", "nothing to see"), repo),
    "clean.txt"
  )
})

test_that("scan_tracked_pdfs finds terms in compressed PDF text that git grep cannot see", {
  skip_on_cran()
  skip_if(!nzchar(Sys.which("git")), "git not available")
  skip_if_not_installed("pdftools")

  repo <- tempfile("denylist_pdf_repo_")
  dir.create(repo)
  on.exit(unlink(repo, recursive = TRUE), add = TRUE)
  git_output(c("init", "-q"), dir = repo)
  write_fixture_pdf(file.path(repo, "dirty.pdf"), "a page that mentions a Secret_Marker")
  write_fixture_pdf(file.path(repo, "clean.pdf"), "nothing to see here")
  git_output(c("add", "dirty.pdf", "clean.pdf"), dir = repo)
  skip_if(length(git_output(c("ls-files"), dir = repo)) != 2, "could not create a git repo")

  # The byte-level scan is blind to the compressed text: that is why the PDF scan exists
  expect_equal(scan_tracked_files("secret_marker", repo), character(0))
  expect_equal(scan_tracked_pdfs("secret_MARKER", repo), "dirty.pdf")
  expect_equal(scan_tracked_pdfs("term_that_is_absent", repo), character(0))
  expect_equal(scan_tracked_pdfs(character(0), repo), character(0))
})

test_that("no tracked file names a term from the local denylist", {
  dl <- denylist_or_skip()

  hits <- scan_tracked_files(dl$terms, dl$root)
  # Report paths only: the matched terms must not be echoed into logs.
  expect(
    length(hits) == 0,
    sprintf(
      "%d tracked file(s) match a denylist term: %s",
      length(hits), paste(hits, collapse = ", ")
    )
  )
  succeed()
})

test_that("no tracked PDF contains a term from the local denylist", {
  skip_if_not_installed("pdftools")
  dl <- denylist_or_skip()

  hits <- scan_tracked_pdfs(dl$terms, dl$root)
  expect(
    length(hits) == 0,
    sprintf(
      "%d tracked PDF(s) contain a denylist term in their text: %s",
      length(hits), paste(hits, collapse = ", ")
    )
  )
  succeed()
})
