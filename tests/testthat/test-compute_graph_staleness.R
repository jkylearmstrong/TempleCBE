# Coverage for the staleness, hash-store, render-plan and storage paths of R/compute_graph.R.
# Every file lives in a temp directory and gets an explicit modification time, so nothing here
# depends on the clock, the file system's time resolution or the working directory.

t0 <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")

# Write `text` to `name` in `dir` with a modification time `hours` after t0
make_file <- function(dir, name, text = name, hours = 0) {
  p <- file.path(dir, name)
  writeLines(text, p)
  Sys.setFileTime(p, t0 + hours * 3600)
  p
}

# Forward slashes on every platform: the hash store keys paths that way
tmp_dir <- function(env = parent.frame()) {
  normalizePath(withr::local_tempdir(.local_envir = env), winslash = "/")
}

touch <- function(path, hours) Sys.setFileTime(path, t0 + hours * 3600)

# ---- is_stale(): mtime ---------------------------------------------------------------------------

test_that("is_stale() compares output and producer modification times", {
  d <- tmp_dir()
  make_file(d, "make.R", hours = 5)
  make_file(d, "fresh.csv", hours = 10)
  make_file(d, "old.csv", hours = 1)
  prod <- FileOutputs("make", file.path(d, "make.R"), renders = FALSE)

  expect_false(is_stale(FilePath("fresh", file.path(d, "fresh.csv")), prod))
  expect_true(is_stale(FilePath("old", file.path(d, "old.csv")), prod))
  # an output that does not exist has no mtime and is stale
  expect_true(is_stale(FilePath("gone", file.path(d, "gone.csv")), prod))
})

test_that("is_stale() on a FileOutputs checks its declared outputs", {
  d <- tmp_dir()
  make_file(d, "make.R", hours = 5)
  make_file(d, "a.csv", hours = 10)
  make_file(d, "b.csv", hours = 1)
  a <- FilePath("a", file.path(d, "a.csv"))
  b <- FilePath("b", file.path(d, "b.csv"))

  expect_false(is_stale(FileOutputs("none", file.path(d, "make.R"), renders = FALSE)))
  expect_false(is_stale(FileOutputs("ok", file.path(d, "make.R"), output = list(a), renders = FALSE)))
  # one stale output is enough
  expect_true(is_stale(FileOutputs("mixed", file.path(d, "make.R"), output = list(a, b), renders = FALSE)))
})

test_that("is_stale() sees a changed input of a helper script, not only the helper", {
  d <- tmp_dir()
  make_file(d, "raw.csv", hours = 20)
  make_file(d, "helper.R", hours = 1)
  make_file(d, "make.R", hours = 2)
  make_file(d, "out.csv", hours = 10)
  raw <- FilePath("raw", file.path(d, "raw.csv"))
  helper <- FileUses("helper", file.path(d, "helper.R"), dependencies = list(raw))
  prod <- FileOutputs("make", file.path(d, "make.R"), dependencies = list(helper), renders = FALSE)
  out <- FilePath("out", file.path(d, "out.csv"))

  expect_true(is_stale(out, prod))
  touch(file.path(d, "raw.csv"), 3)
  prod <- FileOutputs("make", file.path(d, "make.R"), dependencies = list(
    FileUses("helper", file.path(d, "helper.R"), dependencies = list(FilePath("raw", file.path(d, "raw.csv"))))
  ), renders = FALSE)
  expect_false(is_stale(out, prod))
})

test_that("is_stale() follows staleness up the pipeline only when all_objects is given", {
  d <- tmp_dir()
  make_file(d, "raw.csv", hours = 1)
  make_file(d, "step1.R", hours = 30)
  make_file(d, "mid.csv", hours = 2) # older than step1.R: stale
  make_file(d, "step2.R", hours = 3)
  make_file(d, "final.csv", hours = 40) # newer than everything it reads directly

  raw <- FilePath("raw", file.path(d, "raw.csv"))
  mid <- FilePath("mid", file.path(d, "mid.csv"))
  final <- FilePath("final", file.path(d, "final.csv"))
  step1 <- FileOutputs("step1", file.path(d, "step1.R"), dependencies = list(raw), output = list(mid), renders = FALSE)
  step2 <- FileOutputs("step2", file.path(d, "step2.R"), dependencies = list(mid), output = list(final), renders = FALSE)
  objs <- list(raw, step1, step2)

  expect_false(is_stale(final, step2))
  expect_true(is_stale(final, step2, all_objects = objs))
})

test_that(".upstream_stale() handles plain inputs, helper scripts, caching and cycles", {
  d <- tmp_dir()
  make_file(d, "raw.csv", hours = 1)
  make_file(d, "step1.R", hours = 30)
  make_file(d, "mid.csv", hours = 2)
  make_file(d, "helper.R", hours = 1)
  raw <- FilePath("raw", file.path(d, "raw.csv"))
  mid <- FilePath("mid", file.path(d, "mid.csv"))
  step1 <- FileOutputs("step1", file.path(d, "step1.R"), dependencies = list(raw), output = list(mid), renders = FALSE)
  helper_on_mid <- FileUses("helper", file.path(d, "helper.R"), dependencies = list(mid))
  helper_on_raw <- FileUses("helper2", file.path(d, "helper.R"), dependencies = list(raw))
  objs <- list(raw, step1, helper_on_mid, helper_on_raw)

  # a plain input with no producer is never stale
  expect_false(.upstream_stale(raw, objs))
  # a file with a stale producer is stale
  expect_true(.upstream_stale(mid, objs))
  # a helper script is stale when anything it reads is
  expect_true(.upstream_stale(helper_on_mid, objs))
  expect_false(.upstream_stale(helper_on_raw, objs))
  # a node already visited is not walked again
  expect_false(.upstream_stale(mid, objs, seen = "mid"))

  cache <- new.env(parent = emptyenv())
  expect_true(.upstream_stale(mid, objs, cache = cache))
  expect_true(cache[["mid"]])
  # the cached decision wins over the files
  cache[["mid"]] <- FALSE
  expect_false(.upstream_stale(mid, objs, cache = cache))
})

# ---- hash-based staleness ------------------------------------------------------------------------

hash_fixture <- function(env = parent.frame()) {
  d <- tmp_dir(env)
  make_file(d, "raw.csv", "a,b\n1,2", hours = 1)
  make_file(d, "make.R", "write.csv(1)", hours = 2)
  make_file(d, "out.csv", "x\n1", hours = 3)
  raw <- FilePath("raw", file.path(d, "raw.csv"))
  out <- FilePath("out", file.path(d, "out.csv"))
  prod <- FileOutputs("make", file.path(d, "make.R"), dependencies = list(raw), output = list(out), renders = FALSE)
  list(dir = d, raw = raw, out = out, prod = prod, objs = list(raw, prod), hf = file.path(d, "hashes.json"))
}

test_that("record_pipeline_hashes() writes a store that is_stale(stale_by = 'hash') accepts", {
  fx <- hash_fixture()
  res <- withVisible(record_pipeline_hashes(fx$objs, hash_file = fx$hf))
  expect_false(res$visible)
  expect_identical(res$value, fx$hf)
  expect_true(file.exists(fx$hf))

  store <- jsonlite::fromJSON(fx$hf, simplifyVector = FALSE)
  expect_identical(store$version, "1.0")
  expect_match(store$updated_at, "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$")
  expect_match(store$files$raw, "^[0-9a-f]{64}$")
  expect_identical(store$outputs$out$producer, "make")
  expect_identical(store$outputs$out$output_hash, store$files$out)
  expect_true(all(c("make", "raw") %in% names(store$outputs$out$source_hashes)))

  expect_false(is_stale(fx$out, fx$prod, stale_by = "hash", hash_file = fx$hf))
})

test_that("hash staleness ignores timestamps and reacts to content", {
  fx <- hash_fixture()
  record_pipeline_hashes(fx$objs, hash_file = fx$hf)
  check <- function() {
    prod <- FileOutputs(
      "make", file.path(fx$dir, "make.R"),
      dependencies = list(FilePath("raw", file.path(fx$dir, "raw.csv"))),
      output = list(FilePath("out", file.path(fx$dir, "out.csv"))), renders = FALSE
    )
    is_stale(prod@output[[1]], prod, stale_by = "hash", hash_file = fx$hf)
  }

  # a newer timestamp with the same content is not stale
  touch(file.path(fx$dir, "make.R"), 100)
  expect_false(check())
  # a changed producer script is
  writeLines("write.csv(2)", file.path(fx$dir, "make.R"))
  touch(file.path(fx$dir, "make.R"), 2)
  expect_true(check())
  record_pipeline_hashes(fx$objs, hash_file = fx$hf)
  expect_false(check())
  # a changed output is
  writeLines("x\n999", file.path(fx$dir, "out.csv"))
  expect_true(check())
})

test_that("hash staleness treats a missing output, a directory and an unrecorded output as stale", {
  fx <- hash_fixture()
  record_pipeline_hashes(fx$objs, hash_file = fx$hf)
  gone <- FilePath("gone", file.path(fx$dir, "gone.csv"))
  expect_true(is_stale(gone, fx$prod, stale_by = "hash", hash_file = fx$hf))
  adir <- file.path(fx$dir, "adir")
  dir.create(adir)
  expect_true(is_stale(FilePath("adir", adir), fx$prod, stale_by = "hash", hash_file = fx$hf))
  # an existing output that was never recorded
  make_file(fx$dir, "new.csv", hours = 4)
  expect_true(is_stale(FilePath("new", file.path(fx$dir, "new.csv")), fx$prod, stale_by = "hash", hash_file = fx$hf))
  # no store at all
  expect_true(is_stale(fx$out, fx$prod, stale_by = "hash", hash_file = file.path(fx$dir, "none.json")))
})

test_that("a changed input recorded with the output makes the producer stale by hash", {
  fx <- hash_fixture()
  record_pipeline_hashes(fx$objs, hash_file = fx$hf)
  # the input is a source of the producer, so editing it must be noticed
  helper <- FileUses("helper", file.path(fx$dir, "raw.csv"))
  prod <- FileOutputs("make", file.path(fx$dir, "make.R"), dependencies = list(helper), output = list(fx$out), renders = FALSE)
  expect_false(is_stale(fx$out, prod, stale_by = "hash", hash_file = fx$hf))
  writeLines("a,b\n9,9", file.path(fx$dir, "raw.csv"))
  expect_true(is_stale(fx$out, prod, stale_by = "hash", hash_file = fx$hf))
})

test_that("a flat {file: sha256} store is accepted when no output record exists", {
  fx <- hash_fixture()
  flat <- list(files = list(
    out = .file_content_hash(file.path(fx$dir, "out.csv")),
    make = .file_content_hash(file.path(fx$dir, "make.R")),
    raw = .file_content_hash(file.path(fx$dir, "raw.csv"))
  ))
  jsonlite::write_json(flat, fx$hf, auto_unbox = TRUE)
  expect_false(is_stale(fx$out, fx$prod, stale_by = "hash", hash_file = fx$hf))

  writeLines("changed", file.path(fx$dir, "make.R"))
  expect_true(is_stale(fx$out, fx$prod, stale_by = "hash", hash_file = fx$hf))

  # an output whose recorded hash differs
  flat$files$out <- strrep("0", 64)
  jsonlite::write_json(flat, fx$hf, auto_unbox = TRUE)
  expect_true(is_stale(fx$out, fx$prod, stale_by = "hash", hash_file = fx$hf))
})

test_that("hash staleness follows a stale producer further up when all_objects is given", {
  d <- tmp_dir()
  make_file(d, "raw.csv", "r", hours = 1)
  make_file(d, "step1.R", "s1", hours = 2)
  make_file(d, "mid.csv", "m", hours = 3)
  make_file(d, "step2.R", "s2", hours = 4)
  make_file(d, "final.csv", "f", hours = 5)
  hf <- file.path(d, "h.json")
  build <- function() {
    raw <- FilePath("raw", file.path(d, "raw.csv"))
    mid <- FilePath("mid", file.path(d, "mid.csv"))
    final <- FilePath("final", file.path(d, "final.csv"))
    step1 <- FileOutputs("step1", file.path(d, "step1.R"), dependencies = list(raw), output = list(mid), renders = FALSE)
    step2 <- FileOutputs("step2", file.path(d, "step2.R"), dependencies = list(mid), output = list(final), renders = FALSE)
    list(final = final, step2 = step2, objs = list(raw, step1, step2))
  }
  p <- build()
  record_pipeline_hashes(p$objs, hash_file = hf)
  expect_false(is_stale(p$final, p$step2, all_objects = p$objs, stale_by = "hash", hash_file = hf))
  # the first step's script changes: mid is out of date, and so is final through it
  writeLines("s1 changed", file.path(d, "step1.R"))
  p <- build()
  expect_false(is_stale(p$final, p$step2, stale_by = "hash", hash_file = hf))
  expect_true(is_stale(p$final, p$step2, all_objects = p$objs, stale_by = "hash", hash_file = hf))
})

# ---- record_pipeline_hashes() --------------------------------------------------------------------

test_that("record_pipeline_hashes() accepts a single FilePath and keeps earlier entries", {
  d <- tmp_dir()
  make_file(d, "a.csv", "a")
  make_file(d, "b.csv", "b")
  hf <- file.path(d, "sub", "h.json") # the directory is created
  record_pipeline_hashes(FilePath("a", file.path(d, "a.csv")), hash_file = hf)
  record_pipeline_hashes(FilePath("b", file.path(d, "b.csv")), hash_file = hf)
  store <- jsonlite::fromJSON(hf, simplifyVector = FALSE)
  expect_setequal(names(store$files), c(file.path(d, "a.csv"), "a", file.path(d, "b.csv"), "b"))
})

test_that("record_pipeline_hashes() skips files that are missing or are directories", {
  d <- tmp_dir()
  make_file(d, "a.csv", "a")
  dir.create(file.path(d, "adir"))
  hf <- file.path(d, "h.json")
  record_pipeline_hashes(
    list(FilePath("a", file.path(d, "a.csv")), FilePath("missing", file.path(d, "nope.csv")), FilePath("adir", file.path(d, "adir"))),
    hash_file = hf
  )
  store <- jsonlite::fromJSON(hf, simplifyVector = FALSE)
  expect_true("a" %in% names(store$files))
  expect_false(any(c("missing", "adir") %in% names(store$files)))
})

test_that("record_pipeline_hashes() finds the deliverables beside a rendering document", {
  d <- tmp_dir()
  qmd <- make_file(d, "report.qmd", "# report")
  make_file(d, "report.pdf", "pdf")
  make_file(d, "report.docx", "docx")
  hf <- file.path(d, "h.json")
  node <- FileOutputs("report", qmd, renders = TRUE)
  record_pipeline_hashes(list(node), hash_file = hf)
  store <- jsonlite::fromJSON(hf, simplifyVector = FALSE)
  expect_setequal(names(store$outputs), c(file.path(d, "report.pdf"), "report.pdf", file.path(d, "report.docx"), "report.docx"))
  expect_identical(store$outputs[["report.pdf"]]$producer, "report")
  expect_false(file.path(d, "report.html") %in% names(store$outputs))
})

test_that("record_pipeline_hashes() drops a helper script that cannot be read from source_hashes", {
  fx <- hash_fixture()
  helper <- FileUses("helper", file.path(fx$dir, "no_such_helper.R"))
  prod <- FileOutputs("make", file.path(fx$dir, "make.R"), dependencies = list(helper), output = list(fx$out), renders = FALSE)
  record_pipeline_hashes(list(prod), hash_file = fx$hf)
  store <- jsonlite::fromJSON(fx$hf, simplifyVector = FALSE)
  expect_false("helper" %in% names(store$outputs$out$source_hashes))
  expect_true("make" %in% names(store$outputs$out$source_hashes))
})

test_that("record_pipeline_hashes() falls back to the option, then to the declared directory", {
  fx <- hash_fixture()
  opt_file <- file.path(fx$dir, "from_option.json")
  withr::with_options(list(templecbe.pipeline_hashes = opt_file), {
    expect_identical(record_pipeline_hashes(fx$objs), opt_file)
  })
  expect_true(file.exists(opt_file))

  withr::with_options(list(templecbe.pipeline_hashes = NULL), {
    got <- record_pipeline_hashes(fx$objs)
    expect_identical(got, file.path(fx$dir, "pipeline_hashes.json"))
  })
})

test_that("hash store helpers cope with absent, broken and partial stores", {
  d <- tmp_dir()
  empty <- list(version = "1.0", files = list(), outputs = list())
  expect_identical(.read_pipeline_hashes(NULL), empty)
  expect_identical(.read_pipeline_hashes(file.path(d, "none.json")), empty)

  bad <- file.path(d, "bad.json")
  writeLines("{ not json", bad)
  expect_identical(.read_pipeline_hashes(bad), empty)

  scalar <- file.path(d, "scalar.json")
  writeLines("42", scalar)
  expect_identical(.read_pipeline_hashes(scalar), empty)

  partial <- file.path(d, "partial.json")
  writeLines('{"version": "1.0"}', partial)
  got <- .read_pipeline_hashes(partial)
  expect_identical(got$files, list())
  expect_identical(got$outputs, list())
})

test_that(".resolve_hash_file() prefers argument, option, default_dir, declared dir, then the working directory", {
  d <- tmp_dir()
  make_file(d, "a.csv", "a")
  objs <- list(FilePath("a", file.path(d, "a.csv")))
  withr::local_options(templecbe.pipeline_hashes = NULL)

  expect_identical(.resolve_hash_file(hash_file = "x.json", default_dir = d), "x.json")
  withr::with_options(list(templecbe.pipeline_hashes = "opt.json"), {
    expect_identical(.resolve_hash_file(default_dir = d), "opt.json")
  })
  expect_identical(.resolve_hash_file(default_dir = d), file.path(d, "pipeline_hashes.json"))
  expect_identical(.resolve_hash_file(all_objects = objs), file.path(d, "pipeline_hashes.json"))
  # nothing usable: the working directory
  expect_identical(.resolve_hash_file(), file.path(getwd(), "pipeline_hashes.json"))
  expect_identical(
    .resolve_hash_file(all_objects = list(FilePath("m", file.path(d, "no_dir", "m.csv")))),
    file.path(getwd(), "pipeline_hashes.json")
  )
})

test_that("hash helpers: content hash and node keys", {
  d <- tmp_dir()
  p <- make_file(d, "a.txt", "hello")
  h <- .file_content_hash(p)
  expect_match(h, "^[0-9a-f]{64}$")
  expect_identical(h, .file_content_hash(p))
  expect_true(is.na(.file_content_hash(file.path(d, "none"))))
  expect_true(is.na(.file_content_hash(d)))
  expect_true(is.na(.file_content_hash(NULL)))
  expect_true(is.na(.file_content_hash(NA_character_)))
  expect_true(is.na(.file_content_hash("")))
  expect_true(is.na(.file_content_hash(c(p, p))))

  expect_identical(.hash_node_key(FilePath("n", "dir\\sub\\f.csv")), "dir/sub/f.csv")
  expect_identical(.hash_node_key("a\\b"), "a/b")
})

test_that(".collect_source_files() lists transitive helpers once and survives cycles", {
  d <- tmp_dir()
  raw <- FilePath("raw", file.path(d, "raw.csv"))
  h2 <- FileUses("h2", file.path(d, "h2.R"), dependencies = list(raw))
  h1 <- FileUses("h1", file.path(d, "h1.R"), dependencies = list(h2, raw))
  prod <- FileOutputs("make", file.path(d, "make.R"), dependencies = list(h1), renders = FALSE)
  got <- vapply(.collect_source_files(prod), function(x) x@name, character(1))
  expect_true(all(c("make", "h1", "h2", "raw") %in% got))
  expect_identical(.collect_source_files(prod, seen = "make"), list())
})

# ---- .is_renderer_stale() ------------------------------------------------------------------------

test_that(".is_renderer_stale() judges a document with no declared outputs by its deliverables", {
  d <- tmp_dir()
  qmd <- make_file(d, "report.qmd", "# r", hours = 5)
  node <- function() FileOutputs("report", qmd, renders = TRUE)

  # never rendered
  expect_true(.is_renderer_stale(node()))
  make_file(d, "report.pdf", "pdf", hours = 10)
  expect_false(.is_renderer_stale(node()))
  # the document is newer than its deliverable
  touch(qmd, 20)
  expect_true(.is_renderer_stale(node()))
})

test_that(".is_renderer_stale() judges declared outputs one by one", {
  d <- tmp_dir()
  qmd <- make_file(d, "report.qmd", "# r", hours = 5)
  make_file(d, "ok.pdf", hours = 10)
  make_file(d, "old.docx", hours = 1)
  ok <- FilePath("ok", file.path(d, "ok.pdf"))
  old <- FilePath("old", file.path(d, "old.docx"))
  expect_false(.is_renderer_stale(FileOutputs("r", qmd, output = list(ok))))
  expect_true(.is_renderer_stale(FileOutputs("r", qmd, output = list(ok, old))))
})

# ---- get_render_plan() ---------------------------------------------------------------------------

# raw -> prep (script) -> clean.rds -> rep1 (qmd) -> rep1.pdf; rep2 (qmd) reads rep1.pdf
plan_fixture <- function(env = parent.frame(), stale = TRUE) {
  d <- tmp_dir(env)
  make_file(d, "raw.csv", hours = 1)
  make_file(d, "prep.R", hours = if (stale) 30 else 2)
  make_file(d, "clean.rds", hours = 3)
  make_file(d, "rep1.qmd", hours = 4)
  make_file(d, "rep1.pdf", hours = 40)
  make_file(d, "rep2.qmd", hours = 5)
  make_file(d, "rep2.pdf", hours = 41)
  raw <- FilePath("raw", file.path(d, "raw.csv"))
  clean <- FilePath("clean", file.path(d, "clean.rds"))
  prep <- FileOutputs("prep", file.path(d, "prep.R"), dependencies = list(raw), output = list(clean), renders = FALSE)
  pdf1 <- FilePath("rep1_pdf", file.path(d, "rep1.pdf"))
  rep1 <- FileOutputs("rep1", file.path(d, "rep1.qmd"), dependencies = list(clean), output = list(pdf1))
  pdf2 <- FilePath("rep2_pdf", file.path(d, "rep2.pdf"))
  rep2 <- FileOutputs("rep2", file.path(d, "rep2.qmd"), dependencies = list(pdf1), output = list(pdf2))
  list(dir = d, objs = list(raw, prep, rep2, rep1)) # deliberately not in dependency order
}

test_that("get_render_plan() returns stale reports in dependency order", {
  fx <- plan_fixture()
  plan <- suppressMessages(get_render_plan(fx$objs))
  expect_identical(names(plan), c("rep1", "rep2"))
  expect_s4_class(plan$rep1, "FileOutputs")
})

test_that("get_render_plan() reports when everything is up to date", {
  fx <- plan_fixture(stale = FALSE)
  msgs <- testthat::capture_messages(plan <- get_render_plan(fx$objs))
  expect_match(paste(msgs, collapse = ""), "up-to-date")
  expect_identical(plan, list())
})

test_that("get_render_plan() with only_stale = FALSE plans every renderer", {
  fx <- plan_fixture(stale = FALSE)
  plan <- suppressMessages(get_render_plan(fx$objs, only_stale = FALSE))
  expect_identical(names(plan), c("rep1", "rep2"))
  # nothing to render at all
  msgs <- testthat::capture_messages(got <- get_render_plan(list(fx$objs[[1]]), only_stale = FALSE))
  expect_match(paste(msgs, collapse = ""), "No renderable")
  expect_identical(got, list())
})

test_that("get_render_plan() accepts a single object and an empty input", {
  fx <- plan_fixture()
  expect_identical(get_render_plan(list()), list())
  expect_identical(get_render_plan(NULL), list())
  one <- fx$objs[[3]] # rep2 alone: its inputs are not in the list
  msgs <- testthat::capture_messages(plan <- suppressWarnings(get_render_plan(one, only_stale = FALSE)))
  expect_match(msgs[1], "Building")
  expect_identical(names(plan), "rep2")
})

test_that("get_render_plan() selects a range with from and to, by name or position", {
  fx <- plan_fixture()
  p <- function(...) suppressMessages(get_render_plan(fx$objs, ...))
  expect_identical(names(p(from = "rep2")), "rep2")
  expect_identical(names(p(to = "rep1")), "rep1")
  expect_identical(names(p(from = 2)), "rep2")
  expect_identical(names(p(to = 1)), "rep1")
  expect_identical(names(p(from = "rep1", to = "rep2")), c("rep1", "rep2"))
  expect_error(p(from = "nope"), "'from' node not found")
  expect_error(p(from = 3), "'from' node not found")
  expect_error(p(from = 0), "'from' node not found")
  expect_error(p(to = "nope"), "'to' node not found")
  expect_error(p(to = 9), "'to' node not found")
  expect_error(p(from = "rep2", to = "rep1"), "occurs after")
})

test_that("get_render_plan() dry_run prints the plan and returns it invisibly", {
  fx <- plan_fixture()
  out <- NULL
  expect_output(
    res <- withVisible(suppressMessages(get_render_plan(fx$objs, dry_run = TRUE))),
    "Render plan \\(dry run\\):.*\\[1\\] rep1.*\\[2\\] rep2"
  )
  expect_false(res$visible)
  expect_identical(names(res$value), c("rep1", "rep2"))
})

test_that("get_render_plan() refuses a circular pipeline", {
  d <- tmp_dir()
  make_file(d, "a.qmd")
  make_file(d, "b.qmd")
  a_out <- FilePath("a_out", file.path(d, "a.pdf"))
  b_out <- FilePath("b_out", file.path(d, "b.pdf"))
  a <- FileOutputs("a", file.path(d, "a.qmd"), dependencies = list(b_out), output = list(a_out))
  b <- FileOutputs("b", file.path(d, "b.qmd"), dependencies = list(a_out), output = list(b_out))
  expect_error(suppressMessages(suppressWarnings(get_render_plan(list(a, b)))), "Circular dependency")
})

test_that("get_render_plan() warns about declared files that are not on disk", {
  d <- tmp_dir()
  make_file(d, "r.qmd")
  ghost <- FilePath("ghost", file.path(d, "ghost.csv"))
  r <- FileOutputs("r", file.path(d, "r.qmd"), dependencies = list(ghost), output = list(FilePath("r_pdf", file.path(d, "r.pdf"))))
  expect_warning(suppressMessages(get_render_plan(list(r))), "not found on disk.*ghost")
})

test_that("get_render_plan() names an out-of-date helper producer and plans the reports behind it", {
  fx <- plan_fixture(stale = TRUE)
  # drop the reports that read the helper's output: nothing is left to render
  helper_only <- fx$objs[1:2]
  msgs <- testthat::capture_messages(plan <- get_render_plan(helper_only))
  expect_identical(plan, list())
  expect_match(paste(msgs, collapse = ""), "Out of date, but not rendered here: prep")
  expect_match(paste(msgs, collapse = ""), "no report reads the outputs")
})

test_that("get_render_plan() with stale_by = 'hash' follows content, not timestamps", {
  fx <- plan_fixture(stale = FALSE)
  hf <- file.path(fx$dir, "hashes.json")
  record_pipeline_hashes(fx$objs, hash_file = hf)
  expect_identical(suppressMessages(get_render_plan(fx$objs, stale_by = "hash", hash_file = hf)), list())

  # newer mtime, same content: still up to date
  touch(file.path(fx$dir, "prep.R"), 500)
  expect_identical(suppressMessages(get_render_plan(fx$objs, stale_by = "hash", hash_file = hf)), list())

  # a content change in the first report's source rebuilds it and the report that reads it
  writeLines("# changed", file.path(fx$dir, "rep1.qmd"))
  plan <- suppressMessages(get_render_plan(fx$objs, stale_by = "hash", hash_file = hf))
  expect_identical(names(plan), c("rep1", "rep2"))
})

# ---- save_pipeline() / load_pipeline() -----------------------------------------------------------

test_that("save_pipeline() and load_pipeline() round-trip a pipeline", {
  fx <- plan_fixture()
  rds <- file.path(fx$dir, "pipe.rds")
  res <- withVisible(save_pipeline(fx$objs, rds))
  expect_false(res$visible)
  expect_identical(res$value, rds)

  payload <- readRDS(rds)
  expect_named(payload, c("saved_at", "templecbe_version", "pipeline"))
  expect_identical(payload$templecbe_version, as.character(utils::packageVersion("TempleCBE")))

  back <- load_pipeline(rds)
  expect_identical(vapply(back, function(x) x@name, ""), vapply(fx$objs, function(x) x@name, ""))
  expect_s4_class(back[[2]], "FileOutputs")
  expect_identical(back[[2]]@output[[1]]@name, "clean")
})

test_that("save_pipeline() accepts a single object and rejects bad input", {
  d <- tmp_dir()
  f <- FilePath("a", make_file(d, "a.csv"))
  rds <- file.path(d, "one.rds")
  save_pipeline(f, rds)
  expect_length(load_pipeline(rds), 1L)
  expect_error(save_pipeline(list(), rds), "non-empty list")
  expect_error(save_pipeline("not a pipeline", rds), "non-empty list")
  expect_error(save_pipeline(list(f, f), rds), "Duplicate pipeline object name")
})

test_that("load_pipeline() refreshes modification times from disk unless told not to", {
  d <- tmp_dir()
  p_in <- make_file(d, "in.csv", hours = 1)
  p_out <- make_file(d, "out.csv", hours = 2)
  p_make <- make_file(d, "make.R", hours = 3)
  inp <- FilePath("inp", p_in)
  out <- FilePath("out", p_out)
  prod <- FileOutputs("make", p_make, dependencies = list(inp), output = list(out), renders = FALSE)
  rds <- file.path(d, "pipe.rds")
  save_pipeline(list(inp, prod), rds)

  touch(p_in, 50)
  touch(p_make, 60)
  unlink(p_out) # the output vanishes

  fresh <- load_pipeline(rds)
  expect_equal(as.numeric(fresh[[1]]@mtime), as.numeric(t0 + 50 * 3600))
  expect_equal(as.numeric(fresh[[2]]@mtime), as.numeric(t0 + 60 * 3600))
  expect_equal(as.numeric(fresh[[2]]@dependencies[[1]]@mtime), as.numeric(t0 + 50 * 3600))
  expect_true(is.na(fresh[[2]]@output[[1]]@mtime))

  frozen <- load_pipeline(rds, update_mtime = FALSE)
  expect_equal(as.numeric(frozen[[1]]@mtime), as.numeric(t0 + 1 * 3600))
  expect_equal(as.numeric(frozen[[2]]@output[[1]]@mtime), as.numeric(t0 + 2 * 3600))
})

test_that("load_pipeline() reads a bare list and refuses other content", {
  d <- tmp_dir()
  f <- FilePath("a", make_file(d, "a.csv"))
  bare <- file.path(d, "bare.rds")
  saveRDS(list(f), bare)
  expect_s4_class(load_pipeline(bare)[[1]], "FilePath")

  scalar <- file.path(d, "scalar.rds")
  saveRDS(42, scalar)
  expect_error(load_pipeline(scalar), "does not contain a valid pipeline list")
  expect_error(load_pipeline(file.path(d, "missing.rds")), "Pipeline file not found")
})

test_that("load_pipeline() leaves elements that are not pipeline files as they are", {
  d <- withr::local_tempdir()
  rds <- file.path(d, "mixed.rds")
  saveRDS(list(FilePath("a", make_file(d, "a.csv")), "note"), rds)
  back <- load_pipeline(rds)
  expect_identical(back[[2]], "note")
})

test_that("hash staleness skips a recorded source that has since been deleted", {
  fx <- hash_fixture()
  helper <- FileUses("helper", file.path(fx$dir, "helper.R"))
  writeLines("h", helper@path)
  prod <- FileOutputs("make", file.path(fx$dir, "make.R"), dependencies = list(helper), output = list(fx$out), renders = FALSE)
  # recorded store: the producer and its helper are known
  record_pipeline_hashes(list(prod), hash_file = fx$hf)
  unlink(helper@path)
  expect_false(is_stale(fx$out, prod, stale_by = "hash", hash_file = fx$hf))

  # same through the flat store
  flat <- list(files = list(
    out = .file_content_hash(file.path(fx$dir, "out.csv")),
    make = .file_content_hash(file.path(fx$dir, "make.R"))
  ))
  jsonlite::write_json(flat, fx$hf, auto_unbox = TRUE)
  expect_false(is_stale(fx$out, prod, stale_by = "hash", hash_file = fx$hf))
})
