# Key handling for the deterministic (HMAC) pseudonym methods, and the
# mapping-file guard of method = "token".
#
# Known answers are HMAC-SHA256(key, name), first 16 hex characters, computed
# independently with Python's hmac/hashlib. scripts/test_pi_anonymizer.py
# asserts the same literals so the two implementations stay in step.
kat_key_one <- c(Smith = "PI_f8f3433971630c44", Jones = "PI_d030fc55b2278d48")
kat_key_two <- c(Smith = "PI_6673769926c917e6", Jones = "PI_5e2a875f40216d2b")
# What earlier versions produced silently when no key was set (a public constant).
kat_default <- c(Smith = "PI_f4fb09b954d3ec06", Jones = "PI_2b617d93d04b752e")

test_that("HMAC methods stop when no key is supplied", {
  withr::local_envvar(TEMPLECBE_SECRET_KEY = NA)

  expect_error(anonymize_pi(c("Smith", "Jones")), "TEMPLECBE_SECRET_KEY")
  expect_error(anonymize_pi("Smith", method = "hmac_token"), "TEMPLECBE_SECRET_KEY")
  expect_error(anonymize_pi("Smith", method = "hmac_surname"), "TEMPLECBE_SECRET_KEY")
  expect_error(anonymize_pi("Smith", key = ""), "TEMPLECBE_SECRET_KEY")
  expect_error(generate_pseudonym_token("Smith"), "TEMPLECBE_SECRET_KEY")

  # The message says where to put the key and how to opt out on purpose
  expect_error(anonymize_pi("Smith"), "\\.Renviron")
  expect_error(anonymize_pi("Smith"), "allow_default_key")

  # An empty environment variable counts as "not set"
  withr::local_envvar(TEMPLECBE_SECRET_KEY = "")
  expect_error(anonymize_pi("Smith"), "TEMPLECBE_SECRET_KEY")

  # The check does not depend on which values happen to be in the data
  expect_error(anonymize_pi(NA_character_), "TEMPLECBE_SECRET_KEY")
  expect_error(generate_pseudonym_token(character(0)), "TEMPLECBE_SECRET_KEY")
})

test_that("a key must be a single non-missing string", {
  expect_error(anonymize_pi("Smith", key = NA_character_), "`key` must be")
  expect_error(anonymize_pi("Smith", key = c("a", "b")), "`key` must be")
  expect_error(anonymize_pi("Smith", key = 42), "`key` must be")
  expect_error(generate_pseudonym_token("Smith", key = NULL), "`key` must be")
})

test_that("an explicit key or the environment variable gives deterministic pseudonyms", {
  withr::local_envvar(TEMPLECBE_SECRET_KEY = NA)
  nm <- c("Smith", "Jones")

  a1 <- anonymize_pi(nm, key = "key-one")
  expect_identical(a1, unname(kat_key_one))                       # algorithm unchanged
  expect_identical(a1, anonymize_pi(nm, key = "key-one"))         # deterministic
  expect_identical(generate_pseudonym_token(nm, key = "key-one"), a1)
  # non-ASCII names hash as UTF-8, the same bytes Python hashes
  expect_identical(anonymize_pi("Nuñez", key = "key-one"), "PI_3b54cb9a31e625ba")

  # the environment variable is equivalent to passing `key`
  withr::with_envvar(c(TEMPLECBE_SECRET_KEY = "key-one"), {
    expect_identical(anonymize_pi(nm), a1)
    expect_identical(generate_pseudonym_token(nm), a1)
  })

  # different keys -> different tokens
  b1 <- anonymize_pi(nm, key = "key-two")
  expect_identical(b1, unname(kat_key_two))
  expect_false(any(a1 == b1))

  # ...and different surnames (hmac_surname is keyed the same way)
  many <- c("Smith", "Jones", "Patel", "Garcia", "Nguyen", "Okafor", "Ivanov", "Rossi")
  s1 <- anonymize_pi(many, method = "hmac_surname", key = "key-one")
  s2 <- anonymize_pi(many, method = "hmac_surname", key = "key-two")
  expect_identical(s1, anonymize_pi(many, method = "hmac_surname", key = "key-one"))
  expect_false(identical(s1, s2))
})

test_that("allow_default_key = TRUE keeps the old fallback but warns that it is not secret", {
  withr::local_envvar(TEMPLECBE_SECRET_KEY = NA)
  nm <- c("Smith", "Jones")

  expect_warning(res <- anonymize_pi(nm, allow_default_key = TRUE), "NOT secret")
  expect_identical(res, unname(kat_default))                       # same output as before

  expect_warning(
    sur <- anonymize_pi(nm, method = "hmac_surname", allow_default_key = TRUE), "NOT secret"
  )
  expect_length(sur, 2)
  expect_warning(
    tok <- generate_pseudonym_token(nm, allow_default_key = TRUE), "NOT secret"
  )
  expect_identical(tok, unname(kat_default))

  # one warning per call, not one per name
  expect_length(testthat::capture_warnings(anonymize_pi(c("A", "B", "C"), allow_default_key = TRUE)), 1)

  # no warning when a real key is present, even with the opt-out set
  expect_no_warning(anonymize_pi(nm, key = "key-one", allow_default_key = TRUE))
  withr::with_envvar(c(TEMPLECBE_SECRET_KEY = "key-one"), {
    expect_no_warning(anonymize_pi(nm, allow_default_key = TRUE))
  })

  expect_error(anonymize_pi(nm, allow_default_key = NA), "allow_default_key")
})

test_that("the safe default is pinned so flipping it is a deliberate change", {
  expect_false(formals(anonymize_pi)$allow_default_key)
  expect_false(formals(generate_pseudonym_token)$allow_default_key)
  # documented default token length
  withr::local_envvar(TEMPLECBE_SECRET_KEY = NA)
  expect_equal(nchar(anonymize_pi("Smith", key = "k")), nchar("PI_") + 16)
  expect_equal(nchar(generate_pseudonym_token("Smith", key = "k")), nchar("PI_") + 16)
})

test_that("token and synthetic methods work without a key and never use the public salt", {
  withr::local_envvar(TEMPLECBE_SECRET_KEY = NA)
  f <- file.path(withr::local_tempdir(), "map.json")

  expect_no_warning(
    t1 <- anonymize_pi(c("Smith", "Jones", "Smith"), method = "token", secrets_path = f)
  )
  expect_identical(t1[1], t1[3])
  expect_false(t1[1] == t1[2])
  # not derived from the public constant: anyone could precompute those
  expect_false(any(t1[1:2] == kat_default))
  # stable across calls because the mapping is persisted
  expect_identical(anonymize_pi(c("Smith", "Jones"), method = "token", secrets_path = f), t1[1:2])
  # a second mapping file is independent (no shared secret to recompute from)
  f2 <- file.path(withr::local_tempdir(), "map.json")
  expect_false(anonymize_pi("Smith", method = "token", secrets_path = f2) == t1[1])

  # prefix / length / rename options still apply to random tokens
  f3 <- file.path(withr::local_tempdir(), "map.json")
  short <- anonymize_pi("Smith", method = "token", secrets_path = f3,
                        prefix = "INV-", n_chars = 6, rename_fn = tolower)
  expect_match(short, "^inv-[0-9a-f]{6}$")

  # with a key, the mapping file gets the keyed token, unchanged from before
  f4 <- file.path(withr::local_tempdir(), "map.json")
  expect_identical(
    anonymize_pi(c("Smith", "Jones"), method = "token", key = "key-one", secrets_path = f4),
    unname(kat_key_one)
  )

  expect_no_error(anonymize_pi(c("A", "B"), method = "synthetic", seed = 1))
  expect_no_warning(anonymize_pi(c("A", "B"), method = "synthetic", seed = 1))
})

test_that("NA handling is unchanged", {
  withr::local_envvar(TEMPLECBE_SECRET_KEY = NA)
  x <- c("Smith", NA, "Jones", NA)
  f <- file.path(withr::local_tempdir(), "map.json")

  for (m in c("hmac_token", "hmac_surname")) {
    r <- anonymize_pi(x, method = m, key = "key-one")
    expect_length(r, 4)
    expect_identical(is.na(r), is.na(x))
  }
  r_tok <- anonymize_pi(x, method = "token", secrets_path = f)
  expect_identical(is.na(r_tok), is.na(x))
  r_syn <- anonymize_pi(x, method = "synthetic", seed = 3)
  expect_identical(is.na(r_syn), is.na(x))

  # all-NA input with a usable key, or a method that needs no key
  all_na <- c(NA_character_, NA_character_)
  expect_identical(anonymize_pi(all_na, key = "key-one"), all_na)
  expect_identical(anonymize_pi(all_na, method = "token", secrets_path = f), all_na)
  expect_identical(anonymize_pi(all_na, method = "synthetic"), all_na)

  vec <- generate_pseudonym_token(c("Smith", NA), key = "key-one")
  expect_identical(vec, c(unname(kat_key_one["Smith"]), NA_character_))
})

test_that("mapping-file guard: nothing is written or created inside a repository tree", {
  outside <- withr::local_tempdir()
  git_dir <- withr::local_tempdir()
  dir.create(file.path(git_dir, ".git"))
  git_file <- withr::local_tempdir()
  writeLines("gitdir: /somewhere/else", file.path(git_file, ".git"))   # worktree / submodule marker
  withr::local_envvar(TEMPLECBE_SECRET_KEY = NA)

  for (repo in c(git_dir, git_file)) {
    # cwd outside the repository: the path itself decides
    target <- file.path(repo, "new", "sub", "map.json")
    withr::with_dir(outside, {
      expect_error(anonymize_pi("X", method = "token", secrets_path = target), "Refusing")
      expect_error(
        anonymize_pi("X", method = "token",
                     secrets_path = file.path(outside, "..", basename(repo), "map.json")),
        "Refusing"
      )
    })
    expect_false(dir.exists(file.path(repo, "new")))           # not even an empty directory
    expect_false(file.exists(file.path(repo, "map.json")))

    # default location moved into the repository through R_USER_DATA_DIR
    withr::with_envvar(c(R_USER_DATA_DIR = file.path(repo, "userdata")), withr::with_dir(outside, {
      expect_error(anonymize_pi("X", method = "token"), "Refusing")
    }))
    expect_false(dir.exists(file.path(repo, "userdata")))

    # cwd inside the repository with a relative path
    withr::with_dir(repo, {
      expect_error(anonymize_pi("X", method = "token", secrets_path = "map.json"), "Refusing")
    })
    expect_false(file.exists(file.path(repo, "map.json")))
  }

  # the default location honours R_USER_DATA_DIR and sits outside the working tree
  udata <- withr::local_tempdir()
  withr::with_envvar(c(R_USER_DATA_DIR = udata), {
    p <- default_secrets_path()
    expect_equal(basename(p), "pi_mapping.json")
    # p is <udata>/R/TempleCBE/pi_mapping.json: the file is not there yet, its directory is
    expect_path_inside(p, udata)
  })
})

test_that("standalone R script refuses repository paths whatever the working directory", {
  script <- testthat::test_path("..", "..", "scripts", "pi_anonymizer.R")
  skip_if_not(file.exists(script), "scripts/ is not part of the built package")
  env <- new.env(parent = globalenv())
  sys.source(script, envir = env)

  outside <- withr::local_tempdir()
  git_dir <- withr::local_tempdir()
  dir.create(file.path(git_dir, ".git"))
  git_file <- withr::local_tempdir()
  writeLines("gitdir: /somewhere/else", file.path(git_file, ".git"))

  for (repo in c(git_dir, git_file)) {
    withr::with_dir(outside, {
      expect_error(env$anonymize_pi("X", secrets_path = file.path(repo, "map.json")), "Refusing")
    })
    withr::with_dir(repo, {
      expect_error(env$anonymize_pi("X", secrets_path = file.path(repo, "map.json")), "Refusing")
    })
    expect_false(file.exists(file.path(repo, "map.json")))
  }

  ok <- file.path(outside, "map.json")
  tok <- withr::with_dir(outside, env$anonymize_pi("X", secrets_path = ok))
  expect_match(tok, "^PI_[0-9a-f]{16}$")
  expect_identical(withr::with_dir(outside, env$anonymize_pi("X", secrets_path = ok)), tok)
})

test_that("standalone R script supports vectorization, NA handling, parent dir creation, and seed preservation", {
  script <- testthat::test_path("..", "..", "scripts", "pi_anonymizer.R")
  skip_if_not(file.exists(script), "scripts/ is not part of the built package")
  env <- new.env(parent = globalenv())
  sys.source(script, envir = env)

  outside <- withr::local_tempdir()
  nested_target <- file.path(outside, "nested", "dir", "map.json")

  # Vectorization and NA/whitespace handling (A4-15)
  res <- env$anonymize_pi(c("Smith", "", "   ", NA, "Jones"), secrets_path = nested_target)
  expect_equal(length(res), 5L)
  expect_true(grepl("^PI_[0-9a-f]{16}$", res[1]))
  expect_true(is.na(res[2]))
  expect_true(is.na(res[3]))
  expect_true(is.na(res[4]))
  expect_true(grepl("^PI_[0-9a-f]{16}$", res[5]))
  expect_false(res[1] == res[5])

  # Parent directory was automatically created outside repo
  expect_true(file.exists(nested_target))

  # Local RNG preservation (A4-16)
  set.seed(42)
  seed_before <- get(".Random.seed", envir = .GlobalEnv)
  names_gen <- env$generate_pi_names(5, seed = 999)
  seed_after <- get(".Random.seed", envir = .GlobalEnv)
  expect_identical(seed_before, seed_after)

  # Deterministic seeded tokens
  toks1 <- env$generate_pi_names(3, format = "token", seed = 888)
  toks2 <- env$generate_pi_names(3, format = "token", seed = 888)
  expect_identical(toks1, toks2)
  expect_equal(length(toks1), 3L)
})

test_that("standalone Python script agrees with R and enforces the same key rules", {
  script <- testthat::test_path("..", "..", "scripts", "test_pi_anonymizer.py")
  skip_if_not(file.exists(script), "scripts/ is not part of the built package")
  py <- Sys.which(c("python3", "python"))
  py <- unname(py[nzchar(py)][1])
  skip_if(is.na(py), "python is not available")
  out <- suppressWarnings(system2(py, shQuote(normalizePath(script)), stdout = TRUE, stderr = TRUE))
  status <- attr(out, "status")
  expect_true(is.null(status) || identical(status, 0L), info = paste(out, collapse = "\n"))
})

# A repository with a subdirectory, a link to the repository root, a link to that
# subdirectory, and a directory outside it. The link to the subdirectory is the
# awkward one: the repository root is not among the ancestors of the path as
# spelled, and normalizePath() leaves a path that does not exist yet exactly as
# written, so the `.git` walk only reaches the root if the part that does exist is
# resolved first.
link_fixture <- function(env = parent.frame()) {
  base <- withr::local_tempdir(.local_envir = env)
  repo <- file.path(base, "repo")
  dir.create(file.path(repo, ".git"), recursive = TRUE)
  dir.create(file.path(repo, "sub"))
  outside <- file.path(base, "outside")
  dir.create(outside)
  link_root <- file.path(base, "link_root")
  link_sub <- file.path(base, "link_sub")
  skip_if_not(
    make_dir_link(repo, link_root) && make_dir_link(file.path(repo, "sub"), link_sub),
    "cannot create directory links here"
  )
  list(repo = repo, outside = outside, link_root = link_root, link_sub = link_sub)
}

test_that(".canonical_path() resolves the part of a path that exists and keeps the rest", {
  d <- withr::local_tempdir()
  d_norm <- normalizePath(d, winslash = "/")

  expect_identical(.canonical_path(d), d_norm)
  expect_identical(.canonical_path(paste0(d, "/")), d_norm)
  expect_identical(.canonical_path(file.path(d, "a", "b", "map.json")), file.path(d_norm, "a", "b", "map.json"))
  # `..` in the part that exists is resolved
  dir.create(file.path(d, "x"))
  expect_identical(.canonical_path(file.path(d, "x", "..", "new")), file.path(d_norm, "new"))
  # a relative path that does not exist yet becomes absolute
  expect_identical(withr::with_dir(d, .canonical_path(file.path("a", "map.json"))), file.path(d_norm, "a", "map.json"))
  # nothing but the root exists: no doubled separator
  root_case <- .canonical_path("/templecbe-no-such-top-level-dir/map.json")
  expect_false(grepl("//", root_case, fixed = TRUE))
  expect_match(root_case, "/templecbe-no-such-top-level-dir/map.json$")
})

test_that(".canonical_path() keeps a path whose root does not exist as it was given", {
  skip_if_not(.Platform$OS.type == "windows", "needs a drive letter that is not mapped")
  free <- Filter(function(l) !dir.exists(paste0(l, ":/")), LETTERS[-(1:3)])
  skip_if(length(free) == 0L, "every drive letter is in use")
  p <- paste0(free[[1]], ":/no-such-dir/map.json")
  expect_identical(.canonical_path(p), p)   # not cut back to "<drive>:/"
})

test_that("mapping-file guard: a repository reached through a directory link is refused and left untouched", {
  fx <- link_fixture()
  withr::local_envvar(TEMPLECBE_SECRET_KEY = NA)
  entries <- function() list.files(fx$repo, recursive = TRUE, include.dirs = TRUE, all.files = TRUE)
  before <- entries()

  targets <- c(
    file.path(fx$link_root, "new", "sub", "map.json"),
    file.path(fx$link_sub, "map.json"),
    file.path(fx$link_sub, "newdir", "map.json"),
    file.path(fx$link_sub, "a", "b", "map.json")
  )
  for (target in targets) {
    withr::with_dir(fx$outside, {
      expect_error(anonymize_pi("X", method = "token", secrets_path = target), "Refusing", info = target)
    })
    expect_identical(entries(), before, info = target)   # not even an empty directory
  }

  # default location moved into the repository through a link in R_USER_DATA_DIR
  for (udata in file.path(c(fx$link_root, fx$link_sub), "userdata")) {
    withr::with_envvar(c(R_USER_DATA_DIR = udata), withr::with_dir(fx$outside, {
      expect_error(anonymize_pi("X", method = "token"), "Refusing", info = udata)
    }))
    expect_identical(entries(), before, info = udata)
  }
})

# What scripts/pi_anonymizer.R does with each `secrets_path`, run from `wd` in a clean
# session so the script stands on its own: with the package attached it would find
# the package's own validate_secrets_dir() and use that instead.
standalone_outcomes <- function(script, targets, wd) {
  callr::r(
    function(script, targets, wd) {
      Sys.unsetenv("TEMPLECBE_SECRET_KEY")
      setwd(wd)
      env <- new.env(parent = globalenv())
      sys.source(script, envir = env)
      vapply(targets, function(target) {
        tryCatch(
          {
            env$anonymize_pi("X", secrets_path = target)
            "written"
          },
          error = function(e) if (grepl("Refusing", conditionMessage(e))) "refused" else conditionMessage(e)
        )
      }, character(1))
    },
    args = list(normalizePath(script), targets, wd)
  )
}

test_that("standalone R script refuses a repository reached through a directory link", {
  script <- testthat::test_path("..", "..", "scripts", "pi_anonymizer.R")
  skip_if_not(file.exists(script), "scripts/ is not part of the built package")
  skip_if_not_installed("callr")
  fx <- link_fixture()
  targets <- c(
    file.path(fx$link_root, "new", "sub", "map.json"),
    file.path(fx$link_sub, "map.json"),
    file.path(fx$link_sub, "newdir", "map.json")
  )

  outcome <- standalone_outcomes(script, targets, fx$outside)
  expect_identical(
    unname(outcome), rep("refused", length(targets)),
    info = paste(targets, outcome, sep = " -> ", collapse = "\n")
  )
  expect_identical(list.files(fx$repo, recursive = TRUE, include.dirs = TRUE, all.files = TRUE), c(".git", "sub"))
})

# The working directory is inside a repository but is not its root, and the folders a
# relative path names do not exist yet. On Linux and macOS normalizePath() leaves such
# a path exactly as written, so the walk up from it saw only the relative spelling
# and never reached the repository above the working directory.
relative_fixture <- function(env = parent.frame()) {
  base <- withr::local_tempdir(.local_envir = env)
  repo <- file.path(base, "repo")
  dir.create(file.path(repo, ".git"), recursive = TRUE)
  cwd <- file.path(repo, "sub", "deeper")
  dir.create(cwd, recursive = TRUE)
  list(repo = repo, cwd = cwd,
       targets = c(file.path("new", "map.json"),
                   file.path("new", "sub", "map.json"),
                   file.path("..", "newdir", "map.json")))
}

test_that("mapping-file guard: a relative path to folders that do not exist yet is refused from a repository subfolder", {
  fx <- relative_fixture()
  withr::local_envvar(TEMPLECBE_SECRET_KEY = NA)
  entries <- function() list.files(fx$repo, recursive = TRUE, include.dirs = TRUE, all.files = TRUE)
  before <- entries()

  for (target in fx$targets) {
    withr::with_dir(fx$cwd, {
      expect_error(anonymize_pi("X", method = "token", secrets_path = target), "Refusing", info = target)
    })
    expect_identical(entries(), before, info = target)   # not even an empty directory
  }

  # default location moved there through a relative R_USER_DATA_DIR
  withr::with_envvar(c(R_USER_DATA_DIR = "userdata"), withr::with_dir(fx$cwd, {
    expect_error(anonymize_pi("X", method = "token"), "Refusing")
  }))
  expect_identical(entries(), before)
})

test_that("standalone R script refuses a relative path to folders that do not exist yet from a repository subfolder", {
  script <- testthat::test_path("..", "..", "scripts", "pi_anonymizer.R")
  skip_if_not(file.exists(script), "scripts/ is not part of the built package")
  skip_if_not_installed("callr")
  fx <- relative_fixture()

  outcome <- standalone_outcomes(script, fx$targets, fx$cwd)
  expect_identical(
    unname(outcome), rep("refused", length(fx$targets)),
    info = paste(fx$targets, outcome, sep = " -> ", collapse = "\n")
  )
  expect_identical(
    list.files(fx$repo, recursive = TRUE, include.dirs = TRUE, all.files = TRUE),
    c(".git", "sub", "sub/deeper")
  )
})
