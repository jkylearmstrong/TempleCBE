#!/usr/bin/env Rscript
# ==============================================================================
# scripts/deploy_release.R
# ------------------------------------------------------------------------------
# Automated Release & Deployment Pipeline for TempleCBE
#
# Features:
#   - Automated datetime version tagging (e.g. 0.4.45.YYYY.MM.DD.HHMM) for
#     multi-agent / multi-machine traceability and easy git clean-up.
#   - Configurable options to toggle long-running steps (build_site, run_check,
#     run_tests, render_readme).
#   - Defensive git state validation (clean working tree, branch checks,
#     discrete argument vectors, rollback on failure, CRLF protection).
#   - Nothing is pushed unless asked: push = TRUE (--push) names the remote
#     (remote = , --remote NAME; default "origin"), and branch and tag go in
#     one atomic push. When that push fails, the local release commit and tag
#     are rolled back.
#   - CLI and interactive R support.
# ==============================================================================

deploy_release <- function(
  version_tag = "auto",
  base_version = NULL,
  append_datetime = TRUE,
  datetime_format = "%Y.%m.%d.%H%M",
  utc = TRUE,
  commit_msg = NULL,
  run_tests = TRUE,
  render_readme = TRUE,
  update_description = TRUE,
  document = TRUE,
  build_site = FALSE,
  run_check = FALSE,
  commit = TRUE,
  tag = TRUE,
  push = FALSE,
  remote = "origin",
  expected_branch = "master",
  dry_run = FALSE
) {
  # ----------------------------------------------------------------------------
  # 0. Locate Repository Root, DESCRIPTION, and Git Pre-flight Checks (A5-05, A5-12, A5-25)
  # ----------------------------------------------------------------------------
  repo_root <- tryCatch(
    rprojroot::find_root(rprojroot::is_r_package),
    error = function(e) getwd()
  )
  repo_root <- normalizePath(repo_root, winslash = "/")

  desc_path <- file.path(repo_root, "DESCRIPTION")
  if (!file.exists(desc_path)) {
    stop("Could not locate DESCRIPTION file at: ", desc_path, call. = FALSE)
  }

  desc_lines_orig <- readLines(desc_path, encoding = "UTF-8", warn = FALSE)
  ver_line_idx <- grep("^Version:\\s*", desc_lines_orig)
  if (length(ver_line_idx) == 0L) {
    stop("Could not find 'Version:' entry in DESCRIPTION.", call. = FALSE)
  }
  current_pkg_ver <- sub("^Version:\\s*", "", desc_lines_orig[ver_line_idx[1L]])

  # Resolve git executable (A5-25)
  git_exe <- Sys.which("git")
  if (!nzchar(git_exe)) {
    stop("git executable not found on PATH.", call. = FALSE)
  }
  # every argument is quoted: system2() joins them with spaces, so a commit message of several words
  # (the default one) was split into pathspecs and `git commit` failed
  run_git <- function(args) {
    system2(git_exe, shQuote(c("-C", repo_root, args)), stdout = TRUE, stderr = TRUE)
  }
  run_git_status <- function(args) {
    system2(git_exe, shQuote(c("-C", repo_root, args)))
  }

  # Verify inside git working tree
  is_git <- run_git_status(c("rev-parse", "--is-inside-work-tree"))
  if (is_git != 0L) {
    stop("Repository root '", repo_root, "' is not inside a git working tree.", call. = FALSE)
  }

  # The remote matters only when something is to be pushed. A name that git would read as an option, or
  # that is not configured, stops the run before anything changes (also in a dry run).
  push_url <- NA_character_
  if (push) {
    if (!is.character(remote) || length(remote) != 1L || is.na(remote) || !nzchar(remote) || startsWith(remote, "-")) {
      stop("`remote` is not a valid remote name: '", paste(remote, collapse = " "), "'.", call. = FALSE)
    }
    url_res <- suppressWarnings(run_git(c("remote", "get-url", "--push", "--", remote)))
    url_status <- attr(url_res, "status")
    if ((!is.null(url_status) && url_status != 0L) || !length(url_res)) {
      stop("Remote '", remote, "' is not configured in this repository (see `git remote -v`).", call. = FALSE)
    }
    # never print credentials that are part of the URL
    push_url <- sub("^([a-zA-Z0-9+_.-]+://)[^/]*@", "\\1", trimws(url_res[1L]))
  }

  # Check current branch matches expected release branch (A5-05)
  curr_branch_res <- run_git(c("rev-parse", "--abbrev-ref", "HEAD"))
  current_branch <- if (length(curr_branch_res)) trimws(curr_branch_res[1L]) else ""
  if (commit && !dry_run && nzchar(expected_branch) && !identical(current_branch, expected_branch)) {
    stop(sprintf(
      "deploy_release() must be run on branch '%s', but current branch is '%s'. Switch to '%s' before releasing.",
      expected_branch, current_branch, expected_branch
    ), call. = FALSE)
  }

  # Check clean working tree before modifying files (A5-05)
  if (commit && !dry_run) {
    porcelain_res <- run_git(c("status", "--porcelain"))
    if (length(porcelain_res) > 0L && any(nzchar(trimws(porcelain_res)))) {
      stop("Working tree has uncommitted changes or untracked files:\n",
           paste("  ", porcelain_res, collapse = "\n"),
           "\nPlease commit, stash, or clean the working tree before running deploy_release().", call. = FALSE)
    }

    # Verify git identity is configured (A5-12)
    name_check <- run_git(c("config", "user.name"))
    email_check <- run_git(c("config", "user.email"))
    if (length(name_check) == 0L || !nzchar(trimws(name_check[1L])) ||
        length(email_check) == 0L || !nzchar(trimws(email_check[1L]))) {
      stop("Git identity (user.name and user.email) is not configured. Configure git identity before releasing.", call. = FALSE)
    }
  }

  # Rollback guard: restore original DESCRIPTION if anything aborts before commit (A5-12)
  committed_successfully <- FALSE
  on.exit({
    if (update_description && !dry_run && !committed_successfully && file.exists(desc_path)) {
      message("⚠️ Release aborted before successful commit; restoring original DESCRIPTION...")
      con <- file(desc_path, open = "wb")
      writeBin(charToRaw(paste0(paste(desc_lines_orig, collapse = "\n"), "\n")), con)
      close(con)
    }
  }, add = TRUE)

  # ----------------------------------------------------------------------------
  # 1. Resolve Target Version Tag
  # ----------------------------------------------------------------------------
  if (is.null(version_tag) || version_tag == "auto" || version_tag == "") {
    if (!is.null(base_version) && nzchar(base_version)) {
      base_ver <- base_version
    } else {
      # Extract major.minor.patch (first 3 parts if available)
      ver_parts <- strsplit(current_pkg_ver, "[.-]")[[1L]]
      n_parts <- min(3L, length(ver_parts))
      base_ver <- paste(ver_parts[1L:n_parts], collapse = ".")
    }

    if (isTRUE(append_datetime)) {
      stamp <- format(if (utc) Sys.time() else Sys.time(), datetime_format, tz = if (utc) "UTC" else "")
      version_tag <- paste0(base_ver, ".", stamp)
    } else {
      version_tag <- base_ver
    }
  }

  # Validate that version tag conforms to R package version requirements (A5-11: stop on failure)
  is_valid_r_ver <- tryCatch({
    invisible(package_version(version_tag))
    TRUE
  }, error = function(e) FALSE)

  if (!is_valid_r_ver) {
    stop("Target version_tag '", version_tag, "' is not a valid numeric R package version (e.g. 0.4.45 or 0.4.45.2026.09.30.0123).", call. = FALSE)
  }

  if (is.null(commit_msg) || !nzchar(commit_msg)) {
    commit_msg <- paste("Automated release for", version_tag)
  }

  # ----------------------------------------------------------------------------
  # Banner & Step Summary
  # ----------------------------------------------------------------------------
  message("=================================================================")
  message("           TempleCBE Automated Release Pipeline                  ")
  message("=================================================================")
  message("  Repository Root     : ", repo_root)
  message("  Expected Branch     : ", expected_branch)
  message("  Current Version     : ", current_pkg_ver)
  message("  Target Release Tag  : ", version_tag)
  message("  Commit Message      : ", commit_msg)
  message("  Dry Run             : ", if (dry_run) "YES (no changes pushed)" else "NO")
  message("-----------------------------------------------------------------")
  message("  Step 3: Run Tests        : ", if (run_tests) "ENABLED" else "SKIPPED")
  message("  Step 4: Render README    : ", if (render_readme) "ENABLED (README.qmd)" else "SKIPPED")
  message("  Step 5: Update Version   : ", if (update_description) "ENABLED (DESCRIPTION)" else "SKIPPED")
  message("  Step 6a: Document        : ", if (document) "ENABLED (devtools::document)" else "SKIPPED")
  message("  Step 6b: Build Site      : ", if (build_site) "ENABLED (pkgdown::build_site)" else "SKIPPED")
  message("  Step 7: R CMD Check      : ", if (run_check) "ENABLED (devtools::check)" else "SKIPPED")
  message("  Step 8: Git Commit       : ", if (commit) "ENABLED" else "SKIPPED")
  message("  Step 9: Git Tag          : ", if (tag) "ENABLED" else "SKIPPED")
  message(
    "  Step 10: Git Push        : ",
    if (push) paste0("ENABLED (to '", remote, "': ", push_url, ")") else "SKIPPED (add --push to push)"
  )
  message("=================================================================")

  if (dry_run) {
    message("ℹ️ [DRY RUN] Simulation mode active. No files or git state modified.")
    return(invisible(version_tag))
  }

  # ----------------------------------------------------------------------------
  # Step 3: Run Full Test Suite
  # ----------------------------------------------------------------------------
  if (run_tests) {
    message("\n🚀 Step 3: Running full test suite...")
    if (!requireNamespace("devtools", quietly = TRUE)) {
      stop("Package 'devtools' is required to run tests.")
    }
    test_results <- devtools::test()
    df_results <- as.data.frame(test_results)
    if (any(df_results$failed > 0L | df_results$error)) {
      stop("❌ Test suite encountered failures or errors! Release aborted.")
    }
    message("✅ Test suite passed cleanly.")
    unlink(c(file.path(repo_root, "Rplots.pdf"), file.path(repo_root, "tests", "testthat", "Rplots.pdf")), force = TRUE)
  }

  # ----------------------------------------------------------------------------
  # Step 4: Render README.qmd
  # ----------------------------------------------------------------------------
  if (render_readme) {
    readme_path <- file.path(repo_root, "README.qmd")
    if (file.exists(readme_path)) {
      message("\n📝 Step 4: Rendering README.qmd via Quarto...")
      if (!requireNamespace("quarto", quietly = TRUE)) {
        stop("Package 'quarto' is required to render README.qmd.")
      }
      quarto::quarto_render(readme_path)
      message("✅ README.qmd compiled to README.md.")
    } else {
      warning("README.qmd not found at ", readme_path, "; skipping render.")
    }
  }

  # ----------------------------------------------------------------------------
  # Step 5: Update DESCRIPTION Version (binary write for LF line endings, A5-25)
  # ----------------------------------------------------------------------------
  if (update_description) {
    message(sprintf("\n🏷️ Step 5: Synchronizing DESCRIPTION Version to '%s'...", version_tag))
    desc_lines <- desc_lines_orig
    desc_lines[ver_line_idx[1L]] <- paste0("Version: ", version_tag)
    con <- file(desc_path, open = "wb")
    writeBin(charToRaw(paste0(paste(desc_lines, collapse = "\n"), "\n")), con)
    close(con)
    message("✅ DESCRIPTION updated.")
  }

  # ----------------------------------------------------------------------------
  # Step 6a: Document Package
  # ----------------------------------------------------------------------------
  if (document) {
    message("\n📚 Step 6a: Regenerating roxygen documentation and NAMESPACE...")
    devtools::document(pkg = repo_root)
    message("✅ Documentation regenerated.")
  }

  # ----------------------------------------------------------------------------
  # Step 6b: Build pkgdown Site (Optional / Long-running)
  # ----------------------------------------------------------------------------
  if (build_site) {
    message("\n🌐 Step 6b: Building pkgdown site...")
    if (!requireNamespace("pkgdown", quietly = TRUE)) {
      stop("Package 'pkgdown' is required to build documentation site.")
    }
    pkgdown::build_site(pkg = repo_root, new_process = FALSE)
    message("✅ pkgdown site build complete.")
  }

  # ----------------------------------------------------------------------------
  # Step 7: R CMD Check (Optional / Long-running)
  # ----------------------------------------------------------------------------
  if (run_check) {
    message("\n🛠️ Step 7: Running R CMD check...")
    check_res <- devtools::check(pkg = repo_root, error_on = "error")
    message("✅ R CMD check passed with 0 errors.")
  }

  # ----------------------------------------------------------------------------
  # Steps 8, 9, 10: Commit, Tag, and Push (A5-05, A5-11, A5-12)
  # ----------------------------------------------------------------------------
  head_before <- trimws(run_git(c("rev-parse", "HEAD"))[1L])
  commit_created <- FALSE
  tag_created <- FALSE
  tag_name <- if (grepl("^v", version_tag)) version_tag else paste0("v", version_tag)
  # Undo what this run did to the local repository, so that a release that did not go out leaves
  # no commit and no tag behind: the working tree was clean when the run started (checked above).
  roll_back <- function() {
    if (tag_created) run_git_status(c("tag", "-d", tag_name))
    if (commit_created) run_git_status(c("reset", "--hard", head_before))
    if (commit_created || tag_created) {
      message("↩️ Rolled back the local release", if (commit_created) " commit", if (commit_created && tag_created) " and", if (tag_created) " tag", ".")
    }
  }

  if (commit) {
    message(sprintf("\n📦 Step 8: Staging changes and creating git commit '%s'...", commit_msg))

    # Stage only specific release artifacts, never indiscriminate git add . (A5-05)
    candidate_files <- c("DESCRIPTION", "NEWS.md", "README.md", "README.qmd", "man", "NAMESPACE")
    for (f in candidate_files) {
      target_f <- file.path(repo_root, f)
      if (file.exists(target_f)) {
        run_git_status(c("add", f))
      }
    }

    diff_cached <- run_git_status(c("diff", "--cached", "--quiet"))
    if (diff_cached != 0L) {
      commit_res <- run_git_status(c("commit", "-m", commit_msg))
      if (commit_res != 0L) {
        # the release files were staged: leave the index as it was (DESCRIPTION is restored on exit)
        run_git_status(c("reset", "--quiet"))
        stop("git commit failed with status code: ", commit_res, call. = FALSE)
      }
      commit_created <- TRUE
      message("✅ Git commit created.")
    } else {
      message("ℹ️ No staged changes to commit.")
    }
    committed_successfully <- TRUE
  }

  if (tag) {
    message(sprintf("\n🏷️ Step 9: Creating git tag '%s'...", tag_name))
    tag_status <- run_git_status(c("tag", "-a", tag_name, "-m", commit_msg))
    if (tag_status != 0L) {
      roll_back()
      stop("git tag command failed with status code: ", tag_status, call. = FALSE)
    }
    tag_created <- TRUE
    message("✅ Git tag created: ", tag_name)
  }

  if (push) {
    message("\n🚀 Step 10: Pushing branch and tag to '", remote, "' (", push_url, ")...")
    # One atomic push: the branch and the tag both reach the remote or neither does, so a tag is never
    # pushed without its branch (A5-05). When it fails, the local commit and tag are rolled back.
    refs <- c(paste0("HEAD:refs/heads/", expected_branch), if (tag) paste0("refs/tags/", tag_name))
    push_res <- run_git_status(c("push", "--atomic", "--", remote, refs))
    if (push_res != 0L) {
      roll_back()
      stop(
        "git push to remote '", remote, "' failed with status code: ", push_res, ". Nothing was pushed ",
        "(the push is atomic) and the local release commit and tag were rolled back.",
        call. = FALSE
      )
    }
    message("✅ Branch '", expected_branch, "'", if (tag) paste0(" and tag '", tag_name, "'"), " pushed to '", remote, "'.")
  } else {
    message("\nℹ️ Nothing was pushed (add --push, or push = TRUE, to push to a remote).")
  }

  message("\n🎉 Release ", version_tag, " successfully completed!")
  return(invisible(version_tag))
}

# ==============================================================================
# CLI Entrypoint (when invoked directly via Rscript)
# ==============================================================================
if (sys.nframe() == 0L && !interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  
  if ("--help" %in% args || "-h" %in% args) {
    cat("Usage: Rscript scripts/deploy_release.R [options]\n\n",
        "Options:\n",
        "  --version <TAG>       Specify version tag (or 'auto' for timestamped semver)\n",
        "  --base <SEMVER>       Base semver when version is 'auto' (e.g. 0.4.45)\n",
        "  --branch <BRANCH>     Expected release branch (default: master)\n",
        "  --no-datetime         Do not append datetime stamp to version\n",
        "  --no-tests            Skip test suite (devtools::test)\n",
        "  --no-readme           Skip Quarto README render\n",
        "  --no-desc             Do not update DESCRIPTION Version field\n",
        "  --no-doc              Skip roxygen documentation\n",
        "  --build-site          Run pkgdown::build_site (disabled by default)\n",
        "  --run-check           Run devtools::check (disabled by default)\n",
        "  --no-commit           Skip git commit\n",
        "  --no-tag              Skip git tag\n",
        "  --push                Push the branch and the tag in one atomic push; without it nothing is\n",
        "                        pushed (the commit and the tag stay local). If the push fails, the\n",
        "                        local commit and tag are rolled back\n",
        "  --remote <NAME>       Remote that --push pushes to (default: origin)\n",
        "  --no-push             Do not push; already the default, accepted for old command lines\n",
        "  --dry-run             Preview actions without making changes\n",
        "  -m, --message <MSG>   Custom git commit message\n",
        "  -h, --help            Show this help message\n\n")
    quit(status = 0)
  }

  # Parse CLI arguments
  get_arg_val <- function(flag) {
    idx <- match(flag, args)
    if (!is.na(idx) && idx < length(args)) args[idx + 1] else NULL
  }

  cli_ver <- get_arg_val("--version")
  if (is.null(cli_ver)) cli_ver <- "auto"

  cli_base <- get_arg_val("--base")
  cli_msg <- get_arg_val("--message")
  if (is.null(cli_msg)) cli_msg <- get_arg_val("-m")

  cli_branch <- get_arg_val("--branch")
  if (is.null(cli_branch)) cli_branch <- "master"

  cli_remote <- get_arg_val("--remote")
  if (is.null(cli_remote)) {
    if ("--remote" %in% args) stop("'--remote' needs a value: the name of the remote to push to.", call. = FALSE)
    cli_remote <- "origin"
  }
  if ("--push" %in% args && "--no-push" %in% args) {
    stop("'--push' and '--no-push' contradict each other.", call. = FALSE)
  }

  deploy_release(
    version_tag = cli_ver,
    base_version = cli_base,
    append_datetime = !("--no-datetime" %in% args),
    commit_msg = cli_msg,
    run_tests = !("--no-tests" %in% args),
    render_readme = !("--no-readme" %in% args),
    update_description = !("--no-desc" %in% args),
    document = !("--no-doc" %in% args),
    build_site = ("--build-site" %in% args),
    run_check = ("--run-check" %in% args),
    commit = !("--no-commit" %in% args),
    tag = !("--no-tag" %in% args),
    push = ("--push" %in% args),
    remote = cli_remote,
    expected_branch = cli_branch,
    dry_run = ("--dry-run" %in% args)
  )
}
