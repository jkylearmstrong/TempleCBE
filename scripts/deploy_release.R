#!/usr/bin/env Rscript
# ==============================================================================
# scripts/deploy_release.R
# ------------------------------------------------------------------------------
# Automated Release & Deployment Pipeline for TempleCBE
#
# Features:
#   - Automated datetime version tagging (e.g. 0.3.4.YYYY.MM.DD.HH.MM) for
#     multi-agent / multi-machine traceability and easy git clean-up.
#   - Configurable options to toggle long-running steps (build_site, run_check,
#     run_tests, render_readme).
#   - CLI and interactive R support.
# ==============================================================================

deploy_release <- function(
  version_tag = "auto",
  base_version = NULL,
  append_datetime = TRUE,
  datetime_format = "%Y.%m.%d.%H.%M",
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
  push = TRUE,
  dry_run = FALSE
) {
  # ----------------------------------------------------------------------------
  # 0. Locate Repository Root & DESCRIPTION
  # ----------------------------------------------------------------------------
  repo_root <- tryCatch(
    rprojroot::find_root(rprojroot::is_r_package),
    error = function(e) getwd()
  )
  desc_path <- file.path(repo_root, "DESCRIPTION")
  if (!file.exists(desc_path)) {
    stop("Could not locate DESCRIPTION file at: ", desc_path)
  }

  desc_lines <- readLines(desc_path, encoding = "UTF-8")
  ver_line_idx <- grep("^Version:\\s*", desc_lines)
  if (length(ver_line_idx) == 0) {
    stop("Could not find 'Version:' entry in DESCRIPTION.")
  }
  current_pkg_ver <- sub("^Version:\\s*", "", desc_lines[ver_line_idx[1]])

  # ----------------------------------------------------------------------------
  # 1. Resolve Target Version Tag
  # ----------------------------------------------------------------------------
  if (is.null(version_tag) || version_tag == "auto" || version_tag == "") {
    if (!is.null(base_version) && nzchar(base_version)) {
      base_ver <- base_version
    } else {
      # Extract major.minor.patch (first 3 parts if available)
      ver_parts <- strsplit(current_pkg_ver, "[.-]")[[1]]
      n_parts <- min(3, length(ver_parts))
      base_ver <- paste(ver_parts[1:n_parts], collapse = ".")
    }

    if (isTRUE(append_datetime)) {
      stamp <- format(if (utc) Sys.time() else Sys.time(), datetime_format, tz = if (utc) "UTC" else "")
      version_tag <- paste0(base_ver, ".", stamp)
    } else {
      version_tag <- base_ver
    }
  }

  # Validate that version tag conforms to R package version requirements
  is_valid_r_ver <- tryCatch({
    invisible(package_version(version_tag))
    TRUE
  }, error = function(e) FALSE)

  if (!is_valid_r_ver) {
    warning("Target version_tag '", version_tag, "' is not a standard numeric R package version.")
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
  message("  Step 10: Git Push        : ", if (push) "ENABLED" else "SKIPPED")
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
    if (any(df_results$failed > 0 | df_results$error)) {
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
  # Step 5: Update DESCRIPTION Version
  # ----------------------------------------------------------------------------
  if (update_description) {
    message(sprintf("\n🏷️ Step 5: Synchronizing DESCRIPTION Version to '%s'...", version_tag))
    desc_lines[ver_line_idx[1]] <- paste0("Version: ", version_tag)
    writeLines(desc_lines, desc_path, useBytes = FALSE)
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
  # Steps 8, 9, 10: Commit, Tag, and Push
  # ----------------------------------------------------------------------------
  if (commit) {
    message(sprintf("\n📦 Step 8: Staging changes and creating git commit '%s'...", commit_msg))
    name_check <- tryCatch(system("git config user.name", intern = TRUE), error = function(e) character(0))
    if (length(name_check) == 0 || !nzchar(name_check[1])) {
      system('git config --local user.name "J Kyle Armstrong"')
      system('git config --local user.email "j.kyle.armstrong@gmail.com"')
    }
    system("git add .")
    # Commit changes if any staged
    status <- system(sprintf('git commit -m "%s"', commit_msg))
    if (status != 0) {
      message("ℹ️ No new changes to commit or working tree clean.")
    }
  }

  if (tag) {
    message(sprintf("\n🏷️ Step 9: Creating git tag '%s'...", version_tag))
    tag_status <- system(sprintf('git tag -a %s -m "%s"', version_tag, commit_msg))
    if (tag_status != 0) {
      warning("Git tag command returned non-zero status code: ", tag_status)
    } else {
      message("✅ Git tag created.")
    }
  }

  if (push) {
    message("\n🚀 Step 10: Pushing commit and tags to origin...")
    system("git push origin HEAD")
    system(sprintf("git push origin %s", version_tag))
    message("✅ Release pushed to origin.")
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
        "  --base <SEMVER>       Base semver when version is 'auto' (e.g. 0.3.5)\n",
        "  --no-datetime         Do not append datetime stamp to version\n",
        "  --no-tests            Skip test suite (devtools::test)\n",
        "  --no-readme           Skip Quarto README render\n",
        "  --no-desc             Do not update DESCRIPTION Version field\n",
        "  --no-doc              Skip roxygen documentation\n",
        "  --build-site          Run pkgdown::build_site (disabled by default)\n",
        "  --run-check           Run devtools::check (disabled by default)\n",
        "  --no-commit           Skip git commit\n",
        "  --no-tag              Skip git tag\n",
        "  --no-push             Skip git push\n",
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
    push = !("--no-push" %in% args),
    dry_run = ("--dry-run" %in% args)
  )
}
