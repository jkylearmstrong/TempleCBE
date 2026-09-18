deploy_release <- function(version_tag, commit_msg = paste("Release version", version_tag)) {
  
  message("🚀 Step 3: Running full test suite...")
  devtools::test()
  
  message("📝 Step 4: Rendering README.Rmd...")
  quarto::quarto_render("README.Rmd")
  
  # Step 5: Version update is typically done in DESCRIPTION file beforehand 
  # via usethis::use_version() or manually, but we capture the tag here.
  
  message("📚 Step 6: Documenting and building pkgdown site...")
  devtools::document()
  pkgdown::build_site()
  
  message("🛠️ Step 7: Running R CMD check...")
  devtools::check()
  
  message(sprintf("📦 Steps 8-10: Committing, tagging (%s), and pushing...", version_tag))
  system("git add .")
  system(sprintf('git commit -m "%s"', commit_msg))
  system(sprintf('git tag %s', version_tag))
  system("git push origin main")
  system("git push origin main --tags")
  
  message("✅ Release complete!")
}

# Example usage:
# deploy_release(version_tag = "0.3.4.2026.17.09.07.20")
