test_that("install.packages.no_lock forwards to utils::install.packages with --no-lock", {
  called_with <- NULL
  testthat::local_mocked_bindings(
    install.packages = function(pkgs, ...) {
      called_with <<- list(pkgs = pkgs, dots = list(...))
      invisible(NULL)
    },
    .package = "utils"
  )

  install.packages.no_lock("dplyr")

  expect_equal(called_with$pkgs, "dplyr")
  expect_equal(called_with$dots$INSTALL_opts, "--no-lock")
})
