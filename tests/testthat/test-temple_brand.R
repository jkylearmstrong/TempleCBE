test_that("temple_colors returns the brand palette and subsets by name", {
  cols <- temple_colors()
  expect_length(cols, 10)
  expect_true(all(grepl("^#[0-9a-f]{6}$", cols)))
  expect_equal(unname(temple_colors("cherry")), "#a41e35")
  expect_named(temple_colors("dark-blue", "taupe"), c("dark-blue", "taupe"))
  expect_error(temple_colors("maroon"), "Unknown Temple color")
})

test_that("temple_colors matches the bundled brand.yml", {
  skip_if_not_installed("yaml")
  palette <- unlist(yaml::read_yaml(temple_brand_path())$color$palette)
  expect_setequal(names(temple_colors()), names(palette))
  expect_equal(temple_colors()[names(palette)], palette)
})

test_that("temple_pal generates palettes of the requested length", {
  expect_equal(temple_pal()(2), unname(temple_colors("cherry", "dark-blue")))
  expect_error(temple_pal()(8), "7 colors")

  div <- temple_pal("diverging")(3)
  expect_equal(toupper(div), toupper(unname(temple_colors("dark-blue", "white", "cherry"))))
  expect_equal(temple_pal("diverging", reverse = TRUE)(3), rev(div))
  expect_length(temple_pal("sequential")(10), 10)
})

test_that("Temple scales build in discrete and continuous plots", {
  p_disc <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, colour = factor(cyl))) +
    ggplot2::geom_point() +
    scale_colour_temple()
  colours <- unique(ggplot2::ggplot_build(p_disc)$data[[1]]$colour)
  expect_setequal(colours, unname(temple_colors("cherry", "dark-blue", "ochre")))

  # The diverging scale maps `midpoint` (0) to white even for an asymmetric range.
  df <- data.frame(x = 1:3, y = 1, z = c(-1, 0, 2))
  p_div <- ggplot2::ggplot(df, ggplot2::aes(x, y, fill = z)) +
    ggplot2::geom_tile() +
    scale_fill_temple("diverging", discrete = FALSE)
  fills <- ggplot2::ggplot_build(p_div)$data[[1]]$fill
  expect_equal(toupper(fills[2]), "#FFFFFF")

  expect_s3_class(scale_color_temple("sequential", discrete = FALSE), "ScaleContinuous")
  expect_s3_class(scale_fill_temple(), "ScaleDiscrete")
})

test_that("theme_temple is a ggplot theme with cherry titles", {
  th <- theme_temple()
  expect_s3_class(th, "theme")
  expect_equal(th$plot.title$colour, unname(temple_colors("cherry")))
})

test_that("temple_brand_path points at the bundled brand.yml and its logo", {
  path <- temple_brand_path()
  expect_true(file.exists(path))
  expect_true(file.exists(file.path(dirname(path), "Temple_T_logo.png")))
})

test_that("use_temple_brand installs an extension and creates _quarto.yml", {
  skip_if_not_installed("quarto")
  skip_if_not_installed("withr")
  skip_if(is.null(quarto::quarto_path()), "Quarto CLI not available")

  # A local stand-in for the extension, so the test needs no network.
  src <- withr::local_tempdir("temple_ext_src")
  dir.create(file.path(src, "_extensions", "temple"), recursive = TRUE)
  writeLines(c("title: Temple Brand", "version: 1.0.0", "contributes:",
               "  formats:", "    html:", "      toc: true"),
             file.path(src, "_extensions", "temple", "_extension.yml"))

  proj <- file.path(withr::local_tempdir("temple_proj"), "analysis")
  res <- use_temple_brand(proj, extension = src, quiet = TRUE)

  expect_true(res$created_quarto_yml)
  expect_true(file.exists(file.path(proj, "_quarto.yml")))
  expect_equal(basename(res$extension_dir), "temple")
  expect_true(file.exists(file.path(res$extension_dir, "_extension.yml")))

  # A second call leaves the existing _quarto.yml alone.
  expect_false(use_temple_brand(proj, extension = src, quiet = TRUE)$created_quarto_yml)
})
