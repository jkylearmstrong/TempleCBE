test_that("cbe_palette contains expected institutional colors", {
  expect_type(cbe_palette, "character")
  expect_true("cherry" %in% names(cbe_palette))
  expect_true("neutral_grey" %in% names(cbe_palette))
  expect_equal(unname(cbe_palette["cherry"]), "#9D2235")
  expect_equal(unname(cbe_palette["neutral_grey"]), "#6F6F6F")
})

test_that("theme_cbe and theme_cbe_deck return valid ggplot2 themes", {
  th <- theme_cbe()
  expect_s3_class(th, "theme")
  expect_s3_class(th, "gg")

  th_deck <- theme_cbe_deck()
  expect_s3_class(th_deck, "theme")
  expect_s3_class(th_deck, "gg")
})

test_that("scale_color_cbe and scale_fill_cbe return valid ggplot2 scales", {
  sc_col <- scale_color_cbe()
  expect_s3_class(sc_col, "ScaleDiscrete")

  sc_fill <- scale_fill_cbe()
  expect_s3_class(sc_fill, "ScaleDiscrete")
})

test_that("formatting helpers format strings correctly", {
  # fmt_pct
  expect_equal(fmt_pct(0.256), "25.6%")
  expect_equal(fmt_pct(0.5, digits = 0), "50%")

  # fmt_num
  expect_equal(fmt_num(3.14159, digits = 2), "3.14")
  expect_equal(fmt_num(10, digits = 1), "10.0")

  # fmt_sig
  expect_equal(fmt_sig(0.001234, digits = 2), "0.0012")

  # fmt_p
  expect_equal(fmt_p(0.042), "p = 0.042")
  expect_equal(fmt_p(0.0001), "p < 0.001")
  expect_equal(fmt_p(NA), "—")

  # fmt_hr
  expect_equal(fmt_hr(1.25, 1.05, 1.48), "HR 1.25 (95% CI 1.05–1.48)")

  # words
  expect_equal(words(character(0)), "none")
  expect_equal(words(c("A")), "A")
  expect_equal(words(c("A", "B")), "A and B")
  expect_equal(words(c("A", "B", "C")), "A, B, and C")
})
