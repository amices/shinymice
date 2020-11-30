testthat::test_that("md pattern plot", {
  p <- shinymice::plot_md_pattern(mice::boys)
  testthat::expect_is(p, "ggplot")
})
