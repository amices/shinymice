testthat::test_that("is the missing data pattern plot a ggplot object?", {
  p <- shinymice::plot_md_pattern(mice::boys)
  testthat::expect_is(p, "ggplot")
})
