testthat::test_that("is the flux plot a ggplot object?", {
  p <- shinymice::plot_flux(mice::boys)
  testthat::expect_is(p, "ggplot")
})
