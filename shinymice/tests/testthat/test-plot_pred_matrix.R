testthat::test_that("is the predictor matrix plot a ggplot object?", {
  p <- shinymice::plot_pred_matrix(mice::boys)
  testthat::expect_is(p, "ggplot")
})
