test_that("noise_plot() works", {
  dt <- wf_detpeaks[["results"]]
  nsdt <- dt[["Derivative_Noise"]][["SD_noise"]]
  thrdt <- dt[["Derivative_Limits"]][["SD"]]
  res <- noise_plot(nsdt, thrdt)
  expect_true(any(class(res) %in% "ggplot"))
})
