test_that("chrom_deriv() derivative calculation works", {
  xvals <- simlc5[,"Time"]
  yvals <- simlc5[,"Signal"]
  expect_equal(length(chrom_deriv(xvals, yvals)), 2)
})
