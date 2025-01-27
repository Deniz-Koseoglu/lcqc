#smooth_whit()
test_that("smooth_whit() works", {
  expect_vector(smooth_whit(simlc1[,"Signal"], lambda = 1600))
})

#Baseline correction functions
test_that("All baseline correction functions work", {
  bline_sig <- exgc1[,"Signal"]
  bline_nms <- c("Original_Signal", "Corrected_Signal", "Baseline", "Method", "Parameters", "Bline_Plot")
  sink(nullfile())
  res <- unique(c(names(bline_als(bline_sig)),
           names(bline_chang(bline_sig)),
           names(bline_poly(bline_sig)),
           names(bline_isrea(bline_sig))))
  res2 <- names(chrom_bline(bline_sig, plotres = "plot"))
  sink()
  expect_equal(res, bline_nms[-length(bline_nms)])
  expect_equal(res2, bline_nms)
})
