test_that("fastchrom_bline() works", {
  sigvec <- simlc1[,"Signal"]
  strvec <- c(486, 763, 916, 1745, 2428)
  endvec <- c(707, 897, 1050, 1946, 2588)
  crw <- 10

  #Data formatted simply
  res1 <- fastchrom_bline(sig = sigvec, starts = strvec, ends = endvec, crit_w = crw, for_plot = FALSE)
  expect_equal(colnames(res1), c("ind","orig_y","bline","y"))
})
