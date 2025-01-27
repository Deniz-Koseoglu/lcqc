test_that("acc_inf() works", {
  #Get retention time and signal data
  rtvec <- lcqc::simlc1[,"Time"]
  sigvec <- lcqc::simlc1[,"Signal"]

  #Get second derivatives
  sdvec <- chrom_deriv(rtvec,sigvec)[[2]]

  #Set locations of left and right inflection points
  lvec <- c(527,601,621,810,974,1822,2483)
  rvec <- c(546,611,638,828,994,1850,2522)

  res <- acc_inf(xvals = rtvec, yvals = sigvec, sd = sdvec, linfs = lvec, rinfs = rvec)
  expect_true(all(c("left", "right") %in% names(res)) & all(c("acc_x","acc_y","low","high") %in% colnames(res[["left"]])))
})
