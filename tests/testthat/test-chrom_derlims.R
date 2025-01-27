test_that("chrom_derlims() works", {
  #Calculate derivatives
  xvals <- simlc1[,"Time"]
  yvals <- simlc1[,"Signal"]
  ders <- chrom_deriv(xvals, yvals)
  fder <- ders[[1]]
  sder <- ders[[2]]

  #Get optimal z-score peak regions
  #zlims <- z_optim(yvals)[[1]]

  #Get derivative peak detection thresholds
  res <- chrom_derlims(x = c(500,764,920,1753,2411), y = c(704,891,1054,1954,2603), fder, sder, method = "ncore", sens = c(2,1))
  thres <- c(res[["FD"]], res[["SD"]])
  expect_true(is.numeric(thres) & length(thres)==4 & !any(is.na(thres)))
  expect_true(all(names(res[["noise"]]) %in% c("FD_noise","SD_noise")) & all(sapply(res[["noise"]], is.numeric)))
})
