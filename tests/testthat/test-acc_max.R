test_that("acc_max() works", {
  rtvec <- lcqc::simlc1[,"Time"]
  sigvec <- lcqc::simlc1[,"Signal"]

  #Set locations of left and right inflection points as well as apices
  lvec <- c(527,601,621,810,974,1822,2483)
  rvec <- c(546,611,638,828,994,1850,2522)
  mvec <- c(536,608,628,819,983,1834,2499)
  ptype <- c("F","S","F","B","B","B","B")

  #Get accurate maxima
  res <- acc_max(xvals = rtvec, yvals = sigvec, maxes = mvec, linfs = lvec, rinfs = rvec, ptypes = ptype)
  expect_true(is.data.frame(res) & all(colnames(res) %in% c("acc_x","acc_y","maxfit")))
})
