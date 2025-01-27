test_that("chrom_amplim() works", {
  sig <- simlc1[,"Signal"]
  lim1 <- chrom_amplim(sig, method = "quant", pars = 0.05)
  lim2 <- chrom_amplim(sig, method = "diff", pars = 0.05)
  lim3 <- chrom_amplim(sig, method = "zscore", pars = c(30, 5, 2))
  limvec <- c(lim1,lim2,lim3)
  expect_true(!any(is.na(limvec)) & is.numeric(limvec))
})
