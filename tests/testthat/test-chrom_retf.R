test_that("chrom_retf() works", {
  res <- chrom_retf(wf_detpeaks, t0 = 1)
  expect_true(all(c("results", "t0", "information", "call") %in% names(res)))
})
