test_that("chrom_sepf() works", {
  res <- chrom_sepf(wf_detpeaks)
  expect_true(all(c("results", "t0", "information", "call") %in% names(res)))
})
