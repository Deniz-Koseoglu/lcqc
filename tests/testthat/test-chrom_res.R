test_that("chrom_res() works", {
  res <- chrom_res(wf_detpeaks)
  expect_true(all(c("results", "t0", "information", "call") %in% names(res)))
})
