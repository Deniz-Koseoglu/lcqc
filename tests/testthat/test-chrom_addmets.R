test_that("chrom_addmets() works", {
  res <- chrom_addmets(t0 = 1, len = 250, id = 3.2, flow = 1.6, deltap = 70, visc = 0.33, dp = 5)
  expect_true(all(c("results","information","call") %in% names(res)))
})
