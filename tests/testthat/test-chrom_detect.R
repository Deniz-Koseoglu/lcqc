test_that("chrom_detect() works", {
  sink(nullfile())
  res <- chrom_detect(simlc1)
  sink()
  expect_equal(names(res), c("results","plots"))
})
