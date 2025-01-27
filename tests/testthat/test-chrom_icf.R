test_that("chrom_icf() works", {
  det_res <- wf_detpeaks
  sink(nullfile())
  icf_res <- chrom_icf(det_res, method = "egh", modres = FALSE)
  sink()
  expect_true(all(c("main_data", "integ_res", "information", "call") %in% names(icf_res)))
})
