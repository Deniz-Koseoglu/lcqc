test_that("chrom_skim() works", {
  skim_input <- wf_detpeaks
  sink(nullfile())
  res <- chrom_skim(skim_input)
  sink()
  expect_true(all(c("orig_data","indiv_bln","grp_bln","integ_res","max_marks","information", "call") %in% names(res)))
})
