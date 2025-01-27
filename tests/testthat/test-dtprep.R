test_that("dtprep() works", {
  res <- dtprep(wf_detpeaks)
  expect_equal(sort(names(res)), sort(c("main_df", "type_df", "grp_df", "grp_blines", "acc_tops", "peak_list")))
})
