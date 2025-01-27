test_that("ext_lcqc() retrieves external data included with package", {
  expect_true(length(ext_lcqc(full.path = FALSE))>0)
})
