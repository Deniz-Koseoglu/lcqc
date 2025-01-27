test_that("chk_pack() checks whether a package is present", {
  expect_true(chk_pack("base")[[1]])
})
