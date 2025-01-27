test_that("find_peaks() works", {
  vec <- c(1,2,3,2,1,1,2,1)
  expect_equal(find_peaks(vec, m = 3), c(3,7))
  expect_equal(find_peaks(-vec, m = 3), c(5,6))
})
