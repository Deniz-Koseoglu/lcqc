#is.even()
test_that("is.even() works", {
  expect_false(is.even(1))
})

#is.odd()
test_that("is.odd() works", {
  expect_true(is.odd(3))
})

#linspline() and cubspline()
test_that("linspline() and cubspline() both work", {
  x <- 1:10 + runif(10, -0.1, 0.1)
  y <- rnorm(10, 3, 1)
  f <- stats::splinefun(x, y, method = "fmm")
  expect_vector(linspline(x, y, y0 = 2.85, verbose = FALSE))
  expect_vector(cubspline(f, y0 = 2.85, verbose = FALSE))
})

#slope() and intercept()
test_that("slope() and intercept() both work", {
  expect_vector(with(CO2, slope(conc, uptake)), size = 1)
  expect_vector(with(CO2, intercept(conc, uptake)), size = 1)
})

#intersect()
test_that("intersect() works", {
  l1 <- c(10,-2)
  l2 <- c(0,2)
  expect_vector(intersect(l1,l2))
})

#chkdt()
#test_that("chkdt() works", {
#
#})

#cprint()
test_that("cprint() works", {
  expect_vector(capture.output(cprint(wf_detpeaks)))
})
