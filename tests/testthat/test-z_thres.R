test_that("z_thres() works", {
  expect_equal(names(z_thres(lcqc::exlc1[,"Signal"], lag = 13, threshold = 4, influence = 0)), c("signals", "avgFilter", "stdFilter", "params"))
})
