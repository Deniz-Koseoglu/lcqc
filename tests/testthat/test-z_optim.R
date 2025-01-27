test_that("z_optim() works", {
  expect_equal(names(z_optim(simlc1[,"Signal"])), c("Peak_Limits", "Best_ZScore_Run"))
})
