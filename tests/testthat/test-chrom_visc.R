test_that("chrom_visc() works", {
  visc1 <- chrom_visc(ids = "meoh", fracs = 1, frac_type = "vol", temp = 25)
  visc2 <- chrom_visc(ids = c("meoh", "etac"), fracs = c(0.3, 0.7), frac_type = "vol", temp = 25)
  visc3 <- chrom_visc(ids = c("meoh", "h2o"), fracs = c(0.2, 0.8), frac_type = "vol", temp = 25)
  expect_true(all(c("results", "information", "call") %in% unique(c(names(visc1), names(visc2), names(visc3)))))
})
