test_that("chrom_tplate() works", {
  #' #Get main data and peak areas
  dt <- wf_detpeaks
  pas <- wf_ints$integ_res$pa

  #Theoretical plates only
  res <- chrom_tplate(dt, method = c("S5","EP", "EMG"))

  #With AH method, HETP, and h
  res2 <- chrom_tplate(dt, method = "AH", pa = pas, len = 150, dp = 5)

  #With Separation Impedance E
  res3 <- chrom_tplate(dt, method = "AH", pa = pas, len = 150, dp = 5, t0 = 0.85, t0_mode = "manual", visc = 0.5264825, deltap = 105, imped_met = "all")

  expect_equal(sort(unique(c(names(res),names(res2),names(res3)))), sort(c("results", "information", "call")))
})
