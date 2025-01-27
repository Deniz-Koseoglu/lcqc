test_that("chrom_plot() works", {
  #Simple plot
  sink(nullfile())
  chrom_plot(simlc1, chrom_vars = c("Time", "Signal"), draw = FALSE)
  sink()

  #Plot with peak markers
  dt <- wf_detpeaks[["results"]]
  chrom <- dt[["Chromatogram"]] #Get chromatogram data
  pt <- dt[["Peak_Extents"]] #Get peak table
  alim <- dt[["Amplitude_Limit"]] #Get amplitude limit
  sink(nullfile())
  res <- chrom_plot(chrom_df = chrom, ptab = pt, norm_chrom = TRUE, chrom_vars = c("Time", "Signal"), id = "peak", apex = "ind_finmax",
                    inf = c("ind_linf", "ind_rinf"), ups = c("ind_lups", "ind_rups"), bound = c("ind_starts", "ind_ends"),
                    amp_line = alim, draw = FALSE)
  sink()
  expect_true(any(class(res) %in% "ggplot"))
})
