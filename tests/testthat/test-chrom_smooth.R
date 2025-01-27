test_that("chrom_smooth() works", {
  input <- simlc5[,"Signal"]
  res_tri <- chrom_smooth(input, method = "tri")
  res_sg <- chrom_smooth(input, method = "sg_quad")
  expect_vector(c(res_tri, res_sg))
  expect_equal(length(c(res_tri, res_sg)), length(input)*2)
})
