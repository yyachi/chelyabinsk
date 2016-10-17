test_that("cbk.convert.casteml(pmlfile) should return path of converted file",{
  stone <- c("20160627191317-464538","20160627191900-040404")
  pmlfile <- cbk.download.casteml(stone)
  outfile <- cbk.convert.casteml(pmlfile)
  expect_true(file.exists(outfile))
})
