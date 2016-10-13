test_that("cbk.casteml.convert(pmlfile) should return path of converted file",{
  stone <- c("20160627191317-464538","20160627191900-040404")
  pmlfile <- cbk.casteml.download(stone)
  outfile <- cbk.casteml.convert(pmlfile)
  expect_true(file.exists(outfile))
})
