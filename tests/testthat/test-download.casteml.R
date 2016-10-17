test_that("cbk.download.casteml(stone) should return path of pmlfile",{
  stone <- c("20160627191317-464538","20160627191900-040404")
  pmlfile <- cbk.download.casteml(stone)
  expect_true(file.exists(pmlfile))
})
