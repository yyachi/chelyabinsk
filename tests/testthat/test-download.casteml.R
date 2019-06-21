test_that("cbk.download.casteml(stone) should return path of pmlfile",{
  stone <- c("20160627191317-464538","20160627191900-040404")
  pmlfiles <- lapply(stone, cbk.download.casteml)
  expect_true(file.exists(pmlfiles[[1]]))
  expect_true(file.exists(pmlfiles[[2]]))
})

