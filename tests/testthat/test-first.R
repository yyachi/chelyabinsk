test_that("cbk.path should return a path only with a valid file",{
  expect_match(cbk.path("periodic-dflame0.csv"), "periodic-dflame0.csv")
  expect_match(cbk.path("ref1-dflame0.csv"), "ref1-dflame0.csv")
  expect_match(cbk.path("20081202172326.hkitagawa.pml"), "20081202172326.hkitagawa.pml")
  expect_match(cbk.path("20081202172326.hkitagawa_trace.dflame"), "20081202172326.hkitagawa_trace.dflame")
  expect_match(cbk.path("qeriodic-table1.dflame"), "")
})

test_that("cbk.read.casteml(stone, ...) should return Dataframe",{
  bib <- "20081202172326.hkitagawa"
  tbl0 <- cbk.read.casteml(bib,tableunit='ppm',category='trace',force=TRUE)
  expect_true(is.data.frame(tbl0))
  expect_that(as.numeric(tbl0["analysis of I1502","Li"]), equals(as.numeric(4.02)))
})

test_that("cbk.read.casteml(pmlfile, ...) should return Dataframe",{
  pmlfile <- cbk.path("20160921173604-511857.pml")
  tbl0    <- cbk.read.casteml(pmlfile,tableunit='ppm',category='trace')
  expect_true(is.data.frame(tbl0))
  expect_that(as.numeric(tbl0["ys_opx_ok10vma@2572","Li"]), equals(as.numeric(0.046300)))
  expect_that(as.numeric(tbl0["ys_opx_ok10vma@2572","Li_error"]), equals(as.numeric(3.90e-03)))
})

test_that("cbk.read.dflame(pmlcsv) should return Dataframe of periodic table",{
  pmlcsv <- cbk.path('periodic-dflame0.csv')
  periodic_df <- cbk.read.dflame(pmlcsv)
  expect_true(is.data.frame(periodic_df))
  expect_that(as.integer(periodic_df["H","atomicnumber"]), equals(1))
})

test_that("cbk.periodic('atomicnumber') should return Named num",{
  expect_that(as.integer(cbk.periodic("atomicnumber")['H']), equals(1))
})

test_that("cbk.periodic() should return Dataframe of periodic table",{
  expect_that(as.integer(cbk.periodic()["H","atomicnumber"]), equals(1))
})

test_that("cbk.read.dflame(cbk.path('ref1-dflame0.csv')) should return Dataframe of refs with unit converted",{
  refs_df <- cbk.read.dflame(cbk.path('ref1-dflame0.csv'),'ppm')
  expect_true(is.data.frame(refs_df))
  expect_that(as.integer(refs_df['Wasson.1988','H']), equals(20000))
  expect_that(as.numeric(refs_df['McDonough.1995','Li']), equals(as.numeric(1.5)))
})

test_that("cbk.ref('Wasson.1988') should return Named num",{
  ref_W1988_none <- cbk.ref("Wasson.1988","none")
  ref_W1988_none <- cbk.ref("Wasson.1988")
  ref_M1995_ppm  <- cbk.ref("McDonough.1995","ppm")
  expect_that(as.numeric(ref_W1988_none["H"]), equals(as.numeric(0.02)))
  expect_that(as.numeric(ref_W1988_none["H"]), equals(0.02))
  expect_that(as.numeric(ref_M1995_ppm["Li"]), equals(as.numeric(1.5)))
})
