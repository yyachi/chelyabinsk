test_that("cbk.path should return a path only with a valid file",{
  expect_match(cbk.path("periodic-table1.dataframe"), "periodic-table1.dataframe")
  expect_match(cbk.path("ref1.dataframe"), "ref1.dataframe")
  expect_match(cbk.path("20081202172326.hkitagawa.pml"), "20081202172326.hkitagawa.pml")
  expect_match(cbk.path("20081202172326.hkitagawa_trace.dataframe"), "20081202172326.hkitagawa_trace.dataframe")
  expect_match(cbk.path("qeriodic-table1.dataframe"), "")
})

test_that("cbk.read.casteml(stone, ..., download=TRUE) should return Dataframe",{
  bib <- "20081202172326.hkitagawa"
  tbl0 <- cbk.read.casteml(bib,tableunit='ppm',category='trace',download=TRUE)
  expect_true(is.data.frame(tbl0))
  expect_that(as.numeric(tbl0["analysis.of.I1502","Li"]), equals(as.numeric(4.02)))
})

test_that("cbk.read.casteml(pmlfile, ...) should return Dataframe",{
  pmlfile <- cbk.path('20081202172326.hkitagawa.pml')
  tbl0 <- cbk.read.casteml(pmlfile,tableunit='ppm',category='trace')
  expect_true(is.data.frame(tbl0))
  expect_that(as.numeric(tbl0["analysis.of.I1502","Li"]), equals(as.numeric(4.02)))
})

test_that("cbk.read.dataframe(cbkfile) should return Dataframe of periodic table",{
  cbkfile <- cbk.path('periodic-table1.dataframe')
  periodic_df <- cbk.read.dataframe(cbkfile)
  expect_true(is.data.frame(periodic_df))
  expect_that(as.integer(periodic_df["H","atomicnumber"]), equals(1))
})

test_that("cbk.periodic('atomicnumber') should return Named num",{
  expect_that(as.integer(cbk.periodic("atomicnumber")['H']), equals(1))
})

test_that("cbk.periodic() should return Dataframe of periodic table",{
  expect_that(as.integer(cbk.periodic()["H","atomicnumber"]), equals(1))
})

test_that("cbk.read.dataframe(cbk.path('ref1.dataframe')) should return Dataframe of refs with unit converted",{
  refs_df <- cbk.read.dataframe(cbk.path('ref1.dataframe'),'ppm')
  expect_true(is.data.frame(refs_df))
  expect_that(as.integer(refs_df['Wasson.1988','H']), equals(20000))
  expect_that(as.numeric(refs_df['McDonough.1995','Li']), equals(as.numeric(1.5)))
})

test_that("cbk.ref('Wasson.1988') should return Named num",{
  ref_W1988_none <- cbk.ref("Wasson.1988","none")
  ref_W1988_ppm  <- cbk.ref("Wasson.1988")
  ref_M1995_ppm  <- cbk.ref("McDonough.1995","ppm")
  expect_that(as.numeric(ref_W1988_none["H"]), equals(as.numeric(0.02)))
  expect_that(as.integer(ref_W1988_ppm["H"]),  equals(20000))
  expect_that(as.numeric(ref_M1995_ppm["Li"]), equals(as.numeric(1.5)))
})
