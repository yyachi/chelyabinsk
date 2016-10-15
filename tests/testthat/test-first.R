test_that("cbk.path should return a path only with a valid file",{
  expect_match(cbk.path("periodic-table1.dataframe"), "periodic-table1.dataframe")
  expect_match(cbk.path("ref1.dataframe"), "ref1.dataframe")
  expect_match(cbk.path("20081202172326.kitagawa.pml"), "20081202172326.kitagawa.pml")
  expect_match(cbk.path("20081202172326.kitagawa_trace.dataframe"), "20081202172326.kitagawa_trace.dataframe")
  expect_match(cbk.path("qeriodic-table1.dataframe"), "")
})

test_that("cbk.read.dataframe(cbk.path('periodic-table1.dataframe')) should return Dataframe of periodic table",{
  periodic_df <- cbk.read.dataframe(cbk.path('periodic-table1.dataframe'))
  expect_true(is.data.frame(periodic_df))
  expect_that(as.integer(periodic_df["H","atomicnumber"]), equals(1))
})

test_that("cbk.periodic('atomicnumber') should return Named num",{
  expect_that(as.integer(cbk.periodic("atomicnumber")['H']), equals(1))
})

test_that("cbk.periodic() should return Dataframe of periodic table",{
  expect_that(as.integer( cbk.periodic()["H","atomicnumber"]), equals(1))
})

test_that("cbk.read.dataframe(cbk.path('ref1.dataframe')) should return Dataframe of refs with unit converted",{
  refs_df <- cbk.read.dataframe(cbk.path('ref1.dataframe'),'ppm')
  expect_true(is.data.frame(refs_df))
  expect_that(as.integer(refs_df['Wasson.1988','H']), equals(20000))
  expect_that(as.numeric(refs_df['McDonough.1995','Li']), equals(as.numeric(1.5)))
})

test_that("cbk.ref('Wasson.1988') should return Named num",{
  ref_W1988 <- cbk.ref("Wasson.1988")
  ref_M1995 <- cbk.ref("McDonough.1995")
  expect_that(as.integer(ref_W1988["H"]), equals(20000))
  expect_that(as.numeric(ref_M1995["Li"]), equals(as.numeric(1.5)))
})
