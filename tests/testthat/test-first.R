test_that("with existing file castbl.path should return path.",{
	expect_match(castbl.path("periodic-table.csv"), "periodic-table.csv")
})

test_that("without existing file castbl.path should return ''.",{
	expect_match(castbl.path("qeriodic-table.csv"), "")
})

test_that("cas.periodic('atomicnumber') should return ''.",{
	expect_that(as.integer(cas.periodic("atomicnumber")['H']), equals(1) )
})
