test_that("with existing file cas.path should return path.",{
	expect_match(cas.path("periodic-table.csv"), "periodic-table.csv")
})

test_that("without existing file cas.path should return ''.",{
	expect_match(cas.path("qeriodic-table.csv"), "")
})

test_that("cas.periodic('atomicnumber') should return ''.",{
	expect_that(as.integer(cas.periodic("atomicnumber")['H']), equals(1))
})
