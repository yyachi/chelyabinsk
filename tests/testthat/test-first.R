test_that("with existing file cbk.path should return path.",{
	expect_match(cbk.path("periodic-table.csv"), "periodic-table.csv")
})

test_that("without existing file cbk.path should return ''.",{
	expect_match(cbk.path("qeriodic-table.csv"), "")
})

test_that("cbk.periodic('atomicnumber') should return ''.",{
	expect_that(as.integer(cbk.periodic("atomicnumber")['H']), equals(1))
})
