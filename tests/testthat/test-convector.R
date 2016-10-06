test_that("With unit 'ppm', cbk.convector should return 1000000",{
	expect_equivalent(cbk.convector("ppm"), 1000000)
})

test_that("With unit 'qqm', cbk.convector should return 1",{
	expect_equivalent(cbk.convector("qqm"), 1)
})
