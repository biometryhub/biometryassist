test_that("print.satab prints to console", {
	obj <- satab(outdesign_crd_satab)
	expect_s3_class(obj, "satab")

	expect_invisible(print(obj))
	expect_output(print(obj), "Source of Variation")
	expect_output(print(obj), "Residual")
})

test_that("satab examples print expected sections", {
	expect_output(print(satab(outdesign_crd_satab)), "Source of Variation")
	expect_output(print(satab(outdesign_rcbd_satab)), "Block stratum")
	expect_output(print(satab(outdesign_lsd_satab)), "Row")

	expect_output(print(satab(outdesign_crossed_satab)), "Residual")
	expect_output(print(satab(outdesign_crossed_rcbd_satab)), "Residual")
	expect_output(print(satab(outdesign_crossed_lsd_satab)), "Residual")

	expect_output(print(satab(outdesign_nested_satab)), "Column")
	expect_output(print(satab(outdesign_split_satab)), "Whole plot Residual")
})
