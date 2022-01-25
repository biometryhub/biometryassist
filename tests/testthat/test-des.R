test_that("crd works if treatments are not called trt", {
    # Completely Randomised Design
    abcs <- c(1, 5, 10, 20)
    outdesign <- design.crd(trt = abcs, r = 5, seed = 42)
    crd_des_out <- des_info(design.obj = outdesign, nrows = 4, ncols = 5, quiet = TRUE)
    expect_equal(crd_des_out$satab[3],
                 "abcs                                    3\n")
    expect_snapshot_output(crd_des_out$satab)
    vdiffr::expect_doppelganger("crd without trt", print(crd_des_out$plot.des))
})


test_that("rcbd works if treatments are not called trt", {
    # Randomised Complete Block Design
    abcs <- LETTERS[1:11]
    outdesign <- design.rcbd(trt = abcs, r = 4, seed = 42)
    rcbd_des_out <- des_info(design.obj = outdesign, nrows = 11,
                             ncols = 4, brows = 11, bcols = 1, quiet = TRUE)
    expect_equal(rcbd_des_out$satab[5],
                 "abcs                                    10\n")
    expect_snapshot_output(rcbd_des_out$satab)
    vdiffr::expect_doppelganger("rcbd without trt", print(rcbd_des_out$plot.des))
})
