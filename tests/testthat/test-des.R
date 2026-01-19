test_that("design() CRD produces satab output", {
    abcs <- c(1, 5, 10, 20)
    crd_des_out <- design(type = "crd", treatments = abcs, reps = 5,
                          nrows = 4, ncols = 5, seed = 42, quiet = TRUE)

    expect_equal(crd_des_out$satab[3],
                 paste0(format("treatments", width = 40), 3, "\n"))
    expect_snapshot_output(crd_des_out$satab)
    vdiffr::expect_doppelganger("crd without trt", print(crd_des_out$plot.des),
                                variant = ggplot2_variant())
})


test_that("design() RCBD produces satab output", {
    abcs <- LETTERS[1:11]
    rcbd_des_out <- design(type = "rcbd", treatments = abcs, reps = 4,
                           nrows = 11, ncols = 4, brows = 11, bcols = 1,
                           seed = 42, quiet = TRUE)

    expect_equal(rcbd_des_out$satab[5],
                 paste0(format("treatments", width = 40), 10, "\n"))
    expect_snapshot_output(rcbd_des_out$satab)
    vdiffr::expect_doppelganger("rcbd without trt", print(rcbd_des_out$plot.des),
                                variant = ggplot2_variant())
})
