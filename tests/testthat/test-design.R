test_that("CRD designs are supported", {
    # CRD
    d1 <- design(type = "crd", treatments = c(1, 5, 10, 20),
                 reps = 5, nrows = 4, ncols = 5, seed = 42, quiet = TRUE)

    expect_equal(names(d1), c("design", "plot.des", "satab", "seed"))
    expect_equal(d1$seed, 42)
    expect_equal(d1$satab[4], "Residual                                16\n")
    expect_snapshot_output(d1$satab)
    vdiffr::expect_doppelganger(title = "CRD plot produced", autoplot(d1))
})

test_that("RCBD designs are supported", {
    # RCBD
    d2 <- design("rcbd", treatments = LETTERS[1:11], reps = 4,
                 nrows = 11, ncols = 4, brows = 11, bcols = 1, seed = 42, quiet = TRUE)

    expect_equal(d2$seed, 42)
    expect_equal(d2$satab[3],
                 "Block stratum                           3\n")
    expect_snapshot_output(d2$satab)
    vdiffr::expect_doppelganger(title = "RCBD plot produced", autoplot(d2))
})

test_that("RCBD with row-wise blocks are supported", {
    # RCBD with row-wise blocks
    d2.1 <- design("rcbd", treatments = LETTERS[1:6], reps = 4,
                   nrows = 4, ncols = 6, brows = 1, bcols = 6, seed = 42, quiet = TRUE)

    vdiffr::expect_doppelganger(title = "RCBD with row blocks", autoplot(d2.1))

})

test_that("RCBD with square blocks are supported", {
    d2.2 <- design("rcbd", treatments = LETTERS[1:6], reps = 4,
                   nrows = 6, ncols = 4, brows = 3, bcols = 2, seed = 42, quiet = TRUE)

    vdiffr::expect_doppelganger(title = "RCBD with square blocks", autoplot(d2.2))
})

test_that("LSD designs are supported", {
    # LSD
    d3 <- design(type = "lsd", c("S1", "S2", "S3", "S4"),
                 nrows = 4, ncols = 4, seed = 42, quiet = TRUE)

    expect_equal(d3$seed, 42)
    expect_equal(d3$satab[6],
                 "Residual                                6\n")
    expect_snapshot_output(d3$satab)
    vdiffr::expect_doppelganger(title = "LSD plot produced", autoplot(d3))
})

test_that("Split plot designs are supported", {
    # Split
    d4 <- design(type = "split", treatments = c("A", "B"),
                 sub_treatments = 1:4, reps = 4, nrows = 8,
                 ncols = 4, brows = 8, bcols = 1, seed = 42,
                 quiet = TRUE)

    expect_equal(d4$seed, 42)
    expect_equal(d4$satab[11],
                 "         treatments:sub_treatments           3\n")
    expect_snapshot_output(d4$satab)
    vdiffr::expect_doppelganger(title = "Split plot produced", autoplot(d4))
})

test_that("Split plot designs with names are supported", {
    d4.1 <- design(type = "split", treatments = c("A", "B"),
                   sub_treatments = 1:4, reps = 4, nrows = 8,
                   ncols = 4, brows = 4, bcols = 2, seed = 42,
                   fac.names = list(Water = c("Irrigated", "Rain-fed"),
                                    N = seq(50, 200, 50)), quiet = TRUE)

    expect_equal(d4.1$satab[11],
                 "         Water:N                             3\n")
    expect_snapshot_output(d4.1$satab)
    vdiffr::expect_doppelganger(title = "Split plot with names", autoplot(d4.1))
})

test_that("Split plot designs with double row blocks are supported", {
    d4.2 <- design(type = "split", treatments = c("A", "B"),
                   sub_treatments = 1:4, reps = 4, nrows = 8,
                   ncols = 4, brows = 1, bcols = 4, seed = 42, quiet = TRUE)

    expect_equal(d4.2$satab[11],
                 "         treatments:sub_treatments           3\n")
    expect_equal(d4.2$seed, 42)
    vdiffr::expect_doppelganger(title = "Split plot double row blocks", autoplot(d4.2))
})

test_that("Split plot designs with ntrt == bcol are supported", {
    d4.3 <- design(type = "split", treatments = c("A", "B"),
                   sub_treatments = 1:4, reps = 4, nrows = 4,
                   ncols = 8, brows = 1, bcols = 8, seed = 42, quiet = TRUE)

    expect_equal(d4.3$satab[11],
                 "         treatments:sub_treatments           3\n")
    expect_equal(d4.3$seed, 42)
    vdiffr::expect_doppelganger(title = "Split plot ntrt == bcol", autoplot(d4.3))
})

test_that("Split plot designs with column-wise arrangement are supported", {
    d4.4 <- design(type = "split", treatments = c("A", "B"),
                   sub_treatments = 1:4, reps = 4, nrows = 8,
                   ncols = 4, brows = 4, bcols = 2, byrow = FALSE,
                   seed = 42, quiet = TRUE)

    expect_equal(d4.4$satab[11],
                 "         treatments:sub_treatments           3\n")
    expect_equal(d4.4$seed, 42)
    vdiffr::expect_doppelganger(title = "Split plot byrow = F", autoplot(d4.4))
})

test_that("Crossed CRD designs are supported", {
    # Crossed, CRD
    d5 <- design(type = "crossed:crd", treatments = c(3, 2),
                 reps = 3, nrows = 6, ncols = 3, seed = 42,
                 fac.sep = c("", ""), quiet = TRUE)

    expect_equal(d5$seed, 42)
    expect_equal(d5$satab[5],
                 "A:B                                     2\n")
    expect_snapshot_output(d5$satab)
    vdiffr::expect_doppelganger(title = "Factorial CRD plot no space sep", autoplot(d5))

    # Crossed, CRD with renaming
    d5.1 <- design(type = "crossed:crd", treatments = c(3, 2),
                   reps = 3, nrows = 6, ncols = 3, seed = 42,
                   fac.names = list(N = c(50, 100, 150),
                                    Water = c("Irrigated", "Rain-fed")),
                   quiet = TRUE)

    expect_equal(d5.1$satab[5],
                 "N:Water                                 2\n")
    expect_snapshot_output(d5.1$satab)
    vdiffr::expect_doppelganger(title = "Factorial CRD with names", autoplot(d5.1))
})

test_that("Crossed RCBD designs are supported", {
    # Crossed RCBD
    d6 <- design(type = "crossed:rcbd", treatments = c(3, 2),
                 reps = 3, nrows = 6, ncols = 3, brows = 6, bcols = 1,
                 seed = 42, quiet = TRUE)

    expect_equal(d6$seed, 42)
    expect_equal(d6$satab[8],
                 "Residual                                10\n")
    expect_snapshot_output(d6$satab)
    vdiffr::expect_doppelganger(title = "Factorial RCBD plot produced", autoplot(d6))
})

test_that("Crossed RCBD designs with row blocks are supported", {
    d6.1 <- design(type = "crossed:rcbd", treatments = c(3, 2),
                   reps = 3, nrows = 3, ncols = 6, brows = 1, bcols = 6,
                   fac.sep = c(":", ""), seed = 42, quiet = TRUE)

    expect_equal(d6.1$satab[8],
                 "Residual                                10\n")
    vdiffr::expect_doppelganger(title = "Factorial RCBD plot with row blocks", autoplot(d6.1))
})

test_that("Crossed RCBD designs with double row blocks are supported", {
    d6.2 <- design(type = "crossed:rcbd", treatments = c(3, 2),
                   reps = 3, nrows = 6, ncols = 3, brows = 2, bcols = 3,
                   seed = 42, quiet = TRUE)

    expect_equal(d6.2$satab[8],
                 "Residual                                10\n")
    vdiffr::expect_doppelganger(title = "Factorial RCBD plot double row blocks", autoplot(d6.2))
})

test_that("Crossed RCBD designs with double row blocks are supported", {
    d6.3 <- design(type = "crossed:rcbd", treatments = c(3, 2),
                   reps = 4, nrows = 6, ncols = 4, brows = 3, bcols = 2,
                   seed = 42, quiet = TRUE)

    vdiffr::expect_doppelganger(title = "Factorial RCBD plot square blocks", autoplot(d6.3))
})

test_that("Crossed LSD designs are supported", {
    # Crossed LSD with separator
    d7 <- design(type = "crossed:lsd", treatments = c(3, 2),
                 nrows = 6, ncols = 6, fac.sep = "_",
                 seed = 42, quiet = TRUE)

    expect_equal(d7$seed, 42)
    expect_equal(d7$satab[3],
                 "Row                                     5\n")
    expect_snapshot_output(d7$satab)
    vdiffr::expect_doppelganger(title = "Factorial LSD plot with sep", autoplot(d7))
})

test_that("Crossed LSD designs with names are supported", {
    d7.1 <- design(type = "crossed:lsd", treatments = c(3, 2),
                   nrows = 6, ncols = 6, seed = 42, quiet = TRUE,
                   fac.names = list(N = c(50, 100, 150),
                                    W = c("I", "R")))

    expect_equal(d7.1$seed, 42)
    expect_equal(d7.1$satab[3],
                 "Row                                     5\n")
    vdiffr::expect_doppelganger(title = "Factorial LSD with names", autoplot(d7.1))
})

test_that("Crossed LSD designs with names and separator are supported", {
    d7.2 <- design(type = "crossed:lsd", treatments = c(3, 2),
                   nrows = 6, ncols = 6, seed = 42, quiet = TRUE,
                   fac.names = list(N = c(50, 100, 150),
                                    W = c("I", "R")),
                   fac.sep = c(":", ""))

    expect_equal(d7.2$seed, 42)
    expect_equal(d7.2$satab[3],
                 "Row                                     5\n")
    vdiffr::expect_doppelganger(title = "Factorial LSD plot names and sep", autoplot(d7.2))
})

test_that("Nested designs are supported", {
    # Nested LSD
    d8 <- design(type = "lsd", treatments = c("A1", "A2", "A3", "A4", "B1", "B2", "B3"),
                 nrows = 7, ncols = 7, seed = 42, quiet = TRUE)

    expect_equal(d8$seed, 42)
    expect_equal(d8$satab[6],
                 "Residual                                30\n")
    expect_snapshot_output(d8$satab)
    vdiffr::expect_doppelganger(title = "Nested LSD", autoplot(d8))
})

test_that("3 way factorial designs are possible", {
    d9 <- design(type = "crossed:crd", treatments = c(2, 2, 2),
                 reps = 3, nrows = 6, ncols = 4, seed = 42, quiet = TRUE)

    expect_equal(d9$seed, 42)
    expect_equal(d9$satab[6],
                 "A:B:C                                   1\n")
    expect_snapshot_output(d9$satab)
    vdiffr::expect_doppelganger(title = "3 way factorial", autoplot(d9))

    d9.1 <- design(type = "crossed:crd", treatments = c(2, 2, 2),
                   reps = 3, nrows = 6, ncols = 4, seed = 42,
                   fac.names = list(X = c("A", "B"), Y = 1:2, Z = c(10, 20)))

    expect_equal(d9.1$seed, 42)
    expect_equal(d9.1$satab[6],
                 "X:Y:Z                                   1\n")
    expect_snapshot_output(d9.1$satab)
    vdiffr::expect_doppelganger(title = "3 way factorial with names", autoplot(d9.1))

    d9.2 <- design(type = "crossed:rcbd", treatments = c(2, 2, 2),
                   reps = 3, nrows = 8, ncols = 3, brows = 8, bcols = 1, seed = 42,
                   fac.names = list(X = c("A", "B"), Y = 1:2, Z = c(10, 20)))

    expect_equal(d9.2$seed, 42)
    expect_equal(d9.2$satab[3],
                 "Block stratum                           2\n")
    expect_snapshot_output(d9.2$satab)
    vdiffr::expect_doppelganger(title = "3 way rcbd factorial with names", autoplot(d9.2))
})

test_that("Adding names to 3 way factorial designs works", {
    d9.2 <- design(type = "crossed:rcbd", treatments = c(2, 2, 2),
                   reps = 3, nrows = 8, ncols = 3, brows = 8, bcols = 1, seed = 42,
                   fac.names = list(X = c("A", "B"), Y = 1:2, Z = c(10, 20)))

    expect_equal(d9.2$seed, 42)
    expect_equal(d9.2$satab[3],
                 "Block stratum                           2\n")
    expect_snapshot_output(d9.2$satab)
    vdiffr::expect_doppelganger(title = "3 way rcbd factorial with names", autoplot(d9.2))
})

test_that("seed options work", {
    # seed = TRUE
    d1 <- design(type = "crd", treatments = c(1, 5, 10, 20),
                 reps = 5, nrows = 4, ncols = 5, seed = TRUE, quiet = TRUE)

    expect_true(is.numeric(d1$seed))

    # seed = value
    d2 <- design(type = "crd", treatments = c(1, 5, 10, 20),
                 reps = 5, nrows = 4, ncols = 5, seed = 123, quiet = TRUE)

    expect_identical(d2$seed, 123)

    # seed = FALSE
    d3 <- design(type = "crd", treatments = c(1, 5, 10, 20),
                 reps = 5, nrows = 4, ncols = 5, seed = FALSE, quiet = TRUE)

    expect_null(d3$seed)
    expect_equal(names(d3), c("design", "plot.des", "satab"))
})


# Testing messages, warnings and errors
test_that("Invalid seed options give errors or warnings", {
    # seed = NA
    expect_error(design(type = "crd", treatments = c(1, 5, 10, 20),
                        reps = 5, nrows = 4, ncols = 5, seed = NA, quiet = TRUE),
                 "seed must be numeric or TRUE/FALSE")

    # seed = NULL
    expect_error(design(type = "crd", treatments = c(1, 5, 10, 20),
                        reps = 5, nrows = 4, ncols = 5, seed = NULL, quiet = TRUE),
                 "argument is of length zero")

    # seed = "ABC"
    expect_error(design(type = "crd", treatments = c(1, 5, 10, 20),
                        reps = 5, nrows = 4, ncols = 5, seed = "ABC", quiet = TRUE),
                 "seed must be numeric or TRUE/FALSE")

    # seed is vector of numbers
    # expect_warning(
    #     expect_warning(
    #         expect_warning(d1 <- design(type = "crd", treatments = c(1, 5, 10, 20),
    #                                     reps = 5, nrows = 4, ncols = 5, seed = 1:10, quiet = TRUE),
    #                        "the condition has length > 1 and only the first element will be used"),
    #         "the condition has length > 1 and only the first element will be used"),
    #     "the condition has length > 1 and only the first element will be used")
    # expect_true(is.numeric(d1$seed))
    # expect_equal(d1$seed, 1)

    # expect_warning(
    #     expect_error(design(type = "crd", treatments = c(1, 5, 10, 20),
    #                         reps = 5, nrows = 4, ncols = 5, seed = c('a', 'b'), quiet = TRUE),
    #                  "seed must be numeric or TRUE/FALSE"),
    #     "the condition has length > 1 and only the first element will be used")
})

test_that("reps in lsd produces a message", {
    expect_message(x <- design(type = "lsd", 1:4, reps = 3, nrows = 4, ncols = 4, seed = 42, quiet = TRUE),
                   "Number of replicates is not required for Latin Square designs and has been ignored")
})

test_that("rcbd requires brows and bcols", {
    expect_error(design("rcbd", treatments = LETTERS[1:11],
                        reps = 4, nrows = 11, ncols = 4,
                        brows = NA, bcols = 1, seed = 42),
                 "Design has blocks so brows and bcols must be supplied.")
    expect_error(design(type = "crossed:rcbd", treatments = c(3, 2),
                        reps = 3, nrows = 6, ncols = 3, brows = NA, bcols = 1),
                 "Design has blocks so brows and bcols must be supplied.")
})

test_that("unsupported design types give an error", {
    expect_error(design(type = "abc", 1:4, reps = 5,
                        nrows = 4, ncols = 5, seed = 42),
                 "Designs of type 'abc' are not supported")

    expect_error(design(type = "strip", 1:4, reps = 5,
                        nrows = 4, ncols = 5, seed = 42),
                 "Designs of type 'strip' are not supported")

    expect_error(design(type = "crossed:split", 1:4, reps = 5,
                        nrows = 4, ncols = 5, seed = 42),
                 "Crossed designs of type 'split' are not supported")

    expect_error(design(type = "crossed:abc", 1:4, reps = 5,
                        nrows = 4, ncols = 5, seed = 42),
                 "Crossed designs of type 'abc' are not supported")

    expect_error(design(type = "crossed:crd", treatments = 1:4,
                        reps = 5, nrows = 4, ncols = 5, seed = 42),
                 "Crossed designs with more than three treatment factors are not supported")
})

test_that("split plot requires sub_treatments", {
    expect_error(design(type = "split", treatments = c("A", "B"),
                        sub_treatments = NULL, reps = 4, nrows = 8,
                        ncols = 4, brows = 4, bcols = 2, seed = 42),
                 "sub_treatments are required for a split plot design")
})

test_that("split plot requires brows and bcols", {
    expect_error(design(type = "split", treatments = c("A", "B"),
                        sub_treatments = 1:4, reps = 4, nrows = 8,
                        ncols = 4, brows = NA, bcols = 2, seed = 42),
                 "Design has blocks so brows and bcols must be supplied.")
})

test_that("split plot allows a character vector for factor names", {
    # Split with vector of names
    d11 <- design(type = "split", treatments = c("A", "B"),
                  sub_treatments = 1:4, reps = 4, nrows = 8,
                  ncols = 4, brows = 4, bcols = 2, seed = 42,
                  fac.names = c("Water", "Nitrogen"), quiet = TRUE)
    expect_equal(d11$satab[11],
                 "         Water:Nitrogen                      3\n")
    expect_snapshot_output(d11$satab)
    vdiffr::expect_doppelganger(title = "Split plot with vector names", autoplot(d11))
})

test_that("split plot produces warning when incorrect number of treatment labels given", {
    expect_warning(design(type = "split", treatments = c("A", "B"),
                          sub_treatments = 1:4, reps = 4, nrows = 8,
                          ncols = 4, brows = 4, bcols = 2, seed = 42,
                          fac.names = list(Water = "ABC",
                                           N = 1:4)),
                   "Water must contain the correct number of elements. Elements have not been applied.")
    expect_warning(design(type = "split", treatments = c("A", "B"),
                          sub_treatments = 1:4, reps = 4, nrows = 8,
                          ncols = 4, brows = 4, bcols = 2, seed = 42,
                          fac.names = list(Water = c("A", "B"),
                                           N = 1:10)),
                   "N must contain the correct number of elements. Elements have not been applied.")
    expect_warning(design(type = "split", treatments = c("A", "B"),
                          sub_treatments = 1:4, reps = 4, nrows = 8,
                          ncols = 4, brows = 4, bcols = 2, seed = 42,
                          fac.names = list(Water = c("A", "B"),
                                           N = 1:4,
                                           Another = 1:5)),
                   "fac.names contains 3 elements but only the first 2 have been used.")

    expect_warning(design(type = "split", treatments = c("A", "B"),
                          sub_treatments = 1:4, reps = 4, nrows = 8,
                          ncols = 4, brows = 4, bcols = 2, seed = 42,
                          fac.names = list(Water = c("A", "B"))),
                   "fac.names doesn't contain enough elements and has not been used.")
})

test_that("factorial designs produce warnings when incorrect number of treatment labels given", {
    expect_warning(design(type = "crossed:rcbd", treatments = c(3, 2),
                          reps = 3, nrows = 6, ncols = 3, brows = 6, bcols = 1,
                          fac.names = list(Water = c("A", "B"), N = 1:2), quiet = TRUE),
                   "Water must contain the correct number of elements. Elements have not been applied.")

    expect_warning(design(type = "crossed:rcbd", treatments = c(3, 2),
                          reps = 3, nrows = 6, ncols = 3, brows = 6, bcols = 1,
                          fac.names = list(Water = c("A", "B", "C"), N = 1), quiet = TRUE),
                   "N must contain the correct number of elements. Elements have not been applied.")

    expect_warning(design(type = "crossed:rcbd", treatments = c(3, 2),
                          reps = 3, nrows = 6, ncols = 3, brows = 6, bcols = 1,
                          fac.names = list(Water = c("A", "B", "C"), N = 1:2, Another = 1:10), quiet = TRUE),
                   "fac.names contains 3 elements but only the first 2 have been used.")

    expect_warning(design(type = "crossed:rcbd", treatments = c(3, 2),
                          reps = 3, nrows = 6, ncols = 3, brows = 6, bcols = 1,
                          fac.names = list(Water = c("A", "B", "C")), quiet = TRUE),
                   "fac.names doesn't contain enough elements and has not been used.")

    expect_warning(design(type = "crossed:crd", treatments = c(2, 2, 2),
                          reps = 3, nrows = 6, ncols = 4,
                          fac.names = list(Water = c("A", "B"), N = 1:2, Another = 1), quiet = TRUE),
                   "Another must contain the correct number of elements. Elements have not been applied.")
})

test_that("passing unknown arguments to ggsave causes an error", {
    expect_error(
        design(type = "crd", treatments = c(1, 5, 10, 20),
               reps = 5, nrows = 4, ncols = 5, seed = 42, Width = 6),
        "1 components of `...` were not used."
    )
})

test_that("Area and treatment size mismatches produce warnings", {
    # Wrap this in supressWarnings to hide other warning message
    suppressWarnings(expect_warning(
        design(type = "crd", treatments = c(1, 5, 10, 20),
               reps = 5, nrows = 4, ncols = 50, seed = 42, quiet = TRUE),
        "Area provided is larger than treatments applied. Please check inputs."
    ))
    expect_warning(
        design(type = "crd", treatments = c(1, 5, 10, 20),
               reps = 5, nrows = 2, ncols = 5, seed = 42, quiet = TRUE),
        "Area provided is smaller than treatments applied. Please check inputs."
    )
})

test_that("Invalid save option produces an error", {
    expect_error(design("crd", treatments = 1:11, reps = 4, nrows = 11,
                        ncols = 4, save = "abc", quiet = TRUE),
                 "save must be one of 'none'/FALSE, 'both'/TRUE, 'plot', or 'workbook'."
    )
})

test_that("save = 'none' produces nothing", {
    design("crd", treatments = 1:11, reps = 4, nrows = 11, ncols = 4, save = "none", quiet = TRUE)
    expect_false(file.exists("crd_design.csv"))
    expect_false(file.exists("crd_design.pdf"))
})

test_that("save = FALSE produces nothing", {
    design("crd", treatments = 1:11, reps = 4, nrows = 11, ncols = 4, save = FALSE, quiet = TRUE)
    expect_false(file.exists("crd_design.csv"))
    expect_false(file.exists("crd_design.pdf"))
})

test_that("save = 'workbook' produces csv file and not plot", {
    design("crd", treatments = 1:11, reps = 4, nrows = 11, ncols = 4, save = "workbook", savename = "crd_design1", quiet = TRUE)
    withr::local_file("crd_design1.csv")
    expect_true(file.exists("crd_design1.csv"))
    expect_snapshot_file("crd_design1.csv")
    expect_false(file.exists("crd_design1.pdf"))
})

test_that("save = 'plot' produces plot file and not csv", {
    design("crd", treatments = 1:11, reps = 4, nrows = 11, ncols = 4, save = "plot", savename = "crd_design2", quiet = TRUE)
    withr::local_file("crd_design2.pdf")
    expect_false(file.exists("crd_design2.csv"))
    expect_true(file.exists("crd_design2.pdf"))
})

test_that("save = 'both' produces plot file and csv", {
    design("crd", treatments = 1:11, reps = 4, nrows = 11, ncols = 4, save = "both", savename = "crd_design3", quiet = TRUE)
    withr::local_file("crd_design3.pdf")
    withr::local_file("crd_design3.csv")
    expect_true(file.exists("crd_design3.csv"))
    expect_true(file.exists("crd_design3.pdf"))
    expect_snapshot_file("crd_design3.csv")
})

test_that("save = TRUE produces plot file and csv", {
    design("crd", treatments = 1:11, reps = 4, nrows = 11, ncols = 4, save = TRUE, savename = "crd_design4", quiet = TRUE)
    withr::local_file("crd_design4.pdf")
    withr::local_file("crd_design4.csv")
    expect_true(file.exists("crd_design4.csv"))
    expect_true(file.exists("crd_design4.pdf"))
    expect_snapshot_file("crd_design4.csv")
})

test_that("designs have a class of 'design'", {
    d1 <- design("crd", treatments = 1:11, reps = 4, nrows = 11, ncols = 4, quiet = TRUE)
    expect_s3_class(d1, "design")
})

test_that("brows or bcols larger than nrows or ncols gives an error", {
    expect_error(design("rcbd", treatments = 1:4, reps = 4, nrows = 4,
                        ncols = 4, brows = 5, bcols = 1, quiet = TRUE),
                 "brows must not be larger than nrows")
    expect_error(design("rcbd", treatments = 1:4, reps = 4, nrows = 4,
                        ncols = 4, brows = 1, bcols = 5, quiet = TRUE),
                 "bcols must not be larger than ncols")
})

test_that("size argument must be numeric", {
    expect_error(design("crd", treatments = 1:4, reps = 4, nrows = 4,
                        ncols = 4, size = "A", quiet = TRUE),
                 "size must be numeric")
    expect_error(design("crd", treatments = 1:4, reps = 4, nrows = 4,
                        ncols = 4, size = TRUE, quiet = TRUE),
                 "size must be numeric")
})

test_that("plot = FALSE does not produce a plot, but autoplot does", {
    d1 <- design(type = "crd", treatments = c(1, 5, 10, 20),
                 reps = 5, nrows = 4, ncols = 5, seed = 42, quiet = TRUE, plot = FALSE)

    expect_equal(names(d1), c("design", "satab", "seed"))
    expect_null(d1$plot.des)
    vdiffr::expect_doppelganger(title = "Plot produced with plot = FALSE", autoplot(d1))
})

test_that("autoplot responds to margin argument", {
    d1 <- design(type = "crd", treatments = c(1, 5, 10, 20),
                 reps = 5, nrows = 4, ncols = 5, seed = 42, quiet = TRUE)
    vdiffr::expect_doppelganger(title = "autoplot with margin", autoplot(d1, margin = TRUE))
})

test_that("autoplot responds to rotation argument", {
    d1 <- design(type = "crd", treatments = c(1, 5, 10, 20),
                 reps = 5, nrows = 4, ncols = 5, seed = 42, quiet = TRUE)
    vdiffr::expect_doppelganger(title = "autoplot with rotation", autoplot(d1, rotation = 90))
})

test_that("autoplot responds to size argument", {
    d1 <- design(type = "crd", treatments = c(1, 5, 10, 20),
                 reps = 5, nrows = 4, ncols = 5, seed = 42, quiet = TRUE)
    vdiffr::expect_doppelganger(title = "autoplot with size", autoplot(d1, size = 8))
})

test_that("Colour blind friendly plots work", {
    # CRD
    d1 <- design("crd", treatments = LETTERS[1:11], reps = 4,
                 nrows = 11, ncols = 4, seed = 42, quiet = TRUE, plot = FALSE)

    # RCBD
    d2 <- design("rcbd", treatments = LETTERS[1:11], reps = 4,
                 nrows = 11, ncols = 4, brows = 11, bcols = 1,
                 seed = 42, quiet = TRUE, plot = FALSE)


    expect_snapshot_output(d1$satab)
    expect_snapshot_output(d2$satab)
    vdiffr::expect_doppelganger(title = "CRD colour blind", autoplot(d1, palette = "colour blind"))
    vdiffr::expect_doppelganger(title = "RCBD colour blind", autoplot(d2, palette = "cb"))
})

test_that("Colour blind friendly viridis", {
    # CRD
    d1 <- design("crd", treatments = LETTERS[1:11], reps = 4,
                 nrows = 11, ncols = 4, seed = 42, quiet = TRUE, plot = FALSE)

    # RCBD
    d2 <- design("rcbd", treatments = LETTERS[1:11], reps = 4,
                 nrows = 11, ncols = 4, brows = 11, bcols = 1,
                 seed = 42, quiet = TRUE, plot = FALSE)


    vdiffr::expect_doppelganger(title = "CRD colour blind viridis", autoplot(d1, palette = "viridis"))
    vdiffr::expect_doppelganger(title = "RCBD colour blind viridis", autoplot(d2, palette = "viridis"))
})

test_that("Colour blind friendly magma", {
    # CRD
    d1 <- design("crd", treatments = LETTERS[1:11], reps = 4,
                 nrows = 11, ncols = 4, seed = 42, quiet = TRUE, plot = FALSE)

    # RCBD
    d2 <- design("rcbd", treatments = LETTERS[1:11], reps = 4,
                 nrows = 11, ncols = 4, brows = 11, bcols = 1,
                 seed = 42, quiet = TRUE, plot = FALSE)


    vdiffr::expect_doppelganger(title = "CRD colour blind magma", autoplot(d1, palette = "magma"))
    vdiffr::expect_doppelganger(title = "RCBD colour blind magma", autoplot(d2, palette = "magma"))
})

test_that("Colour blind friendly inferno", {
    # CRD
    d1 <- design("crd", treatments = LETTERS[1:11], reps = 4,
                 nrows = 11, ncols = 4, seed = 42, quiet = TRUE, plot = FALSE)

    # RCBD
    d2 <- design("rcbd", treatments = LETTERS[1:11], reps = 4,
                 nrows = 11, ncols = 4, brows = 11, bcols = 1,
                 seed = 42, quiet = TRUE, plot = FALSE)


    vdiffr::expect_doppelganger(title = "CRD colour blind inferno", autoplot(d1, palette = "inferno"))
    vdiffr::expect_doppelganger(title = "RCBD colour blind inferno", autoplot(d2, palette = "inferno"))
})

test_that("Colour blind friendly plasma", {
    # CRD
    d1 <- design("crd", treatments = LETTERS[1:11], reps = 4,
                 nrows = 11, ncols = 4, seed = 42, quiet = TRUE, plot = FALSE)

    # RCBD
    d2 <- design("rcbd", treatments = LETTERS[1:11], reps = 4,
                 nrows = 11, ncols = 4, brows = 11, bcols = 1,
                 seed = 42, quiet = TRUE, plot = FALSE)


    vdiffr::expect_doppelganger(title = "CRD colour blind plasma", autoplot(d1, palette = "plasma"))
    vdiffr::expect_doppelganger(title = "RCBD colour blind plasma", autoplot(d2, palette = "plasma"))
})

test_that("Colour blind friendly cividis", {
    # CRD
    d1 <- design("crd", treatments = LETTERS[1:11], reps = 4,
                 nrows = 11, ncols = 4, seed = 42, quiet = TRUE, plot = FALSE)

    # RCBD
    d2 <- design("rcbd", treatments = LETTERS[1:11], reps = 4,
                 nrows = 11, ncols = 4, brows = 11, bcols = 1,
                 seed = 42, quiet = TRUE, plot = FALSE)


    vdiffr::expect_doppelganger(title = "CRD colour blind cividis", autoplot(d1, palette = "cividis"))
    vdiffr::expect_doppelganger(title = "RCBD colour blind cividis", autoplot(d2, palette = "cividis"))
})

test_that("Various colour blind spellings and options", {
    # CRD
    d1 <- design("crd", treatments = LETTERS[1:11], reps = 4,
                 nrows = 11, ncols = 4, seed = 42, quiet = TRUE, plot = FALSE)

    # RCBD
    d2 <- design("rcbd", treatments = LETTERS[1:11], reps = 4,
                 nrows = 11, ncols = 4, brows = 11, bcols = 1,
                 seed = 42, quiet = TRUE, plot = FALSE)

    vdiffr::expect_doppelganger(title = "CRD colour blind option1", autoplot(d1, palette = "colour-blind"))
    vdiffr::expect_doppelganger(title = "CRD colour blind option2", autoplot(d1, palette = "colour blind"))
    vdiffr::expect_doppelganger(title = "CRD colour blind option3", autoplot(d1, palette = "colour_blind"))
    vdiffr::expect_doppelganger(title = "CRD colour blind option4", autoplot(d1, palette = "colour.blind"))
    vdiffr::expect_doppelganger(title = "CRD colour blind option5", autoplot(d1, palette = "colourblind"))

    vdiffr::expect_doppelganger(title = "CRD color blind option1", autoplot(d1, palette = "color-blind"))
    vdiffr::expect_doppelganger(title = "CRD color blind option2", autoplot(d1, palette = "color blind"))
    vdiffr::expect_doppelganger(title = "CRD color blind option3", autoplot(d1, palette = "color_blind"))
    vdiffr::expect_doppelganger(title = "CRD color blind option4", autoplot(d1, palette = "color.blind"))
    vdiffr::expect_doppelganger(title = "CRD color blind option5", autoplot(d1, palette = "colorblind"))

    vdiffr::expect_doppelganger(title = "RCBD colour blind option1", autoplot(d2, palette = "colour-blind"))
    vdiffr::expect_doppelganger(title = "RCBD color blind option1", autoplot(d2, palette = "color-blind"))

})

test_that("Alternative palattes work", {
    # CRD
    d1 <- design("crd", treatments = LETTERS[1:11], reps = 4,
                 nrows = 11, ncols = 4, seed = 42, quiet = TRUE, plot = FALSE)

    # RCBD
    d2 <- design("rcbd", treatments = LETTERS[1:11], reps = 4,
                 nrows = 11, ncols = 4, brows = 11, bcols = 1,
                 seed = 42, quiet = TRUE, plot = FALSE)


    vdiffr::expect_doppelganger(title = "CRD RdBu palatte", autoplot(d1, palette = "RdBu"))
    vdiffr::expect_doppelganger(title = "CRD Set3 palatte", autoplot(d1, palette = "Set3"))
    vdiffr::expect_doppelganger(title = "CRD Paired palatte", autoplot(d1, palette = "Paired"))
    vdiffr::expect_doppelganger(title = "RCBD RdBu palatte", autoplot(d2, palette = "RdBu"))
    vdiffr::expect_doppelganger(title = "RCBD Set3 palatte", autoplot(d2, palette = "Set3"))
    vdiffr::expect_doppelganger(title = "RCBD Paired palatte", autoplot(d2, palette = "Paired"))
})

test_that("Invalid palette option produces error", {
    # CRD
    d1 <- design("crd", treatments = LETTERS[1:11], reps = 4,
                 nrows = 11, ncols = 4, seed = 42, quiet = TRUE, plot = FALSE)

    expect_error(autoplot(d1, palette = "abc"), "Invalid value for palette.")
    expect_error(autoplot(d1, palette = "set3"), "Invalid value for palette.")
    expect_error(autoplot(d1, palette = "spectral"), "Invalid value for palette.")
})

#
