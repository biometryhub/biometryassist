test_that("CRD designs are supported", {
    # CRD
    d1 <- design(type = "crd", treatments = c(1, 5, 10, 20),
                 reps = 5, nrows = 4, ncols = 5, seed = 42, quiet = TRUE)

    expect_design_output(d1, expected_seed = 42)
    expect_design_df_starts_with(d1$design, c("row", "col"))
    expect_design_df_has_cols(d1$design, c("plots", "reps", "treatments"))
    expect_design_df_ends_with(d1$design, "treatments")
    expect_equal(d1$satab[4], "Residual                                16\n")
    expect_snapshot_output(d1$satab)
    vdiffr::expect_doppelganger(title = "CRD plot produced",
                                autoplot(d1), variant = ggplot2_variant())
})

test_that("RCBD designs are supported", {
    # RCBD
    d2 <- design("rcbd", treatments = LETTERS[1:11], reps = 4,
                 nrows = 11, ncols = 4, brows = 11, bcols = 1, seed = 42, quiet = TRUE)

    expect_design_output(d2, expected_seed = 42)
    expect_design_df_starts_with(d2$design, c("row", "col"))
    expect_design_df_has_cols(d2$design, c("plots", "block", "treatments"))
    expect_design_df_ends_with(d2$design, "treatments")
    expect_equal(d2$satab[3],
                 "Block stratum                           3\n")
    expect_snapshot_output(d2$satab)
    vdiffr::expect_doppelganger(title = "RCBD plot produced",
                                autoplot(d2), variant = ggplot2_variant())
})

test_that("RCBD with row-wise blocks are supported", {
    # RCBD with row-wise blocks
    d2.1 <- design("rcbd", treatments = LETTERS[1:6], reps = 4,
                   nrows = 4, ncols = 6, brows = 1, bcols = 6, seed = 42, quiet = TRUE)

    expect_design_output(d2.1, expected_seed = 42)
    expect_design_df_starts_with(d2.1$design, c("row", "col"))
    expect_design_df_has_cols(d2.1$design, c("plots", "block", "treatments"))
    expect_equal(d2.1$design$block, factor(d2.1$design$row))
    expect_equal(d2.1$satab[6], "Residual                                15\n")
    vdiffr::expect_doppelganger(title = "RCBD with row blocks",
                                autoplot(d2.1), variant = ggplot2_variant())

})

test_that("RCBD with square blocks are supported", {
    d2.2 <- design("rcbd", treatments = LETTERS[1:6], reps = 4,
                   nrows = 6, ncols = 4, brows = 3, bcols = 2, seed = 42, quiet = TRUE)

    expect_design_output(d2.2, expected_seed = 42)
    expect_design_df_starts_with(d2.2$design, c("row", "col"))
    expect_design_df_has_cols(d2.2$design, c("plots", "block", "treatments"))
    expect_equal(d2.2$satab[6], "Residual                                15\n")
    vdiffr::expect_doppelganger(title = "RCBD with square blocks",
                                autoplot(d2.2), variant = ggplot2_variant())
})

test_that("LSD designs are supported", {
    # LSD
    d3 <- design(type = "lsd", treatments = c("S1", "S2", "S3", "S4"),
                 nrows = 4, ncols = 4, seed = 42, quiet = TRUE)

    expect_design_output(d3, expected_seed = 42)
    expect_design_df_has_cols(d3$design, c("plots", "row", "col", "treatments"))
    expect_design_df_ends_with(d3$design, "treatments")
    expect_equal(d3$satab[6],
                 "Residual                                6\n")
    expect_snapshot_output(d3$satab)
    vdiffr::expect_doppelganger(title = "LSD plot produced",
                                autoplot(d3), variant = ggplot2_variant())
})

test_that("Split plot designs are supported", {
    # Split
    d4 <- design(type = "split", treatments = c("A", "B"),
                 sub_treatments = 1:4, reps = 4, nrows = 8,
                 ncols = 4, brows = 8, bcols = 1, seed = 42, quiet = TRUE)

    expect_design_output(d4, expected_seed = 42)
    expect_design_df_starts_with(d4$design, c("row", "col"))
    expect_design_df_has_cols(d4$design, c("plots", "block", "wholeplots", "subplots", "sub_treatments", "treatments"))
    expect_equal(d4$satab[11],
                 "         treatments:sub_treatments           3\n")
    expect_snapshot_output(d4$satab)
    vdiffr::expect_doppelganger(title = "Split plot produced",
                                autoplot(d4), variant = ggplot2_variant())
})

test_that("Strip plot designs are supported", {
    d_strip <- design(type = "strip", treatments = c("A", "B", "C", "D"),
                      sub_treatments = c("E", "F"), reps = 4, nrows = 8,
                      ncols = 4, brows = 4, bcols = 2, seed = 42, quiet = TRUE)

    expect_design_output(d_strip, expected_seed = 42)
    expect_design_df_starts_with(d_strip$design, c("row", "col"))
    expect_design_df_has_cols(d_strip$design, c("plots", "block", "wholeplots", "subplots",
                                                "wp_treatments", "sub_treatments", "treatments"))

    # Strip-plot structural constraint: within each block, each within-block row
    # has a single wp_treatments value; within each within-block column has a
    # single sub_treatments value.
    by_block_row <- aggregate(wp_treatments ~ block + row,
                              data = d_strip$design,
                              FUN = function(x) length(unique(x)))
    expect_true(all(by_block_row$wp_treatments == 1))

    by_block_col <- aggregate(sub_treatments ~ block + col,
                              data = d_strip$design,
                              FUN = function(x) length(unique(x)))
    expect_true(all(by_block_col$sub_treatments == 1))

    satab_text <- paste0(d_strip$satab, collapse = "")
    expect_match(satab_text, "Block stratum\\s+3")
    expect_match(satab_text, "Interaction Residual\\s+9")
    expect_match(satab_text, "Total\\s+31")
})

test_that("Split plot designs with names are supported", {
    d4.1 <- design(type = "split", treatments = c("A", "B"),
                   sub_treatments = 1:4, reps = 4, nrows = 8,
                   ncols = 4, brows = 4, bcols = 2,
                   fac.names = list(Water = c("Irrigated", "Rain-fed"),
                                    N = seq(50, 200, 50)),
                   seed = 42, quiet = TRUE)

    expect_design_output(d4.1, expected_seed = 42)
    expect_design_df_starts_with(d4.1$design, c("row", "col"))
    expect_design_df_has_cols(d4.1$design, c("plots", "block", "wholeplots", "subplots", "Water", "N", "treatments"))
    expect_equal(d4.1$satab[11],
                 "         Water:N                             3\n")
    expect_snapshot_output(d4.1$satab)
    vdiffr::expect_doppelganger(title = "Split plot with names",
                                autoplot(d4.1), variant = ggplot2_variant())
})

test_that("Split plot designs with double row blocks are supported", {
    d4.2 <- design(type = "split", treatments = c("A", "B"),
                   sub_treatments = 1:4, reps = 4, nrows = 8,
                   ncols = 4, brows = 1, bcols = 4, seed = 42, quiet = TRUE)

    expect_design_output(d4.2, expected_seed = 42)
    expect_design_df_starts_with(d4.2$design, c("row", "col"))
    expect_design_df_has_cols(d4.2$design, c("plots", "block", "wholeplots", "subplots", "sub_treatments", "treatments"))
    expect_equal(d4.2$satab[11],
                 "         treatments:sub_treatments           3\n")
    vdiffr::expect_doppelganger(title = "Split plot double row blocks",
                                autoplot(d4.2), variant = ggplot2_variant())
})

test_that("Split plot designs with ntrt == bcol are supported", {
    d4.3 <- design(type = "split", treatments = c("A", "B"),
                   sub_treatments = 1:4, reps = 4, nrows = 4,
                   ncols = 8, brows = 1, bcols = 8, seed = 42, quiet = TRUE)

    expect_design_output(d4.3, expected_seed = 42)
    expect_design_df_starts_with(d4.3$design, c("row", "col"))
    expect_design_df_has_cols(d4.3$design, c("plots", "block", "wholeplots", "subplots", "sub_treatments", "treatments"))
    expect_equal(d4.3$satab[11],
                 "         treatments:sub_treatments           3\n")
    vdiffr::expect_doppelganger(title = "Split plot ntrt == bcol",
                                autoplot(d4.3), variant = ggplot2_variant())
})

test_that("Split plot designs with column-wise arrangement are supported", {
    d4.4 <- design(type = "split", treatments = c("A", "B"),
                   sub_treatments = 1:4, reps = 4, nrows = 8,
                   ncols = 4, brows = 4, bcols = 2, byrow = FALSE, seed = 42, quiet = TRUE)

    expect_design_output(d4.4, expected_seed = 42)
    expect_design_df_starts_with(d4.4$design, c("row", "col"))
    expect_design_df_has_cols(d4.4$design, c("plots", "block", "wholeplots", "subplots", "sub_treatments", "treatments"))
    expect_equal(d4.4$satab[11],
                 "         treatments:sub_treatments           3\n")
    vdiffr::expect_doppelganger(title = "Split plot byrow = F",
                                autoplot(d4.4), variant = ggplot2_variant())
})

test_that("Crossed CRD designs are supported", {
    # Crossed, CRD
    d5 <- design(type = "crossed:crd", treatments = c(3, 2),
                 reps = 3, nrows = 6, ncols = 3,
                 fac.sep = c("", ""), seed = 42, quiet = TRUE)

    expect_design_output(d5, expected_seed = 42)
    expect_design_df_starts_with(d5$design, c("row", "col"))
    expect_design_df_has_cols(d5$design, c("plots", "reps", "A", "B", "treatments"))
    expect_design_df_ends_with(d5$design, "treatments")
    expect_equal(d5$satab[5],
                 "A:B                                     2\n")
    expect_snapshot_output(d5$satab)
    vdiffr::expect_doppelganger(title = "Factorial CRD plot no space sep",
                                autoplot(d5), variant = ggplot2_variant())

    # Crossed, CRD with renaming
    d5.1 <- design(type = "crossed:crd", treatments = c(3, 2),
                   reps = 3, nrows = 6, ncols = 3,
                   fac.names = list(N = c(50, 100, 150),
                                    Water = c("Irrigated", "Rain-fed")),
                   seed = 42, quiet = TRUE)

    expect_design_output(d5.1, expected_seed = 42)
    expect_design_df_starts_with(d5.1$design, c("row", "col"))
    expect_design_df_has_cols(d5.1$design, c("plots", "reps", "N", "Water", "treatments"))
    expect_equal(d5.1$satab[5],
                 "N:Water                                 2\n")
    expect_snapshot_output(d5.1$satab)
    vdiffr::expect_doppelganger(title = "Factorial CRD with names",
                                autoplot(d5.1), variant = ggplot2_variant())
})

test_that("Crossed RCBD designs are supported", {
    # Crossed RCBD
    d6 <- design(type = "crossed:rcbd", treatments = c(3, 2),
                 reps = 3, nrows = 6, ncols = 3, brows = 6, bcols = 1, seed = 42, quiet = TRUE)

    expect_design_output(d6, expected_seed = 42)
    expect_design_df_starts_with(d6$design, c("row", "col"))
    expect_design_df_has_cols(d6$design, c("plots", "block", "A", "B", "treatments"))
    expect_equal(d6$satab[8],
                 "Residual                                10\n")
    expect_snapshot_output(d6$satab)
    vdiffr::expect_doppelganger(title = "Factorial RCBD plot produced",
                                autoplot(d6), variant = ggplot2_variant())
})

test_that("Crossed RCBD designs with row blocks are supported", {
    d6.1 <- design(type = "crossed:rcbd", treatments = c(3, 2),
                   reps = 3, nrows = 3, ncols = 6, brows = 1, bcols = 6,
                   fac.sep = c(":", ""), seed = 42, quiet = TRUE)

    expect_design_output(d6.1, expected_seed = 42)
    expect_design_df_starts_with(d6.1$design, c("row", "col"))
    expect_design_df_has_cols(d6.1$design, c("plots", "block", "A", "B", "treatments"))
    expect_equal(d6.1$satab[8],
                 "Residual                                10\n")
    vdiffr::expect_doppelganger(title = "Factorial RCBD plot with row blocks",
                                autoplot(d6.1), variant = ggplot2_variant())
})

test_that("Crossed RCBD designs with double row blocks are supported", {
    d6.2 <- design(type = "crossed:rcbd", treatments = c(3, 2),
                   reps = 3, nrows = 6, ncols = 3, brows = 2, bcols = 3, seed = 42, quiet = TRUE)

    expect_design_output(d6.2, expected_seed = 42)
    expect_design_df_starts_with(d6.2$design, c("row", "col"))
    expect_design_df_has_cols(d6.2$design, c("plots", "block", "A", "B", "treatments"))
    expect_equal(d6.2$satab[8],
                 "Residual                                10\n")
    vdiffr::expect_doppelganger(title = "Factorial RCBD plot double row blocks",
                                autoplot(d6.2), variant = ggplot2_variant())
})

test_that("Crossed RCBD designs with square blocks are supported", {
    d6.3 <- design(type = "crossed:rcbd", treatments = c(3, 2),
                   reps = 4, nrows = 6, ncols = 4, brows = 3, bcols = 2, seed = 42, quiet = TRUE)

    expect_design_output(d6.3, expected_seed = 42)
    expect_design_df_starts_with(d6.3$design, c("row", "col"))
    expect_design_df_has_cols(d6.3$design, c("plots", "block", "A", "B", "treatments"))
    vdiffr::expect_doppelganger(title = "Factorial RCBD plot square blocks",
                                autoplot(d6.3), variant = ggplot2_variant())
})

test_that("Crossed LSD designs are supported", {
    # Crossed LSD with separator
    d7 <- design(type = "crossed:lsd", treatments = c(3, 2),
                 nrows = 6, ncols = 6, fac.sep = "_", seed = 42, quiet = TRUE)

    expect_design_output(d7, expected_seed = 42)
    expect_design_df_has_cols(d7$design, c("plots", "row", "col", "A", "B", "treatments"))
    expect_design_df_ends_with(d7$design, "treatments")
    expect_equal(d7$satab[3],
                 "Row                                     5\n")
    expect_snapshot_output(d7$satab)
    vdiffr::expect_doppelganger(title = "Factorial LSD plot with sep",
                                autoplot(d7), variant = ggplot2_variant())
})

test_that("Crossed LSD designs with names are supported", {
    d7.1 <- design(type = "crossed:lsd", treatments = c(3, 2),
                   nrows = 6, ncols = 6,
                   fac.names = list(N = c(50, 100, 150),
                                    W = c("I", "R")),
                   seed = 42, quiet = TRUE)

    expect_design_output(d7.1, expected_seed = 42)
    expect_design_df_has_cols(d7.1$design, c("plots", "row", "col", "N", "W", "treatments"))
    expect_equal(d7.1$satab[3],
                 "Row                                     5\n")
    vdiffr::expect_doppelganger(title = "Factorial LSD with names",
                                autoplot(d7.1), variant = ggplot2_variant())
})

test_that("Crossed LSD designs with names and separator are supported", {
    d7.2 <- design(type = "crossed:lsd", treatments = c(3, 2),
                   nrows = 6, ncols = 6,
                   fac.names = list(N = c(50, 100, 150),
                                    W = c("I", "R")),
                   fac.sep = c(":", ""), seed = 42, quiet = TRUE)

    expect_design_output(d7.2, expected_seed = 42)
    expect_design_df_has_cols(d7.2$design, c("plots", "row", "col", "N", "W", "treatments"))
    expect_equal(d7.2$satab[3],
                 "Row                                     5\n")
    vdiffr::expect_doppelganger(title = "Factorial LSD plot names and sep",
                                autoplot(d7.2), variant = ggplot2_variant())
})

test_that("Nested designs are supported", {
    # Nested LSD
    d8 <- design(type = "lsd", treatments = c("A1", "A2", "A3", "A4", "B1", "B2", "B3"),
                 nrows = 7, ncols = 7, seed = 42, quiet = TRUE)

    expect_design_output(d8, expected_seed = 42)
    expect_design_df_has_cols(d8$design, c("plots", "row", "col", "treatments"))
    expect_equal(d8$satab[6],
                 "Residual                                30\n")
    expect_snapshot_output(d8$satab)
    vdiffr::expect_doppelganger(title = "Nested LSD",
                                autoplot(d8), variant = ggplot2_variant())
})

test_that("3 way factorial designs are possible", {
    d9 <- design(type = "crossed:crd", treatments = c(2, 2, 2),
                 reps = 3, nrows = 6, ncols = 4, seed = 42, quiet = TRUE)

    expect_design_output(d9, expected_seed = 42)
    expect_design_df_starts_with(d9$design, c("row", "col"))
    expect_design_df_has_cols(d9$design, c("plots", "reps", "A", "B", "C", "treatments"))
    expect_equal(d9$satab[6],
                 "A:B:C                                   1\n")
    expect_snapshot_output(d9$satab)
    vdiffr::expect_doppelganger(title = "3 way factorial",
                                autoplot(d9), variant = ggplot2_variant())

    d9.1 <- design(type = "crossed:crd", treatments = c(2, 2, 2),
                   reps = 3, nrows = 6, ncols = 4,
                   fac.names = list(X = c("A", "B"), Y = 1:2, Z = c(10, 20)),
                   seed = 42, quiet = TRUE)

    expect_design_output(d9.1, expected_seed = 42)
    expect_design_df_starts_with(d9.1$design, c("row", "col"))
    expect_design_df_has_cols(d9.1$design, c("plots", "reps", "X", "Y", "Z", "treatments"))
    expect_equal(d9.1$satab[6],
                 "X:Y:Z                                   1\n")
    expect_snapshot_output(d9.1$satab)
    vdiffr::expect_doppelganger(title = "3 way factorial with names",
                                autoplot(d9.1), variant = ggplot2_variant())
})

test_that("Adding names to 3 way factorial designs works", {
    d9.2 <- design(type = "crossed:rcbd", treatments = c(2, 2, 2),
                   reps = 3, nrows = 8, ncols = 3, brows = 8, bcols = 1,
                   fac.names = list(X = c("A", "B"), Y = 1:2, Z = c(10, 20)),
                   seed = 42, quiet = TRUE)

    expect_design_output(d9.2, expected_seed = 42)
    expect_design_df_starts_with(d9.2$design, c("row", "col"))
    expect_design_df_has_cols(d9.2$design, c("plots", "block", "X", "Y", "Z", "treatments"))
    expect_identical(levels(d9.2$design$X), c("A", "B"))
    expect_identical(levels(d9.2$design$Y), as.character(1:2))
    expect_identical(levels(d9.2$design$Z), as.character(c(10, 20)))
    expect_equal(d9.2$satab[3],
                 "Block stratum                           2\n")
    expect_snapshot_output(d9.2$satab)
    vdiffr::expect_doppelganger(title = "3 way rcbd factorial with names",
                                autoplot(d9.2), variant = ggplot2_variant())
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
                 "seed must be numeric or TRUE/FALSE")

    # seed = "ABC"
    expect_error(design(type = "crd", treatments = c(1, 5, 10, 20),
                        reps = 5, nrows = 4, ncols = 5, seed = "ABC", quiet = TRUE),
                 "seed must be numeric or TRUE/FALSE")
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
    expect_error(design(type = "split", treatments = c("A", "B"), quiet = TRUE,
                        sub_treatments = NULL, reps = 4, nrows = 8,
                        ncols = 4, brows = 4, bcols = 2, seed = 42),
                 "sub_treatments are required for a split plot design")
})

test_that("strip plot requires sub_treatments", {
    expect_error(design(type = "strip", treatments = c("A", "B", "C"), quiet = TRUE,
               sub_treatments = NULL, reps = 4, nrows = 12,
               ncols = 4, brows = 3, bcols = 4, seed = 42),
           "sub_treatments are required for a strip plot design")

    expect_error(design(type = "strip", treatments = c("A", "B", "C"), quiet = TRUE,
               sub_treatments = c(1, NA), reps = 4, nrows = 12,
               ncols = 4, brows = 3, bcols = 4, seed = 42),
           "sub_treatments are required for a strip plot design")
})

test_that("split plot requires brows and bcols", {
    expect_error(design(type = "split", treatments = c("A", "B"), quiet = TRUE,
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
    vdiffr::expect_doppelganger(title = "Split plot with vector names",
                                autoplot(d11), variant = ggplot2_variant())
})

test_that("split plot produces warning when incorrect number of treatment labels given", {
    expect_warning(design(type = "split", treatments = c("A", "B"), quiet = TRUE,
                          sub_treatments = 1:4, reps = 4, nrows = 8,
                          ncols = 4, brows = 4, bcols = 2, seed = 42,
                          fac.names = list(Water = "ABC",
                                           N = 1:4)),
                   "Water must contain the correct number of elements. Elements have not been applied.")
    expect_warning(design(type = "split", treatments = c("A", "B"), quiet = TRUE,
                          sub_treatments = 1:4, reps = 4, nrows = 8,
                          ncols = 4, brows = 4, bcols = 2, seed = 42,
                          fac.names = list(Water = c("A", "B"),
                                           N = 1:10)),
                   "N must contain the correct number of elements. Elements have not been applied.")
    expect_warning(design(type = "split", treatments = c("A", "B"), quiet = TRUE,
                          sub_treatments = 1:4, reps = 4, nrows = 8,
                          ncols = 4, brows = 4, bcols = 2, seed = 42,
                          fac.names = list(Water = c("A", "B"),
                                           N = 1:4,
                                           Another = 1:5)),
                   "fac.names contains 3 elements but only the first 2 have been used.")

    expect_warning(design(type = "split", treatments = c("A", "B"), quiet = TRUE,
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
    expect_error(design(type = "crd", treatments = c(1, 5, 10, 20),
                        reps = 5, nrows = 4, ncols = 5, seed = 42, Width = 6, quiet = TRUE), NULL)
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
    withr::local_file("crd_design1.csv")
    d <- design("crd", treatments = 1:11, reps = 4, nrows = 11, ncols = 4,
                save = "workbook", savename = "crd_design1", quiet = TRUE)

    expect_csv_matches_df(d$design, "crd_design1.csv")
    expect_false(file.exists("crd_design1.pdf"))
})

test_that("save = 'plot' produces plot file and not csv", {
    withr::local_file("crd_design2.pdf")
    expect_message(
        design("crd", treatments = 1:11, reps = 4, nrows = 11, ncols = 4,
               save = "plot", savename = "crd_design2", quiet = TRUE),
        "Saving [0-9]\\.?[0-9]* x [0-9]\\.?[0-9]* in image")
    expect_false(file.exists("crd_design2.csv"))
    expect_true(file.exists("crd_design2.pdf"))
})

test_that("save = 'both' produces plot file and csv", {
    withr::local_file("crd_design3.pdf")
    withr::local_file("crd_design3.csv")
    expect_message(
        d <- design("crd", treatments = 1:11, reps = 4, nrows = 11, ncols = 4,
                    save = "both", savename = "crd_design3", quiet = TRUE),
        "Saving [0-9]\\.?[0-9]* x [0-9]\\.?[0-9]* in image")

    expect_csv_matches_df(d$design, "crd_design3.csv")
    expect_true(file.exists("crd_design3.pdf"))
})

test_that("save = TRUE produces plot file and csv", {
    withr::local_file("crd_design4.pdf")
    withr::local_file("crd_design4.csv")
    expect_message(
        d <- design("crd", treatments = 1:11, reps = 4, nrows = 11, ncols = 4,
                    save = TRUE, savename = "crd_design4", quiet = TRUE),
        "Saving [0-9]\\.?[0-9]* x [0-9]\\.?[0-9]* in image")

    expect_csv_matches_df(d$design, "crd_design4.csv")
    expect_true(file.exists("crd_design4.pdf"))
})

test_that("Output is produced when quiet = FALSE", {
    withr::local_file("Rplots.pdf")
    expect_output(des <- design("crd", treatments = 1:11, reps = 4,
                                nrows = 11, ncols = 4, quiet = FALSE),
                  "Source of Variation                     df")
    expect_snapshot(cat(des$satab))
    vdiffr::expect_doppelganger(title = "Plot output", des$plot.des, variant = ggplot2_variant())
})

test_that("designs have a class of 'design'", {
    d1 <- design("crd", treatments = 1:11, reps = 4,
                 nrows = 11, ncols = 4, quiet = TRUE)
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
    vdiffr::expect_doppelganger(title = "Plot produced with plot = FALSE",
                                autoplot(d1), variant = ggplot2_variant())
})

test_that("autoplot responds to margin argument", {
    d1 <- design(type = "crd", treatments = c(1, 5, 10, 20),
                 reps = 5, nrows = 4, ncols = 5, seed = 42, quiet = TRUE)
    vdiffr::expect_doppelganger(title = "autoplot with margin",
                                autoplot(d1, margin = TRUE), variant = ggplot2_variant())
})

test_that("autoplot responds to rotation argument", {
    d1 <- design(type = "crd", treatments = c(1, 5, 10, 20),
                 reps = 5, nrows = 4, ncols = 5, seed = 42, quiet = TRUE)
    vdiffr::expect_doppelganger(title = "autoplot with rotation",
                                autoplot(d1, rotation = 90), variant = ggplot2_variant())
})

test_that("autoplot responds to size argument", {
    d1 <- design(type = "crd", treatments = c(1, 5, 10, 20),
                 reps = 5, nrows = 4, ncols = 5, seed = 42, quiet = TRUE)
    vdiffr::expect_doppelganger(title = "autoplot with size",
                                autoplot(d1, size = 8), variant = ggplot2_variant())
})

test_that("autoplot responds to legend argument", {
    d1 <- design(type = "crd", treatments = c(1, 5, 10, 20),
                 reps = 5, nrows = 4, ncols = 5, seed = 42, quiet = TRUE)
    vdiffr::expect_doppelganger(title = "autoplot no legend",
                                autoplot(d1, legend = FALSE), variant = ggplot2_variant())
})

test_that("Colour blind friendly plots work", {
    # CRD
    d1 <- design("crd", treatments = LETTERS[1:11], reps = 4,
                 nrows = 11, ncols = 4, seed = 42, quiet = TRUE)

    # RCBD
    d2 <- design("rcbd", treatments = LETTERS[1:11], reps = 4,
                 nrows = 11, ncols = 4, brows = 11, bcols = 1,
                 seed = 42, quiet = TRUE)


    expect_snapshot_output(d1$satab)
    expect_snapshot_output(d2$satab)
    vdiffr::expect_doppelganger(title = "CRD colour blind",
                                autoplot(d1, palette = "colour blind"), variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "RCBD colour blind",
                                autoplot(d2, palette = "cb"), variant = ggplot2_variant())
})

test_that("Colour blind friendly viridis", {
    # CRD
    d1 <- design("crd", treatments = LETTERS[1:11], reps = 4,
                 nrows = 11, ncols = 4, seed = 42, quiet = TRUE)

    # RCBD
    d2 <- design("rcbd", treatments = LETTERS[1:11], reps = 4,
                 nrows = 11, ncols = 4, brows = 11, bcols = 1,
                 seed = 42, quiet = TRUE)


    vdiffr::expect_doppelganger(title = "CRD colour blind viridis",
                                autoplot(d1, palette = "viridis"), variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "RCBD colour blind viridis",
                                autoplot(d2, palette = "viridis"), variant = ggplot2_variant())
})

test_that("Colour blind friendly magma", {
    # CRD
    d1 <- design("crd", treatments = LETTERS[1:11], reps = 4,
                 nrows = 11, ncols = 4, seed = 42, quiet = TRUE)

    # RCBD
    d2 <- design("rcbd", treatments = LETTERS[1:11], reps = 4,
                 nrows = 11, ncols = 4, brows = 11, bcols = 1,
                 seed = 42, quiet = TRUE)


    vdiffr::expect_doppelganger(title = "CRD colour blind magma",
                                autoplot(d1, palette = "magma"), variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "RCBD colour blind magma",
                                autoplot(d2, palette = "magma"), variant = ggplot2_variant())
})

test_that("Colour blind friendly inferno", {
    # CRD
    d1 <- design("crd", treatments = LETTERS[1:11], reps = 4,
                 nrows = 11, ncols = 4, seed = 42, quiet = TRUE)

    # RCBD
    d2 <- design("rcbd", treatments = LETTERS[1:11], reps = 4,
                 nrows = 11, ncols = 4, brows = 11, bcols = 1,
                 seed = 42, quiet = TRUE)


    vdiffr::expect_doppelganger(title = "CRD colour blind inferno",
                                autoplot(d1, palette = "inferno"), variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "RCBD colour blind inferno",
                                autoplot(d2, palette = "inferno"), variant = ggplot2_variant())
})

test_that("Colour blind friendly plasma", {
    # CRD
    d1 <- design("crd", treatments = LETTERS[1:11], reps = 4,
                 nrows = 11, ncols = 4, seed = 42, quiet = TRUE)

    # RCBD
    d2 <- design("rcbd", treatments = LETTERS[1:11], reps = 4,
                 nrows = 11, ncols = 4, brows = 11, bcols = 1,
                 seed = 42, quiet = TRUE)


    vdiffr::expect_doppelganger(title = "CRD colour blind plasma",
                                autoplot(d1, palette = "plasma"), variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "RCBD colour blind plasma",
                                autoplot(d2, palette = "plasma"), variant = ggplot2_variant())
})

test_that("Colour blind friendly cividis", {
    # CRD
    d1 <- design("crd", treatments = LETTERS[1:11], reps = 4,
                 nrows = 11, ncols = 4, seed = 42, quiet = TRUE)

    # RCBD
    d2 <- design("rcbd", treatments = LETTERS[1:11], reps = 4,
                 nrows = 11, ncols = 4, brows = 11, bcols = 1,
                 seed = 42, quiet = TRUE)


    vdiffr::expect_doppelganger(title = "CRD colour blind cividis",
                                autoplot(d1, palette = "cividis"), variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "RCBD colour blind cividis",
                                autoplot(d2, palette = "cividis"), variant = ggplot2_variant())
})

test_that("Various colour blind spellings and options", {
    # CRD
    d1 <- design("crd", treatments = LETTERS[1:11], reps = 4,
                 nrows = 11, ncols = 4, seed = 42, quiet = TRUE)

    # RCBD
    d2 <- design("rcbd", treatments = LETTERS[1:11], reps = 4,
                 nrows = 11, ncols = 4, brows = 11, bcols = 1,
                 seed = 42, quiet = TRUE)

    vdiffr::expect_doppelganger(title = "CRD colour blind option1",
                                autoplot(d1, palette = "colour-blind"), variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "CRD colour blind option2",
                                autoplot(d1, palette = "colour blind"), variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "CRD colour blind option3",
                                autoplot(d1, palette = "colour_blind"), variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "CRD colour blind option4",
                                autoplot(d1, palette = "colour.blind"), variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "CRD colour blind option5",
                                autoplot(d1, palette = "colourblind"), variant = ggplot2_variant())

    vdiffr::expect_doppelganger(title = "CRD color blind option1",
                                autoplot(d1, palette = "color-blind"), variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "CRD color blind option2",
                                autoplot(d1, palette = "color blind"), variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "CRD color blind option3",
                                autoplot(d1, palette = "color_blind"), variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "CRD color blind option4",
                                autoplot(d1, palette = "color.blind"), variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "CRD color blind option5",
                                autoplot(d1, palette = "colorblind"), variant = ggplot2_variant())

    vdiffr::expect_doppelganger(title = "RCBD colour blind option1",
                                autoplot(d2, palette = "colour-blind"), variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "RCBD color blind option1",
                                autoplot(d2, palette = "color-blind"), variant = ggplot2_variant())

})

test_that("Alternative palettes work", {
    # CRD
    d1 <- design("crd", treatments = LETTERS[1:11], reps = 4,
                 nrows = 11, ncols = 4, seed = 42, quiet = TRUE)

    # RCBD
    d2 <- design("rcbd", treatments = LETTERS[1:11], reps = 4,
                 nrows = 11, ncols = 4, brows = 11, bcols = 1,
                 seed = 42, quiet = TRUE)


    vdiffr::expect_doppelganger(title = "CRD RdBu palette",
                                autoplot(d1, palette = "RdBu"), variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "CRD Set3 palette",
                                autoplot(d1, palette = "Set3"), variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "CRD Paired palette",
                                autoplot(d1, palette = "Paired"), variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "RCBD RdBu palette",
                                autoplot(d2, palette = "RdBu"), variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "RCBD Set3 palette",
                                autoplot(d2, palette = "Set3"), variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "RCBD Paired palette",
                                autoplot(d2, palette = "Paired"), variant = ggplot2_variant())
})

test_that("Users can provide custom colours for the palette argument", {
    # CRD
    d1 <- design("crd", treatments = LETTERS[1:5], reps = 4,
                 nrows = 5, ncols = 4, seed = 42, quiet = TRUE)

    vdiffr::expect_doppelganger(title = "Custom palette",
                                autoplot(d1, palette = c("red",
                                                         "blue",
                                                         "orange",
                                                         "darkgreen",
                                                         "purple")),
                                variant = ggplot2_variant())
})

test_that("Incorrect number of custom colours for palette results in error", {
    # CRD
    d1 <- design("crd", treatments = LETTERS[1:5], reps = 4,
                 nrows = 5, ncols = 4, seed = 42, quiet = TRUE)

    expect_error(autoplot(d1, palette = c("red", "blue")),
                 "palette needs to be a single string to choose a predefined palette, or 5 custom colours\\.")
    expect_error(autoplot(d1, palette = c("red", "blue", "red", "blue", "red", "blue")),
                 "palette needs to be a single string to choose a predefined palette, or 5 custom colours\\.")
})

test_that("Invalid palette option produces error", {
    # CRD
    d1 <- design("crd", treatments = LETTERS[1:11], reps = 4,
                 nrows = 11, ncols = 4, seed = 42, quiet = TRUE)

    expect_error(autoplot(d1, palette = "abc"), "Invalid value for palette.")
    expect_error(autoplot(d1, palette = 1), "Invalid value for palette.")
})

test_that("Adding buffers to plots works", {
    # CRD
    d1_nobuffer <- design("crd", treatments = LETTERS[1:11], reps = 4,
                          nrows = 11, ncols = 4, seed = 42, quiet = TRUE)
    d1 <- design("crd", treatments = LETTERS[1:11], reps = 4,
                 nrows = 11, ncols = 4, seed = 42, quiet = TRUE, buffer = "row")
    d2 <- design("crd", treatments = LETTERS[1:11], reps = 4,
                 nrows = 11, ncols = 4, seed = 42, quiet = TRUE, buffer = "column")
    d3 <- design("crd", treatments = LETTERS[1:11], reps = 4,
                 nrows = 11, ncols = 4, seed = 42, quiet = TRUE, buffer = "edge")
    d4 <- design("crd", treatments = LETTERS[1:11], reps = 4,
                 nrows = 11, ncols = 4, seed = 42, quiet = TRUE, buffer = "double row")
    d5 <- design("crd", treatments = LETTERS[1:11], reps = 4,
                 nrows = 11, ncols = 4, seed = 42, quiet = TRUE, buffer = "double column")

    expect_false(identical(d1, d1_nobuffer))
    expect_false(identical(d1, d2))
    expect_in("buffer", d1$design$treatments)

    expect_equal(n_unique(d1$design$row), 23)
    expect_equal(n_unique(d1$design$col), 4)
    vdiffr::expect_doppelganger(title = "Row buffers",
                                autoplot(d1), variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "Column buffers",
                                autoplot(d2), variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "Edge buffers",
                                autoplot(d3), variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "Double row buffers",
                                autoplot(d4), variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "Double Column buffers",
                                autoplot(d5), variant = ggplot2_variant())
})

test_that("Adding buffers to plots works for RCBD", {
    # RCBD
    d1 <- design("rcbd", treatments = LETTERS[1:11], reps = 4,
                 nrows = 11, ncols = 4, brows = 11, bcols = 1,
                 seed = 42, quiet = TRUE, buffer = "row")
    d2 <- design("rcbd", treatments = LETTERS[1:11], reps = 4,
                 nrows = 11, ncols = 4, brows = 11, bcols = 1,
                 seed = 42, quiet = TRUE, buffer = "column")
    d3 <- design("rcbd", treatments = LETTERS[1:11], reps = 4,
                 nrows = 11, ncols = 4, brows = 11, bcols = 1,
                 seed = 42, quiet = TRUE, buffer = "edge")
    d4 <- design("rcbd", treatments = LETTERS[1:11], reps = 4,
                 nrows = 11, ncols = 4, brows = 11, bcols = 1,
                 seed = 42, quiet = TRUE, buffer = "double row")
    d5 <- design("rcbd", treatments = LETTERS[1:11], reps = 4,
                 nrows = 11, ncols = 4, brows = 11, bcols = 1,
                 seed = 42, quiet = TRUE, buffer = "double column")

    expect_in("buffer", d1$design$treatments)

    expect_equal(n_unique(d2$design$row), 11)
    expect_equal(n_unique(d2$design$col), 9)
    vdiffr::expect_doppelganger(title = "Row buffers RCBD",
                                autoplot(d1), variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "Column buffers RCBD",
                                autoplot(d2), variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "Edge buffers RCBD",
                                autoplot(d3), variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "Double row buffers RCBD",
                                autoplot(d4), variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "Double Column buffers RCBD",
                                autoplot(d5), variant = ggplot2_variant())
})

test_that("Invalid buffer options produce an error", {
    # RCBD
    expect_error(design("rcbd", treatments = LETTERS[1:11], reps = 4,
                        nrows = 11, ncols = 4, brows = 11, bcols = 1,
                        seed = 42, quiet = TRUE, buffer = "block"),
                 "Block buffers are not yet supported\\.")

    expect_error(design("rcbd", treatments = LETTERS[1:11], reps = 4,
                        nrows = 11, ncols = 4, brows = 11, bcols = 1,
                        seed = 42, quiet = TRUE, buffer = "abc"),
                 "Invalid buffer option: abc")
})

test_that("Ability to provide arbitrary column names for plotting works", {
    des <- expand.grid(ro = 1:4, co = 1:5)
    des$bl <- des$co
    set.seed(42)
    des$treat <- sample(rep(LETTERS[1:4], times = 5))
    class(des) <- c("design", class(des))
    vdiffr::expect_doppelganger(title = "Quoted column names without blocks",
                                autoplot(des, row = "ro", column = "co", treatments = "treat"),
                                variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "Quoted column names with blocks",
                                autoplot(des, row = "ro", column = "co", treatments = "treat"),
                                variant = ggplot2_variant())
})

test_that("Arbitrary unquoted column names for plotting works", {
    des <- expand.grid(ro = 1:4, co = 1:5)
    des$bl <- des$ro
    set.seed(42)
    des$treat <- sample(rep(LETTERS[1:5], times = 4))
    class(des) <- c("design", class(des))
    vdiffr::expect_doppelganger(title = "NSE of column names without blocks",
                                autoplot(des, row = ro, column = co, treatments = treat),
                                variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "NSE of column names with blocks",
                                autoplot(des, row = ro, column = co, block = bl, treatments = treat),
                                variant = ggplot2_variant())
})


# des_info() (deprecated) tests ----

test_that("des_info() returns a CRD design without plotting", {
    crd_obj <- agricolae::design.crd(trt = c(1, 5, 10, 20), r = 2, seed = 42)

    # Simulate agricolae naming the treatment column from the expression passed.
    if (is.data.frame(crd_obj$book)) {
        struct_cols <- c("plots", "r", "block", "row", "col")
        candidates <- setdiff(names(crd_obj$book), struct_cols)
        if (length(candidates) >= 1) {
            trt_col <- candidates[length(candidates)]
            names(crd_obj$book)[names(crd_obj$book) == trt_col] <- "c(1, 5, 10, 20)"
        }
    }

    expect_warning(
        out <- des_info(
            design.obj = crd_obj,
            nrows = 4,
            ncols = 2,
            plot = FALSE,
            quiet = TRUE,
            save = FALSE,
            return.seed = TRUE
        ),
        "des_info\\(\\) is deprecated"
    )

    expect_design_output(out, expected_names = c("design", "satab", "seed"), expected_seed = 42, expect_plot = FALSE)
    expect_design_df_starts_with(out$design, c("row", "col"))
    expect_design_df_has_cols(out$design, c("plots", "reps", "treatments"))
})

test_that("des_info() errors for block designs when brows/bcols are missing", {
    rcbd_obj <- agricolae::design.rcbd(trt = c("T1", "T2", "T3"), r = 2, seed = 42)

    expect_error(
        suppressWarnings(
            des_info(
                design.obj = rcbd_obj,
                nrows = 3,
                ncols = 2,
                plot = FALSE,
                quiet = TRUE
            )
        ),
        "brows and bcols must be supplied"
    )
})

test_that("des_info() applies fac.names for factorial designs", {
    fac_obj <- agricolae::design.ab(trt = c(2, 2), r = 2, design = "crd", seed = 42)

    expect_warning(
        out <- des_info(
            design.obj = fac_obj,
            nrows = 4,
            ncols = 2,
            fac.names = list(N = c("Low", "High"), Water = c("Dry", "Wet")),
            plot = FALSE,
            quiet = TRUE,
            save = FALSE
        ),
        "des_info\\(\\) is deprecated"
    )

    expect_true("N" %in% names(out$design))
    expect_true("Water" %in% names(out$design))
    expect_false("A" %in% names(out$design))
    expect_false("B" %in% names(out$design))
    expect_equal(levels(out$design$N), c("Low", "High"))
    expect_equal(levels(out$design$Water), c("Dry", "Wet"))
    expect_true("treatments" %in% names(out$design))
})

test_that("des_info() recycles fac.sep when length 1", {
    fac_obj <- agricolae::design.ab(trt = c(2, 2), r = 2, design = "crd", seed = 42)

    expect_warning(
        out <- des_info(
            design.obj = fac_obj,
            nrows = 4,
            ncols = 2,
            fac.sep = ":",
            plot = FALSE,
            quiet = TRUE,
            save = FALSE
        ),
        "des_info\\(\\) is deprecated"
    )

    expect_true("treatments" %in% names(out$design))
    expect_true(any(grepl(":", as.character(out$design$treatments), fixed = TRUE)))
})

test_that("des_info() renames split plot factor columns when fac.names is a character vector", {
    split_obj <- agricolae::design.split(trt1 = c("A", "B"), trt2 = 1:2, r = 2, seed = 42)

    # Simulate agricolae naming treatment columns from the expressions passed.
    if (is.data.frame(split_obj$book)) {
        struct_cols <- c("plots", "block", "r", "row", "col", "splots", "wplots", "wholeplots", "subplots")
        candidates <- setdiff(names(split_obj$book), struct_cols)
        if (length(candidates) >= 2) {
            main_col <- candidates[length(candidates) - 1]
            sub_col <- candidates[length(candidates)]
            names(split_obj$book)[names(split_obj$book) == main_col] <- "c(\"A\", \"B\")"
            names(split_obj$book)[names(split_obj$book) == sub_col] <- "1:2"
        }
    }

    expect_warning(
        out <- des_info(
            design.obj = split_obj,
            nrows = 4,
            ncols = 2,
            brows = 4,
            bcols = 2,
            byrow = FALSE,
            fac.names = c("Main", "Sub"),
            plot = FALSE,
            quiet = TRUE,
            save = FALSE
        ),
        "des_info\\(\\) is deprecated"
    )

    expect_true("Main" %in% names(out$design))
    expect_true("Sub" %in% names(out$design))
    expect_true("treatments" %in% names(out$design))
    expect_false("sub_treatments" %in% names(out$design))
    expect_s3_class(out$design$Main, "factor")
})

test_that("des_info() applies fac.names list for split designs", {
    split_obj <- agricolae::design.split(trt1 = c("A", "B"), trt2 = 1:2, r = 2, seed = 42)

    # Also simulate agricolae naming from expressions; this forces
    # normalize_agricolae_book() to infer treatment columns by position.
    if (is.data.frame(split_obj$book)) {
        struct_cols <- c("plots", "block", "r", "row", "col", "splots", "wplots", "wholeplots", "subplots")
        candidates <- setdiff(names(split_obj$book), struct_cols)
        if (length(candidates) >= 2) {
            main_col <- candidates[length(candidates) - 1]
            sub_col <- candidates[length(candidates)]
            names(split_obj$book)[names(split_obj$book) == main_col] <- "c(\"A\", \"B\")"
            names(split_obj$book)[names(split_obj$book) == sub_col] <- "1:2"
        }
    }

    expect_warning(
        out <- des_info(
            design.obj = split_obj,
            nrows = 4,
            ncols = 2,
            brows = 4,
            bcols = 2,
            byrow = FALSE,
            fac.names = list(
                Main = c("Irrigated", "Rain-fed"),
                Sub = c("Low", "High")
            ),
            plot = FALSE,
            quiet = TRUE,
            save = FALSE
        ),
        "des_info\\(\\) is deprecated"
    )

    expect_true("Main" %in% names(out$design))
    expect_true("Sub" %in% names(out$design))
    expect_true("treatments" %in% names(out$design))
    expect_false("sub_treatments" %in% names(out$design))
    expect_s3_class(out$design$Main, "factor")
    expect_s3_class(out$design$Sub, "factor")
    expect_equal(levels(out$design$Main), c("Irrigated", "Rain-fed"))
    expect_equal(levels(out$design$Sub), c("Low", "High"))
})

test_that("normalise_agricolae_book() renames split trt1/trt2 columns", {
    design_info <- list(is_factorial = FALSE, type = "split", base = "split")

    design_book <- data.frame(
        plots = 1:2,
        trt1 = c("A", "B"),
        trt2 = c("Low", "High")
    )

    out <- biometryassist:::normalise_agricolae_book(design_book, design_info)

    expect_true("treatments" %in% names(out))
    expect_true("sub_treatments" %in% names(out))
    expect_false("trt1" %in% names(out))
    expect_false("trt2" %in% names(out))
})

test_that("normalise_agricolae_book() renames strip trt1/trt2 columns", {
    design_info <- list(is_factorial = FALSE, type = "strip", base = "strip")

    design_book <- data.frame(
        plots = 1:2,
        trt1 = c("A", "B"),
        trt2 = c("Low", "High")
    )

    out <- biometryassist:::normalise_agricolae_book(design_book, design_info)

    expect_true("treatments" %in% names(out))
    expect_true("sub_treatments" %in% names(out))
    expect_false("trt1" %in% names(out))
    expect_false("trt2" %in% names(out))
})

test_that("normalise_agricolae_book() infers split sub_treatments from candidates", {
    design_info <- list(is_factorial = FALSE, type = "split", base = "split")

    # Simulate a split book where the main treatment column has already been
    # standardised, but the subplot column has not.
    design_book <- data.frame(
        plots = 1:2,
        treatments = c("A", "B"),
        subplot_col = c("Low", "High")
    )

    out <- biometryassist:::normalise_agricolae_book(design_book, design_info)

    expect_true("treatments" %in% names(out))
    expect_true("sub_treatments" %in% names(out))
    expect_false("subplot_col" %in% names(out))
})

test_that("normalise_agricolae_book() infers split treatments from single candidate", {
    design_info <- list(is_factorial = FALSE, type = "split", base = "split")

    # Simulate a split/strip-style book that has no treatments column yet and
    # only one non-structural candidate column.
    design_book <- data.frame(
        plots = 1:2,
        main_col = c("A", "B")
    )

    out <- biometryassist:::normalise_agricolae_book(design_book, design_info)

    expect_true("treatments" %in% names(out))
    expect_false("main_col" %in% names(out))
    expect_false("sub_treatments" %in% names(out))
})

test_that("normalise_agricolae_book() infers split treatments and sub_treatments from two candidates", {
    design_info <- list(is_factorial = FALSE, type = "split", base = "split")

    # No explicit trt1/trt2 and no pre-existing treatments/sub_treatments,
    # so both should be inferred from the last two non-structural columns.
    design_book <- data.frame(
        plots = 1:2,
        some_main = c("A", "B"),
        some_sub = c("Low", "High")
    )

    out <- biometryassist:::normalise_agricolae_book(design_book, design_info)

    expect_true("treatments" %in% names(out))
    expect_true("sub_treatments" %in% names(out))
    expect_false("some_main" %in% names(out))
    expect_false("some_sub" %in% names(out))
})

test_that("des_info() adds buffers and passes blocks = FALSE for non-block designs", {
    crd_obj <- agricolae::design.crd(trt = c(1, 5, 10, 20), r = 2, seed = 42)

    testthat::local_mocked_bindings(
        create_buffers = function(des, type, blocks) {
            attr(des, "blocks_arg") <- blocks
            des
        },
        .package = "biometryassist"
    )

    expect_warning(
        out <- des_info(
            design.obj = crd_obj,
            nrows = 4,
            ncols = 2,
            buffer = "row",
            plot = FALSE,
            quiet = TRUE,
            save = FALSE
        ),
        "des_info\\(\\) is deprecated"
    )

    expect_identical(attr(out$design, "blocks_arg"), FALSE)
})

test_that("des_info() adds buffers and passes blocks = TRUE for block designs", {
    rcbd_obj <- agricolae::design.rcbd(trt = c("T1", "T2", "T3"), r = 2, seed = 42)

    testthat::local_mocked_bindings(
        create_buffers = function(des, type, blocks) {
            attr(des, "blocks_arg") <- blocks
            des
        },
        .package = "biometryassist"
    )

    expect_warning(
        out <- des_info(
            design.obj = rcbd_obj,
            nrows = 3,
            ncols = 2,
            brows = 3,
            bcols = 1,
            buffer = "row",
            plot = FALSE,
            quiet = TRUE,
            save = FALSE
        ),
        "des_info\\(\\) is deprecated"
    )

    expect_identical(attr(out$design, "blocks_arg"), TRUE)
})

test_that("des_info() creates a plot and prints output when quiet = FALSE", {
    crd_obj <- agricolae::design.crd(trt = c(1, 5, 10, 20), r = 2, seed = 42)

    calls <- new.env(parent = emptyenv())
    calls$autoplot_called <- FALSE
    calls$plot_called <- FALSE
    calls$print_satab_called <- FALSE

    testthat::local_mocked_bindings(
        autoplot = function(des, rotation, size, margin, ...) {
            calls$autoplot_called <- TRUE
            structure(list(), class = "ggplot")
        },
        plot = function(x, ...) {
            calls$plot_called <- TRUE
            invisible(NULL)
        },
        print.satab = function(x, ...) {
            calls$print_satab_called <- TRUE
            invisible(x)
        },
        .package = "biometryassist"
    )

    expect_warning(
        out <- des_info(
            design.obj = crd_obj,
            nrows = 4,
            ncols = 2,
            plot = TRUE,
            quiet = FALSE,
            save = FALSE,
            return.seed = FALSE
        ),
        "des_info\\(\\) is deprecated"
    )

    expect_true(calls$autoplot_called)
    expect_true(calls$plot_called)
    expect_true(calls$print_satab_called)
    expect_true("plot.des" %in% names(out))
})

