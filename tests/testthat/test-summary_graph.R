test_that("Objects produced by summary_graph are ggplot objects", {
    sg <- summary_graph(iris, "Petal.Length", "Species", "mm")
    expect_s3_class(sg, "gg")
    expect_s3_class(sg, "ggplot")
})

test_that("Summary graph works for one explanatory variable", {
    vdiffr::expect_doppelganger(title = "Summary graph of iris petal length by Species",
                                summary_graph(iris, "Petal.Length", "Species", "mm"))

})

test_that("Summary graph works for two explanatory variables", {
    vdiffr::expect_doppelganger(title = "Summary graph of npk yield by N and P",
                                summary_graph(npk, "yield", c("N", "P"), "lb/plot"))

})

test_that("Summary graph works for three explanatory variables", {
    vdiffr::expect_doppelganger(title = "Summary graph of npk yield by N, P and K",
                                summary_graph(npk, "yield", c("N", "P", "K"), "lb/plot"))

})

test_that("Summary graph works for three explanatory variables", {
    vdiffr::expect_doppelganger(title = "Summary graph of npk yield by N, P and K",
                                summary_graph(npk, "yield", c("N", "P", "K"), "lb/plot"))

})

test_that("Errors are produced for invalid input", {
    expect_error(summary_graph("abc", "def", "ghi", "jkl"), "abc is not a data frame\\.")
    expect_error(summary_graph(iris, "def", "ghi", "jkl"), "def does not appear to be a column of data\\. Please check input\\.")
    expect_error(summary_graph(iris, Species, "ghi", "jkl"), "Species is not a numeric variable\\.")
    expect_error(summary_graph(iris, Petal.Length, 'Species', 123), "resp_units must be provided as a string with quotes\\.")
    expect_error(summary_graph(mtcars, mpg, c("cyl", "vs", "am", "gear"), "abc"), "Additional explanatory variables are not currently supported\\.")
})
