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
