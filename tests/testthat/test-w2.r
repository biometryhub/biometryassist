# filenames <- list.files("../W2 Analysis/", "*.csv")
# for (i in seq_along(filenames)) {
#     assign(sub("\\.csv$", '', filenames[i]), read.csv(paste0("../W2 Analysis/", filenames[i]), stringsAsFactors = T))
# }
# filenames <- sub("\\.csv$", '', filenames)
# save(list = filenames, file = "tests/testthat/data/w2_data.Rdata")
load(test_path("data", "w2_data.Rdata"), envir = .GlobalEnv)

# test_that("example 1 works", {
#     example1.aov <- aov(RL ~ trt, data = example1)
#     vdiffr::expect_doppelganger(title = "example1resplot", resplot(example1.aov))
#     expect_snapshot_output(anova(example1.aov))
#     pred1.out <- multiple_comparisons(example1.aov, classify = "trt")
#     expect_equal(pred1.out$predicted.value, c(9.96, 12.26, 16.14, 17.77))
#     expect_snapshot_output(pred1.out)
#     vdiffr::expect_doppelganger(title = "example1autoplot", autoplot(pred1.out))
# })
#
# test_that("example 2 works", {
#     example2.aov <- aov(TuberLengthGrowth ~ trt, data = example2)
#     vdiffr::expect_doppelganger(title = "example2resplot", resplot(example2.aov))
#     expect_snapshot_output(anova(example2.aov))
#     pred2.out <- multiple_comparisons(example2.aov, classify =  "trt")
#     expect_equal(pred2.out$predicted.value, c(11.15, 12.45, 14.02, 15.1, 16.11, 17.24, 17.83))
#     expect_snapshot_output(pred2.out)
#     vdiffr::expect_doppelganger(title = "example2autoplot", autoplot(pred2.out))
# })
#
# test_that("example 3 works", {
#     example3.aov <- aov(Yield ~ Block + Variety, data = example3)
#     vdiffr::expect_doppelganger(title = "example3resplot", resplot(example3.aov))
#     expect_snapshot_output(anova(example3.aov))
#     pred3.out <- multiple_comparisons(example3.aov, classify = "Variety")
#     expect_equal(pred3.out$predicted.value, c(1.68, 2.68, 4.72, 4.85))
#     expect_snapshot_output(pred3.out)
#     vdiffr::expect_doppelganger(title = "example3autoplot", autoplot(pred3.out))
# })
#
# test_that("example 4 works", {
#     example4.aov <- aov(DM ~ row + col + trt, data = example4)
#     vdiffr::expect_doppelganger(title = "example4resplot", resplot(example4.aov))
#     expect_snapshot_output(anova(example4.aov))
#     expect_warning(pred4.out <- multiple_comparisons(example4.aov, classify = "trt"),
#                    "Missing treatments' combination appeared, predicted means maybe misleading!")
#     expect_equal(pred4.out$predicted.value, c(1707.94, 1802.7, 2053.73, 2200.08))
#     expect_snapshot_output(pred4.out)
#     vdiffr::expect_doppelganger(title = "example4autoplot", autoplot(pred4.out))
# })
#
# test_that("example 3 LMM works", {
#     skip_if_not_installed("asreml")
#     library(asreml, quietly = T)
#     example3.asr <- asreml(Yield ~ Variety, random = ~ Block, residual = ~ id(Plot),
#                            data = example3, trace = FALSE)
#     vdiffr::expect_doppelganger(title = "example3lmmresplot", resplot(example3.asr))
#     expect_snapshot_output(wald(example3.asr, denDF = "default")$Wald)
#     pred3asr.out <- multiple_comparisons(example3.asr, classify = "Variety")
#     expect_equal(pred3asr.out$predicted.value, c(1.68, 2.68, 4.72, 4.85))
#     expect_snapshot_output(pred3asr.out)
#     vdiffr::expect_doppelganger(title = "example3lmmautoplot", autoplot(pred3asr.out))
# })
#
# test_that("example 4 LMM works", {
#     skip_if_not_installed("asreml")
#     expect_warning(example4.asr <- asreml(DM ~ trt, random = ~ row + col,
#                                           residual = ~ id(plots), data = example4,
#                                           trace = FALSE),
#                    "Some components changed by more than 1% on the last iteration.")
#     example4.asr <- update(example4.asr)
#     vdiffr::expect_doppelganger(title = "example4lmmresplot", resplot(example4.asr))
#     expect_snapshot_output(wald(example4.asr, denDF = "default")$Wald)
#     pred4lmm.out <- multiple_comparisons(example4.asr, classify = "trt")
#     expect_equal(pred4lmm.out$predicted.value, c(1707.94, 1802.7, 2053.73, 2200.08))
#     expect_snapshot_output(pred4lmm.out)
#     vdiffr::expect_doppelganger(title = "example4lmmautoplot", autoplot(pred4lmm.out))
# })
#
# test_that("example 5 works", {
#     skip_if_not_installed("asreml")
#     example5.asr <- asreml(Yield ~ Genotype + Fungicide + Genotype:Fungicide,
#                            random = ~ Block + Block:WholePlot, residual = ~ units,
#                            data = example5, trace = FALSE)
#     expect_snapshot_output(wald(example5.asr, denDF = "default")$Wald)
#     vdiffr::expect_doppelganger(title = "example5lmmresplot", resplot(example5.asr))
#     pred5.out <- multiple_comparisons(example5.asr, classify = "Genotype")
#     expect_snapshot_output(pred5.out)
#     vdiffr::expect_doppelganger(title = "example5lmmautoplot1", autoplot(pred5.out))
#     pred5.out <- multiple_comparisons(example5.asr, classify = "Fungicide")
#     expect_snapshot_output(pred5.out)
#     vdiffr::expect_doppelganger(title = "example5lmmautoplot2", autoplot(pred5.out))
# })
#
# test_that("example 6 works", {
#     skip_if_not_installed("asreml")
#     example6.asr <- asreml(Yield ~ Treatment, random = ~ Block,
#                            residual = ~ id(Column):ar1(Row),
#                            data = example6, trace = FALSE)
#     expect_snapshot_output(wald(example6.asr, denDF = "default")$Wald)
#     vdiffr::expect_doppelganger(title = "example6lmmresplot", resplot(example6.asr))
#     expect_warning(vg6 <- variogram(example6.asr),
#                    "Removed 79 rows containing non-finite values \\(stat_contour\\)\\.")
#     vdiffr::expect_doppelganger(title = "example6variogram", vg6)
#     expect_snapshot_output(summary(example6.asr)$varcomp)
#     expect_equal(summary(example6.asr)$varcomp$component, c(0.0000002864157, 0.1790097741606, 0.5407770686889))
#
#     expect_warning(logl.tab <- logl_test(example6.asr,
#                                          rand.terms = NULL, resid.terms = c("ar1(Row)")),
#                    "Some components changed by more than 1% on the last iteration.")
#
#     expect_equal(logl.tab$LogLRT.pvalue, '0.003')
#     pred6.out <- multiple_comparisons(example6.asr, classify = "Treatment")
#     expect_snapshot_output(pred6.out)
#     vdiffr::expect_doppelganger(title = "example6lmmautoplot2", autoplot(pred6.out))
# })
#
# test_that("example 7 works", {
#     skip_if_not_installed("asreml")
#     example7.asr <- asreml(Yield ~ Control + Herbicide + Rate + Herbicide:Rate,
#                            random = ~ Block,  residual = ~ id(Column):ar1(Row),
#                            data = example7, trace = FALSE)
#     vdiffr::expect_doppelganger(title = "example7lmmresplot", resplot(example7.asr))
#     expect_snapshot_output(wald(example7.asr, denDF = "default")$Wald)
#     expect_warning(vg7 <- variogram(example7.asr),
#                    "Removed 79 rows containing non-finite values \\(stat_contour\\)\\.")
#     vdiffr::expect_doppelganger(title = "example7variogram", vg7)
#     expect_snapshot_output(summary(example7.asr)$varcomp)
#     expect_warning(logl.tab <- logl_test(example7.asr,
#                                          rand.terms = NULL, resid.terms = c("ar1(Row)")),
#                    "Some components changed by more than 1% on the last iteration.")
#     expect_equal(logl.tab$LogLRT.pvalue, '0.003')
#     expect_warning(pred7.out <- multiple_comparisons(example7.asr, classify = "Herbicide:Rate",
#                                                      present = c("Control", "Herbicide", "Rate")),
#                    "Some levels of Herbicide:Rate are aliased. They have been removed from predicted output.")
#     expect_snapshot_output(pred7.out)
#     vdiffr::expect_doppelganger(title = "example7lmmautoplot", autoplot(pred7.out))
# })


###############
## Exercises ##
###############

test_that("exercise 1 works", {
    exercise1.aov <- aov(Yield ~ Variety, data = exercise1)
    vdiffr::expect_doppelganger(title = "exercise1resplot", resplot(exercise1.aov))
    expect_snapshot_output(anova(exercise1.aov))
    pred1e.out <- multiple_comparisons(exercise1.aov, classify = "Variety")
    expect_equal(pred1e.out$predicted.value, c(1.97, 2.13, 2.13, 2.14, 2.19, 2.24, 2.27, 2.28, 2.53, 2.54, 2.75, 2.75))
    expect_snapshot_output(pred1e.out)
    vdiffr::expect_doppelganger(title = "exercise1autoplot", autoplot(pred1e.out))
})

test_that("exercise 2 works", {
    exercise2.aov <- aov(Time ~ Treatment, data = exercise2)
    vdiffr::expect_doppelganger(title = "exercise2resplot", resplot(exercise2.aov))
    expect_snapshot_output(anova(exercise2.aov))
    pred2e.out <- multiple_comparisons(exercise2.aov, classify = "Treatment")
    expect_equal(pred2e.out$predicted.value, c(2.12, 2.17, 2.62, 2.77, 2.8, 3.37))
    expect_snapshot_output(pred2e.out)
    vdiffr::expect_doppelganger(title = "exercise2autoplot", autoplot(pred2e.out, rotation = 90))
})

test_that("exercise 3 works", {
    exercise3.aov <- aov(AverageFruitSize ~ Replicate + Variety, data = exercise3)
    vdiffr::expect_doppelganger(title = "exercise3resplot", resplot(exercise3.aov))
    expect_snapshot_output(anova(exercise3.aov))
    pred3e.out <- multiple_comparisons(exercise3.aov, classify = "Variety")
    expect_equal(pred3e.out$predicted.value, c(2.84, 2.86, 3.08, 4.7, 4.78, 4.96, 8.88))
    expect_snapshot_output(pred3e.out)
    vdiffr::expect_doppelganger(title = "exercise3autoplot", autoplot(pred3e.out))
})

test_that("exercise 4 works", {
    exercise4.aov <- aov(Yield ~ Block + SeedingRate, data = exercise4)
    vdiffr::expect_doppelganger(title = "exercise4resplot", resplot(exercise4.aov))
    expect_snapshot_output(anova(exercise4.aov))
    expect_equal(anova(exercise4.aov)$`Mean Sq`, c(0.64812028, 0.17470687, 0.13221151))
})
#
# test_that("exercise 5 works", {
#
# })
#
# test_that("exercise 6 works", {
#
# })
#
# test_that("exercise 7 works", {
#
# })
#
# test_that("exercise 8 works", {
#
# })
#
# test_that("exercise 9 works", {
#
# })
#
# test_that("exercise 10 works", {
#
# })
#
# test_that("exercise 11 works", {
#
# })
#
# test_that("exercise 12 works", {
#
# })
#
# test_that("exercise 13 works", {
#
# })
#
# test_that("exercise 14 works", {
#
# })
#
# test_that("exercise 15 works", {
#
# })
#
# test_that("exercise 16 works", {
#
# })
#
# test_that("exercise 17 works", {
#
# })
