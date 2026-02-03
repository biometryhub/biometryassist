# Load pre-computed models once for reuse across tests
quiet(load(test_path("data", "w2_models.Rdata"), envir = .GlobalEnv))

test_that("example 1 works", {
    skip_on_cran()
    withr::local_options(scipen = 100)
    expect_snapshot_output(anova(example1.aov))
    pred1.out <- multiple_comparisons(example1.aov, classify = "trt")
    expect_equal(pred1.out$predictions$predicted.value, c(9.96, 12.26, 16.14, 17.77))
    expect_snapshot_output(pred1.out$predictions)
    skip_on_ci()
    skip_on_covr()
    skip_if(packageVersion("grid") < "4.2.1")
    vdiffr::expect_doppelganger(title = "example1resplot", resplot(example1.aov), variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "example1autoplot", autoplot(pred1.out))
})

test_that("example 2 works", {
    skip_on_cran()
    expect_snapshot_output(anova(example2.aov))
    pred2.out <- multiple_comparisons(example2.aov, classify =  "trt", decimals = 4)
    expect_equal(pred2.out$predictions$predicted.value, c(11.15, 12.45, 14.02, 15.1, 16.11, 17.24, 17.83), tolerance = 0.001)
    expect_snapshot_output(pred2.out$predictions)
    skip_on_ci()
    skip_on_covr()
    skip_if(packageVersion("grid") < "4.2.1")
    vdiffr::expect_doppelganger(title = "example2resplot", resplot(example2.aov), variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "example2autoplot", autoplot(pred2.out))
})

test_that("example 3 works", {
    skip_on_cran()
    expect_snapshot_output(anova(example3.aov))
    pred3.out <- multiple_comparisons(example3.aov, classify = "Variety")
    expect_equal(pred3.out$predictions$predicted.value, c(1.68, 2.68, 4.72, 4.85))
    expect_snapshot_output(pred3.out$predictions)
    skip_on_ci()
    skip_on_covr()
    skip_if(packageVersion("grid") < "4.2.1")
    vdiffr::expect_doppelganger(title = "example3resplot", resplot(example3.aov), variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "example3autoplot", autoplot(pred3.out))
})

test_that("example 4 works", {
    skip_on_cran()
    expect_snapshot_output(anova(example4.aov))
    pred4.out <- multiple_comparisons(example4.aov, classify = "trt")
    expect_equal(pred4.out$predictions$predicted.value, c(1707.94, 1802.7, 2053.73, 2200.08))
    expect_snapshot_output(pred4.out$predictions)
    skip_on_ci()
    skip_on_covr()
    skip_if(packageVersion("grid") < "4.2.1")
    vdiffr::expect_doppelganger(title = "example4resplot", resplot(example4.aov), variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "example4autoplot", autoplot(pred4.out))
})

test_that("example 3 LMM works", {
    skip_on_cran()
    skip_if_not_installed("asreml")
    expect_snapshot_output(print.data.frame(asreml::wald(example3.asr, denDF = "default")$Wald))
    pred3asr.out <- multiple_comparisons(example3.asr, classify = "Variety")
    expect_equal(pred3asr.out$predictions$predicted.value, c(1.68, 2.68, 4.72, 4.85))
    expect_snapshot_output(pred3asr.out$predictions)
    skip_on_ci()
    skip_on_covr()
    skip_if(packageVersion("grid") < "4.2.1")
    vdiffr::expect_doppelganger(title = "example3lmmresplot", resplot(example3.asr), variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "example3lmmautoplot", autoplot(pred3asr.out))
})

test_that("example 4 LMM works", {
    skip_on_cran()
    skip_if_not_installed("asreml")
    # example4.asr <- update(example4.asr)
    expect_snapshot_output(print.data.frame(asreml::wald(example4.asr, denDF = "default")$Wald))
    pred4lmm.out <- multiple_comparisons(example4.asr, classify = "trt")
    expect_equal(pred4lmm.out$predictions$predicted.value, c(1707.94, 1802.7, 2053.73, 2200.08), tolerance = 0.01)
    expect_snapshot_output(pred4lmm.out$predictions)
    skip_on_ci()
    skip_on_covr()
    skip_if(packageVersion("grid") < "4.2.1")
    vdiffr::expect_doppelganger(title = "example4lmmresplot", resplot(example4.asr), variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "example4lmmautoplot", autoplot(pred4lmm.out))
})

test_that("example 5 works", {
    skip_on_cran()
    skip_if_not_installed("asreml")
    expect_snapshot_output(print.data.frame(asreml::wald(example5.asr, denDF = "default")$Wald))
    skip_on_ci()
    skip_on_covr()
    skip_if(packageVersion("grid") < "4.2.1")
    skip_on_os("linux")
    expect_message(pred5.out1 <- multiple_comparisons(example5.asr, classify = "Genotype"),
                   "Some treatments sharing the same letter group have non-overlapping confidence intervals")

    # Robust checks for output structure and content (avoids fragile snapshots)
    expect_s3_class(pred5.out1, "mct")
    expect_true(is.data.frame(pred5.out1$predictions))

    # Check dimensions and column structure
    expect_true(nrow(pred5.out1$predictions) > 0)
    expect_named(pred5.out1$predictions, c("Genotype", "predicted.value", "std.error", "groups", "ci", "low", "up"))

    # Check column types
    expect_true(is.numeric(pred5.out1$predictions$predicted.value))
    expect_true(is.numeric(pred5.out1$predictions$std.error))
    expect_true(is.numeric(pred5.out1$predictions$low))
    expect_true(is.numeric(pred5.out1$predictions$up))
    expect_type(pred5.out1$predictions$groups, "character")

    # Check ordering (should be ascending by predicted.value)
    expect_true(all(diff(pred5.out1$predictions$predicted.value) >= 0))

    # Check value ranges and relationships
    expect_true(all(pred5.out1$predictions$std.error > 0))
    expect_true(all(pred5.out1$predictions$low < pred5.out1$predictions$predicted.value))
    expect_true(all(pred5.out1$predictions$up > pred5.out1$predictions$predicted.value))
    expect_true(all(pred5.out1$predictions$low < pred5.out1$predictions$up))

    # Check that groups column has expected properties (lowercase letters)
    expect_true(all(grepl("^[a-z]+$", pred5.out1$predictions$groups)))

    pred5.out2 <- multiple_comparisons(example5.asr, classify = "Fungicide")
    expect_snapshot_output(pred5.out2$predictions)
    vdiffr::expect_doppelganger(title = "example5lmmresplot", resplot(example5.asr), variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "example5lmmautoplot1", autoplot(pred5.out1))
    vdiffr::expect_doppelganger(title = "example5lmmautoplot2", autoplot(pred5.out2))
})

test_that("example 6 works", {
    skip_on_cran()
    skip_if_not_installed("asreml")
    expect_snapshot_output(print.data.frame(asreml::wald(example6.asr, denDF = "default")$Wald))
    vg6 <- variogram(example6.asr)
    expect_snapshot_output(summary(example6.asr)$varcomp)
    expect_equal(summary(example6.asr)$varcomp$component, c(0.0000002864157, 0.1790097741606, 0.5407770686889))

    # expect_warning(
    logl.tab <- logl_test(example6.asr, rand.terms = NULL,
                          resid.terms = c("ar1(Row)"), quiet = TRUE)#,
    # "Some components changed by more than 1% on the last iteration.")

    expect_equal(logl.tab$LogLRT.pvalue, '0.003')
    expect_message(pred6.out <- multiple_comparisons(example6.asr, classify = "Treatment"),
                   "Some treatments sharing the same letter group have non-overlapping confidence intervals")
    expect_snapshot_output(pred6.out$predictions)
    skip_on_ci()
    skip_on_covr()
    skip_if(packageVersion("grid") < "4.2.1")
    vdiffr::expect_doppelganger(title = "example6lmmresplot", resplot(example6.asr), variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "example6variogram", vg6, variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "example6lmmautoplot2", autoplot(pred6.out))
})

test_that("example 7 works", {
    skip_on_cran()
    skip_if_not_installed("asreml")
    expect_snapshot_output(print.data.frame(asreml::wald(example7.asr, denDF = "default")$Wald))
    vg7 <- variogram(example7.asr)
    expect_snapshot_output(print(summary(example7.asr)$varcomp, digits = 2))
    # expect_warning(
    logl.tab <- logl_test(example7.asr, rand.terms = NULL,
                          resid.terms = c("ar1(Row)"), quiet = TRUE)#,
    # "Some components changed by more than 1% on the last iteration.")
    expect_equal(logl.tab$LogLRT.pvalue, '0.003')
    expect_message(expect_warning(pred7.out <- multiple_comparisons(example7.asr, classify = "Herbicide:Rate",
                                                     present = c("Control", "Herbicide", "Rate")),
                   "Some levels of Herbicide:Rate are aliased. They have been removed from predicted output."),
                   "Some treatments sharing the same letter group have non-overlapping confidence intervals")
    expect_snapshot_output(pred7.out$predictions)
    skip_on_ci()
    skip_on_covr()
    skip_if(packageVersion("grid") < "4.2.1")
    vdiffr::expect_doppelganger(title = "example7lmmresplot", resplot(example7.asr), variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "example7variogram", vg7, variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "example7lmmautoplot", autoplot(pred7.out))
})


###############
## Exercises ##
###############

test_that("exercise 1 works", {
    skip_on_cran()
    expect_snapshot_output(anova(exercise1.aov))
    expect_message(pred1e.out <- multiple_comparisons(exercise1.aov, classify = "Variety", decimals = 5),
                   "Some treatments sharing the same letter group have non-overlapping confidence intervals")
    expect_equal(pred1e.out$predictions$predicted.value, c(1.97333, 2.13000, 2.13000, 2.14000, 2.19333, 2.24000, 2.27000, 2.28333, 2.52667, 2.54000, 2.75000, 2.75333))
    pred1e.out$predictions <- pred1e.out$predictions[order(pred1e.out$predictions$predicted.value, as.character(pred1e.out$predictions$Variety)),]
    expect_snapshot_output(pred1e.out$predictions)
    skip_on_ci()
    skip_on_covr()
    skip_if(packageVersion("grid") < "4.2.1")
    vdiffr::expect_doppelganger(title = "exercise1resplot", resplot(exercise1.aov), variant = ggplot2_variant())
    skip_on_os("linux")
    vdiffr::expect_doppelganger(title = "exercise1autoplot", autoplot(pred1e.out))
})

test_that("exercise 2 works", {
    skip_on_cran()
    expect_snapshot_output(anova(exercise2.aov))
    pred2e.out <- multiple_comparisons(exercise2.aov, classify = "Treatment")
    pred2e.out$predictions$predicted.value <- round(pred2e.out$predictions$predicted.value, 1)
    expect_equal(pred2e.out$predictions$predicted.value, c(2.1, 2.2, 2.6, 2.8, 2.8, 3.4))
    expect_snapshot(data.frame(lapply(pred2e.out$predictions, function(y) if(is.numeric(y)) round(y, 1) else y)))
    skip_on_ci()
    skip_on_covr()
    skip_if(packageVersion("grid") < "4.2.1")
    vdiffr::expect_doppelganger(title = "exercise2resplot", resplot(exercise2.aov), variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "exercise2autoplot", autoplot(pred2e.out, rotation = 90))
})

test_that("exercise 3 works", {
    skip_on_cran()
    expect_snapshot_output(anova(exercise3.aov))
    expect_message(pred3e.out <- multiple_comparisons(exercise3.aov, classify = "Variety"),
                   "Some treatments sharing the same letter group have non-overlapping confidence intervals")
    expect_equal(pred3e.out$predictions$predicted.value, c(2.84, 2.86, 3.08, 4.7, 4.78, 4.96, 8.88))
    expect_snapshot_output(pred3e.out$predictions)
    skip_on_ci()
    skip_on_covr()
    skip_if(packageVersion("grid") < "4.2.1")
    vdiffr::expect_doppelganger(title = "exercise3resplot", resplot(exercise3.aov), variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "exercise3autoplot", autoplot(pred3e.out))
})

test_that("exercise 4 works", {
    skip_on_cran()
    expect_snapshot_output(anova(exercise4.aov))
    expect_equal(anova(exercise4.aov)$`Mean Sq`, c(0.64812028, 0.17470687, 0.13221151))
    skip_on_ci()
    skip_on_covr()
    skip_if(packageVersion("grid") < "4.2.1")
    vdiffr::expect_doppelganger(title = "exercise4resplot", resplot(exercise4.aov), variant = ggplot2_variant())
})

test_that("exercise 5 works", {
    skip_on_cran()
    expect_snapshot_output(anova(exercise5.aov))
    pred5e.out <- multiple_comparisons(exercise5.aov, classify = "Treatment")
    expect_equal(pred5e.out$predictions$predicted.value, c(31.61, 35.98, 38.95, 43.52, 48.12))
    expect_snapshot_output(pred5e.out$predictions)
    skip_on_ci()
    skip_on_covr()
    skip_if(packageVersion("grid") < "4.2.1")
    vdiffr::expect_doppelganger(title = "exercise5resplot", resplot(exercise5.aov), variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "exercise5autoplot", autoplot(pred5e.out))
})

test_that("exercise 6 works", {
    skip_on_cran()
    expect_snapshot_output(anova(exercise6.aov))
    pred6e.out <- multiple_comparisons(exercise6.aov, classify = "Treatment")
    expect_equal(pred6e.out$predictions$predicted.value, c(16.01, 17.51, 21.40, 24.39))
    expect_snapshot_output(pred6e.out$predictions)
    skip_on_ci()
    skip_on_covr()
    skip_if(packageVersion("grid") < "4.2.1")
    vdiffr::expect_doppelganger(title = "exercise6resplot", resplot(exercise6.aov), variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "exercise6autoplot", autoplot(pred6e.out))
})

test_that("exercise 7 works", {
    skip_on_cran()
    skip_if_not_installed("asreml")
    expect_snapshot_output(print.data.frame(asreml::wald(exercise7.asr, denDF = "default")$Wald))
    pred7e.out <- multiple_comparisons(exercise7.asr, classify = "Variety")
    expect_equal(pred7e.out$predictions$predicted.value, c(2.84, 2.86, 3.08, 4.70, 4.78, 4.96, 8.88))
    expect_snapshot_output(pred7e.out$predictions)
    skip_on_ci()
    skip_on_covr()
    skip_if(packageVersion("grid") < "4.2.1")
    vdiffr::expect_doppelganger(title = "exercise7resplot", resplot(exercise7.asr), variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "exercise7autoplot", autoplot(pred7e.out))
})

test_that("exercise 8 works", {
    skip_on_cran()
    skip_if_not_installed("asreml")
    expect_equal(asreml::wald(exercise8.asr, denDF = "default")$Wald$Pr[2], 0.30758014, tolerance = 0.0001)
    expect_snapshot_output(print.data.frame(asreml::wald(exercise8.asr, denDF = "default")$Wald))
    skip_on_ci()
    skip_on_covr()
    skip_if(packageVersion("grid") < "4.2.1")
    vdiffr::expect_doppelganger(title = "exercise8resplot", resplot(exercise8.asr), variant = ggplot2_variant())
})

test_that("exercise 9 works", {
    skip_on_cran()
    skip_if_not_installed("asreml")
    expect_snapshot_output(print.data.frame(asreml::wald(exercise9.asr, denDF = "default")$Wald))
    pred9e.out <- multiple_comparisons(exercise9.asr, classify = "Treatment")
    expect_equal(pred9e.out$predictions$predicted.value, c(31.61, 35.98, 38.95, 43.52, 48.12))
    expect_snapshot_output(pred9e.out$predictions)
    skip_on_ci()
    skip_on_covr()
    skip_if(packageVersion("grid") < "4.2.1")
    vdiffr::expect_doppelganger(title = "exercise9resplot", resplot(exercise9.asr), variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "exercise9autoplot", autoplot(pred9e.out))
})

test_that("exercise 10 works", {
    skip_on_cran()
    skip_if_not_installed("asreml")
    expect_snapshot_output(print.data.frame(asreml::wald(exercise10.asr, denDF = "default")$Wald))
    pred10e.out <- multiple_comparisons(exercise10.asr, classify = "Treatment")
    expect_equal(pred10e.out$predictions$predicted.value, c(16.01, 17.51, 21.40, 24.39))
    expect_snapshot_output(pred10e.out$predictions)
    skip_on_ci()
    skip_on_covr()
    skip_if(packageVersion("grid") < "4.2.1")
    vdiffr::expect_doppelganger(title = "exercise10resplot", resplot(exercise10.asr), variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "exercise10autoplot", autoplot(pred10e.out))
})

test_that("exercise 11 works", {
    skip_on_cran()
    skip_if_not_installed("asreml")
    expect_snapshot_output(print.data.frame(asreml::wald(exercise11.asr, denDF = "default")$Wald))
    pred11e.out1 <- multiple_comparisons(exercise11.asr, classify = "Genotype")
    expect_equal(pred11e.out1$predictions$predicted.value, c(97.68, 104.89, 109.35))
    pred11e.out2 <- multiple_comparisons(exercise11.asr, classify = "Nitrogen")
    expect_equal(pred11e.out2$predictions$predicted.value, c(79.39, 98.89, 114.22, 123.39))
    expect_snapshot_output(pred11e.out1$predictions)
    expect_snapshot_output(pred11e.out2$predictions)
    skip_on_ci()
    skip_on_covr()
    skip_if(packageVersion("grid") < "4.2.1")
    vdiffr::expect_doppelganger(title = "exercise11resplot", resplot(exercise11.asr), variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "exercise11autoplot1", autoplot(pred11e.out1))
    vdiffr::expect_doppelganger(title = "exercise11autoplot2", autoplot(pred11e.out2))
})

test_that("exercise 12 works", {
    skip_on_cran()
    skip_if_not_installed("asreml")
    expect_snapshot_output(print.data.frame(asreml::wald(exercise12.asr, denDF = "default")$Wald))
    pred12e.out <- multiple_comparisons(exercise12.asr, classify = "Variety:Irrigation")
    expect_equal(pred12e.out$predictions$predicted.value,
                 c(4.61, 5.47, 5.91, 6.19, 6.43, 6.92, 7.02, 7.68, 7.7, 7.75))
    expect_snapshot_output(pred12e.out$predictions)
    skip_on_ci()
    skip_on_covr()
    skip_if(packageVersion("grid") < "4.2.1")
    vdiffr::expect_doppelganger(title = "exercise12resplot", resplot(exercise12.asr), variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "exercise12autoplot", autoplot(pred12e.out))
})

test_that("exercise 13 works", {
    skip_on_cran()
    skip_if_not_installed("asreml")
    expect_snapshot_output(print.data.frame(asreml::wald(exercise13.asr, denDF = "default")$Wald, digits = 3))

    logl.tab <- logl_test(model.obj = exercise13.asr,
                          rand.terms = NULL,
                          resid.terms = "ar1(Row)",
                          quiet = TRUE)
    expect_equal(logl.tab$LogLRT.pvalue, "0.221")
    pred13e.out1 <- multiple_comparisons(exercise13.asr, classify = "Genotype")
    expect_equal(pred13e.out1$predictions$predicted.value, c(97.41, 104.31, 110.07))
    expect_snapshot_output(pred13e.out1$predictions)
    pred13e.out2 <- multiple_comparisons(exercise13.asr, classify = "Nitrogen")
    expect_equal(pred13e.out2$predictions$predicted.value, c(79.44, 98.82, 114.08, 123.37))
    expect_snapshot_output(pred13e.out2$predictions)
    skip_on_ci()
    skip_on_covr()
    skip_if(packageVersion("grid") < "4.2.1")
    vdiffr::expect_doppelganger(title = "exercise13resplot", resplot(exercise13.asr), variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "exercise13autoplot1", autoplot(pred13e.out1))
    vdiffr::expect_doppelganger(title = "exercise13autoplot2", autoplot(pred13e.out2))
    # skip_on_os(c("windows", "mac"))
    vdiffr::expect_doppelganger(title = "exercise13variogram", variogram(exercise13.asr), variant = ggplot2_variant())
})

test_that("exercise 14 works", {
    skip_on_cran()
    skip_if_not_installed("asreml")
    expect_snapshot_output(print.data.frame(asreml::wald(exercise14.asr, denDF = "default")$Wald))

    logl.tab <- logl_test(model.obj = exercise14.asr,
                          rand.terms = NULL,
                          resid.terms = "ar1(Row)",
                          quiet = TRUE)
    expect_equal(logl.tab$LogLRT.pvalue, "<0.001")
    expect_message(pred14e.out <- multiple_comparisons(exercise14.asr, classify = "Genotype"),
                   "Some treatments sharing the same letter group have non-overlapping confidence intervals")
    expect_equal(pred14e.out$predictions$predicted.value,
                 c(3.68, 3.79, 3.9, 3.91, 3.92, 3.96, 3.96,
                   3.98, 3.98, 4.02, 4.03, 4.04, 4.06, 4.06,
                   4.06, 4.07, 4.12, 4.13, 4.13, 4.15, 4.15,
                   4.15, 4.16, 4.16, 4.18, 4.18, 4.19, 4.19,
                   4.2, 4.21, 4.22, 4.23, 4.26, 4.27, 4.31,
                   4.31, 4.33, 4.33, 4.33, 4.34, 4.36, 4.36,
                   4.38, 4.38, 4.41, 4.42, 4.43, 4.43, 4.49, 4.52))
    expect_snapshot_output(pred14e.out$predictions)
    skip_on_ci()
    skip_on_covr()
    skip_if(packageVersion("grid") < "4.2.1")
    vdiffr::expect_doppelganger(title = "exercise14resplot", resplot(exercise14.asr), variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "exercise14autoplot", autoplot(pred14e.out))
    # skip_on_os(c("windows", "mac"))
    vdiffr::expect_doppelganger(title = "exercise14variogram", variogram(exercise14.asr), variant = ggplot2_variant())
})

test_that("exercise 15 works", {
    skip_on_cran()
    skip_if_not_installed("asreml")
    expect_snapshot_output(print.data.frame(asreml::wald(exercise15.asr, denDF = "default")$Wald))

    logl.tab <- logl_test(model.obj = exercise15.asr,
                          rand.terms = NULL,
                          resid.terms = "ar1(col)",
                          quiet = TRUE)
    expect_equal(logl.tab$LogLRT.pvalue, "0.156")
    pred15e.out1 <- multiple_comparisons(exercise15.asr, classify = "Control",
                                         present = c("Control", "Rate", "Season"))
    expect_equal(pred15e.out1$predictions$predicted.value, c(2.37, 3.06))
    expect_snapshot_output(pred15e.out1$predictions)
    pred15e.out2 <- multiple_comparisons(exercise15.asr, classify = "Rate",
                                         present = c("Control", "Rate", "Season"))
    expect_equal(pred15e.out2$predictions$predicted.value, c(1.99, 2.48, 2.62, 3.06))
    expect_snapshot_output(pred15e.out2$predictions)
    pred15e.out3 <- multiple_comparisons(exercise15.asr, classify = "Season",
                                         present = c("Control", "Rate", "Season"))
    expect_equal(pred15e.out3$predictions$predicted.value, c(2.14, 2.59, 3.06))
    expect_snapshot_output(pred15e.out3$predictions)
    skip_on_ci()
    skip_on_covr()
    skip_if(packageVersion("grid") < "4.2.1")

    vdiffr::expect_doppelganger(title = "exercise15resplot", resplot(exercise15.asr), variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "exercise15autoplot1", autoplot(pred15e.out1))
    vdiffr::expect_doppelganger(title = "exercise15autoplot2", autoplot(pred15e.out2))
    vdiffr::expect_doppelganger(title = "exercise15autoplot3", autoplot(pred15e.out3))
    # skip_on_os(c("windows", "mac"))
    vdiffr::expect_doppelganger(title = "exercise15variogram", variogram(exercise15.asr), variant = ggplot2_variant())
})

