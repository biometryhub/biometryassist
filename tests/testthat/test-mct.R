dat.aov <- aov(Petal.Width ~ Species, data = iris)

test_that("mct produces output", {
    output <- multiple_comparisons(dat.aov, classify = "Species", plot = TRUE)
    expect_equal(output$predicted.value, c(0.25, 1.33, 2.03), tolerance = 5e-2)
    vdiffr::expect_doppelganger("mct output", autoplot(output))
})

test_that("mct ylab handles call/language labels", {
    pp <- data.frame(
        trt = factor(c("A", "B")),
        predicted.value = c(1, 2),
        std.error = c(0.1, 0.1),
        Df = c(10, 10),
        ci = c(0.2, 0.2),
        low = c(0.8, 1.8),
        up = c(1.2, 2.2),
        groups = c("a", "b")
    )

    out <- biometryassist:::add_attributes(pp, ylab = quote(sqrt(response)),
                                          crit_val = matrix(0, nrow = 2, ncol = 2),
                                          aliased_names = NULL)

    expect_identical(attr(out, "ylab"), quote(sqrt(response)))
})

test_that("mct transformation: log", {
    dat.aov.log <- aov(log(Petal.Width) ~ Species, data = iris)
    output.log <- multiple_comparisons(dat.aov.log, classify = "Species", trans = "log", offset = 0)
    output.log2 <- multiple_comparisons(dat.aov.log, classify = "Species", trans = "log", offset = 0, int.type = "1se")
    output.log3 <- multiple_comparisons(dat.aov.log, classify = "Species", trans = "log", offset = 0, int.type = "2se")

    expect_identical(attr(output.log, "ylab"), "Petal.Width")
    expect_equal(output.log$predicted.value, c(-1.48, 0.27, 0.70), tolerance = 5e-2)
    expect_equal(output.log2$low, c(0.22, 1.26, 1.94), tolerance = 5e-2)
    expect_equal(output.log3$up, c(0.25, 1.41, 2.17), tolerance = 5e-2)
    vdiffr::expect_doppelganger("mct log output", autoplot(output.log))
})

test_that("mct transformation: sqrt", {
    dat.aov.sqrt <- aov(sqrt(Petal.Width) ~ Species, data = iris)
    output.sqrt <- multiple_comparisons(dat.aov.sqrt, classify = "Species", trans = "sqrt", offset = 0)
    output.sqrt2 <- multiple_comparisons(dat.aov.sqrt, classify = "Species", trans = "sqrt", offset = 0, int.type = "1se")
    output.sqrt3 <- multiple_comparisons(dat.aov.sqrt, classify = "Species", trans = "sqrt", offset = 0, int.type = "2se")

    expect_identical(attr(output.sqrt, "ylab"), "Petal.Width")
    expect_equal(output.sqrt$predicted.value, c(0.49, 1.15, 1.42), tolerance = 5e-2)
    expect_equal(output.sqrt2$low, c(0.23, 1.29, 1.98), tolerance = 5e-2)
    expect_equal(output.sqrt3$up, c(0.27, 1.38, 2.09), tolerance = 5e-2)
    vdiffr::expect_doppelganger("mct sqrt output", autoplot(output.sqrt))
})

test_that("mct transformation: logit", {
    dat.aov.logit <- aov(logit(1/Petal.Width) ~ Species, data = iris)
    expect_warning(
        output.logit <- multiple_comparisons(dat.aov.logit, classify = "Species", trans = "logit", offset = 0),
        "Some standard errors are very small and would round to zero with 2 decimal places"
    )
    expect_warning(
        output.logit2 <- multiple_comparisons(dat.aov.logit, classify = "Species", trans = "logit", offset = 0, int.type = "1se"),
        "Some standard errors are very small and would round to zero with 2 decimal places"
    )
    expect_warning(
        output.logit3 <- multiple_comparisons(dat.aov.logit, classify = "Species", trans = "logit", offset = 0, int.type = "2se"),
        "Some standard errors are very small and would round to zero with 2 decimal places"
    )

    expect_identical(attr(output.logit, "ylab"), "1/Petal.Width")
    expect_equal(output.logit$predicted.value, c(-5.30, -4.87, -3.07), tolerance = 5e-2)
    expect_equal(output.logit2$low, c(0.00, 0.01, 0.04), tolerance = 5e-2)
    expect_equal(output.logit3$up, c(0.01, 0.01, 0.05), tolerance = 5e-2)
    vdiffr::expect_doppelganger("mct logit output", autoplot(output.logit))
})

test_that("mct transformation: inverse", {
    dat.aov.inverse <- aov((1/Petal.Width) ~ Species, data = iris)
    output.inverse <- multiple_comparisons(dat.aov.inverse, classify = "Species", trans = "inverse", offset = 0)
    output.inverse2 <- multiple_comparisons(dat.aov.inverse, classify = "Species", trans = "inverse", offset = 0, int.type = "1se")
    output.inverse3 <- multiple_comparisons(dat.aov.inverse, classify = "Species", trans = "inverse", offset = 0, int.type = "2se")

    expect_identical(attr(output.inverse, "ylab"), "Petal.Width")
    expect_equal(output.inverse$predicted.value, c(0.50, 0.77, 4.79), tolerance = 5e-2)
    expect_equal(output.inverse2$up, c(3.01, 1.66, 0.22), tolerance = 5e-2)
    expect_equal(output.inverse3$low, c(1.20, 0.90, 0.20), tolerance = 5e-2)
    vdiffr::expect_doppelganger("mct inverse output", autoplot(output.inverse))
})

test_that("mct transformation: power", {
    iris_new <- iris
    iris_new$Petal.Width <- (iris_new$Petal.Width+1)^3
    dat.aov.power <- aov(Petal.Width ~ Species, data = iris_new)
    output.power <- multiple_comparisons(dat.aov.power, classify = "Species", trans = "power", offset = 1, power = 3)
    output.power2 <- multiple_comparisons(dat.aov.power, classify = "Species", trans = "power", offset = 1, power = 3, int.type = "1se")
    output.power3 <- multiple_comparisons(dat.aov.power, classify = "Species", trans = "power", offset = 1, power = 3, int.type = "2se")

    expect_identical(attr(output.power, "ylab"), "Petal.Width")
    expect_equal(output.power$predicted.value, c(1.98, 12.85, 28.38), tolerance = 5e-2)
    expect_equal(output.power2$low, c(0.09, 1.30, 2.03), tolerance = 5e-2)
    expect_equal(output.power3$up, c(0.49, 1.42, 2.10), tolerance = 5e-2)
    vdiffr::expect_doppelganger("mct power output", autoplot(output.power))
})

test_that("mct transformation: arcsin", {
    iris_arc <- iris
    iris_arc$PW <- iris_arc$Petal.Width / max(iris_arc$Petal.Width)
    dat.aov.arcsin <- aov(asin(sqrt(PW)) ~ Species, data = iris_arc)
    expect_warning(output.arcsin <- multiple_comparisons(dat.aov.arcsin,
                                                         classify = "Species",
                                                         trans = "arcsin", offset = 0),
                   "There are unevaluated constants in the response formula")

    expect_identical(attr(output.arcsin, "ylab"), "PW")
    expect_true("PredictedValue" %in% names(output.arcsin))
    expect_true(all(output.arcsin$PredictedValue >= 0 & output.arcsin$PredictedValue <= 1))
    expected_pw_means <- tapply(iris_arc$PW, iris_arc$Species, mean)
    expect_equal(output.arcsin$PredictedValue,
                 as.numeric(expected_pw_means[as.character(output.arcsin$Species)]),
                 tolerance = 5e-2)
})

test_that("apply_transformation warns when offset is missing", {
    pp <- data.frame(
        predicted.value = c(0),
        std.error = c(0.1),
        ci = c(0.2)
    )

    expect_warning(
        out <- biometryassist:::apply_transformation(pp, trans = "log", offset = NULL, power = NULL),
        "Offset value assumed to be 0\\. Change with `offset` argument\\."
    )

    expect_true(all(c("PredictedValue", "ApproxSE", "low", "up") %in% names(out)))
})

test_that("multiple_comparisons warns when offset is omitted", {
    dat.aov <- aov(log(Petal.Width) ~ Species, data = iris)
    expect_warning(
        multiple_comparisons(dat.aov, classify = "Species", trans = "log"),
        "Offset value assumed to be 0\\. Change with `offset` argument\\."
    )
})

test_that("apply_transformation warns for invalid back-transformed values", {
    # sqrt: negative value after offset removal
    pp.sqrt <- data.frame(
        predicted.value = c(0.1),
        std.error = c(0.1),
        ci = c(0.2)
    )
    expect_warning(
        out.sqrt <- biometryassist:::apply_transformation(pp.sqrt, trans = "sqrt", offset = 1, power = NULL),
        "Square root back-transformation produced negative values\\. Check offset parameter\\."
    )
    expect_true(out.sqrt$PredictedValue < 0)

    # log: non-positive value after offset removal
    pp.log <- data.frame(
        predicted.value = c(0),
        std.error = c(0.1),
        ci = c(0.2)
    )
    expect_warning(
        out.log <- biometryassist:::apply_transformation(pp.log, trans = "log", offset = 1, power = NULL),
        "Log back-transformation produced non-positive values\\. Check offset parameter\\."
    )
    expect_true(out.log$PredictedValue <= 0)

    # logit: force exact boundary value with -Inf
    pp.logit <- data.frame(
        predicted.value = c(-Inf),
        std.error = c(0.1),
        ci = c(0.2)
    )
    expect_warning(
        out.logit <- biometryassist:::apply_transformation(pp.logit, trans = "logit", offset = 0, power = NULL),
        "Logit back-transformation produced values outside \\(0,1\\)"
    )
    expect_true(out.logit$PredictedValue <= 0 || out.logit$PredictedValue >= 1)
})

test_that("apply_transformation warns for inverse edge cases", {
    pp.inv <- data.frame(
        predicted.value = c(0.1),
        std.error = c(0.1),
        ci = c(0.2)
    )

    warnings <- character()
    out <- withCallingHandlers(
        biometryassist:::apply_transformation(pp.inv, trans = "inverse", offset = 0, power = NULL),
        warning = function(w) {
            warnings <<- c(warnings, conditionMessage(w))
            invokeRestart("muffleWarning")
        }
    )

    expect_true(any(grepl("Inverse transformation: confidence interval crosses zero", warnings, fixed = TRUE)))
    expect_true("PredictedValue" %in% names(out))
    expect_true(out$low <= out$up)
})

test_that("apply_transformation warns for inverse near-zero predicted values", {
    pp.inv <- data.frame(
        predicted.value = c(1e-20),
        std.error = c(0.1),
        ci = c(1e-25)
    )

    expect_warning(
        out <- biometryassist:::apply_transformation(pp.inv, trans = "inverse", offset = 0, power = NULL),
        "Inverse transformation: predicted values very close to zero detected\\."
    )
    expect_true("PredictedValue" %in% names(out))
})

test_that("apply_transformation power validation and warnings", {
    pp <- data.frame(
        predicted.value = c(1),
        std.error = c(0.1),
        ci = c(0.2)
    )

    expect_error(
        biometryassist:::apply_transformation(pp, trans = "power", offset = 0, power = NULL),
        "Power transformation requires a non-zero numeric 'power' argument\\."
    )

    expect_warning(
        out <- biometryassist:::apply_transformation(pp, trans = "power", offset = 1, power = -1),
        "Power back-transformation with negative power produced values near zero\\."
    )
    expect_true("PredictedValue" %in% names(out))
})

test_that("apply_transformation arcsin warning and bounds", {
    pp <- data.frame(
        predicted.value = c(pi),
        std.error = c(0.1),
        ci = c(0.2)
    )

    expect_warning(
        out <- biometryassist:::apply_transformation(pp, trans = "arcsin", offset = 0, power = NULL),
        "Arcsin transformation: some predicted values outside \\[-pi/2, pi/2\\]\\."
    )

    expect_true("PredictedValue" %in% names(out))
    expect_true(out$PredictedValue >= 0 && out$PredictedValue <= 1)
})

test_that("apply_transformation arcsin warns when back-transformation is outside [0,1]", {
    # This branch is numerically very hard to reach with real sin() output because sin(x)^2
    # should be in [0,1]. Here we temporarily override `sin()` and `sqrt()` in the
    # apply_transformation() function environment to force an out-of-bounds value.
    fn2 <- biometryassist:::apply_transformation
    fn2_env <- new.env(parent = environment(fn2))
    fn2_env$sin <- function(x) rep(2, length(x))
    fn2_env$sqrt <- function(x) rep(0, length(x))
    environment(fn2) <- fn2_env

    pp <- data.frame(
        predicted.value = c(0),
        std.error = c(0.1),
        ci = c(0.2)
    )

    expect_warning(
        out <- fn2(pp, trans = "arcsin", offset = 0, power = NULL),
        "Arcsin back-transformation produced values outside \\[0,1\\]\\. This may indicate numerical issues\\."
    )

    expect_true("PredictedValue" %in% names(out))
    expect_true(out$PredictedValue > 1)
})

test_that("apply_transformation errors on invalid trans", {
    pp <- data.frame(
        predicted.value = c(0),
        std.error = c(0.1),
        ci = c(0.2)
    )

    expect_error(
        biometryassist:::apply_transformation(pp, trans = "not-a-transformation", offset = 0, power = NULL),
        "Invalid trans value\\. Must be one of: 'sqrt', 'log', 'logit', 'power', 'inverse', 'arcsin'\\.",
        fixed = FALSE
    )
})

test_that("ordering output works", {
    output1 <- multiple_comparisons(dat.aov, classify = "Species", descending = FALSE)
    output2 <- multiple_comparisons(dat.aov, classify = "Species", descending = TRUE)
    expect_equal(output1$predicted.value, c(0.25, 1.33, 2.03), tolerance = 5e-2)
    expect_equal(output2$predicted.value, c(2.03, 1.33, 0.25), tolerance = 5e-2)

    vdiffr::expect_doppelganger("mct ascending order", autoplot(output1))
    vdiffr::expect_doppelganger("mct descending output", autoplot(output2))
})

test_that("different interval types work", {
    output1 <- multiple_comparisons(dat.aov, classify = "Species", int.type = "1se")
    output2 <- multiple_comparisons(dat.aov, classify = "Species", int.type = "2se")
    expect_equal(output1$low, c(0.22, 1.30, 2.00), tolerance = 5e-2)
    expect_equal(output1$up, c(0.28, 1.36, 2.06), tolerance = 5e-2)
    expect_equal(output2$low, c(0.19, 1.27, 1.97), tolerance = 5e-2)
    expect_equal(output2$up, c(0.31, 1.39, 2.09), tolerance = 5e-2)

    vdiffr::expect_doppelganger("mct output 1se", autoplot(output1))
    vdiffr::expect_doppelganger("mct output 2se", autoplot(output2))
})

test_that("Testing asreml predictions", {
    skip_if_not_installed("Matrix")
    load(test_path("data", "asreml_model.Rdata"), .GlobalEnv)
    expect_warning(output <- multiple_comparisons(model.asr,
                                                  classify = "Nitrogen",
                                                  pred.obj = pred.asr,
                                                  dendf = dendf),
                   "Argument `pred\\.obj` has been deprecated and will be removed in a future version\\. Predictions are now performed internally in the function\\.")
    expect_equal(output$predicted.value,
                 c(77.76, 100.15, 114.41, 123.23),
                 tolerance = 5e-2)
    expect_snapshot_output(output)
    vdiffr::expect_doppelganger("asreml predictions", autoplot(output))
})

test_that("save produces output", {
    withr::local_file("pred_vals.csv")
    output <- multiple_comparisons(dat.aov, classify = "Species", save = TRUE, savename = "pred_vals")
    expect_snapshot_output(output)

    expect_csv_matches_df(output, "pred_vals.csv")
})

test_that("Interaction terms work", {
    load(test_path("data", "asreml_model.Rdata"), .GlobalEnv)
    skip_if_not_installed("asreml")
    quiet(library(asreml))
    output <- multiple_comparisons(model.asr, classify = "Nitrogen:Variety", pvals = T)
    expect_equal(output$predicted.value,
                 c(70.85, 76.58, 85.86, 92.22, 99.91, 108.32, 113.1, 113.5, 116.63, 118.4, 123.75, 127.53),
                 tolerance = 5e-2)

    vdiffr::expect_doppelganger("Interactions work", autoplot(output))
})

test_that("order argument is deprecated", {
    # dat.aov <- aov(Petal.Width ~ Species, data = iris)
    expect_warning(multiple_comparisons(dat.aov, classify = "Species", order = "xyz"),
                   "Argument `order` has been deprecated and will be removed in a future version. Please use `descending` instead.")
})

test_that("dashes are handled", {
    iris2 <- iris
    # Replace 'gin' in setosa with '-'
    iris2$Species <- as.factor(gsub("to", "-", iris2$Species))
    dat.aov2 <- aov(Petal.Width ~ Species, data = iris2)
    output2 <- suppressWarnings(multiple_comparisons(dat.aov2, classify = "Species"))
    expect_warning(multiple_comparisons(dat.aov2, classify = "Species"),
                   "The treatment level se-sa contained '-', which has been replaced in the final output with '_'")
    expect_equal(output2$predicted.value, c(0.25, 1.33, 2.03), tolerance = 5e-2)

    # Replace 'gin' in virginica with '-' as well
    iris2$Species <- as.factor(gsub("gin", "-", iris2$Species))
    dat.aov2 <- aov(Petal.Width ~ Species, data = iris2)

    expect_warning(multiple_comparisons(dat.aov2, classify = "Species"),
                   "The treatment levels se-sa, vir-ica contained '-', which has been replaced in the final output with '_'")
    expect_equal(output2$predicted.value, c(0.25, 1.33, 2.03), tolerance = 5e-2)

    # skip_if(interactive())
    vdiffr::expect_doppelganger("mct dashes output", autoplot(output2))
})

test_that("mct removes aliased treatments in aov", {
    CO_2 <- CO2
    CO_2$uptake[CO_2$Type=="Quebec" & CO_2$Treatment=="nonchilled"] <- NA
    model <- aov(uptake~Type+Treatment+Type:Treatment, data = CO_2)
    expect_warning(output1 <- multiple_comparisons(model, classify = "Type:Treatment"),
                   "A level of Type\\:Treatment is aliased\\. It has been removed from predicted output\\.\n  Aliased level is\\: Quebec:nonchilled\\.\n  This level is saved as an attribute of the output object\\.")
    expect_snapshot_output(output1$predicted.value)
    # skip_if(interactive())
    vdiffr::expect_doppelganger("aov aliased output", autoplot(output1))
})


test_that("mct handles aliased results in asreml with a warning", {
    skip_if_not_installed("asreml")
    quiet(library(asreml))
    load(test_path("data", "asreml_model.Rdata"), envir = .GlobalEnv)
    load(test_path("data", "oats_data.Rdata"), envir = .GlobalEnv)
    expect_warning(
        output <- multiple_comparisons(model.asr, classify = "Nitrogen:Variety"),
        "Aliased level is: 0\\.2_cwt:Golden_rain\\."
    )
    expect_snapshot_output(output)

    load(test_path("data", "oats_data2.Rdata"), envir = .GlobalEnv)

    expect_warning(
        output <- multiple_comparisons(model2.asr, classify = "Nitrogen:Variety"),
        "Some levels of Nitrogen:Variety are aliased\\. They have been removed from predicted output\\."
    )
    expect_snapshot_output(output)
    expect_warning(multiple_comparisons(model2.asr, classify = "Nitrogen:Variety"),
                   "Aliased levels are: 0\\.2_cwt:Golden_rain, 0\\.2_cwt:Victory\\.")
})

test_that("Invalid classify argument causes an error", {
    dat.aov <- aov(Petal.Width ~ Species, data = iris)
    expect_error(multiple_comparisons(dat.aov, classify = "ABC"),
                 "ABC is not a term in the model\\. Please check model specification\\.")
    expect_error(multiple_comparisons(model.asr, classify = "ABC"),
                 "ABC is not a term in the model\\. Please check model specification\\.")
})

test_that("Significance values that are too high give a warning or error", {
    # dat.aov <- aov(Petal.Width ~ Species, data = iris)
    expect_warning(multiple_comparisons(dat.aov, classify = "Species", sig = 0.95),
                   "Significance level given by `sig` is high. Perhaps you meant 0.05?")
    expect_error(multiple_comparisons(dat.aov, classify = "Species", sig = 5),
                 "Significance level given by `sig` is high. Perhaps you meant 0.05?")
    expect_error(multiple_comparisons(dat.aov, classify = "Species", sig = 95),
                 "Significance level given by `sig` is high. Perhaps you meant 0.05?")
})

test_that("Use of pred argument gives warning", {
    # dat.aov <- aov(Petal.Width ~ Species, data = iris)
    expect_warning(multiple_comparisons(dat.aov, classify = "Species", pred = "Species"),
                   "Argument `pred` has been deprecated and will be removed in a future version. Please use `classify` instead.")
})

test_that("Invalid column name causes an error", {
    dat <- design("crd", LETTERS[1:4], 4, nrow = 4, ncols = 4, quiet = TRUE)$design
    names(dat)[5] <- "groups"
    dat.aov <- aov(rnorm(16, 10)~groups, data = dat)

    expect_error(multiple_comparisons(dat.aov, classify = "groups"),
                 "Invalid column name. Please change the name of column\\(s\\): groups")
})

test_that("Including pred.obj object causes warning", {
    skip_if_not_installed("asreml")
    quiet(library(asreml))
    load(test_path("data", "asreml_model.Rdata"), envir = .GlobalEnv)
    expect_warning(multiple_comparisons(model.asr, pred.obj = pred.asr, classify = "Nitrogen"),
                   "Argument \\`pred.obj\\` has been deprecated and will be removed in a future version\\. Predictions are now performed internally in the function\\.")
})

test_that("Providing a random term in classify produces an error.", {
    skip_if_not_installed("asreml")
    load(test_path("data", "oats_data2.Rdata"), envir = .GlobalEnv)
    expect_error(multiple_comparisons(model2.asr, classify = "Blocks"),
                 "All predicted values are aliased\\. Perhaps you need the `present` argument\\?")
})

test_that("lme4 model works", {
    skip_if_not_installed("lme4")
    quiet(library(lme4))
    dat.lmer <- lmer(yield ~ Nitrogen*Variety + (1|Blocks), data = dat)
    output <- multiple_comparisons(dat.lmer, classify = "Nitrogen")
    expect_equal(output$std.error, rep(7.39, 4), tolerance = 5e-2)
    expect_equal(min(output$predicted.value), 79.39, tolerance = 5e-2)
    expect_equal(max(output$predicted.value), 123.39, tolerance = 5e-2)
    expect_equal(output$predicted.value, c(79.39, 98.89, 114.22, 123.39), tolerance = 5e-2)
    vdiffr::expect_doppelganger("lme4 output", autoplot(output))
})

test_that("3 way interaction works", {
    des <- design(type = "crossed:crd", treatments = c(3, 3, 3),
                  reps = 3, nrows = 9, ncols = 9, seed = 42, quiet = TRUE)
    des$design$response <- rnorm(81, 100)
    des$design$A <- factor(des$design$A)
    des$design$B <- factor(des$design$B)
    des$design$C <- factor(des$design$C)
    dat.aov <- aov(response~A*B*C, data = des$design)
    output <- multiple_comparisons(dat.aov, classify = "A:B:C")
    expect_snapshot_output(output$predicted.value)
    expect_equal(output$std.error,
                 rep(0.63, 27))
    # skip_if(interactive())
    vdiffr::expect_doppelganger("3 way interaction", autoplot(output), variant = ggplot2_variant())
})

test_that("plots are produced when requested", {
    des <- design(type = "crossed:crd", treatments = c(3, 3, 3),
                  reps = 3, nrows = 9, ncols = 9, seed = 42, quiet = TRUE)
    des$design$response <- rnorm(81, 100)
    des$design$A <- factor(des$design$A)
    des$design$B <- factor(des$design$B)
    des$design$C <- factor(des$design$C)
    dat.aov <- aov(response~A*B*C, data = des$design)

    expect_snapshot_output(output <- multiple_comparisons(dat.aov, classify = "A:B:C", plot = TRUE))
    # expect_snapshot_output(output$predicted.value)
    expect_equal(output$std.error,
                 rep(0.63, 27))
    skip_if(interactive())
    vdiffr::expect_doppelganger("3 way interaction internal",
                                output <- multiple_comparisons(dat.aov, classify = "A:B:C", plot = TRUE),
                                variant = ggplot2_variant())
})

test_that("nlme model produces an error", {
    skip_if_not_installed("nlme")
    suppressPackageStartupMessages(library(nlme))
    fm1 <- lme(distance ~ age, data = Orthodont)
    expect_error(multiple_comparisons(fm1, classify = "age"),
                 "model\\.obj must be a linear \\(mixed\\) model object\\. Currently supported model types are: aov, lm, lmerMod, lmerModLmerTest, asreml")
})

test_that("multiple_comparisons output has a class of 'mct'", {
    output <- multiple_comparisons(dat.aov, classify = "Species")
    expect_s3_class(output, "mct")
})

test_that("Setting groups to FALSE disables letter groups", {
    output <- multiple_comparisons(dat.aov, classify = "Species")
    expect_true("groups" %in% colnames(output))
    expect_equal(output$groups, c("a", "b", "c"))

    output <- multiple_comparisons(dat.aov, classify = "Species", groups = FALSE)
    expect_false("groups" %in% colnames(output))

    output <- multiple_comparisons(dat.aov, classify = "Species", letters = FALSE)
    expect_false("groups" %in% colnames(output))

    vdiffr::expect_doppelganger("No letter groups",
                                autoplot(output))
})

test_that("Check for letters as an alias of groups", {
    expect_warning(output <- multiple_comparisons(dat.aov, classify = "Species",
                                                  groups = FALSE, letters = TRUE),
                   "Both 'groups' and 'letters' provided\\. Using 'groups'\\.")
    expect_false("groups" %in% colnames(output))
})

test_that("autoplot can rotate axis and labels independently", {
    output <- multiple_comparisons(dat.aov, classify = "Species")
    vdiffr::expect_doppelganger("label rotation",
                                autoplot(output, label_rotation = 90))
    vdiffr::expect_doppelganger("axis rotation",
                                autoplot(output, axis_rotation = 90))
    vdiffr::expect_doppelganger("axis rotation -90",
                                autoplot(output, axis_rotation = -90))
    vdiffr::expect_doppelganger("axis and label rotation",
                                autoplot(output, axis_rotation = 45, label_rotation = 90))
    vdiffr::expect_doppelganger("rotation and axis rotation",
                                autoplot(output, rotation = 45, axis_rotation = 90))
    vdiffr::expect_doppelganger("rotation and label rotation",
                                autoplot(output, rotation = 45, label_rotation = 90))
})

test_that("Autoplot can output column graphs", {
    output <- multiple_comparisons(dat.aov, classify = "Species")
    p1 <- autoplot(output, type = "column", label_height = 1)
    p2 <- autoplot(output, type = "col", label_height = 1)
    expect_in("GeomCol", class(p1$layers[[1]]$geom))
    expect_in("GeomCol", class(p2$layers[[1]]$geom))
    expect_true(equivalent_ggplot2(p1, p2))
    vdiffr::expect_doppelganger("autoplot column", p1)
})

test_that("A warning is printed if a transformation is detected with no trans argument provided", {
    dat.aov.log <- aov(log(Petal.Width) ~ Species, data = iris)
    dat.aov.sqrt <- aov(sqrt(Petal.Width) ~ Species, data = iris)
    dat.aov.logit <- aov(logit(1/Petal.Width) ~ Species, data = iris)
    dat.aov.inverse <- aov((1/Petal.Width) ~ Species, data = iris)
    dat.aov.power <- aov(Petal.Width^3 ~ Species, data = iris)

    expect_warning(multiple_comparisons(dat.aov.log, classify = "Species"),
                   "The response variable appears to be transformed in the model formula: log\\(Petal\\.Width\\)\\.
Please specify the 'trans' argument if you want back-transformed predictions\\.")
    expect_warning(multiple_comparisons(dat.aov.sqrt, classify = "Species"),
                   "The response variable appears to be transformed in the model formula: sqrt\\(Petal\\.Width\\)\\.
Please specify the 'trans' argument if you want back-transformed predictions\\.")
    expect_warning(multiple_comparisons(dat.aov.logit, classify = "Species"),
                   "The response variable appears to be transformed in the model formula: logit\\(1/Petal\\.Width\\)\\.
Please specify the 'trans' argument if you want back-transformed predictions\\.")
    expect_warning(multiple_comparisons(dat.aov.inverse, classify = "Species"),
                   "The response variable appears to be transformed in the model formula: \\(1/Petal\\.Width\\)\\.
Please specify the 'trans' argument if you want back-transformed predictions\\.")
    expect_warning(multiple_comparisons(dat.aov.power, classify = "Species"),
                   "The response variable appears to be transformed in the model formula: Petal\\.Width\\^3\\.
Please specify the 'trans' argument if you want back-transformed predictions\\.")
})


# Test aliased output prints
test_that("print.mct with no aliased attribute", {
    dat.aov <- aov(Petal.Width ~ Species, data = iris)
    output <- multiple_comparisons(dat.aov, classify = "Species", plot = FALSE)
    attr(output, "aliased") <- "ABC"

    expect_true("aliased" %in% names(attributes(output)))
    expect_length(attr(output, "aliased"), 1)
    expect_output(print(output),
                  "Aliased level is: ABC")

    attr(output, "aliased") <- c("ABC", "DEF")

    expect_length(attr(output, "aliased"), 2)
    expect_true("aliased" %in% names(attributes(output)))
    expect_output(print(output),
                  "Aliased levels are: ABC and DEF")

    attr(output, "aliased") <- c("ABC", "DEF", "GHI")

    expect_length(attr(output, "aliased"), 3)
    expect_true("aliased" %in% names(attributes(output)))
    expect_output(print(output),
                  "Aliased levels are: ABC, DEF and GHI")

})

test_that("Standard error rounding preserves error bars", {
    # Create data with very small standard errors
    set.seed(123)

    # Create a mock model that would produce very small standard errors
    # We'll use a simple approach with very precise data
    precise_data <- data.frame(
        treatment = factor(rep(c("A", "B", "C"), each = 100)),
        response = c(rep(1.000001, 100), rep(1.000002, 100), rep(1.000003, 100)) +
            rnorm(300, 0, 0.0000001)  # Very small error
    )

    dat.aov <- aov(response ~ treatment, data = precise_data)

    # Test with default decimals (2) - should trigger warning
    expect_warning(
        output <- multiple_comparisons(dat.aov, classify = "treatment", decimals = 2),
        "Some standard errors are very small and would round to zero with 2 decimal places"
    )

    # Check that standard errors are preserved (not rounded to 0)
    expect_true(all(output$std.error > 0))
    expect_false(any(output$std.error == 0))

    # Check that other columns are still rounded to 2 decimal places
    expect_true(all(nchar(sub(".*\\.", "", as.character(output$predicted.value))) <= 2))
})

test_that("Standard error rounding works with transformed data", {
    # Create data that will have small standard errors on the transformed scale
    set.seed(456)
    # Use data that's already on log scale with very small differences
    transform_data <- data.frame(
        treatment = factor(rep(c("A", "B", "C"), each = 100)),
        # Create log-scale response with tiny differences and very small error
        log_response = c(rep(-0.0001, 100), rep(0.0000, 100), rep(0.0001, 100)) +
            rnorm(300, 0, 0.000001)  # Very small error on log scale
    )

    # Apply log transformation - the model is on log scale, transform back to original
    dat.aov <- aov(log_response ~ treatment, data = transform_data)

    # Test with transformation - should handle both std.error and ApproxSE
    expect_warning(
        output <- multiple_comparisons(dat.aov, classify = "treatment",
                                       trans = "log", offset = 0, decimals = 3),
        "Some standard errors are very small"
    )

    # Check that both standard error columns are preserved
    expect_true(all(output$std.error > 0))
    expect_true(all(output$ApproxSE > 0))
    expect_false(any(output$std.error == 0))
    expect_false(any(output$ApproxSE == 0))
})

test_that("Normal rounding works when standard errors are not too small", {
    # Use the standard iris data which has reasonable standard errors
    dat.aov <- aov(Petal.Width ~ Species, data = iris)

    # Should not trigger warning with normal data
    expect_no_warning(
        output <- multiple_comparisons(dat.aov, classify = "Species", decimals = 2)
    )

    # Check that all numeric columns are properly rounded
    expect_true(all(nchar(sub(".*\\.", "", as.character(output$predicted.value))) <= 2))
    expect_true(all(nchar(sub(".*\\.", "", as.character(output$std.error))) <= 2))
})

test_that("Standard error rounding works with different decimal settings", {
    # Create data with moderately small standard errors
    set.seed(789)
    moderate_data <- data.frame(
        treatment = factor(rep(c("A", "B"), each = 20)),
        response = c(rep(1.001, 20), rep(1.002, 20)) + rnorm(40, 0, 0.001)
    )

    dat.aov <- aov(response ~ treatment, data = moderate_data)

    # Test with decimals = 4 (should not trigger warning)
    expect_no_warning(
        output4 <- multiple_comparisons(dat.aov, classify = "treatment", decimals = 4)
    )

    # Test with decimals = 1 (might trigger warning depending on actual SE values)
    expect_warning(
        output1 <- multiple_comparisons(dat.aov, classify = "treatment", decimals = 1),
        "Some standard errors are very small and would round to zero with 1 decimal places"
    )

    # Ensure standard errors are never exactly 0
    expect_true(all(output1$std.error > 0))
    expect_true(all(output4$std.error > 0))
})


test_that("ApproxSE column is also preserved during rounding", {
    # Create data that will trigger the standard error preservation
    set.seed(111)
    # Create data with extremely small values to get tiny standard errors after log transformation
    precise_data <- data.frame(
        treatment = factor(rep(c("A", "B"), each = 100)),
        # Use very small response values that will have tiny standard errors
        response = c(rep(0.000001, 100), rep(0.000002, 100)) + rnorm(200, 0, 0.0000001)
    )

    # Use log transformation to create ApproxSE column
    dat.aov <- aov(log(response) ~ treatment, data = precise_data)

    # Should trigger warning and preserve both std.error and ApproxSE
    expect_warning(
        output <- multiple_comparisons(dat.aov, classify = "treatment",
                                       trans = "log", offset = 0, decimals = 2),
        "Some standard errors are very small"
    )

    # Both standard error columns should be preserved
    expect_true(all(output$std.error > 0))
    expect_true(all(output$ApproxSE > 0))
    expect_false(any(output$std.error == 0))
    expect_false(any(output$ApproxSE == 0))
})

