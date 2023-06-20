logit <- function (p, percents = range.p[2] > 1, adjust)
{
    range.p <- range(p, na.rm = TRUE)
    if (percents) {
        if (range.p[1] < 0 || range.p[1] > 100)
            stop("p must be in the range 0 to 100")
        p <- p/100
        range.p <- range.p/100
    }
    else if (range.p[1] < 0 || range.p[1] > 1)
        stop("p must be in the range 0 to 1")
    a <- if (missing(adjust)) {
        if (isTRUE(all.equal(range.p[1], 0)) || isTRUE(all.equal(range.p[2],
                                                                 1)))
            0.025
        else 0
    }
    else adjust
    if (missing(adjust) && a != 0)
        warning(paste("proportions remapped to (", a, ", ",
                      1 - a, ")", sep = ""))
    a <- 1 - 2 * a
    log((0.5 + a * (p - 0.5))/(1 - (0.5 + a * (p - 0.5))))
}

dat.aov <- aov(Petal.Width ~ Species, data = iris)

test_that("mct produces output", {
    # dat.aov <- aov(Petal.Width ~ Species, data = iris)
    output <- multiple_comparisons(dat.aov, classify = "Species", plot = TRUE)
    expect_equal(output$predicted.value, c(0.25, 1.33, 2.03), tolerance = 5e-2)
    # skip_if(interactive())
    vdiffr::expect_doppelganger("mct output", autoplot(output))
})

test_that("transformations are handled", {
    dat.aov.log <- aov(log(Petal.Width) ~ Species, data = iris)
    output.log <- multiple_comparisons(dat.aov.log, classify = "Species", trans = "log", offset = 0)
    output.log2 <- multiple_comparisons(dat.aov.log, classify = "Species", trans = "log", offset = 0, int.type = "1se")
    output.log3 <- multiple_comparisons(dat.aov.log, classify = "Species", trans = "log", offset = 0, int.type = "2se")
    dat.aov.sqrt <- aov(sqrt(Petal.Width) ~ Species, data = iris)
    output.sqrt <- multiple_comparisons(dat.aov.sqrt, classify = "Species", trans = "sqrt", offset = 0)
    output.sqrt2 <- multiple_comparisons(dat.aov.sqrt, classify = "Species", trans = "sqrt", offset = 0, int.type = "1se")
    output.sqrt3 <- multiple_comparisons(dat.aov.sqrt, classify = "Species", trans = "sqrt", offset = 0, int.type = "2se")
    dat.aov.logit <- aov(logit(1/Petal.Width) ~ Species, data = iris)
    output.logit <- multiple_comparisons(dat.aov.logit, classify = "Species", trans = "logit", offset = 0)
    output.logit2 <- multiple_comparisons(dat.aov.logit, classify = "Species", trans = "logit", offset = 0, int.type = "1se")
    output.logit3 <- multiple_comparisons(dat.aov.logit, classify = "Species", trans = "logit", offset = 0, int.type = "2se")
    dat.aov.inverse <- aov((1/Petal.Width) ~ Species, data = iris)
    output.inverse <- multiple_comparisons(dat.aov.inverse, classify = "Species", trans = "inverse", offset = 0)
    output.inverse2 <- multiple_comparisons(dat.aov.inverse, classify = "Species", trans = "inverse", offset = 0, int.type = "1se")
    output.inverse3 <- multiple_comparisons(dat.aov.inverse, classify = "Species", trans = "inverse", offset = 0, int.type = "2se")
    iris_new <- iris
    iris_new$Petal.Width <- (iris_new$Petal.Width+1)^3
    dat.aov.power <- aov(Petal.Width ~ Species, data = iris_new)
    output.power <- multiple_comparisons(dat.aov.power, classify = "Species", trans = "power", offset = 1, power = 3)
    output.power2 <- multiple_comparisons(dat.aov.power, classify = "Species", trans = "power", offset = 1, power = 3, int.type = "1se")
    output.power3 <- multiple_comparisons(dat.aov.power, classify = "Species", trans = "power", offset = 1, power = 3, int.type = "2se")

    expect_equal(output.log$predicted.value, c(-1.48, 0.27, 0.70), tolerance = 5e-2)
    expect_equal(output.log2$low, c(0.22, 1.26, 1.94), tolerance = 5e-2)
    expect_equal(output.log3$up, c(0.25, 1.41, 2.17), tolerance = 5e-2)
    expect_equal(output.sqrt$predicted.value, c(0.49, 1.15, 1.42), tolerance = 5e-2)
    expect_equal(output.sqrt2$low, c(0.23, 1.29, 1.98), tolerance = 5e-2)
    expect_equal(output.sqrt3$up, c(0.27, 1.38, 2.09), tolerance = 5e-2)
    expect_equal(output.logit$predicted.value, c(-5.30, -4.87, -3.07), tolerance = 5e-2)
    expect_equal(output.logit2$low, c(0.00, 0.01, 0.04), tolerance = 5e-2)
    expect_equal(output.logit3$up, c(0.01, 0.01, 0.05), tolerance = 5e-2)
    expect_equal(output.inverse$predicted.value, c(0.50, 0.77, 4.79), tolerance = 5e-2)
    expect_equal(output.inverse2$low, c(3.01, 1.66, 0.22), tolerance = 5e-2)
    expect_equal(output.inverse3$up, c(1.20, 0.90, 0.20), tolerance = 5e-2)
    expect_equal(output.power$predicted.value, c(1.98, 12.85, 28.38), tolerance = 5e-2)
    expect_equal(output.power2$low, c(0.09, 1.30, 2.03), tolerance = 5e-2)
    expect_equal(output.power3$up, c(0.49, 1.42, 2.10), tolerance = 5e-2)

    # skip_if(interactive())
    vdiffr::expect_doppelganger("mct log output", autoplot(output.log))
    vdiffr::expect_doppelganger("mct sqrt output", autoplot(output.sqrt))
    vdiffr::expect_doppelganger("mct logit output", autoplot(output.logit))
    vdiffr::expect_doppelganger("mct inverse output", autoplot(output.inverse))
    vdiffr::expect_doppelganger("mct power output", autoplot(output.power))
})

test_that("transformations with no offset produces a warning", {
    dat.aov <- aov(log(Petal.Width) ~ Species, data = iris)
    expect_warning(multiple_comparisons(dat.aov, classify = "Species", trans = "log"),
                   "Offset value assumed to be 0. Change with `offset` argument.")
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
    # dat.aov <- aov(Petal.Width ~ Species, data = iris)
    output1 <- multiple_comparisons(dat.aov, classify = "Species", int.type = "1se")
    output2 <- multiple_comparisons(dat.aov, classify = "Species", int.type = "2se")
    expect_equal(output1$low, c(0.22, 1.30, 2.00), tolerance = 5e-2)
    expect_equal(output1$up, c(0.28, 1.36, 2.06), tolerance = 5e-2)
    expect_equal(output2$low, c(0.19, 1.27, 1.97), tolerance = 5e-2)
    expect_equal(output2$up, c(0.31, 1.39, 2.09), tolerance = 5e-2)

    vdiffr::expect_doppelganger("mct output 1se", autoplot(output1))
    vdiffr::expect_doppelganger("mct output 2se", autoplot(output2))
})

test_that("save produces output", {
    # dat.aov <- aov(Petal.Width ~ Species, data = iris)
    withr::local_file("pred_vals.csv")
    output <- multiple_comparisons(dat.aov, classify = "Species", save = TRUE, savename = "pred_vals")
    expect_snapshot_output(output)
    expect_snapshot_file("pred_vals.csv")
    pred_vals <- read.csv("pred_vals.csv")
    expect_equal(dim(output), dim(pred_vals))
    expect_equal(colMeans(output[,c(2:4, 6:8)]), colMeans(pred_vals[,c(2:4, 6:8)]))
    expect_equal(colnames(output), colnames(pred_vals))
})

test_that("Interaction terms work", {
    load(test_path("data", "asreml_model.Rdata"), .GlobalEnv)
    skip_if_not(requireNamespace("asreml", quietly = TRUE))
    quiet(library(asreml))
    # model.asr <- readRDS(test_path("data", "model_asr.rds"))
    # load(test_path("data", "oats_data.Rdata"), envir = .GlobalEnv)
    output <- multiple_comparisons(model.asr, classify = "Nitrogen:Variety", pvals = T)
    expect_equal(output$predicted.value,
                 c(70.85, 76.58, 85.86, 92.22, 99.91, 108.32, 113.1, 113.5, 116.63, 118.4, 123.75, 127.53),
                 tolerance = 5e-2)

    # skip_if(interactive())
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
    skip_if_not(requireNamespace("asreml", quietly = TRUE))
    quiet(library(asreml))
    # model.asr <- readRDS(test_path("data", "model_asr.rds"))
    load(test_path("data", "asreml_model.Rdata"), envir = .GlobalEnv)
    load(test_path("data", "oats_data.Rdata"), envir = .GlobalEnv)
    expect_warning(
        expect_snapshot_output(
            multiple_comparisons(model.asr, classify = "Nitrogen:Variety")
        ),
        "Aliased level is: 0\\.2_cwt:Golden_rain\\."
    )
    # model2.asr <- readRDS(test_path("data", "model_asr2.rds"))
    load(test_path("data", "oats_data2.Rdata"), envir = .GlobalEnv)

    expect_warning(
        expect_snapshot_output(
            multiple_comparisons(model2.asr, classify = "Nitrogen:Variety")
        ),
        "Some levels of Nitrogen:Variety are aliased\\. They have been removed from predicted output\\."
    )
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

test_that("Significance values that are too high give a warning", {
    # dat.aov <- aov(Petal.Width ~ Species, data = iris)
    expect_warning(multiple_comparisons(dat.aov, classify = "Species", sig = 0.95),
                   "Significance level given by `sig` is high. Perhaps you meant 0.05?")
})

test_that("Use of pred argument gives warning", {
    # dat.aov <- aov(Petal.Width ~ Species, data = iris)
    expect_warning(multiple_comparisons(dat.aov, pred = "Species"),
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
    skip_if_not(requireNamespace("asreml", quietly = TRUE))
    quiet(library(asreml))
    load(test_path("data", "asreml_model.Rdata"), envir = .GlobalEnv)
    # load(test_path("data", "oats_data.Rdata"), envir = .GlobalEnv)
    # model.asr <- readRDS(test_path("data", "model_asr.rds"))
    expect_warning(multiple_comparisons(model.asr, pred.obj = pred.asr, classify = "Nitrogen"),
                   "Argument \\`pred.obj\\` has been deprecated and will be removed in a future version\\. Predictions are now performed internally in the function\\.")
})

test_that("Providing a random term in classify produces an error.", {
  skip_if_not(requireNamespace("asreml", quietly = TRUE))
  expect_error(multiple_comparisons(model2.asr, classify = "Blocks"), 
               "All predicted values are aliased\\. Perhaps you need the `present` argument\\?")
})

test_that("lme4 model works", {
    skip_if_not_installed("lme4")
    quiet(library(lme4))
    # load(test_path("data", "oats_data.Rdata"), envir = .GlobalEnv)
    dat.lmer <- lmer(yield ~ Nitrogen*Variety + (1|Blocks), data = dat)
    output <- multiple_comparisons(dat.lmer, classify = "Nitrogen")
    expect_equal(output$std.error, rep(7.39, 4), tolerance = 5e-2)
    expect_equal(min(output$predicted.value), 79.39, tolerance = 5e-2)
    expect_equal(max(output$predicted.value), 123.39, tolerance = 5e-2)
    # skip_on_os("linux")
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
    vdiffr::expect_doppelganger("3 way interaction", autoplot(output))
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
    vdiffr::expect_doppelganger("3 way interaction internal", output <- multiple_comparisons(dat.aov, classify = "A:B:C", plot = TRUE))
})

test_that("nlme model produces an error", {
    skip_if_not_installed("nlme")
    suppressPackageStartupMessages(library(nlme))
    fm1 <- lme(distance ~ age, data = Orthodont)
    expect_error(multiple_comparisons(fm1, classify = "age"),
                 "Models of type lme are not supported.")
})

test_that("multiple_comparisons output has a class of 'mct'", {
    output <- multiple_comparisons(dat.aov, classify = "Species")
    expect_s3_class(output, "mct")
})


test_that("autoplot can rotate axis and labels independently", {
    output <- multiple_comparisons(dat.aov, classify = "Species")
    vdiffr::expect_doppelganger("label rotation",
                                autoplot(output, label_rotation = 90))
    vdiffr::expect_doppelganger("axis rotation",
                                autoplot(output, axis_rotation = 90))
    vdiffr::expect_doppelganger("axis and label rotation",
                                autoplot(output, axis_rotation = 45, label_rotation = 90))
    vdiffr::expect_doppelganger("rotation and axis rotation",
                                autoplot(output, rotation = 45, axis_rotation = 90))
    vdiffr::expect_doppelganger("rotation and label rotation",
                                autoplot(output, rotation = 45, label_rotation = 90))
    vdiffr::expect_doppelganger("rotation with hjust and vjust",
                                autoplot(output, rotation = 45, label_rotation = 90, hjust = 0, vjust = 0.5))
})












# test_that("sommer model works", {
#     skip_if_not_installed("sommer")
#     quiet(library(sommer))
#     data("DT_yatesoats")
#     dat.sommer <- mmer(Y ~ N*V, random = ~B + B/MP, data = DT_yatesoats, verbose = FALSE)
#     output <- multiple_comparisons(dat.sommer, classify = "N")
#     expect_identical(output$predicted.value, c(0.25, 1.33, 2.03))
#     skip_if(interactive())
#     vdiffr::expect_doppelganger("mct output", output$predicted_plot)
# })

# skip if not local/asreml installed
# model.asr <- asreml(yield ~ Nitrogen + Variety + Nitrogen:Variety,
#                     random = ~ Blocks + Blocks:Wplots,
#                     residual = ~ units,
#                     data = asreml::oats)
#
# pred.asr <- predict(model.asr, classify = "Nitrogen:Variety", sed = TRUE)

# multiple_comparisons(model.obj = model.asr, pred.obj = pred.asr, classify = "Nitrogen:Variety", label_height = 0.1)

# model.asr <- asreml(log(yield) ~ Nitrogen + Variety + Nitrogen:Variety,
#                     random = ~ Blocks + Blocks:Wplots,
#                     residual = ~ units,
#                     data = asreml::oats)
#
# pred.asr <- predict(model.asr, classify = "Nitrogen", sed = TRUE)
#
# multiple_comparisons(model.obj = model.asr, pred.obj = pred.asr, classify = "Nitrogen", trans = "log", offset = 0, label_height = 0.1)

