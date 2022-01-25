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
    expect_equal(output$predicted.value, c(0.25, 1.33, 2.03))
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

    expect_equal(output.log$predicted.value, c(-1.48, 0.27, 0.70))
    expect_equal(output.log2$predicted.value, c(-1.48, 0.27, 0.70))
    expect_equal(output.log3$predicted.value, c(-1.48, 0.27, 0.70))
    expect_equal(output.sqrt$predicted.value, c(0.49, 1.15, 1.42))
    expect_equal(output.sqrt2$predicted.value, c(0.49, 1.15, 1.42))
    expect_equal(output.sqrt3$predicted.value, c(0.49, 1.15, 1.42))
    expect_equal(output.logit$predicted.value, c(-3.07, -4.87, -5.30))
    expect_equal(output.logit2$predicted.value, c(-3.07, -4.87, -5.30))
    expect_equal(output.logit3$predicted.value, c(-3.07, -4.87, -5.30))
    expect_equal(output.inverse$predicted.value, c(4.79, 0.77, 0.50))
    expect_equal(output.inverse2$predicted.value, c(4.79, 0.77, 0.50))
    expect_equal(output.inverse3$predicted.value, c(4.79, 0.77, 0.50))

    # skip_if(interactive())
    vdiffr::expect_doppelganger("mct log output", autoplot(output.log))
    vdiffr::expect_doppelganger("mct sqrt output", autoplot(output.sqrt))
    vdiffr::expect_doppelganger("mct logit output", autoplot(output.logit))
    vdiffr::expect_doppelganger("mct inverse output", autoplot(output.inverse))
})

test_that("transformations with no offset produces an error", {
    dat.aov <- aov(log(Petal.Width) ~ Species, data = iris)
    expect_error(multiple_comparisons(dat.aov, classify = "Species", trans = "log"))
})

test_that("ordering output works", {
    output1 <- multiple_comparisons(dat.aov, classify = "Species", order = "asc")
    output2 <- multiple_comparisons(dat.aov, classify = "Species", order = "desc")
    expect_equal(output1$predicted.value, c(0.25, 1.33, 2.03))
    expect_equal(output2$predicted.value, c(2.03, 1.33, 0.25))

    vdiffr::expect_doppelganger("mct ascending order", autoplot(output1))
    vdiffr::expect_doppelganger("mct descending output", autoplot(output2))
})

test_that("different interval types work", {
    # dat.aov <- aov(Petal.Width ~ Species, data = iris)
    output1 <- multiple_comparisons(dat.aov, classify = "Species", int.type = "1se")
    output2 <- multiple_comparisons(dat.aov, classify = "Species", int.type = "2se")
    expect_equal(output1$low, c(0.22, 1.30, 2.00))
    expect_equal(output1$up, c(0.27, 1.35, 2.05))
    expect_equal(output2$low, c(0.19, 1.27, 1.97))
    expect_equal(output2$up, c(0.30, 1.38, 2.08))

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
    skip_if_not(requireNamespace("asreml", quietly = TRUE))
    quiet(library(asreml))
    load(test_path("data", "asreml_model.Rdata"), .GlobalEnv)
    output <- multiple_comparisons(model.asr, pred.asr, classify = "Nitrogen:Variety")
    expect_equal(output$predicted.value,
                 c(76.58, 85.86, 70.85, 99.91, 108.32, 92.22, 116.63, 113.50, 113.10, 123.75, 127.53, 118.40))

    # skip_if(interactive())
    vdiffr::expect_doppelganger("Interactions work", autoplot(output))
})

test_that("invalid order input produces an error", {
    # dat.aov <- aov(Petal.Width ~ Species, data = iris)
    expect_error(multiple_comparisons(dat.aov, classify = "Species", order = "xyz"),
                 "order must be one of 'ascending', 'increasing', 'descending', 'decreasing' or 'default'")
    expect_error(
        expect_warning(multiple_comparisons(dat.aov, classify = "Species", order = 1:2),
                       "argument 'pattern' has length > 1 and only the first element will be used"),
        "order must be one of 'ascending', 'increasing', 'descending', 'decreasing' or 'default'")
})

test_that("dashes are handled", {
    iris2 <- iris
    # Replace 'gin' in setosa with '-'
    iris2$Species <- as.factor(gsub("to", "-", iris2$Species))
    dat.aov2 <- aov(Petal.Width ~ Species, data = iris2)
    output2 <- suppressWarnings(multiple_comparisons(dat.aov2, classify = "Species"))
    expect_warning(multiple_comparisons(dat.aov2, classify = "Species"),
                   "The treatment level se-sa contained '-', which has been replaced in the final output with '_'")
    expect_equal(output2$predicted.value, c(0.25, 1.33, 2.03))

    # Replace 'gin' in virginica with '-' as well
    iris2$Species <- as.factor(gsub("gin", "-", iris2$Species))
    dat.aov2 <- aov(Petal.Width ~ Species, data = iris2)

    expect_warning(multiple_comparisons(dat.aov2, classify = "Species"),
                   "The treatment levels se-sa, vir-ica contained '-', which has been replaced in the final output with '_'")
    expect_equal(output2$predicted.value, c(0.25, 1.33, 2.03))

    # skip_if(interactive())
    vdiffr::expect_doppelganger("mct dashes output", autoplot(output2))
})

test_that("mct removes aliased treatments in aov", {
    iris1 <- iris
    iris1$Petal.Length[1:50] <- NA
    dat.aov1 <- aov(Petal.Length ~ Species, data = iris1)
    output1 <- multiple_comparisons(dat.aov1, classify = "Species")
    expect_equal(output1$predicted.value, c(4.26, 5.55))
    # skip_if(interactive())
    vdiffr::expect_doppelganger("aov aliased output", autoplot(output1))
})


test_that("mct handles aliased results in asreml with a warning", {
    skip_if_not(requireNamespace("asreml", quietly = TRUE))
    quiet(library(asreml))
    model.asr <- readRDS(test_path("data", "model_asr.rds"))
    pred.asr <- readRDS(test_path("data", "pred_asr.rds"))
    model2.asr <- readRDS(test_path("data", "model_asr2.rds"))
    pred2.asr <- readRDS(test_path("data", "pred_asr2.rds"))
    dat <- readRDS(test_path("data", "oats_data.rds"))
    pred.asr$pvals$predicted.value[12] <- NA
    pred.asr$sed[12, ] <- NA
    pred.asr$sed[, 12] <- NA
    expect_warning(
        expect_output(
            print(multiple_comparisons(model.asr, pred.asr, classify = "Nitrogen:Variety")),
            "Aliased level is:  0.6_cwt:Victory"),
        NULL)
    pred.asr$pvals$predicted.value[11] <- NA
    pred.asr$sed[11, ] <- NA
    pred.asr$sed[, 11] <- NA
    expect_warning(multiple_comparisons(model.asr, pred.asr, classify = "Nitrogen:Variety"), NULL)
    pred2.asr$pvals$predicted.value[4] <- NA
    pred2.asr$sed[4, ] <- NA
    pred2.asr$sed[, 4] <- NA
    expect_warning(multiple_comparisons(model2.asr, pred2.asr, classify = "Nitrogen"), NULL)
    pred2.asr$pvals$predicted.value[3] <- NA
    pred2.asr$sed[3, ] <- NA
    pred2.asr$sed[, 3] <- NA
    expect_warning(multiple_comparisons(model2.asr, pred2.asr, classify = "Nitrogen"), NULL)
})

test_that("Significance values that are too high give a warning", {
    # dat.aov <- aov(Petal.Width ~ Species, data = iris)
    expect_warning(multiple_comparisons(dat.aov, classify = "Species", sig = 0.95),
                   "Significance level given by sig is high. Perhaps you meant 0.05?")
})

test_that("Use of pred argument gives warning", {
    # dat.aov <- aov(Petal.Width ~ Species, data = iris)
    expect_warning(multiple_comparisons(dat.aov, pred = "Species"),
                   "Argument pred has been deprecated and will be removed in a future version. Please use classify instead.")
})

test_that("Missing pred.obj object causes error", {
    skip_if_not(requireNamespace("asreml", quietly = TRUE))
    quiet(library(asreml))
    model.asr <- readRDS(test_path("data", "model_asr.rds"))
    dat <- readRDS(test_path("data", "oats_data.rds"))
    expect_error(suppressWarnings(multiple_comparisons(model.asr, classify = "Nitrogen")),
                 "You must provide a prediction object in pred.obj")
})

test_that("Forgetting sed = T in pred.obj object causes error", {
    skip_if_not(requireNamespace("asreml", quietly = TRUE))
    quiet(library(asreml))
    dat.asr <- quiet(asreml(Petal.Width ~ Species, data = iris, trace = FALSE))
    pred.out <- predict.asreml(dat.asr, classify = "Species")
    expect_error(multiple_comparisons(dat.asr, pred.out, classify = "Species"),
                 "Prediction object \\(pred.obj\\) must be created with argument sed = TRUE\\.")
})

test_that("lme4 model works", {
    skip_if_not_installed("lme4")
    quiet(library(lme4))
    dat <- readRDS(test_path("data", "oats_data.rds"))
    dat.lmer <- lmer(yield ~ Nitrogen*Variety + (1|Blocks), data = dat)
    output <- multiple_comparisons(dat.lmer, classify = "Nitrogen")
    expect_equal(output$std.error, rep(7.4, 4))
    expect_equal(min(output$predicted.value), 79.39)
    expect_equal(max(output$predicted.value), 123.39)
    skip_on_os("linux")
    expect_equal(output$predicted.value, c(79.39, 98.89, 114.22, 123.39))
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
    expect_equal(output$predicted.value[1:10],
                 c(100.68, 100.46, 99.79, 99.08, 100.48, 99.90, 100.19, 99.26, 100.22, 99.94))
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
    output <- multiple_comparisons(dat.aov, classify = "A:B:C")
    expect_equal(output$predicted.value[1:10],
                 c(100.68, 100.46, 99.79, 99.08, 100.48, 99.90, 100.19, 99.26, 100.22, 99.94))
    expect_equal(output$std.error,
                 rep(0.63, 27))
    # skip_if(interactive())
    vdiffr::expect_doppelganger("3 way interaction", autoplot(output))
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

