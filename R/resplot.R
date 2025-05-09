#' Produce residual plots of linear models
#'
#' Produces plots of residuals for assumption checking of linear (mixed) models.
#'
#' @param model.obj An `aov`, `lm`, `lme` ([nlme::lme()]), `lmerMod` ([lme4::lmer()]), `asreml` or `mmer` (sommer) model object.
#' @param shapiro (Logical) Display the Shapiro-Wilk test of normality on the plot? This test is unreliable for larger numbers of observations and will not work with n >= 5000 so will be omitted from any plots.
#' @param call (Logical) Display the model call on the plot?
#' @param axes.size A numeric value for the size of the axes label font size in points.
#' @param label.size A numeric value for the size of the label (A,B,C) font point size.
#' @param call.size A numeric value for the size of the model displayed on the plot.
#' @param mod.obj Deprecated to be consistent with other functions. Please use `model.obj` instead.
#'
#' @returns A ggplot2 object containing the diagnostic plots.
#'
#' @importFrom ggplot2 ggplot geom_histogram aes theme_bw stat_qq labs geom_qq_line geom_point
#' @importFrom stats fitted qnorm quantile residuals sd shapiro.test
#' @importFrom cowplot plot_grid add_sub
#'
#' @examples
#' dat.aov <- aov(Petal.Length ~ Petal.Width, data = iris)
#' resplot(dat.aov)
#' resplot(dat.aov, call = TRUE)
#' @export

resplot <- function(model.obj, shapiro = TRUE, call = FALSE, label.size = 10, axes.size = 10, call.size = 9, mod.obj){

    handle_deprecated_param("mod.obj", "model.obj")
    # Need to pass on old argument if provided
    if(!missing(mod.obj)) {
        warning("mod.obj has been deprecated to be consistent with other functions. Please use `model.obj` instead.")
        model.obj <- mod.obj
    }

    if(inherits(model.obj, c("aov", "lm", "lme", "lmerMod", "lmerModLmerTest"))) {
        facet <- 1
        facet_name <- NULL
        resids <- residuals(model.obj)
        k <- length(resids)
        fits <- fitted(model.obj)
        if(call) {
            if(inherits(model.obj, c("aov", "lm", "lme"))) {
                model_call <- paste(trimws(deparse(model.obj$call, width.cutoff = 50)), collapse = "\n")
            }
            else if(inherits(model.obj, c("lmerMod", "lmerModLmerTest")))  {
                model_call <- paste(trimws(deparse(model.obj@call, width.cutoff = 50)), collapse = "\n")
            }
        }
    }
    else if(inherits(model.obj, "asreml")){
        facet <- length(names(model.obj$R.param))
        if(facet > 1) {
            facet_name <- names(model.obj$R.param)
            k <- unlist(lapply(1:facet, function(i) model.obj$R.param[[i]]$variance$size))
        }
        else {
            facet_name <- NULL
            k <- length(model.obj$residual)
        }
        resids <- as.numeric(model.obj[["residuals"]])
        fits <- fitted(model.obj)
        fits <- ifelse(rep("fitted" %in% names(model.obj), nrow(model.obj$mf)), model.obj[["fitted"]], fitted(model.obj))

        if(call) {
            model_call <- paste(trimws(deparse(model.obj$call, width.cutoff = 50)), collapse = "\n")
            model_call <- gsub("G\\.param \\= model\\.asr\\$G\\.param, ", "", model_call)
            model_call <- gsub("R\\.param = model\\.asr\\$R\\.param, \\\n", "", model_call)
        }
    }
    else if(inherits(model.obj, c("mmer"))) { # sommer doesn't display residuals the same way
        facet <- model.obj$termsN$rcov
        facet_name <- NULL
        k <- length(model.obj$residual)

        resids <- residuals(model.obj)
        fits <- fitted(model.obj)
        model_call <- paste(trimws(deparse(model.obj$call[c("fixed", "random", "rcov")], width.cutoff = 50)), collapse = "\n")
        model_call <- gsub("list", "mmer", model_call)
    }
    else if(inherits(model.obj, "mmes")) { # new sommer function. More like other mixed model functions
        facet <- 1 #model.obj$termsN$rcov
        facet_name <- NULL
        k <- length(model.obj$residual)

        resids <- as.numeric(residuals(model.obj))
        fits <- as.numeric(fitted(model.obj))#$dataWithFitted[,paste0(model.obj$terms$response[[1]], ".fitted")]
        model_call <- "Model call not currently available for mmes models."
            # paste(trimws(deparse(model.obj$call[c("fixed", "random", "rcov")], width.cutoff = 50)), collapse = "\n")
        # model_call <- gsub("list", "mmer", model_call)
    }
    else if(inherits(model.obj, "art")) {
        facet <- 1
        facet_name <- NULL
        resids <- residuals(model.obj)
        k <- length(resids)
        fits <- model.obj$cell.means[,ncol(model.obj$cell.means)]
        if(call) {
            model_call <- paste(trimws(deparse(model.obj$call, width.cutoff = 50)), collapse = "\n")
        }
    }
    else {
        stop("model.obj must be a linear (mixed) model object. Currently supported model types are: aov, lm, lmerMod, lmerModLmerTest, asreml, mmer or art", call. = FALSE)
    }

    aa <- data.frame(residuals = resids, fitted = fits, lvl = rep(1:facet, k))

    output <- list()

    for(i in 1:facet){

        aa.f <- aa[aa$lvl==i,]
        aa.f$stdres <- aa.f$residuals/(sd(aa.f$residuals, na.rm = TRUE)*sqrt((length(!is.na(aa.f$residuals)-1))/(length(!is.na(aa.f$residuals)))))

        a <- ggplot2::ggplot(data = aa.f, mapping = ggplot2::aes(x = stdres)) +
            ggplot2::geom_histogram(bins = ifelse(nrow(aa) < 31, 7, 11), fill = "aquamarine3", colour = "black") +
            ggplot2::theme_bw(base_size = axes.size) + ggplot2::labs(y = "Frequency", x = "Standardised Residual")

        b <- ggplot2::ggplot(aa.f, ggplot2::aes(sample = stdres)) + ggplot2::geom_qq(colour = "black", fill = "aquamarine3", size = 2 , shape = 21) +
            ggplot2::geom_qq_line() + ggplot2::theme_bw(base_size = axes.size) +
            ggplot2::labs(y = "Standardised Residual", x = "Theoretical")

        c <- ggplot2::ggplot(data = aa.f, mapping = ggplot2::aes(x = fitted, y = stdres)) +
            ggplot2::geom_point(colour = "black", fill = "aquamarine3", size = 2 , shape = 21) + ggplot2::theme_bw(base_size = axes.size) +
            ggplot2::labs(y = "Standardised Residual", x = "Fitted Value")

        top_row <- cowplot::plot_grid(a, b, ncol=2, labels = c("A", "B"), label_size = label.size)

        if(nrow(aa.f) >= 5000 & shapiro) {
            warning("Shapiro-Wilk test p-values are unreliable for more than 5000 observations and has not been performed.")
            shapiro <- FALSE
        }

        if(shapiro) {
            if(nrow(aa.f) >= 2000) {
                warning("Shapiro-Wilk test p-values are unreliable for large numbers of observations.")
            }
            shap <- shapiro.test(aa.f$residuals)

            shapiro_text <- c(paste(shap$method, "p-value:", round(shap$p.value, 4)),
                              ifelse(shap$p.value>0.05,
                                     paste0("The residuals appear to be normally distributed. (n = ", length(aa.f$residuals), ")"),
                                     paste0("The residuals do not appear to be normally distributed. (n = ", length(aa.f$residuals), ")")))

            bottom_row <- cowplot::plot_grid(NULL, cowplot::add_sub(cowplot::add_sub(c, shapiro_text[1], size = 11, vjust = 1.2),
                                                                    shapiro_text[2], size = 9, vjust = 0.2), NULL,
                                             ncol=3, rel_widths=c(0.25,0.5,0.25), labels = c("", "C", ""),
                                             label_size = label.size, hjust = 1)
        }
        else{
            bottom_row <- cowplot::plot_grid(NULL, c, NULL, ncol=3, rel_widths=c(0.25,0.5,0.25), labels = c("", "C", ""), hjust = -1, label_size = label.size)
        }
        if(call) {
            title <- cowplot::ggdraw() + cowplot::draw_label(model_call, size = call.size, hjust = 0.5)
            call_row <- cowplot::plot_grid(title, ncol=1)
            output[[i]] <- cowplot::plot_grid(call_row, top_row, bottom_row, ncol=1, rel_heights = c(0.1, 0.4, 0.4))
        }
        else{
            output[[i]] <- cowplot::plot_grid(top_row, bottom_row, ncol=1, rel_heights = c(0.4, 0.4))
        }
    }

    if(facet>1) {
        names(output) <- facet_name
        return(output)
    }
    else {
        return(output[[1]])
    }
}

# resplt
#' @title Residual plots of linear models.
#' @description Produces plots of residuals for assumption checking of linear (mixed) models.
#' @param model.obj An `aov`, `lm`, `lme` ([nlme::lme()]), `lmerMod` ([lme4::lmer()]), `asreml` or `mmer` (sommer) model object.
#' @param shapiro (Logical) Display the Shapiro-Wilks test of normality on the plot?
#' @param call (Logical) Display the model call on the plot?
#' @param axes.size A numeric value for the size of the axes label font size in points.
#' @param label.size A numeric value for the size of the label (A,B,C) font point size.
#' @param call.size A numeric value for the size of the model displayed on the plot.
#' @param mod.obj Deprecated to be consistent with other functions. Please use `model.obj` instead.
#'
#' @return A list containing ggplot2 objects which are diagnostic plots.
#'
#' @name resplt-deprecated
#' @usage resplt(model.obj, shapiro = TRUE, call = FALSE, label.size = 10,
#' axes.size = 10, call.size = 9, mod.obj)
#' @seealso \code{\link{biometryassist-deprecated}}
#' @keywords internal
NULL

#' @rdname biometryassist-deprecated
#' @section resplt:
#' Residual plots of linear models.
#' @return A list containing ggplot2 objects which are diagnostic plots.
#' For `resplt`, use [resplot()].
#'
#' @export
resplt <- function(model.obj, shapiro = TRUE, call = FALSE, label.size = 10, axes.size = 10, call.size = 9, mod.obj) {
    .Deprecated(msg = "resplt has been deprecated in version 1.0.1 and will be removed in a future version.\nPlease use resplot() instead.")
    resplot(model.obj, shapiro, call, label.size, axes.size, call.size, mod.obj)
}
