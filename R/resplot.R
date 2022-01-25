#' Residual plots of linear models.
#'
#' Produces plots of residuals for assumption checking of linear (mixed) model.
#'
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
#' @importFrom ggplot2 ggplot geom_histogram aes theme_bw stat_qq labs geom_qq_line geom_point
#' @importFrom stats fitted qnorm quantile residuals sd shapiro.test
#' @importFrom cowplot plot_grid add_sub
#'
#' @aliases resplt
#'
#' @examples
#' dat.aov <- aov(Petal.Length ~ Petal.Width, data = iris)
#' resplot(dat.aov)
#' resplt(dat.aov, call = TRUE)
#' @export

resplot <- function(model.obj, shapiro = TRUE, call = FALSE, label.size = 10, axes.size = 10, call.size = 9, mod.obj){
    if(!missing(mod.obj)) {
        warning("mod.obj has been deprecated to be consistent with other functions. Please use `model.obj` instead.")
        model.obj <- mod.obj
    }

    if (inherits(model.obj, c("aov", "lm", "lmerMod", "lme", "lmerModLmerTest"))) {
        facet <- 1
        facet_name <- NULL
        resids <- residuals(model.obj)
        k <- length(resids)
        fits <- fitted(model.obj)
    }
    else if (inherits(model.obj, "asreml")){
        facet <- length(names(model.obj$R.param))
        if (facet > 1) {
            facet_name <- names(model.obj$R.param)
            k <- unlist(lapply(1:facet, function(i) model.obj$R.param[[i]]$variance$size))
        }
        else {
            facet_name <- NULL
            k <- length(model.obj$residual)
        }
        resids <- residuals(model.obj)
        fits <- fitted(model.obj)
    }
    else if(inherits(model.obj, "mmer")) { # sommer doesn't display residuals the same way
        facet <- model.obj$termsN$rcov
        facet_name <- NULL
        k <- length(model.obj$residual)

        resids <- residuals(model.obj)[,ncol(residuals(model.obj))]
        fits <- fitted(model.obj)$dataWithFitted[,paste0(model.obj$terms$response[[1]], ".fitted")]
    }
    else {
        stop("model.obj must be an aov, lm, lmerMod, lmerModLmerTest, asreml or mmer object")
    }

    aa <- data.frame(residuals = resids, fitted = fits, lvl = rep(1:facet, k))

    output <- list()

    for (i in 1:facet){

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

        if(shapiro) {
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
            title <- cowplot::ggdraw() + cowplot::draw_label(paste(deparse(model.obj$call), collapse = "\n"), size = call.size, hjust = 0.5)
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

#' @rdname resplot
#' @export
resplt <- resplot
