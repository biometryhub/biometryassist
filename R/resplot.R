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
#' @param onepage (Logical) If TRUE and there are multiple plots, combines up to 6 plots per page.
#' @param onepage_cols Integer. Number of columns to use in grid layout when onepage=TRUE. Default is 3.
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
resplot <- function(model.obj, shapiro = TRUE, call = FALSE,
                    label.size = 10, axes.size = 10, call.size = 9,
                    onepage = FALSE, onepage_cols = 3, mod.obj) {

    handle_deprecated_param("mod.obj", "model.obj")

    # Extract model information using S3 dispatch
    model_info <- extract_model_info(model.obj, call = call)

    # Create data frame for plotting
    residuals_df <- data.frame(residuals = model_info$resids,
                               fitted = model_info$fits,
                               lvl = rep(1:model_info$facet, model_info$k))

    output <- list()

    for (i in 1:model_info$facet) {
        group_residuals <- residuals_df[residuals_df$lvl == i, ]
        group_residuals$stdres <- group_residuals$residuals /
            (sd(group_residuals$residuals, na.rm = TRUE) *
                 sqrt((sum(!is.na(group_residuals$residuals)) - 1) /
                          sum(!is.na(group_residuals$residuals))))

        # Create individual plots
        plots <- create_diagnostic_plots(group_residuals, axes.size, label.size)

        # Handle Shapiro-Wilk test
        shapiro_result <- NULL
        if(shapiro) {
            shapiro_result <- shapiro_test(group_residuals)
        }

        # Combine plots
        output[[i]] <- combine_plots(
            plots,
            shapiro_result,
            model_info$model_call,
            call,
            call.size,
            label.size
        )
    }

    # Handle output formatting based on facet structure
    result <- format_output_resplot(output,
        model_info$facet,
        model_info$facet_name,
        onepage,
        onepage_cols
    )

    return(result)
}

#' Create diagnostic plots
#' @param group_residuals Data frame with residuals and fitted values
#' @param axes.size Size of axes labels
#' @param label.size Size of plot labels
#' @keywords internal
create_diagnostic_plots <- function(group_residuals, axes.size, label.size) {
    a <- ggplot2::ggplot(data = group_residuals, mapping = ggplot2::aes(x = stdres)) +
        ggplot2::geom_histogram(
            bins = ifelse(nrow(group_residuals) < 31, 7, 11),
            fill = "aquamarine3",
            colour = "black",
            breaks = seq(floor(min(group_residuals$stdres, na.rm = TRUE)), ceiling(max(group_residuals$stdres, na.rm = TRUE)), by = 0.5)
        ) +
        ggplot2::theme_bw(base_size = axes.size) +
        ggplot2::labs(y = "Frequency", x = "Standardised Residual")

    b <- ggplot2::ggplot(group_residuals, ggplot2::aes(sample = stdres)) +
        ggplot2::geom_qq(
            colour = "black",
            fill = "aquamarine3",
            size = 2,
            shape = 21
        ) +
        ggplot2::geom_qq_line() +
        ggplot2::theme_bw(base_size = axes.size) +
        ggplot2::labs(y = "Standardised Residual", x = "Theoretical")

    c <- ggplot2::ggplot(
        data = group_residuals,
        mapping = ggplot2::aes(x = fitted, y = stdres)
    ) +
        ggplot2::geom_point(
            colour = "black",
            fill = "aquamarine3",
            size = 2,
            shape = 21
        ) +
        ggplot2::theme_bw(base_size = axes.size) +
        ggplot2::labs(y = "Standardised Residual", x = "Fitted Value")

    list(histogram = a, qq = b, scatter = c)
}

#' Shapiro-Wilk test
#' @param group_residuals Data frame with residuals
#' @keywords internal
shapiro_test <- function(group_residuals) {

    if (nrow(group_residuals) >= 5000) {
        warning("Shapiro-Wilk test p-values are unreliable for more than 5000 observations and has not been performed.", call. = FALSE)
        return(NULL)
    }

    if (nrow(group_residuals) >= 2000) {
        warning("Shapiro-Wilk test p-values are unreliable for large numbers of observations.", call. = FALSE)
    }

    shap <- shapiro.test(group_residuals$residuals)
    n <- length(group_residuals$residuals)
    list(
        text1 = paste(shap$method, "p-value:", round(shap$p.value, 4)),
        text2 = ifelse(
            shap$p.value > 0.05,
            paste0("The residuals appear to be normally distributed. (n = ", n, ")"),
            paste0("The residuals do not appear to be normally distributed. (n = ", n, ")")
        )
    )
}

#' Combine plots into final output
#' @param plots List of ggplot objects
#' @param shapiro_result Result from Shapiro test or NULL
#' @param model_call Model call string or NULL
#' @param call Logical, whether to include call
#' @param call.size Size of call text
#' @param label.size Size of labels
#' @keywords internal
combine_plots <- function(plots, shapiro_result, model_call, call, call.size, label.size) {
    top_row <- cowplot::plot_grid(
        plots$histogram,
        plots$qq,
        ncol = 2,
        labels = c("A", "B"),
        label_size = label.size
    )

    if (!is.null(shapiro_result)) {
        bottom_row <- cowplot::plot_grid(
            NULL,
            cowplot::add_sub(
                cowplot::add_sub(
                    plots$scatter,
                    shapiro_result$text1,
                    size = 11,
                    vjust = 1.2
                ),
                shapiro_result$text2,
                size = 9,
                vjust = 0.2
            ),
            NULL,
            ncol = 3,
            rel_widths = c(0.25, 0.5, 0.25),
            labels = c("", "C", ""),
            label_size = label.size,
            hjust = 1
        )
    } else {
        bottom_row <- cowplot::plot_grid(
            NULL,
            plots$scatter,
            NULL,
            ncol = 3,
            rel_widths = c(0.25, 0.5, 0.25),
            labels = c("", "C", ""),
            hjust = -1,
            label_size = label.size
        )
    }

    if (call && !is.null(model_call)) {
        title <- cowplot::ggdraw() +
            cowplot::draw_label(model_call, size = call.size, hjust = 0.5)
        call_row <- cowplot::plot_grid(title, ncol = 1)
        cowplot::plot_grid(
            call_row,
            top_row,
            bottom_row,
            ncol = 1,
            rel_heights = c(0.1, 0.4, 0.4)
        )
    } else {
        cowplot::plot_grid(
            top_row,
            bottom_row,
            ncol = 1,
            rel_heights = c(0.4, 0.4)
        )
    }
}

#' Format final output based on facet structure
#' @param output List of plot objects
#' @param facet Number of facets
#' @param facet_name Names of facets
#' @param onepage Logical, combine plots on one page
#' @param onepage_cols Number of columns for onepage layout
#' @keywords internal
format_output_resplot <- function(output, facet, facet_name, onepage, onepage_cols) {
    if (facet > 1) {
        names(output) <- facet_name

        if (onepage) {
            # Validate onepage_cols
            onepage_cols <- min(max(1, onepage_cols), facet)

            # Calculate number of pages needed based on onepage_cols
            plots_per_page <- onepage_cols * ceiling(6 / onepage_cols)
            n_pages <- ceiling(facet / plots_per_page)
            pages <- vector("list", n_pages)

            for (page in 1:n_pages) {
                # Get index range for current page
                start_idx <- (page - 1) * plots_per_page + 1
                end_idx <- min(page * plots_per_page, facet)

                # Calculate grid dimensions for current page
                n_plots_on_page <- end_idx - start_idx + 1
                n_cols <- min(onepage_cols, n_plots_on_page)
                n_rows <- ceiling(n_plots_on_page / n_cols)

                # Create combined plot for current page
                pages[[page]] <- cowplot::plot_grid(
                    plotlist = output[start_idx:end_idx],
                    ncol = n_cols,
                    nrow = n_rows,
                    scale = 0.90,
                    labels = facet_name[start_idx:end_idx]
                )
            }
            return(pages)
        } else {
            return(output)
        }
    } else {
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
resplt <- function(
        model.obj,
        shapiro = TRUE,
        call = FALSE,
        label.size = 10,
        axes.size = 10,
        call.size = 9,
        mod.obj
) {
    .Deprecated(
        msg = "resplt has been deprecated in version 1.0.1 and will be removed in a future version.\nPlease use resplot() instead."
    )
    resplot(model.obj, shapiro, call, label.size, axes.size, call.size, mod.obj)
}
