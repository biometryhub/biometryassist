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
#' @importFrom patchwork wrap_plots plot_layout plot_annotation plot_spacer
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

        n_non_missing <- sum(!is.na(group_residuals$residuals))
        sd_resids <- sd(group_residuals$residuals, na.rm = TRUE)
        denom <- sd_resids * sqrt((n_non_missing - 1) / n_non_missing)

        group_residuals$stdres <- if (is.finite(denom) && denom > 0) {
            group_residuals$residuals / denom
        } else {
            rep(NA_real_, nrow(group_residuals))
        }

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
                                    onepage_cols,
                                    label.size
    )

    return(result)
}

#' Create diagnostic plots
#' @param group_residuals Data frame with residuals and fitted values
#' @param axes.size Size of axes labels
#' @param label.size Size of plot labels
#' @keywords internal
create_diagnostic_plots <- function(group_residuals, axes.size, label.size) {

    # ggplot2 stats/geoms warn when non-finite values are dropped during build/print.
    # Filter here so downstream printing (e.g., vdiffr) stays clean.
    group_residuals_clean <- group_residuals[
        is.finite(group_residuals$stdres) & !is.na(group_residuals$stdres),
    ]

    group_scatter_clean <- group_residuals_clean[
        is.finite(group_residuals_clean$fitted) & !is.na(group_residuals_clean$fitted),
    ]

    # Set histogram x axis width
    max_val <- max(abs(group_residuals_clean$stdres), na.rm = TRUE)
    max_val <- ifelse(max_val > 3.5, max_val+0.25, 3.5)
    breaks_seq <- seq(0.25, max_val, by = 0.5)
    breaks_seq <- c(-rev(breaks_seq), breaks_seq)

    a <- ggplot2::ggplot(data = group_residuals_clean, mapping = ggplot2::aes(x = stdres)) +
        ggplot2::geom_histogram(
            bins = ifelse(nrow(group_residuals) < 31, 7, 11),
            fill = "aquamarine3",
            colour = "black",
            breaks = breaks_seq
        ) +
        ggplot2::theme_bw(base_size = axes.size) +
        ggplot2::labs(y = "Frequency", x = "Standardised Residual", tag = "A") +
        ggplot2::theme(plot.tag = ggplot2::element_text(size = label.size))

    b <- ggplot2::ggplot(group_residuals_clean, ggplot2::aes(sample = stdres)) +
        ggplot2::geom_qq(
            colour = "black",
            fill = "aquamarine3",
            size = 2,
            shape = 21
        ) +
        ggplot2::geom_qq_line() +
        ggplot2::theme_bw(base_size = axes.size) +
        ggplot2::labs(y = "Standardised Residual", x = "Theoretical", tag = "B") +
        ggplot2::theme(plot.tag = ggplot2::element_text(size = label.size))

    c <- ggplot2::ggplot(
        data = group_scatter_clean,
        mapping = ggplot2::aes(x = fitted, y = stdres)
    ) +
        ggplot2::geom_point(
            colour = "black",
            fill = "aquamarine3",
            size = 2,
            shape = 21
        ) +
        ggplot2::theme_bw(base_size = axes.size) +
        ggplot2::labs(y = "Standardised Residual", x = "Fitted Value", tag = "C") +
        ggplot2::theme(plot.tag = ggplot2::element_text(size = label.size))

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
    output <- list(text1 = paste(shap$method, "p-value:", round(shap$p.value, 4)),
                   text2 = ifelse(
                       shap$p.value > 0.05,
                       paste0("The residuals appear to be normally distributed. (n = ", n, ")"),
                       paste0("The residuals do not appear to be normally distributed. (n = ", n, ")"))
    )

    return(output)
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
    # Build the complete layout using patchwork's design specification
    if (!is.null(shapiro_result)) {
        shapiro_plot <- ggplot2::ggplot() +
            ggplot2::annotate(
                "text",
                x = 0, y = 0.75,
                label = shapiro_result$text1,
                size = 3.5,
                hjust = 0.5,
                vjust = 1
            ) +
            ggplot2::annotate(
                "text",
                x = 0, y = 0.25,
                label = shapiro_result$text2,
                size = 3.0,
                hjust = 0.5,
                vjust = 0
            ) +
            ggplot2::theme_void() +
            ggplot2::coord_cartesian(xlim = c(-1, 1), ylim = c(0, 1), clip = "off")

        # Create complete layout with all elements including Shapiro
        combined <- plots$histogram + plots$qq +
            plots$scatter +
            shapiro_plot +
            patchwork::plot_layout(
                design = "AAAAABBBBB
                          ##CCCCCC##
                          DDDDDDDDDD",
                heights = c(1, 1, 0.4)
            )
    } else {
        # Create layout without Shapiro text
        combined <- plots$histogram + plots$qq + plots$scatter +
            patchwork::plot_layout(
                design = "AABB
                          #CC#"
            )
    }

    # Add model call if requested
    if (call && !is.null(model_call)) {
        combined <- combined +
            patchwork::plot_annotation(
                title = model_call,
                theme = ggplot2::theme(
                    plot.title = ggplot2::element_text(size = call.size, hjust = 0.5)
                )
            )
    }

    combined
}

#' Format final output based on facet structure
#' @param output List of plot objects
#' @param facet Number of facets
#' @param facet_name Names of facets
#' @param onepage Logical, combine plots on one page
#' @param onepage_cols Number of columns for onepage layout
#' @keywords internal
format_output_resplot <- function(output, facet, facet_name, onepage, onepage_cols, label.size) {
    if (facet > 1) {
        names(output) <- facet_name

        if (onepage) {
            onepage_cols <- min(max(1, onepage_cols), facet)
            plots_per_page <- onepage_cols * ceiling(6 / onepage_cols)
            n_pages <- ceiling(facet / plots_per_page)
            pages <- vector("list", n_pages)

            for (page in 1:n_pages) {
                start_idx <- (page - 1) * plots_per_page + 1
                end_idx <- min(page * plots_per_page, facet)
                n_plots_on_page <- end_idx - start_idx + 1
                n_cols <- min(onepage_cols, n_plots_on_page)
                n_rows <- ceiling(n_plots_on_page / n_cols)

                # Add titles to each plot group using wrap_elements
                plots_with_titles <- lapply(start_idx:end_idx, function(i) {
                    patchwork::wrap_elements(full = output[[i]]) +
                        ggplot2::ggtitle(facet_name[i]) +
                        ggplot2::theme(
                            plot.title = ggplot2::element_text(
                                face = "bold",
                                size = 14,
                                hjust = 0.5
                            )
                        )
                })

                pages[[page]] <- patchwork::wrap_plots(
                    plots_with_titles,
                    ncol = n_cols,
                    nrow = n_rows
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
resplt <- function(model.obj,
                   shapiro = TRUE,
                   call = FALSE,
                   label.size = 10,
                   axes.size = 10,
                   call.size = 9,
                   mod.obj) {
    .Deprecated(
        msg = "resplt has been deprecated in version 1.0.1 and will be removed in a future version.\nPlease use resplot() instead."
    )
    resplot(model.obj, shapiro, call, label.size, axes.size, call.size, mod.obj)
}
