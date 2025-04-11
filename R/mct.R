#' Perform Multiple Comparison Tests on a statistical model
#'
#' A function for comparing and ranking predicted means with Tukey's Honest Significant Difference (HSD) Test.
#'
#' @param model.obj An ASReml-R or aov model object. Will likely also work with `lme` ([nlme::lme()]), `lmerMod` ([lme4::lmer()]) models as well.
#' @param classify Name of predictor variable as string.
#' @param sig The significance level, numeric between 0 and 1. Default is 0.05.
#' @param int.type The type of confidence interval to calculate. One of `ci`, `1se` or `2se`. Default is `ci`.
#' @param trans Transformation that was applied to the response variable. One of `log`, `sqrt`, `logit`, `power` or `inverse`. Default is `NA`.
#' @param offset Numeric offset applied to response variable prior to transformation. Default is `NA`. Use 0 if no offset was applied to the transformed data. See Details for more information.
#' @param power Numeric power applied to response variable with power transformation. Default is `NA`. See Details for more information.
#' @param decimals Controls rounding of decimal places in output. Default is 2 decimal places.
#' @param descending Logical (default `FALSE`). Order of the output sorted by the predicted value. If `TRUE`, largest will be first, through to smallest last.
#' @param groups Logical (default `TRUE`). If `TRUE`, the significance letter groupings will be calculated and displayed. This can get overwhelming for large numbers of comparisons, so can be turned off by setting to `FALSE`.
#' @param plot Automatically produce a plot of the output of the multiple comparison test? Default is `FALSE`. This is maintained for backwards compatibility, but the preferred method now is to use `autoplot(<multiple_comparisons output>)`. See [biometryassist::autoplot.mct()] for more details.
#' @param label_height Height of the text labels above the upper error bar on the plot. Default is 0.1 (10%) of the difference between upper and lower error bars above the top error bar.
#' @param rotation Rotate the text output as Treatments within the plot. Allows for easier reading of long treatment labels. Number between 0 and 360 (inclusive) - default 0
#' @param save Logical (default `FALSE`). Save the predicted values to a csv file?
#' @param savename A file name for the predicted values to be saved to. Default is `predicted_values`.
#' @param order Deprecated. Use `descending` instead.
#' @param pred Deprecated. Use `classify` instead.
#' @param pred.obj Deprecated. Predicted values are calculated within the function from version 1.0.1 onwards.
#' @param ... Other arguments passed through to `predict.asreml()`.
#'
#' @importFrom multcompView multcompLetters
#' @importFrom emmeans emmeans
#' @importFrom stats model.frame predict qtukey qt terms var
#' @importFrom utils packageVersion
#'
#' @details
#' ## Offset
#' Some transformations require that data has a small offset to be
#' applied, otherwise it will cause errors (for example taking a log of 0, or
#' the square root of negative values). In order to correctly reverse this
#' offset, if the `trans` argument is supplied, a value should also be supplied
#' in the `offset` argument. By default the function assumes no offset was
#' required for a transformation, implying a value of 0 for the `offset`
#' argument. If an offset value is provided, use the same value as provided in
#' the model, not the inverse. For example, if adding 0.1 to values for a log
#' transformation, add 0.1 in the `offset` argument.
#'
#' @details
#' ## Power
#' The power argument allows the specification of arbitrary powers to be
#' back transformed, if they have been used to attempt to improve normality of
#' residuals.
#'
#' @returns A list containing a data frame with predicted means, standard errors,
#'  confidence interval upper and lower bounds, and significant group
#'  allocations (named `predicted_values`), as well as a plot visually
#'  displaying the predicted values (named `predicted_plot`). If some of the
#'  predicted values are aliased, a warning is printed, and the aliased
#'  treatment levels are returned in the output (named `aliased`).
#'
#' @references Jørgensen, E. & Pedersen, A. R. (1997). How to Obtain Those Nasty
#'  Standard Errors From Transformed Data - and Why They Should Not Be Used. [<https://pure.au.dk/portal/en/publications/how-to-obtain-those-nasty-standard-errors-from-transformed-data--and-why-they-should-not-be-used(d649ca20-d15f-11db-8e26-000ea68e967b).html>](https://pure.au.dk/portal/en/publications/how-to-obtain-those-nasty-standard-errors-from-transformed-data--and-why-they-should-not-be-used(d649ca20-d15f-11db-8e26-000ea68e967b).html)
#'
#' @examples
#' # Fit aov model
#' model <- aov(Petal.Length ~ Species, data = iris)
#'
#' # Display the ANOVA table for the model
#' anova(model)
#'
#' # Determine ranking and groups according to Tukey's Test
#' pred.out <- multiple_comparisons(model, classify = "Species")
#'
#' # Display the predicted values table
#' pred.out
#'
#' # Show the predicted values plot
#' autoplot(pred.out, label_height = 0.5)
#'
#'
#' # AOV model example with transformation
#' my_iris <- iris
#' my_iris$Petal.Length <- exp(my_iris$Petal.Length) # Create exponential response
#' exp_model <- aov(Petal.Length ~ Species, data = my_iris)
#'
#' resplot(exp_model) # Residual plot shows problems
#'
#' # Fit a new model using a log transformation of the response
#' log_model <- aov(log(Petal.Length) ~ Species, data = my_iris)
#'
#' resplot(log_model) # Looks much better
#'
#' # Display the ANOVA table for the model
#' anova(log_model)
#'
#' # Back transform, because the "original" data was exponential
#' pred.out <- multiple_comparisons(log_model, classify = "Species",
#'                                    trans = "log")
#'
#' # Display the predicted values table
#' pred.out
#'
#' # Show the predicted values plot
#' autoplot(pred.out, label_height = 15)
#'
#' \dontrun{
#' # ASReml-R Example
#' library(asreml)
#'
#' #Fit ASReml Model
#' model.asr <- asreml(yield ~ Nitrogen + Variety + Nitrogen:Variety,
#'                     random = ~ Blocks + Blocks:Wplots,
#'                     residual = ~ units,
#'                     data = asreml::oats)
#'
#' wald(model.asr) #Nitrogen main effect significant
#'
#' #Determine ranking and groups according to Tukey's Test
#' pred.out <- multiple_comparisons(model.obj = model.asr, classify = "Nitrogen",
#'                     descending = TRUE, decimals = 5)
#'
#' pred.out
#'
#' # Example using a box-cox transformation
#' set.seed(42) # See the seed for reproducibility
#' resp <- rnorm(n = 50, 5, 1)^3
#' trt <- as.factor(sample(rep(LETTERS[1:10], 5), 50))
#' block <- as.factor(rep(1:5, each = 10))
#' ex_data <- data.frame(resp, trt, block)
#'
#' # Change one treatment random values to get significant difference
#' ex_data$resp[ex_data$trt=="A"] <- rnorm(n = 5, 7, 1)^3
#'
#' model.asr <- asreml(resp ~ trt,
#'                     random = ~ block,
#'                     residual = ~ units,
#'                     data = ex_data)
#'
#' resplot(model.asr)
#'
#' # Perform Box-Cox transformation and get maximum value
#' out <- MASS::boxcox(ex_data$resp~ex_data$trt)
#' out$x[which.max(out$y)] # 0.3838
#'
#' # Fit cube root to the data
#' model.asr <- asreml(resp^(1/3) ~ trt,
#'                     random = ~ block,
#'                     residual = ~ units,
#'                     data = ex_data)
#' resplot(model.asr) # residual plots look much better
#'
#' #Determine ranking and groups according to Tukey's Test
#' pred.out <- multiple_comparisons(model.obj = model.asr,
#'                                  classify = "trt",
#'                                  trans = "power", power = (1/3))
#'
#' pred.out
#' autoplot(pred.out)
#' }
#'
#' @export
#'
multiple_comparisons <- function(model.obj,
                                 classify,
                                 sig = 0.05,
                                 int.type = "ci",
                                 trans = NA,
                                 offset = NA,
                                 power = NA,
                                 decimals = 2,
                                 descending = FALSE,
                                 groups = TRUE,
                                 plot = FALSE,
                                 label_height = 0.1,
                                 rotation = 0,
                                 save = FALSE,
                                 savename = "predicted_values",
                                 order,
                                 pred.obj,
                                 pred,
                                 ...) {

    # Handle deprecated parameters
    handle_deprecated_param("pred", "classify", classify)
    handle_deprecated_param("order", "descending", descending)

    vars <- validate_inputs(sig, classify, model.obj)

    # Handle deprecated parameter that's being removed
    handle_deprecated_param("pred.obj", NULL, "Predictions are now performed internally in the function.")

    # Process dots
    rlang::check_dots_used()
    args <- list(...)

    # Check for alias 'letters' instead of 'groups'
    if ("letters" %in% names(args)) {
        if (!missing(groups)) {
            warning("Both 'groups' and 'letters' provided. Using 'groups'.")
        } else {
            groups <- args$letters
        }
    }

    # asr_args <- args[names(args) %in% names(formals(asreml::predict.asreml))]

    # Get model-specific predictions and SED
    result <- get_predictions(model.obj, classify, args, pred.obj, ...)

    pp <- result$predictions
    sed <- result$sed
    ndf <- result$df
    ylab <- result$ylab

    # Process treatment names
    pp <- process_treatment_names(pp, classify)

    # Calculate critical values and determine pairs that are significantly different
    result <- calculate_differences(pp, sed, ndf, sig)
    crit_val <- result$crit_val
    diffs <- result$diffs

    # Add letter groups if requested
    if (groups) {
        pp <- add_letter_groups(pp, diffs, descending)
    }

    # Calculate confidence intervals
    pp <- add_confidence_intervals(pp, int.type, sig, ndf)

    # Apply transformations if requested
    if (!is.na(trans)) {
        pp <- apply_transformation(pp, trans, offset, power)
    } else {
        pp$low <- pp$predicted.value - pp$ci
        pp$up <- pp$predicted.value + pp$ci
    }

    # Order results and format output
    pp <- format_output(pp, descending, vars, decimals)

    # Save if requested
    if (save) {
        utils::write.csv(pp, file = paste0(savename, ".csv"), row.names = FALSE)
    }

    # Add attributes
    pp <- add_attributes(pp, ylab, crit_val, result$aliased_names)

    # Plot if requested
    if (plot) {
        print(autoplot(pp))
    }

    return(pp)
}


#' Print output of multiple_comparisons
#'
#' @param x An mct object to print to the console.
#' @param ... Other arguments
#'
#' @returns The original object invisibly.
#' @seealso [multiple_comparisons()]
#' @method print mct
#' @export
#' @examples
#' dat.aov <- aov(Petal.Width ~ Species, data = iris)
#' output <- multiple_comparisons(dat.aov, classify = "Species")
#' print(output)
print.mct <- function(x, ...) {
    stopifnot(inherits(x, "mct"))
    print.data.frame(x, ...)

    if(!is.null(attr(x, "aliased"))) {
        aliased <- attr(x, "aliased")
        if(length(aliased) > 1) {
            cat("\nAliased levels are:", paste(aliased[1:(length(aliased)-1)], collapse = ", "), "and", aliased[length(aliased)], "\n")
        }
        else {
            cat("\nAliased level is:", aliased, "\n")
        }
    }
    invisible(x)
}

validate_inputs <- function(sig, classify, model.obj) {
    # Check significance level
    if (sig > 0.5) {
        warning("Significance level given by `sig` is high. Perhaps you meant ", 1-sig, "?", call. = FALSE)
    }

    # Check model type
    supported_models <- c("asreml", "aov", "lm", "lmerMod", "lmerModLmerTest")
    if (!any(sapply(supported_models, function(x) inherits(model.obj, x)))) {
        stop("Models of type ", class(model.obj), " are not supported.", call. = FALSE)
    }

    # Get the individual names provided in classify
    vars <- unlist(strsplit(classify, "\\:"))
    reserved_col_names <- c("predicted.value", "std.error", "Df",
                            "groups", "PredictedValue", "ApproxSE", "ci", "low", "up")
    if (any(vars %in% reserved_col_names)) {
        stop("Invalid column name. Please change the name of column(s): ",
             vars[vars %in% reserved_col_names], call. = FALSE)
    }

    return(vars)
}

process_treatment_names <- function(pp, classify, vars) {
    # Create Names column
    if (grepl(":", classify)) {
        pp$Names <- apply(pp[, vars], 1, paste, collapse = "_")
    } else {
        pp$Names <- pp[[classify]]
    }

    # Check and replace dashes in treatment names
    if (any(grepl("-", pp$Names) | any(grepl("-", pp[, 1])))) {
        levs <- unique(c(grep("-", pp[, 1], value = TRUE), grep("-", pp$Names, value = TRUE)))
        if (length(levs) > 1) {
            warning("The treatment levels ", paste(levs, collapse = ", "),
                    " contained '-', which has been replaced in the final output with '_'")
        } else {
            warning("The treatment level ", levs,
                    " contained '-', which has been replaced in the final output with '_'")
        }
        pp[, 1] <- gsub(pattern = "-", replacement = "_", pp[, 1])
        pp$Names <- gsub(pattern = "-", replacement = "_", pp$Names)
    }

    return(pp)
}

calculate_differences <- function(pp, sed, ndf, sig) {
    # Calculate the critical value
    crit_val <- 1 / sqrt(2) * stats::qtukey((1 - sig), nrow(pp), ndf) * sed

    # Determine pairs that are significantly different
    diffs <- abs(outer(pp$predicted.value, pp$predicted.value, "-")) > crit_val
    diffs <- diffs[lower.tri(diffs)]

    # Create a vector of treatment comparison names
    m <- outer(pp$Names, pp$Names, paste, sep = "-")
    m <- m[lower.tri(m)]

    names(diffs) <- m

    # Return both the critical value and the differences
    return(list(crit_val = crit_val, diffs = diffs))
}

add_confidence_intervals <- function(pp, int.type, sig, ndf) {
    # Calculate confidence interval width
    pp$ci <- switch(
        tolower(int.type),
        "ci" = stats::qt(p = sig, ndf, lower.tail = FALSE) * pp$std.error,
        "1se" = pp$std.error,
        "2se" = 2 * pp$std.error,
        stop("Invalid int.type.")
    )

    return(pp)
}

apply_transformation <- function(pp, trans, offset, power) {
    # Set default offset if not provided
    if (is.na(offset)) {
        warning("Offset value assumed to be 0. Change with `offset` argument.")
        offset <- 0
    }

    # Apply appropriate transformation
    if (trans == "sqrt") {
        pp$PredictedValue <- (pp$predicted.value)^2 - ifelse(!is.na(offset), offset, 0)
        pp$ApproxSE <- 2 * abs(pp$std.error) * sqrt(pp$PredictedValue)

        pp$low <- (pp$predicted.value - pp$ci)^2 - ifelse(!is.na(offset), offset, 0)
        pp$up <- (pp$predicted.value + pp$ci)^2 - ifelse(!is.na(offset), offset, 0)
    } else if (trans == "log") {
        pp$PredictedValue <- exp(pp$predicted.value) - ifelse(!is.na(offset), offset, 0)
        pp$ApproxSE <- abs(pp$std.error) * pp$PredictedValue

        pp$low <- exp(pp$predicted.value - pp$ci) - ifelse(!is.na(offset), offset, 0)
        pp$up <- exp(pp$predicted.value + pp$ci) - ifelse(!is.na(offset), offset, 0)
    } else if (trans == "logit") {
        pp$PredictedValue <- exp(pp$predicted.value) / (1 + exp(pp$predicted.value))
        pp$ApproxSE <- pp$PredictedValue * (1 - pp$PredictedValue) * abs(pp$std.error)

        ll <- pp$predicted.value - pp$ci
        pp$low <- exp(ll) / (1 + exp(ll))
        uu <- pp$predicted.value + pp$ci
        pp$up <- exp(uu) / (1 + exp(uu))
    } else if (trans == "power") {
        pp$PredictedValue <- (pp$predicted.value)^(1/power) - ifelse(!is.na(offset), offset, 0)
        pp$ApproxSE <- pp$std.error * (1/(power * pp$PredictedValue^(power-1)))

        pp$low <- (pp$predicted.value - pp$ci)^(1/power) - ifelse(!is.na(offset), offset, 0)
        pp$up <- (pp$predicted.value + pp$ci)^(1/power) - ifelse(!is.na(offset), offset, 0)
    } else if (trans == "inverse") {
        pp$PredictedValue <- 1/pp$predicted.value
        pp$ApproxSE <- abs(pp$std.error) * pp$PredictedValue^2

        pp$low <- 1/(pp$predicted.value - pp$ci)
        pp$up <- 1/(pp$predicted.value + pp$ci)
    }

    return(pp)
}

add_letter_groups <- function(pp, diffs, descending) {
    ll <- multcompView::multcompLetters3("Names", "predicted.value", diffs, pp, reversed = !descending)

    rr <- data.frame(groups = ll$Letters)
    rr$Names <- row.names(rr)

    pp <- merge(pp, rr)
    return(pp)
}

format_output <- function(pp, descending, vars, decimals) {
    # Order by predicted value
    pp <- pp[base::order(pp$predicted.value, decreasing = descending), ]

    # Remove Names column
    pp$Names <- NULL

    # Extract treatment variable names
    trtindex <- max(unlist(lapply(paste0("^", vars, "$"), grep, x = names(pp))))

    # Operator for "not in"
    `%!in%` <- function(x, y) !(`%in%`(x, y))

    trtnam <- names(pp)[1:trtindex]
    # Exclude reserved column names
    trtnam <- trtnam[trtnam %!in% c("predicted.value", "std.error", "Df",
                                    "groups", "PredictedValue", "ApproxSE", "ci", "low", "up")]

    # Convert treatment columns to factors with ordered levels
    for (i in seq_along(trtnam)) {
        pp[[trtnam[i]]] <- factor(pp[[trtnam[i]]], levels = unique(pp[[trtnam[i]]]))
    }

    # Round numeric columns to specified decimal places
    pp <- rapply(object = pp, f = round, classes = "numeric", how = "replace", digits = decimals)

    # Remove row names
    rownames(pp) <- NULL

    return(pp)
}

add_attributes <- function(pp, ylab, crit_val, aliased_names) {
    # If there are brackets in the label, grab the text from inside
    if (is.call(ylab)) {
        ylab <- as.character(ylab)[2]
    }
    attr(pp, "ylab") <- ylab

    # Add class
    class(pp) <- c("mct", class(pp))

    # Add aliased treatments as attribute
    if (!is.null(aliased_names)) {
        attr(pp, 'aliased') <- as.character(aliased_names)
    }

    # Add critical value as attribute
    if (stats::var(as.vector(crit_val), na.rm = TRUE) < 1e-10) {
        attr(pp, 'HSD') <- crit_val[1, 2]
    } else {
        attr(pp, 'HSD') <- crit_val
    }

    return(pp)
}
