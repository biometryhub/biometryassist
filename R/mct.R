#' Perform Multiple Comparison Tests on a statistical model
#'
#' A function for comparing and ranking predicted means with Tukey's Honest Significant Difference (HSD) Test.
#'
#' @param model.obj An ASReml-R or aov model object. Will likely also work with `lme` ([nlme::lme()]), `lmerMod` ([lme4::lmer()]) models as well.
#' @param classify Name of predictor variable as string.
#' @param sig The significance level, numeric between 0 and 1. Default is 0.05.
#' @param int.type The type of confidence interval to calculate. One of `ci`, `tukey`, `1se`, `2se`, or `none`. Default is `ci`.
#' @param trans Transformation that was applied to the response variable. One of `log`, `sqrt`, `logit`, `power`, `inverse`, or `arcsin`. Default is `NULL`.
#' @param offset Numeric offset applied to response variable prior to transformation. Default is `NULL`. Use 0 if no offset was applied to the transformed data. See Details for more information.
#' @param power Numeric power applied to response variable with power transformation. Default is `NULL`. See Details for more information.
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
#' @param ... Other arguments passed through to [get_predictions()].
#'
#' @importFrom multcompView multcompLetters
#' @importFrom emmeans emmeans
#' @importFrom stats model.frame predict qtukey qt terms var ptukey
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
#' ## Power
#' The power argument allows the specification of arbitrary powers to be
#' back transformed, if they have been used to attempt to improve normality of
#' residuals.
#'
#' ## Confidence Intervals & Comparison Intervals
#'
#' The function provides several options for confidence intervals via the `int.type` argument:
#'
#' - **`ci` (default)**: Traditional confidence intervals for individual means. These estimate the precision of each individual mean but may not align with the multiple comparison results. Non-overlapping traditional confidence intervals do not necessarily indicate significant differences in multiple comparison tests.
#'
#' - **`tukey`**: Tukey comparison intervals that are consistent with the multiple comparison test. These intervals are wider than regular confidence intervals and are designed so that non-overlapping intervals correspond to statistically significant differences in the Tukey HSD test. This ensures visual consistency between the intervals and letter groupings.
#'
#' - **`1se`** and **`2se`**: Intervals of ±1 or ±2 standard errors around each mean.
#'
#' - **`none`**: No confidence intervals will be calculated or displayed in plots.
#'
#' By default, the function displays regular confidence intervals (`int.type = "ci"`),
#' which estimate the precision of individual treatment means. However, when
#' performing multiple comparisons, these regular confidence intervals may not
#' align with the letter groupings from Tukey's HSD test. Specifically, you may
#' observe non-overlapping confidence intervals for treatments that share the
#' same letter group (indicating no significant difference).
#'
#' This occurs because regular confidence intervals and Tukey's HSD test serve
#' different purposes:
#' - Regular confidence intervals estimate individual mean precision
#' - Tukey's HSD controls the family-wise error rate across all pairwise comparisons
#'
#' To resolve this visual inconsistency, you can use Tukey comparison intervals
#' (`int.type = "tukey"`). These intervals are specifically designed for multiple
#' comparisons and will be consistent with the letter groupings: non-overlapping
#' Tukey intervals indicate significant differences, while overlapping intervals
#' suggest no significant difference.
#'
#' The function will issue a message if it detects potential inconsistencies
#' between non-overlapping confidence intervals and letter groupings, suggesting
#' the use of Tukey intervals for clearer interpretation.
#' For multiple comparison contexts, Tukey comparison intervals are recommended as they provide visual consistency with the statistical test being performed and avoid the common confusion where traditional confidence intervals don't overlap but groups share the same significance letter.
#'
#' @returns An object of class `mct` (a list with class attributes) containing:
#'  \item{predictions}{A data frame with predicted means, standard errors,
#'    confidence interval upper and lower bounds, and significant group
#'    allocations}
#'  \item{pairwise_pvalues}{A symmetric matrix of p-values for all pairwise
#'    comparisons using Tukey's HSD test}
#'  \item{hsd}{The Honest Significant Difference value(s) used in the comparisons.
#'    Either a single numeric value (if constant across comparisons) or a matrix
#'    (if varies by comparison)}
#'  \item{aliased}{Character vector of aliased treatment levels (only present if
#'    some predictions are aliased)}
#'  \item{sig_level}{The significance level used (default 0.05)}
#'
#' @references Jørgensen, E. & Pedersen, A. R. (1997). How to Obtain Those Nasty
#'  Standard Errors From Transformed Data - and Why They Should Not Be Used. <https://pure.au.dk/portal/en/publications/how-to-obtain-those-nasty-standard-errors-from-transformed-data-a>
#'
#' @examples
#' # Fit aov model
#' model <- aov(Petal.Length ~ Species, data = iris)
#'
#' # Display the ANOVA table for the model
#' anova(model)
#'
#' # Determine ranking and groups according to Tukey's Test (with Tukey intervals)
#' pred.out <- multiple_comparisons(model, classify = "Species")
#'
#' # Display the predicted values table
#' pred.out
#'
#' # Access the p-value matrix
#' pred.out$pairwise_pvalues
#'
#' # Access the HSD value
#' pred.out$hsd
#'
#' # Show the predicted values plot
#' autoplot(pred.out, label_height = 0.5)
#'
#' # Use traditional confidence intervals instead of Tukey comparison intervals
#' pred.out.ci <- multiple_comparisons(model, classify = "Species", int.type = "ci")
#' pred.out.ci
#'
#' # Plot without confidence intervals
#' pred.out.none <- multiple_comparisons(model, classify = "Species", int.type = "none")
#' autoplot(pred.out.none)
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
#' wald(model.asr) # Nitrogen main effect significant
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
#' # lambda = 1/3 from MASS::boxcox()
#' model.asr <- asreml(resp^(1/3) ~ trt,
#'                     random = ~ block,
#'                     residual = ~ units,
#'                     data = ex_data)
#'
#' resplot(model.asr) # Look much better
#'
#' pred.out <- multiple_comparisons(model.obj = model.asr,
#'                                  classify = "trt",
#'                                  trans = "power", power = (1/3))
#'
#' pred.out
#' autoplot(pred.out, label_height = 0.5)
#' }
#'
#' @export
#'
multiple_comparisons <- function(model.obj,
                                 classify,
                                 sig = 0.05,
                                 int.type = "ci",
                                 trans = NULL,
                                 offset = NULL,
                                 power = NULL,
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

    vars <- validate_inputs(sig, classify, model.obj, trans)

    # Handle deprecated parameter that's being removed
    handle_deprecated_param("pred.obj", NULL, "Predictions are now performed internally in the function.")

    # Process dots
    rlang::check_dots_used()
    args = list(...)

    # Check for alias 'letters' instead of 'groups'
    if ("letters" %in% names(args)) {
        if (!missing(groups)) {
            warning("Both 'groups' and 'letters' provided. Using 'groups'.")
        } else {
            groups <- args$letters
        }
    }

    # Get model-specific predictions and SED
    result <- get_predictions(model.obj, classify, pred.obj, ...)

    pp <- result$predictions
    sed <- result$sed
    ndf <- result$df
    ylab <- result$ylab
    aliased <- result$aliased_names

    # Process treatment names
    pp <- process_treatment_names(pp, classify, vars)

    # Calculate critical values and determine pairs that are significantly different
    result <- get_diffs(pp, sed, ndf, sig)
    crit_val <- result$crit_val
    diffs <- result$diffs

    # Calculate p-value matrix for all pairwise comparisons
    pval_matrix <- calculate_pvalue_matrix(pp, sed, ndf)

    # Add letter groups if requested
    if (groups) {
        pp <- add_letter_groups(pp, diffs, descending)
    }

    # Calculate confidence intervals
    pp <- add_confidence_intervals(pp, int.type, sig, ndf)

    # Apply transformations if requested
    if (!is.null(trans)) {
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

    # Check for CI/letter group inconsistencies and warn if needed
    if (groups && tolower(int.type) == "ci") {
        check_ci_consistency(pp)
    }

    # Prepare HSD value(s) for output
    if (stats::var(as.vector(crit_val), na.rm = TRUE) < 1e-10) {
        hsd_output <- crit_val[1, 2]
    } else {
        hsd_output <- crit_val
    }

    # Create output list
    output <- list(
        predictions = pp,
        pairwise_pvalues = pval_matrix,
        hsd = hsd_output,
        sig_level = sig
    )

    # Add aliased treatments if present
    if (!is.null(aliased)) {
        output$aliased <- as.character(aliased)
    }

    # Strip transformation from ylab if trans is provided
    ylab <- strip_transformation_from_label(ylab, trans)

    # Add attributes for backward compatibility
    attr(output, "ylab") <- ylab
    attr(output, "HSD") <- hsd_output  # Keep for backward compatibility
    if (!is.null(aliased)) {
        attr(output, "aliased") <- as.character(aliased)
    }

    # Add class
    class(output) <- c("mct", "list")

    # Plot if requested
    if (plot) {
        print(autoplot(output))
    }

    return(output)
}


#' Print output of multiple_comparisons
#'
#' @param x An mct object to print to the console.
#' @param ... Other arguments passed to print.data.frame
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

    cat("Multiple Comparisons of Means: Tukey's HSD Test\n")
    cat("Significance level:", x$sig_level, "\n")
    cat("HSD value:", if(length(x$hsd) == 1) x$hsd else "varies by comparison (see $hsd)\n", "\n")
    cat("\nPredicted values:\n")
    print.data.frame(x$predictions, ...)

    if(!is.null(x$aliased)) {
        aliased <- x$aliased
        if(length(aliased) > 1) {
            cat("\nAliased levels are:", paste(aliased[1:(length(aliased)-1)], collapse = ", "), "and", aliased[length(aliased)], "\n")
        }
        else {
            cat("\nAliased level is:", aliased, "\n")
        }
    }

    invisible(x)
}


#' Calculate matrix of p-values for all pairwise comparisons
#'
#' @param pp Data frame with predicted values and Names column
#' @param sed Standard error of differences (matrix or scalar)
#' @param ndf Degrees of freedom
#'
#' @return Symmetric matrix of p-values with treatment names as row/column names
#' @noRd
calculate_pvalue_matrix <- function(pp, sed, ndf) {
    n <- nrow(pp)
    treatment_names <- pp$Names

    # Initialize p-value matrix
    pval_matrix <- matrix(1, nrow = n, ncol = n,
                         dimnames = list(treatment_names, treatment_names))

    if (n <= 1) {
        return(pval_matrix)
    }

    # Vectorized pairwise differences for the upper triangle
    pair_idx <- which(upper.tri(pval_matrix), arr.ind = TRUE)
    i <- pair_idx[, 1]
    j <- pair_idx[, 2]

    diff <- abs(pp$predicted.value[i] - pp$predicted.value[j])

    # Get the appropriate SED for each comparison.
    # `sed` may be a scalar, a base matrix, or a Matrix::* matrix.
    sed_ij <- if (!is.null(dim(sed))) {
        as.numeric(sed[cbind(i, j)])
    } else {
        rep_len(sed, length(diff))
    }

    # Calculate the studentized range statistic and p-values in one call
    q_stat <- as.numeric(diff / sed_ij) * sqrt(2)
    pvals <- stats::ptukey(q_stat, nmeans = n, df = ndf, lower.tail = FALSE)

    pval_matrix[cbind(i, j)] <- pvals
    pval_matrix[cbind(j, i)] <- pvals

    return(pval_matrix)
}


#' @noRd
add_letter_groups <- function(pp, diffs, descending) {
    ll <- multcompView::multcompLetters3("Names", "predicted.value", diffs, pp, reversed = !descending)

    rr <- data.frame(groups = ll$Letters)
    rr$Names <- row.names(rr)

    pp <- merge(pp, rr)
    return(pp)
}


#' @noRd
format_output <- function(pp, descending, vars, decimals) {
    # Order by predicted value
    pp <- pp[base::order(pp$predicted.value, decreasing = descending), ]

    # Remove Names column
    pp$Names <- NULL

    # Extract treatment variable names
    trtindex <- max(unlist(lapply(paste0("^", vars, "$"), grep, x = names(pp))))
    trtnam <- names(pp)[1:trtindex]

    # Exclude reserved column names
    reserved_cols <- c("predicted.value", "std.error", "Df", "groups",
                       "PredictedValue", "ApproxSE", "ci", "low", "up")
    trtnam <- trtnam[trtnam %!in% reserved_cols]

    # Convert treatment columns to factors with ordered levels
    for (i in seq_along(trtnam)) {
        pp[[trtnam[i]]] <- factor(pp[[trtnam[i]]], levels = unique(pp[[trtnam[i]]]))
    }

    # Helper function to calculate decimal places needed to avoid rounding to zero
    calc_se_decimals <- function(values, default_decimals) {
        min_val <- min(values, na.rm = TRUE)
        if (min_val > 0 && round(min_val, default_decimals) == 0) {
            max(default_decimals, -floor(log10(min_val)) + 1)
        } else {
            default_decimals
        }
    }

    # Identify SE columns and calculate needed decimal places
    se_cols <- intersect(c("std.error", "ApproxSE"), names(pp))
    se_decimals <- sapply(se_cols, function(col) calc_se_decimals(pp[[col]], decimals))

    # Warn if any SE columns need more decimal places
    if (any(se_decimals > decimals)) {
        warning("Some standard errors are very small and would round to zero with ",
                decimals, " decimal places. Using ", max(se_decimals),
                " decimal places for standard error columns to preserve error bar display.",
                call. = FALSE)
    }

    # Round all numeric columns
    numeric_cols <- names(pp)[sapply(pp, is.numeric)]
    for (col in numeric_cols) {
        pp[[col]] <- round(pp[[col]],
                           if (col %in% names(se_decimals)) se_decimals[[col]] else decimals)
    }

    # Remove row names
    rownames(pp) <- NULL
    return(pp)
}


#' @noRd
strip_transformation_from_label <- function(ylab, trans) {
    if (is.null(trans)) {
        return(ylab)
    }

    # Convert to character
    ylab_char <- if (is.language(ylab)) deparse(ylab) else as.character(ylab)

    # Simple pattern matching for common transformations
    stripped <- switch(trans,
                       "log" = sub("^log(10)?\\((.+)\\)$", "\\2", ylab_char),
                       "sqrt" = sub("^sqrt\\((.+)\\)$", "\\1", ylab_char),
                       "logit" = sub("^logit\\((.+)\\)$", "\\1", ylab_char),
                       "arcsin" = sub("^a?r?c?sin\\(sqrt\\((.+)\\)\\)$", "\\1", ylab_char),
                       "inverse" = sub("^\\(?1/([^)]+)\\)?$", "\\1", ylab_char),
                       "power" = sub("^\\(?(.+?)\\)?\\^.+$", "\\1", ylab_char),
                       ylab_char  # default: return as-is
    )

    return(stripped)
}


#' @noRd
check_ci_consistency <- function(pp) {

    result <- FALSE
    # Pre-extract and validate data once
    n <- nrow(pp)
    low <- pp$predicted.value - pp$ci
    up <- pp$predicted.value + pp$ci
    groups <- as.character(pp$groups)

    # Pre-process group letters efficiently
    group_letters <- vector("list", length(groups))
    for (i in seq_along(groups)) {
        group_letters[[i]] <- unique(strsplit(groups[i], "")[[1]])
    }

    # Vectorized overlap detection using outer operations
    # Two intervals [a,b] and [c,d] overlap if max(a,c) <= min(b,d)
    low_matrix <- matrix(low, nrow = n, ncol = n, byrow = TRUE)
    up_matrix <- matrix(up, nrow = n, ncol = n, byrow = FALSE)

    # Only compute upper triangle to avoid redundant comparisons
    upper_tri <- upper.tri(matrix(TRUE, n, n))

    # Vectorized overlap check
    max_lows <- pmax(low_matrix, t(low_matrix))
    min_ups <- pmin(up_matrix, t(up_matrix))
    overlaps <- max_lows <= min_ups

    # Find non-overlapping pairs in upper triangle only
    non_overlapping <- upper_tri & !overlaps

    if (!any(non_overlapping)) return(FALSE)

    # Get indices efficiently
    indices <- which(non_overlapping, arr.ind = TRUE)

    # Check for shared letters only among non-overlapping pairs
    for (k in seq_len(nrow(indices))) {
        i <- indices[k, 1]
        j <- indices[k, 2]

        if (groups[i] == groups[j] | length(intersect(group_letters[[i]], group_letters[[j]])) > 0) {
            message("Note: Some treatments sharing the same letter group have non-overlapping confidence intervals.\n",
                    "This is expected behavior as regular confidence intervals estimate individual mean precision,\n",
                    "while Tukey's HSD controls family-wise error rates. For visual consistency with letter groups,\n",
                    "consider using 'int.type = \"tukey\"' to display Tukey comparison intervals.")
            return(TRUE)
        }
    }

    invisible(result)
}


#' @importFrom stats formula
#' @keywords internal
#' @noRd
validate_inputs <- function(sig, classify, model.obj, trans) {
    # Check significance level
    if (sig >= 0.5) {
        if(sig >= 1 & sig < 50) {
            stop("Significance level given by `sig` is high. Perhaps you meant ", sig/100, "?", call. = FALSE)
        }
        else if(sig >= 1 & sig >= 50) {
            stop("Significance level given by `sig` is high. Perhaps you meant ", 1-(sig/100), "?", call. = FALSE)
        }
        else {
            warning("Significance level given by `sig` is high. Perhaps you meant ", 1-sig, "?", call. = FALSE)
        }
    }

    # Get the individual names provided in classify
    vars <- unlist(strsplit(classify, "\\:"))
    reserved_col_names <- c("predicted.value", "std.error", "Df",
                            "groups", "PredictedValue", "ApproxSE", "ci", "low", "up")
    if (any(vars %in% reserved_col_names)) {
        stop("Invalid column name. Please change the name of column(s): ",
             vars[vars %in% reserved_col_names], call. = FALSE)
    }

    # Check if the response variable is transformed in the model formula
    model_formula <- stats::formula(model.obj)
    if(inherits(model.obj, "asreml")) {
        response_part <- model_formula[[1]][[2]]
    }
    else {
        response_part <- model_formula[[2]]
    }
    if (is.call(response_part) & is.null(trans)) {
        warning(call. = FALSE,
            sprintf(
                "The response variable appears to be transformed in the model formula: %s.",
                deparse(response_part)
            ),
            "\nPlease specify the 'trans' argument if you want back-transformed predictions."
        )
    }

    return(vars)
}


#' @noRd
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


#' @noRd
get_diffs <- function(pp, sed, ndf, sig) {
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


#' @noRd
add_confidence_intervals <- function(pp, int.type, sig, ndf) {
    # Calculate confidence interval width
    pp$ci <- switch(
        tolower(int.type),
        "ci" = stats::qt(p = sig/2, ndf, lower.tail = FALSE) * pp$std.error,
        "tukey" = stats::qtukey(p = 1 - sig, nmeans = nrow(pp), df = ndf) / sqrt(2) * pp$std.error,
        "1se" = pp$std.error,
        "2se" = 2 * pp$std.error,
        "none" = 0,
        stop("Invalid int.type. Use 'ci', 'tukey', '1se', '2se', or 'none'.")
    )

    return(pp)
}


#' @noRd
apply_transformation <- function(pp, trans, offset, power) {
    # Set default offset if not provided
    if (is.null(offset)) {
        warning("Offset value assumed to be 0. Change with `offset` argument.", call. = FALSE)
        offset <- 0
    }

    # Apply appropriate transformation
    if (trans == "sqrt") {
        # From paper: g(x) = sqrt(x), g^-1(y) = y^2, g'(x) = 1/(2*sqrt(x))
        # Back-transformed value: X~ = Y^2 - offset
        # Approx SE: ~X = 2*sqrt(X~ + offset)*Y_se
        pp$PredictedValue <- (pp$predicted.value)^2 - offset

        # Bounds check: predicted values should be non-negative after transformation
        if (any(pp$PredictedValue < 0, na.rm = TRUE)) {
            warning("Square root back-transformation produced negative values. Check offset parameter.", call. = FALSE)
        }

        # SE uses value on original scale (before offset removal) per paper formula
        pp$ApproxSE <- 2 * abs(pp$std.error) * abs(pp$predicted.value)

        pp$low <- (pp$predicted.value - pp$ci)^2 - offset
        pp$up <- (pp$predicted.value + pp$ci)^2 - offset

    } else if (trans == "log") {
        # From paper: g(x) = ln(x), g^-1(y) = exp(y), g'(x) = 1/x
        # Back-transformed value: X~ = exp(Y) - offset
        # Approx SE: ~X = X~*Y_se (where X~ is before offset removal)
        x_tilde <- exp(pp$predicted.value)
        pp$PredictedValue <- x_tilde - offset

        # Bounds check: values should be positive after offset removal
        if (any(pp$PredictedValue <= 0, na.rm = TRUE)) {
            warning("Log back-transformation produced non-positive values. Check offset parameter.", call. = FALSE)
        }

        # SE uses back-transformed value before offset removal
        pp$ApproxSE <- abs(pp$std.error) * x_tilde

        pp$low <- exp(pp$predicted.value - pp$ci) - offset
        pp$up <- exp(pp$predicted.value + pp$ci) - offset

    } else if (trans == "logit") {
        # From paper: g(x) = ln(x/(1-x)), g^-1(y) = exp(y)/(1+exp(y)), g'(x) = 1/(x(1-x))
        # Back-transformed value: X~ = exp(Y)/(1+exp(Y))
        # Approx SE: ~X = X~*(1-X~)*Y_se
        pp$PredictedValue <- exp(pp$predicted.value) / (1 + exp(pp$predicted.value))

        # Bounds check: values should be in (0, 1)
        if (any(pp$PredictedValue <= 0 | pp$PredictedValue >= 1, na.rm = TRUE)) {
            warning("Logit back-transformation produced values outside (0,1). This may indicate numerical issues.", call. = FALSE)
        }

        pp$ApproxSE <- pp$PredictedValue * (1 - pp$PredictedValue) * abs(pp$std.error)

        ll <- pp$predicted.value - pp$ci
        pp$low <- exp(ll) / (1 + exp(ll))
        uu <- pp$predicted.value + pp$ci
        pp$up <- exp(uu) / (1 + exp(uu))

    } else if (trans == "power") {
        # From paper: g(x) = x^a, g^-1(y) = y^(1/a), g'(x) = a*x^(a-1)
        # Back-transformed value: X~ = Y^(1/a) - offset
        # Approx SE: ~X = Y_se / |a*X~^(a-1)| (where X~ is before offset removal)
        if (is.null(power) || !is.numeric(power) || power == 0) {
            stop("Power transformation requires a non-zero numeric 'power' argument.", call. = FALSE)
        }

        x_tilde <- (pp$predicted.value)^(1/power)
        pp$PredictedValue <- x_tilde - offset

        # Bounds check depends on power sign and offset
        if (power < 0 && any(abs(pp$PredictedValue) < .Machine$double.eps, na.rm = TRUE)) {
            warning("Power back-transformation with negative power produced values near zero.", call. = FALSE)
        }

        # SE uses back-transformed value before offset removal (x_tilde not PredictedValue)
        pp$ApproxSE <- abs(pp$std.error) / abs(power * x_tilde^(power - 1))

        pp$low <- (pp$predicted.value - pp$ci)^(1/power) - offset
        pp$up <- (pp$predicted.value + pp$ci)^(1/power) - offset

    } else if (trans == "inverse") {
        # From paper: g(x) = 1/x, g^-1(y) = 1/y, g'(x) = -1/x^2
        # Back-transformed value: X~ = 1/Y
        # Approx SE: ~X = X~^2*Y_se (using absolute value of g'(x))

        # Bounds check: avoid division by zero
        if (any(abs(pp$predicted.value) < .Machine$double.eps, na.rm = TRUE)) {
            warning("Inverse transformation: predicted values very close to zero detected.", call. = FALSE)
        }

        pp$PredictedValue <- 1 / pp$predicted.value
        pp$ApproxSE <- abs(pp$std.error) * pp$PredictedValue^2

        # Check for sign changes across confidence interval
        lower_bound <- pp$predicted.value - pp$ci
        upper_bound <- pp$predicted.value + pp$ci

        if (any(sign(lower_bound) != sign(upper_bound), na.rm = TRUE)) {
            warning("Inverse transformation: confidence interval crosses zero. Results may be unreliable.", call. = FALSE)
        }

        # Inverse function is monotone decreasing, so bounds swap
        # Use pmin/pmax to ensure pp$low < pp$up regardless of sign
        inv_lower <- 1 / (pp$predicted.value + pp$ci)
        inv_upper <- 1 / (pp$predicted.value - pp$ci)

        pp$low <- pmin(inv_lower, inv_upper)
        pp$up <- pmax(inv_lower, inv_upper)

    } else if (trans == "arcsin") {
        # From paper: g(x) = sin^-1(sqrt(x)), g^-1(y) = sin(y)^2,
        # g'(x) = 1/(2*sqrt(x)*cos(sin^-1(sqrt(x))))
        # Back-transformed value: X~ = sin(Y)^2
        # Approx SE: ~X = 2*sqrt(X~)*cos(sin^-1(sqrt(X~)))*Y_se

        # Bounds check: predicted.value should be in valid arcsin range
        if (any(pp$predicted.value < -pi/2 | pp$predicted.value > pi/2, na.rm = TRUE)) {
            warning("Arcsin transformation: some predicted values outside [-pi/2, pi/2].", call. = FALSE)
        }

        pp$PredictedValue <- sin(pp$predicted.value)^2

        # Bounds check: values should be in [0, 1]
        if (any(pp$PredictedValue < 0 | pp$PredictedValue > 1, na.rm = TRUE)) {
            warning("Arcsin back-transformation produced values outside [0,1]. This may indicate numerical issues.", call. = FALSE)
        }

        # Simplify: cos(asin(sqrt(x))) = sqrt(1 - x) for x in [0,1]
        pp$ApproxSE <- 2 * abs(pp$std.error) * sqrt(pp$PredictedValue * (1 - pp$PredictedValue))

        pp$low <- sin(pp$predicted.value - pp$ci)^2
        pp$up <- sin(pp$predicted.value + pp$ci)^2

    } else {
        stop("Invalid trans value. Must be one of: 'sqrt', 'log', 'logit', 'power', 'inverse', 'arcsin'.", call. = FALSE)
    }

    return(pp)
}


#' @noRd
add_attributes <- function(pp, ylab, crit_val, aliased_names, trans = NULL) {
    # Strip transformation from ylab if trans is provided
    ylab <- strip_transformation_from_label(ylab, trans)

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
