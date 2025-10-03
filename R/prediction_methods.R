#' Check if classify term exists in model terms
#'
#' Checks if the classify term exists in the model terms, handling interaction
#' terms specified in any order (e.g., B:A when model has A:B, or A:C:B when 
#' model has A:B:C).
#'
#' @param classify Name of predictor variable as a string
#' @param model_terms Character vector of model term labels
#'
#' @return The classify term as it appears in the model (potentially reordered),
#' or throws an error if not found
#' @keywords internal
check_classify_in_terms <- function(classify, model_terms) {
    # First check if classify is directly in model terms
    if(classify %in% model_terms) {
        return(classify)
    }
    
    # If classify contains ":", it might be an interaction in a different order
    if(grepl(":", classify)) {
        # Split the classify term into parts
        classify_parts <- unlist(strsplit(classify, ":"))
        
        # For each model term, check if it's the same interaction in a different order
        for(term in model_terms) {
            if(grepl(":", term)) {
                term_parts <- unlist(strsplit(term, ":"))
                
                # Check if they have the same components (regardless of order)
                # and the same number of components
                if(length(classify_parts) == length(term_parts)) {
                    # Sort both sets of parts and compare
                    if(all(sort(classify_parts) == sort(term_parts))) {
                        # Found a match! Return the model's version
                        return(term)
                    }
                }
            }
        }
    }
    
    # If we get here, classify is not in the model in any order
    stop(classify, " is not a term in the model. Please check model specification.", call. = FALSE)
}

#' Get Predictions for Statistical Models
#'
#' A generic function to get predictions for statistical models.
#'
#' @param model.obj A model object. Currently supported model objects are asreml, aov/lm, lmerMod/lmerModLmerTest.
#' @param classify Name of predictor variable as a string.
#' @param pred.obj Optional precomputed prediction object.
#' @param ... Additional arguments passed to specific methods.
#'
#' @name predictions
#'
#' @return A list containing predictions, standard errors, degrees of freedom,
#' response variable label, and aliased names.
#' @keywords internal
get_predictions <- function(model.obj, classify, pred.obj = NULL, ...) {
    UseMethod("get_predictions")
}

#' @rdname predictions
#'
#' @keywords internal
get_predictions.default <- function(model.obj, ...) {
    supported_types <- c("aov", "lm", "lmerMod", "lmerModLmerTest",
                         "asreml")
    stop("model.obj must be a linear (mixed) model object. Currently supported model types are: ",
         paste(supported_types, collapse = ", "), call. = FALSE)
}

#' @rdname predictions
#'
#' @keywords internal
get_predictions.asreml <- function(model.obj, classify, pred.obj = NULL, ...) {

    args <- list(...)
    # asr_args <- args[names(args) %in% names(formals(asreml::predict.asreml))]
    # Check if classify is in model terms (handles reversed interaction order)
    model_terms <- c(attr(stats::terms(model.obj$formulae$fixed), 'term.labels'),
                     attr(stats::terms(model.obj$formulae$random), 'term.labels'))
    classify <- check_classify_in_terms(classify, model_terms)

    # Generate predictions if not provided
    if(missing(pred.obj) || is.null(pred.obj)) {
        pred.obj <- quiet(asreml::predict.asreml(object = model.obj, classify = classify, sed = TRUE, trace = FALSE, ...))
    }

    # Check if all predicted values are NA
    if(all(is.na(pred.obj$pvals$predicted.value)) & all(is.na(pred.obj$pvals$std.error))) {
        stop("All predicted values are aliased. Perhaps you need the `present` argument?", call. = FALSE)
    }

    # For use with asreml 4+
    pp <- pred.obj$pvals
    sed <- pred.obj$sed

    # Process aliased treatments with asreml-specific exclude columns
    aliased_result <- process_aliased(pp, sed, classify,
                                    exclude_cols = c("predicted.value", "std.error", "status"))
    pp <- aliased_result$predictions
    sed <- aliased_result$sed
    aliased_names <- aliased_result$aliased_names

    # Remove status column if present
    pp$status <- NULL

    if (!"dendf" %in% names(args)) {
        dat.ww <- quiet(asreml::wald(model.obj, ssType = "conditional", denDF = "default", trace = FALSE)$Wald)
        dendf <- data.frame(Source = row.names(dat.ww), denDF = dat.ww$denDF)
    }
    else {
        dendf <- args$dendf
    }

    vars <- unlist(strsplit(classify, "\\:"))
    ndf <- dendf$denDF[grepl(classify, dendf$Source) & nchar(classify) == nchar(as.character(dendf$Source))]
    if(rlang::is_empty(ndf)) {
        ndf <- model.obj$nedf
        rand_terms <- vars[vars %in% attr(stats::terms(model.obj$formulae$random), 'term.labels')]
        warning(rand_terms, " is not a fixed term in the model. The denominator degrees of freedom are estimated using the residual degrees of freedom. This may be inaccurate.", call. = FALSE)
    }

    # Get response variable for plot label
    ylab <- model.obj$formulae$fixed[[2]]
    # ylab <- trimws(gsub("\\(|\\)", "", ylab))

    return(list(
        predictions = pp,
        sed = sed,
        df = ndf,
        ylab = ylab,
        aliased_names = aliased_names
    ))
}

#' @rdname predictions
#'
#' @importFrom emmeans emmeans
#'
#' @keywords internal
get_predictions.lm <- function(model.obj, classify, ...) {
    # Check if classify is in model terms (handles reversed interaction order)
    model_terms <- attr(stats::terms(model.obj), 'term.labels')
    classify <- check_classify_in_terms(classify, model_terms)

    # Set emmeans options
    on.exit(options(emmeans = emmeans::emm_defaults))
    emmeans::emm_options("msg.interaction" = FALSE, "msg.nesting" = FALSE)

    # Generate predictions
    pred.out <- emmeans::emmeans(model.obj, as.formula(paste("~", classify)))

    # Extract standard errors and predictions
    sed <- pred.out@misc$sigma * sqrt(outer(1 / pred.out@grid$.wgt., 1 / pred.out@grid$.wgt., "+"))
    pred.out <- as.data.frame(pred.out)
    pred.out <- pred.out[, !grepl("CL", names(pred.out))]

    # Rename columns for consistency
    pp <- pred.out
    names(pp)[names(pp) == "emmean"] <- "predicted.value"
    names(pp)[names(pp) == "SE"] <- "std.error"

    # Set diagonals to NA
    diag(sed) <- NA

    # Process aliased treatments
    aliased_result <- process_aliased(pp, sed, classify)
    pp <- aliased_result$predictions
    sed <- aliased_result$sed
    aliased_names <- aliased_result$aliased_names

    # Get degrees of freedom
    ndf <- pp$df[1]

    # Get response variable for plot label
    formula_text <- deparse(stats::formula(model.obj))
    ylab <- strsplit(formula_text, "~")[[1]][1]
    ylab <- trimws(ylab)

    return(list(
        predictions = pp,
        sed = sed,
        df = ndf,
        ylab = ylab,
        aliased_names = aliased_names
    ))
}

#' @rdname predictions
#'
#' @keywords internal
get_predictions.lmerMod <- function(model.obj, classify, ...) {
    # Reuse lm method for common functionality
    result <- get_predictions.lm(model.obj, classify, ...)

    # Override ylab extraction for lmerMod
    # result$ylab <- model.obj@call[[2]][[2]]

    return(result)
}

#' @rdname predictions
#' @keywords internal
get_predictions.lmerModLmerTest <- function(model.obj, classify, ...) {
    get_predictions.lmerMod(model.obj, classify, ...)
}

#' Process aliased treatments in predictions
#'
#' @param pp Data frame of predictions
#' @param sed Standard error of differences matrix
#' @param classify Name of predictor variable
#' @param exclude_cols Column names to exclude when processing aliased names
#'
#' @return List containing processed predictions, sed matrix and aliased names
#' @keywords internal
process_aliased <- function(pp, sed, classify, exclude_cols = c("predicted.value", "std.error", "df", "Names")) {
    aliased_names <- NULL

    if(anyNA(pp$predicted.value)) {
        aliased <- which(is.na(pp$predicted.value))
        # Get aliased treatment levels
        aliased_names <- pp[aliased, !names(pp) %in% exclude_cols]

        # Convert to character vector
        if(is.data.frame(aliased_names)) {
            aliased_names <- apply(aliased_names, 1, paste, collapse = ":")
        }

        # Create warning message
        if(length(aliased_names) > 1) {
            warn_string <- paste0("Some levels of ", classify, " are aliased. They have been removed from predicted output.\n",
                  "  Aliased levels are: ", paste(aliased_names, collapse = ", "),
                  ".\n  These levels are saved in the output object.")
        } else {
            warn_string <- paste0("A level of ", classify, " is aliased. It has been removed from predicted output.\n",
                  "  Aliased level is: ", aliased_names,
                  ".\n  This level is saved as an attribute of the output object.")
        }

        # Remove aliased values
        pp <- pp[!is.na(pp$predicted.value), ]
        pp <- droplevels(pp)
        sed <- sed[-aliased, -aliased]
        warning(warn_string, call. = FALSE)
    }

    return(list(
        predictions = pp,
        sed = sed,
        aliased_names = aliased_names
    ))
}
