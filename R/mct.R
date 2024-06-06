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
#' @details Some transformations require that data has a small offset applied, otherwise it will cause errors (for example taking a log of 0, or square root of negative values). In order to correctly reverse this offset, if the `trans` argument is supplied, an offset value must also be supplied. If there was no offset required for a transformation, then use a value of 0 for the `offset` argument.
#'
#' @return A list containing a data frame with predicted means, standard errors, confidence interval upper and lower bounds, and significant group allocations (named `predicted_values`), as well as a plot visually displaying the predicted values (named `predicted_plot`). If some of the predicted values are aliased, a warning is printed, and the aliased treatment levels are returned in the output (named `aliased`).
#'
#' @references Jørgensen, E. & Pedersen, A. R. (1997). How to Obtain Those Nasty Standard Errors From Transformed Data - and Why They Should Not Be Used. [<https://pure.au.dk/portal/en/publications/how-to-obtain-those-nasty-standard-errors-from-transformed-data--and-why-they-should-not-be-used(d649ca20-d15f-11db-8e26-000ea68e967b).html>](https://pure.au.dk/portal/en/publications/how-to-obtain-those-nasty-standard-errors-from-transformed-data--and-why-they-should-not-be-used(d649ca20-d15f-11db-8e26-000ea68e967b).html)
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
                                 plot = FALSE,
                                 label_height = 0.1,
                                 rotation = 0,
                                 save = FALSE,
                                 savename = "predicted_values",
                                 order,
                                 pred.obj,
                                 pred,
                                 ...) {

    rlang::check_dots_used()

    if(!missing(pred)) {
        warning("Argument `pred` has been deprecated and will be removed in a future version. Please use `classify` instead.")
        classify <- pred
    }

    if(!missing(order)) {
        warning("Argument `order` has been deprecated and will be removed in a future version. Please use `descending` instead.")
    }

    if(sig > 0.5)  {
        warning("Significance level given by `sig` is high. Perhaps you meant ", 1-sig, "?", call. = FALSE)
    }

    # Get the individual names provided in classify
    vars <- unlist(strsplit(classify, "\\:"))
    reserved_col_names <- c("predicted.value", "std.error", "Df",
                            "groups", "PredictedValue", "ApproxSE", "ci", "low", "up")
    if(any(vars %in% reserved_col_names)) {
        stop("Invalid column name. Please change the name of column(s): ", vars[vars %in% reserved_col_names])
    }

    if(inherits(model.obj, "asreml")){

        if(classify %!in% c(attr(stats::terms(model.obj$formulae$fixed), 'term.labels'),
                            attr(stats::terms(model.obj$formulae$random), 'term.labels'))) {
            stop(classify, " is not a term in the model. Please check model specification.", call. = FALSE)
        }

        if(!missing(pred.obj)) {
            warning("Argument `pred.obj` has been deprecated and will be removed in a future version. Predictions are now performed internally in the function.")
        }

        pred.obj <- quiet(asreml::predict.asreml(model.obj, classify = classify, sed = TRUE, trace = FALSE, ...))
        # Check if all the predicted values are NA. If so, suggests the need of the `present` argument
        if(all(is.na(pred.obj$pvals$predicted.value)) & all(is.na(pred.obj$pvals$std.error))) {
            stop("All predicted values are aliased. Perhaps you need the `present` argument?")
        }

        # Check if any treatments are aliased, and remove them and print a warning
        if(anyNA(pred.obj$pvals$predicted.value)) {
            aliased <- which(is.na(pred.obj$pvals$predicted.value))
            # Get the level values of the aliased treatments
            # If only one treatment (classify does not contain :) all levels printed separated with ,
            # If multiple treatments, first need to concatenate columns, then collapse rows
            aliased_names <- pred.obj$pvals[aliased, !names(pred.obj$pvals) %in% c("predicted.value", "std.error", "status")]

            # This pastes rows of the dataframe together across the columns, and turns into a vector
            if(is.data.frame(aliased_names)) {
                aliased_names <- apply(aliased_names, 1, paste, collapse = ":")
            }

            if(length(aliased_names) > 1) {
                warn_string <- paste0("Some levels of ", classify, " are aliased. They have been removed from predicted output.\n  Aliased levels are: ", paste(aliased_names, collapse = ", "), ".\n  These levels are saved in the output object.")
            }
            else {
                warn_string <- paste0("A level of ", classify, " is aliased. It has been removed from predicted output.\n  Aliased level is: ", aliased_names, ".\n  This level is saved as an attribute of the output object.")
            }

            pred.obj$pvals <- pred.obj$pvals[!is.na(pred.obj$pvals$predicted.value),]
            pred.obj$pvals <- droplevels(pred.obj$pvals)
            pred.obj$sed <- pred.obj$sed[-aliased, -aliased]
            warning(warn_string, call. = FALSE)
        }

        #For use with asreml 4+
        if(utils::packageVersion("asreml") > "4") {
            pp <- pred.obj$pvals
            sed <- pred.obj$sed
        }

        pp <- pp[!is.na(pp$predicted.value),]
        pp$status <- NULL

        dat.ww <- quiet(asreml::wald(model.obj, ssType = "conditional", denDF = "default", trace = FALSE)$Wald)

        dendf <- data.frame(Source = row.names(dat.ww), denDF = dat.ww$denDF)

        ifelse(grepl(":", classify),
               pp$Names <- apply(pp[,vars], 1, paste, collapse = "_"),
               pp$Names <- pp[[classify]])

        ndf <- dendf$denDF[grepl(classify, dendf$Source) & nchar(classify) == nchar(as.character(dendf$Source))]
        if(rlang::is_empty(ndf)) {
            ndf <- model.obj$nedf
            rand_terms <- vars[vars %in% attr(stats::terms(model.obj$formulae$random), 'term.labels')]
            warning(rand_terms, " is not a fixed term in the model. The denominator degrees of freedom are estimated using the residual degrees of freedom. This may be inaccurate.", call. = FALSE)
        }
        crit.val <- 1/sqrt(2)*stats::qtukey((1-sig), nrow(pp), ndf)*sed

        # Grab the response from the formula to create plot Y label
        ylab <- model.obj$formulae$fixed[[2]]
    }

    else if(inherits(model.obj, c("aov", "lm", "lmerMod", "lmerModLmerTest"))) {
        if(classify %!in% attr(stats::terms(model.obj), 'term.labels')) {
            stop(classify, " is not a term in the model. Please check model specification.", call. = FALSE)
        }

        on.exit(options(emmeans = emmeans::emm_defaults))
        emmeans::emm_options("msg.interaction" = FALSE, "msg.nesting" = FALSE)
        pred.out <- emmeans::emmeans(model.obj, as.formula(paste("~", classify)))

        sed <- pred.out@misc$sigma*sqrt(outer(1/pred.out@grid$.wgt., 1/pred.out@grid$.wgt., "+"))
        pred.out <- as.data.frame(pred.out)
        pred.out <- pred.out[,!grepl("CL", names(pred.out))]
        pp <- pred.out
        names(pp)[names(pp) == "emmean"] <- "predicted.value"
        names(pp)[names(pp) == "SE"] <- "std.error"


        # SED <- matrix(data = sed, nrow = nrow(pp), ncol = nrow(pp))
        diag(sed) <- NA
        ifelse(grepl(":", classify),
               pp$Names <- apply(pp[,vars], 1, paste, collapse = "_"),
               pp$Names <- pp[[classify]])

        if(anyNA(pp$predicted.value)) {
            aliased <- which(is.na(pp$predicted.value))
            # Get the level values of the aliased treatments
            # If only one treatment (classify does not contain :) all levels printed separated with ,
            # If multiple treatments, first need to concatenate columns, then collapse rows
            aliased_names <- pp[aliased, !names(pp) %in% c("predicted.value", "std.error", "df", "Names")]

            # This pastes rows of the dataframe together across the columns, and turns into a vector
            if(is.data.frame(aliased_names)) {
                aliased_names <- apply(aliased_names, 1, paste, collapse = ":")
            }

            if(length(aliased_names) > 1) {
                warn_string <- paste0("Some levels of ", classify, " are aliased. They have been removed from predicted output.\n  Aliased levels are: ", paste(aliased_names, collapse = ", "), ".\n  These levels are saved in the output object.")
            }
            else {
                warn_string <- paste0("A level of ", classify, " is aliased. It has been removed from predicted output.\n  Aliased level is: ", aliased_names, ".\n  This level is saved as an attribute of the output object.")
            }

            pp <- pp[!is.na(pp$predicted.value),]
            pp <- droplevels(pp)
            sed <- sed[-aliased, -aliased]
            warning(warn_string, call. = FALSE)
        }

        ndf <- pp$df[1]
        crit.val <- 1/sqrt(2)*stats::qtukey((1-sig), nrow(pp), ndf)*sed

        # Grab the response from the formula to create plot Y label
        if(inherits(model.obj, c("lmerMod", "lmerModLmerTest"))) {
            ylab <- model.obj@call[[2]][[2]]
        }
        else {
            ylab <- model.obj$terms[[2]]
        }
    }

    else {
        stop("Models of type ", class(model.obj), " are not supported.")
    }

    # Check that the predicted levels don't contain a dash -, if they do replace and display warning
    if(any(any(grepl("-", pp$Names) | grepl("-", pp[,1])))) {
        levs <- unique(c(grep("-", pp[,1], value = TRUE), grep("-", pp$Names, value = TRUE)))
        if(length(levs)>1) {
            warning("The treatment levels ", paste(levs, collapse = ", "), " contained '-', which has been replaced in the final output with '_'")
        }
        else {
            warning("The treatment level ", levs, " contained '-', which has been replaced in the final output with '_'")
        }
        pp[,1] <- gsub(pattern = "-", replacement = "_", pp[,1])
        pp$Names <- gsub(pattern = "-", replacement = "_", pp$Names)
    }

    Names <-  as.character(pp$Names)

    # Determine pairs that are significantly different
    diffs <- abs(outer(pp$predicted.value, pp$predicted.value, "-")) > crit.val
    # (diffs*sqrt(2))/sed # check against ptukey (use top/bottom triangle)
    diffs <- diffs[lower.tri(diffs)]

    # Create a vector of treatment comparison names
    m <- outer(pp$Names, pp$Names, paste, sep="-")
    m <- m[lower.tri(m)]

    names(diffs) <- m

    ll <- multcompView::multcompLetters3("Names", "predicted.value", diffs, pp, reversed = !descending)

    rr <- data.frame(groups = ll$Letters)
    rr$Names <- row.names(rr)

    pp.tab <- merge(pp,rr)

    if(!is.na(trans)){
        if(is.na(offset)) {
            warning("Offset value assumed to be 0. Change with `offset` argument.")
            offset <- 0
        }

        if(trans == "sqrt"){
            pp.tab$PredictedValue <- (pp.tab$predicted.value)^2 - ifelse(!is.na(offset), offset, 0)
            pp.tab$ApproxSE <- 2*abs(pp.tab$std.error)*sqrt(pp.tab$PredictedValue)
            if(int.type == "ci"){
                pp.tab$ci <- stats::qt(p = sig, ndf, lower.tail = FALSE) * pp.tab$std.error
            }
            if(int.type == "1se"){
                pp.tab$ci <- pp.tab$std.error
            }
            if(int.type == "2se"){
                pp.tab$ci <- 2*pp.tab$std.error
            }
            pp.tab$low <- (pp.tab$predicted.value - pp.tab$ci)^2 - ifelse(!is.na(offset), offset, 0)
            pp.tab$up <- (pp.tab$predicted.value + pp.tab$ci)^2 - ifelse(!is.na(offset), offset, 0)
        }

        if(trans == "log"){
            pp.tab$PredictedValue <- exp(pp.tab$predicted.value) - ifelse(!is.na(offset), offset, 0)
            pp.tab$ApproxSE <- abs(pp.tab$std.error)*pp.tab$PredictedValue
            if(int.type == "ci"){
                pp.tab$ci <- stats::qt(p = sig, ndf, lower.tail = FALSE) * pp.tab$std.error
            }
            if(int.type == "1se"){
                pp.tab$ci <- pp.tab$std.error
            }
            if(int.type == "2se"){
                pp.tab$ci <- 2*pp.tab$std.error
            }
            pp.tab$low <- exp(pp.tab$predicted.value - pp.tab$ci) - ifelse(!is.na(offset), offset, 0)
            pp.tab$up <- exp(pp.tab$predicted.value + pp.tab$ci) - ifelse(!is.na(offset), offset, 0)
        }

        if(trans == "logit"){
            pp.tab$PredictedValue <- exp(pp.tab$predicted.value)/(1 + exp(pp.tab$predicted.value))
            pp.tab$ApproxSE <- pp.tab$PredictedValue * (1 - pp.tab$PredictedValue)* abs(pp.tab$std.error)
            if(int.type == "ci"){
                pp.tab$ci <- stats::qt(p = sig, ndf, lower.tail = FALSE) * pp.tab$std.error
            }
            if(int.type == "1se"){
                pp.tab$ci <- pp.tab$std.error
            }
            if(int.type == "2se"){
                pp.tab$ci <- 2*pp.tab$std.error
            }
            pp.tab$ll <- pp.tab$predicted.value - pp.tab$ci
            pp.tab$low <- exp(pp.tab$ll)/(1 + exp(pp.tab$ll))
            pp.tab$uu <- pp.tab$predicted.value + pp.tab$ci
            pp.tab$up <- exp(pp.tab$uu)/(1 + exp(pp.tab$uu))

            pp.tab$ll <- NULL
            pp.tab$uu <- NULL
        }

        if(trans == "power"){
            pp.tab$PredictedValue <- (pp.tab$predicted.value)^(1/power) - ifelse(!is.na(offset), offset, 0)
            pp.tab$ApproxSE <- pp.tab$std.error*(1/(power*pp.tab$PredictedValue^(power-1)))
            if(int.type == "ci"){
                pp.tab$ci <- stats::qt(p = sig, ndf, lower.tail = FALSE) * pp.tab$std.error
            }
            if(int.type == "1se"){
                pp.tab$ci <- pp.tab$std.error
            }
            if(int.type == "2se"){
                pp.tab$ci <- 2*pp.tab$std.error
            }
            pp.tab$low <- (pp.tab$predicted.value - pp.tab$ci)^(1/power) - ifelse(!is.na(offset), offset, 0)
            pp.tab$up <- (pp.tab$predicted.value + pp.tab$ci)^(1/power) - ifelse(!is.na(offset), offset, 0)
        }

        if(trans == "inverse"){
            pp.tab$PredictedValue <- 1/pp.tab$predicted.value
            pp.tab$ApproxSE <- abs(pp.tab$std.error)*pp.tab$PredictedValue^2
            if(int.type == "ci"){
                pp.tab$ci <- stats::qt(p = sig, ndf, lower.tail = FALSE) * pp.tab$std.error
            }
            if(int.type == "1se"){
                pp.tab$ci <- pp.tab$std.error
            }
            if(int.type == "2se"){
                pp.tab$ci <- 2*pp.tab$std.error
            }
            pp.tab$low <- 1/(pp.tab$predicted.value - pp.tab$ci)
            pp.tab$up <- 1/(pp.tab$predicted.value + pp.tab$ci)
        }
    }

    else {

        if(int.type == "ci"){
            pp.tab$ci <- stats::qt(p = sig, ndf, lower.tail = FALSE) * pp.tab$std.error
        }
        if(int.type == "1se"){
            pp.tab$ci <- pp.tab$std.error
        }
        if(int.type == "2se"){
            pp.tab$ci <- 2*pp.tab$std.error
        }
        pp.tab$low <- pp.tab$predicted.value - pp.tab$ci
        pp.tab$up <- pp.tab$predicted.value + pp.tab$ci

    }

    pp.tab <- pp.tab[base::order(pp.tab$predicted.value, decreasing = descending),]

    pp.tab$Names <- NULL
    trtindex <- max(unlist(lapply(paste0("^", vars, "$"), grep, x = names(pp.tab))))

    trtnam <- names(pp.tab)[1:trtindex]
    # Exclude reserved column names
    trtnam <- trtnam[trtnam %!in% c("predicted.value", "std.error", "Df",
                             "groups", "PredictedValue", "ApproxSE", "ci", "low", "up")]

    for(i in seq_along(trtnam)){
        pp.tab[[trtnam[i]]] <- factor(pp.tab[[trtnam[i]]], levels = unique(pp.tab[[trtnam[i]]]))
    }

    # rounding to the correct number of decimal places
    pp.tab <- rapply(object = pp.tab, f = round, classes = "numeric", how = "replace", digits = decimals)

    if(save) {
        write.csv(pp.tab, file = paste0(savename, ".csv"), row.names = FALSE)
    }

    # If there are brackets in the label, grab the text from inside
    if(is.call(ylab)) {
        ylab <- as.character(ylab)[2]
    }
    attr(pp.tab, "ylab") <- ylab

    if(length(vars)>2) {
        classify3 <- vars[3]
    }
    else if(length(vars) > 1) {
        classify2 <- vars[2]
        classify <- vars[1]
    }

    class(pp.tab) <- c("mct", class(pp.tab))
    if(plot) {
        print(autoplot(pp.tab))
    }

    if(exists("aliased_names")) {
        attr(pp.tab, 'aliased') <- as.character(aliased_names)
    }

    # Add the critical value as an attribute
    if(stats::var(as.vector(crit.val), na.rm = TRUE) < 1e-10) {
        attr(pp.tab, 'HSD') <- crit.val[1,2]
    }
    else {
        attr(pp.tab, 'HSD') <- crit.val
    }

    rownames(pp.tab) <- NULL

    return(pp.tab)
}


#' Print output of multiple_comparisons
#'
#' @param x An mct object to print to the console.
#' @param ... Other arguments
#'
#' @return The original object invisibly.
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

