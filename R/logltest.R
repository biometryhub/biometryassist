#' Conduct a log-likelihood test for comparing terms in ASReml-R models
#'
#' @param model.obj An ASReml-R model object
#' @param rand.terms Character vector of random terms to test. Default is NULL.
#' @param resid.terms Character vector of residual terms to test. Default is NULL.
#' @param decimals Number of decimal places to round p-values. Default is 3.
#' @param numeric Logical. Should p-values be returned as numeric? Default is FALSE (formatted).
#' @param quiet Logical. Suppress model update messages and warnings? Default is FALSE.
#'
#' @returns A data frame of terms and corresponding log-likelihood ratio test p-values.
#' @export
logl_test <- function(model.obj, rand.terms = NULL, resid.terms = NULL, decimals = 3, numeric = FALSE, quiet = FALSE) {
    if (!inherits(model.obj, "asreml")) {
        stop("Only asreml models are supported.", call. = FALSE)
    }

    if (is.null(rand.terms) & is.null(resid.terms)) {
        stop("At least one of rand.terms or resid.terms must be provided.", call. = FALSE)
    }

    # Prepare term table
    terms.df <- data.frame(
        Term = c(rand.terms, resid.terms),
        Type = c(rep("random", length(rand.terms)), rep("residual", length(resid.terms))),
        stringsAsFactors = FALSE
    )

    # Identify boundary terms
    vc <- summary(model.obj)$varcomp
    boundary <- rownames(vc[vc$bound == "B", , drop = FALSE])
    terms.df$OnBoundary <- terms.df$Term %in% boundary

    # Terms to test
    test.terms <- terms.df[!terms.df$OnBoundary, ]

    # Terms on boundary – assign p = 1
    boundary.df <- terms.df[terms.df$OnBoundary, ]
    if (nrow(boundary.df) > 0) {
        boundary.df$LogLRT.pvalue <- 1
    }

    # Helper: suppress messages if quiet = TRUE
    # qupdate <- function(...) {
    #     if (quiet) suppressMessages(suppressWarnings(update(...))) else update(...)
    # }

    # Helper: refit loop
    refit_until_converged <- function(model, max_iter = 10) {
        i <- 1
        while (!model$converge && i <= max_iter) {
            model <- quiet(update(model, trace = !quiet))
            i <- i + 1
        }
        return(model)
    }

    # Helper: log-likelihood test
    loglik_test <- function(full, reduced, decimals = 3) {
        df <- (length(full$vparameters) + length(full$coefficients$fixed)) -
            (length(reduced$vparameters) + length(reduced$coefficients$fixed))
        logl <- 2 * (full$loglik - reduced$loglik)
        pval <- 1 - pchisq(logl, df)
        round(pval, decimals)
    }

    # Drop any random terms that are on the boundary before starting
    if (!is.null(rand.terms)) {
        boundary.rand <- intersect(rand.terms, boundary)
        if (length(boundary.rand) > 0) {
            rand.terms <- setdiff(rand.terms, boundary.rand)
            model.obj <- quiet(update(model.obj, random = as.formula(paste("~ . -", paste(boundary.rand, collapse = " - "))), trace = !quiet))
            model.obj <- refit_until_converged(model.obj)
        }
    }

    # Perform tests
    result.list <- list()

    for (i in seq_len(nrow(test.terms))) {
        term <- test.terms$Term[i]
        type <- test.terms$Type[i]

        if (type == "residual") {
            orig.resid <- as.character(model.obj$formulae$residual)[2]
            if (grepl("^ar[0-9]", term)) {
                # Handle AR terms
                updated.resid <- gsub(term, paste0("id", substring(term, 4)), orig.resid, fixed = TRUE)
            } else {
                # Handle other residual structures - might need different logic
                updated.resid <- gsub(paste0("\\b", term, "\\b"), "units", orig.resid)
            }
            new.resid.formula <- as.formula(paste("~", updated.resid))
            reduced.model <- quiet(update(model.obj, residual = new.resid.formula, trace = !quiet))
        }
        else if (type == "random") {
            new.rand.formula <- as.formula(paste("~ . -", term))
            reduced.model <- quiet(update(model.obj, random = new.rand.formula, trace = !quiet))
        }

        reduced.model <- refit_until_converged(reduced.model)

        pval <- loglik_test(model.obj, reduced.model, decimals)

        result.list[[length(result.list) + 1]] <- data.frame(
            Term = term,
            LogLRT.pvalue = pval,
            stringsAsFactors = FALSE
        )
    }

    test.df <- do.call(rbind, result.list)

    # Combine with boundary terms
    if (nrow(boundary.df) > 0) {
        test.df <- rbind(test.df, boundary.df[, c("Term", "LogLRT.pvalue")])
    }

    # Format p-values
    if (!numeric) {
        test.df$LogLRT.pvalue <- format.pval(
            test.df$LogLRT.pvalue,
            digits = decimals,
            eps = max(as.numeric(paste0("1e-", decimals)), .Machine$double.eps)
        )
    } else {
        test.df$LogLRT.pvalue[test.df$LogLRT.pvalue == 0] <-
            max(as.numeric(paste0("1e-", decimals)), .Machine$double.eps)
    }

    test.df <- test.df[match(c(rand.terms, resid.terms), test.df$Term), , drop = FALSE]
    rownames(test.df) <- NULL
    return(test.df)
}
