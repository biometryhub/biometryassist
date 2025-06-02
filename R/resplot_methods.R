#' Extract model information using S3 dispatch
#' @param model.obj Model object
#' @param call Logical, whether to extract model call
#' @keywords internal
extract_model_info <- function(model.obj, call = FALSE) {
    UseMethod("extract_model_info")
}

#' @keywords internal
extract_model_info.default <- function(model.obj, call = FALSE) {
    supported_types <- c("aov", "lm", "lme", "lmerMod", "lmerModLmerTest",
                         "asreml", "mmer", "mmes", "art")
    stop("model.obj must be a linear (mixed) model object. Currently supported model types are: ",
         paste(supported_types, collapse = ", "), call. = FALSE)
}

#' @keywords internal
extract_model_info.aov <- function(model.obj, call = FALSE) {
    resids <- residuals(model.obj)
    fits <- fitted(model.obj)
    k <- length(resids)

    model_call <- NULL
    if(call) {
        model_call <- paste(trimws(deparse(model.obj$call, width.cutoff = 50)), collapse = "\n")
    }

    output <- list(facet = 1,
        facet_name = NULL,
        resids = resids,
        fits = fits,
        k = k,
        model_call = model_call
    )

    return(output)
}

#' @keywords internal
extract_model_info.lm <- extract_model_info.aov

#' @keywords internal
extract_model_info.lme <- extract_model_info.aov

#' @keywords internal
extract_model_info.lmerMod <- function(model.obj, call = FALSE) {
    resids <- residuals(model.obj)
    fits <- fitted(model.obj)
    k <- length(resids)

    model_call <- NULL
    if(call) {
        model_call <- paste(trimws(deparse(model.obj@call, width.cutoff = 50)), collapse = "\n")
    }

    output <- list(facet = 1,
        facet_name = NULL,
        resids = resids,
        fits = fits,
        k = k,
        model_call = model_call
    )

    return(output)
}

#' @keywords internal
extract_model_info.lmerModLmerTest <- extract_model_info.lmerMod

#' @keywords internal
extract_model_info.asreml <- function(model.obj, call = FALSE) {
    facet <- length(names(model.obj$R.param))

    if(facet > 1) {
        facet_name <- names(model.obj$R.param)
        k <- unlist(lapply(1:facet, function(i) model.obj$R.param[[i]]$variance$size))
    } else {
        facet_name <- NULL
        k <- length(model.obj$residual)
    }

    resids <- as.numeric(model.obj[["residuals"]])
    fits <- model.obj[["linear.predictors"]]

    model_call <- NULL
    if(call) {
        model_call <- paste(trimws(deparse(model.obj$call, width.cutoff = 50)), collapse = "\n")
        model_call <- gsub("G\\.param \\= model\\.asr\\$G\\.param, ", "", model_call)
        model_call <- gsub("R\\.param = model\\.asr\\$R\\.param, \\\n", "", model_call)
    }

    output <- list(facet = facet,
        facet_name = facet_name,
        resids = resids,
        fits = fits,
        k = k,
        model_call = model_call
    )

    return(output)
}

#' @keywords internal
extract_model_info.mmer <- function(model.obj, call = FALSE) {
    facet <- model.obj$termsN$rcov
    k <- length(model.obj$residual)

    resids <- residuals(model.obj)
    fits <- fitted(model.obj)

    model_call <- NULL
    if(call) {
        model_call <- paste(trimws(deparse(model.obj$call[c("fixed", "random", "rcov")], width.cutoff = 50)), collapse = "\n")
        model_call <- gsub("list", "mmer", model_call)
    }

    output <- list(facet = facet,
        facet_name = NULL,
        resids = resids,
        fits = fits,
        k = k,
        model_call = model_call
    )

    return(output)
}

#' @keywords internal
extract_model_info.mmes <- function(model.obj, call = FALSE) {
    k <- length(model.obj$residual)

    resids <- as.numeric(residuals(model.obj))
    fits <- as.numeric(fitted(model.obj))

    model_call <- NULL
    if(call) {
        model_call <- "Model call not currently available for mmes models."
    }

    list(
        facet = 1,
        facet_name = NULL,
        resids = resids,
        fits = fits,
        k = k,
        model_call = model_call
    )
}

#' @keywords internal
extract_model_info.art <- function(model.obj, call = FALSE) {
    resids <- residuals(model.obj)
    k <- length(resids)
    fits <- model.obj$cell.means[,ncol(model.obj$cell.means)]

    model_call <- NULL
    if(call) {
        model_call <- paste(trimws(deparse(model.obj$call, width.cutoff = 50)), collapse = "\n")
    }

    list(
        facet = 1,
        facet_name = NULL,
        resids = resids,
        fits = fits,
        k = k,
        model_call = model_call
    )
}
