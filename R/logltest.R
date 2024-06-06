#' Conduct a log-likelihood test for comparing terms in ASReml-R models
#'
#' @param model.obj An ASReml-R model object
#' @param rand.terms Random terms from the model. Default is NULL.
#' @param resid.terms Residual terms from the model. Default is NULL.
#' @param decimals Controls rounding of decimal places in output. Default is 3 decimal places.
#' @param numeric Return p-values as numeric? Default is that they are characters, where very small values shown as less than a small number. See `details` for more.
#' @param quiet Logical (default: `FALSE`). Hide warnings and messages?
#'
#' @details Typically p-values cannot be 0, and are usually just below some threshold of accuracy in calculation of probability.
#'
#' @importFrom stats as.formula update pchisq
#'
#' @return A dataframe containing the results of the test.
#' @export
#'
#' @examples
#' \dontrun{
#' library(asreml)
#' dat <- asreml::oats
#' dat <- dat[order(dat$Row, dat$Column),]
#'
#' #Fit ASReml Model
#' model.asr <- asreml(yield ~ Nitrogen + Variety + Nitrogen:Variety,
#'                     random = ~ Blocks + Blocks:Wplots,
#'                     residual = ~ ar1(Row):ar1(Column),
#'                     data = dat)
#' oats.logl <- logl_test(
#'   model.obj = model.asr, rand.terms = c("Blocks", "Blocks:Wplots"),
#'   resid.terms = c("ar1(Row)", "ar1(Column)")
#' )
#' oats.logl
#' }
#'
logl_test <- function(model.obj, rand.terms = NULL, resid.terms = NULL, decimals = 3, numeric = FALSE, quiet = FALSE) {

    if(!inherits(model.obj, "asreml")){
        stop("Only asreml models are supported at this time.")
    }

    if(is.null(rand.terms) & is.null(resid.terms)) {
        stop("One of rand.terms or resid.terms must be provided.")
    }

    # Find terms on the boundary

    n <- rep(0, 6)
    # warns <- character()

    bnd <- rownames(summary(model.obj)$varcomp[summary(model.obj)$varcomp$bound == "B",])

    if (any(grepl("!cor", bnd))) {
        trm <- substring(
            bnd[grepl("!cor", bnd, fixed = TRUE)],
            (unlist(gregexpr("!", bnd[grepl("!cor", bnd, fixed = TRUE)]))[1] + 1),
            (unlist(gregexpr("!", bnd[grepl("!cor", bnd, fixed = TRUE)]))[2] - 1)
        )

        bnd[grepl("!cor", bnd)] <- resid.terms[grepl(trm, resid.terms)]
    }

    all.terms <- c(rand.terms, resid.terms)
    all.bnd <- all(is.element(all.terms, bnd))

    # terms to conduct loglikehood ratio test on
    tt <- c(rand.terms[!is.element(rand.terms, bnd)], resid.terms[!is.element(resid.terms, bnd)])

    if (length(bnd) > 0) {
        test.df <- data.frame(Term = bnd, LogLRT.pvalue = 1)
    } else {
        test.df <- data.frame(Term = character(), LogLRT.pvalue = numeric())
    }

    # Loglikehood ratio tests
    if (!all.bnd) {

        # update model excluding the boundary terms - random
        brand.terms <- c()
        brand.terms <- rand.terms[is.element(rand.terms, bnd)]

        if(any(is.na(model.obj$vparameters.pc))) {
            model.obj$vparameters.pc[is.na(model.obj$vparameters.pc)] <- 0
        }

        if (length(brand.terms > 0)) {
            model.obj <- quiet(update(model.obj, random = as.formula(paste("~ . - ", paste(brand.terms, collapse = " - "), sep = " "))))
            n[1] <- 1
            while (!model.obj$converge & (n[1] < 10)) {
                model.obj <- quiet(update(model.obj))
                n[1] <- n[1] + 1
            }

            n[2] <- 1
            while (any(model.obj$vparameters.pc > 1) & n[2] < 10) {
                model.obj <- quiet(update(model.obj, ))
                n[2] <- n[2] + 1
            }
        }

        # Fitting the models

        for (i in seq_along(tt)) {
            if (grepl("ar", tt[i])) {
                tt.new <- paste("id", substring(tt[i], 4), sep = "")
                old.resid <- substring(toString(model.obj$formulae$residual), 4)
                new.resid <- gsub(tt[[i]], tt.new, old.resid, fixed = TRUE)

                # Fit reduced model
                model.obj1 <- quiet(update(model.obj, residual = as.formula(paste("~", new.resid, sep = " "))))

                n[3] <- 1
                while (!model.obj1$converge & n[3] < 10) {
                    model.obj1 <- quiet(update(model.obj1))
                    n[3] <- n[3] + 1
                }

                n[4] <- 1
                while (!model.obj1$converge & n[4] < 10) {
                    model.obj1 <- quiet(update(model.obj1))
                    n[4] <- n[4] + 1
                }

                # Logl test

                p <- (length(model.obj$vparameters) +
                          length(model.obj$coefficients$fixed))-
                    (length(model.obj1$vparameters) +
                         length(model.obj1$coefficients$fixed))

                logl <- 2*(model.obj$loglik-model.obj1$loglik)

                ll.test <- round(1-pchisq(logl, p), decimals)

                result.df <- data.frame(Term = tt[i], LogLRT.pvalue = ll.test)
                test.df <- rbind(test.df, result.df)
            }
            if (!grepl("ar", tt[i])) {

                # Fit reduced model
                tst.terms <- tt[grepl(tt[i], tt)]
                model.obj1 <- quiet(update(model.obj, random = as.formula(paste("~ . - ", paste(tst.terms, collapse = " - "), sep = " "))))

                n[5] <- 1
                while (!model.obj1$converge & n[5] < 10) {
                    model.obj1 <- quiet(update(model.obj1))
                    n[5] <- n[5] + 1
                }

                n[6] <- 1
                while (any(model.obj1$vparameters.pc > 1) & n[6] < 10) {
                    model.obj1 <- quiet(update(model.obj1))
                    n[6] <- n[6] + 1
                }

                # Logl test
                p <- (length(model.obj$vparameters) +
                          length(model.obj$coefficients$fixed))-
                    (length(model.obj1$vparameters) +
                         length(model.obj1$coefficients$fixed))
                logl <- -2*(model.obj1$loglik-model.obj$loglik)

                ll.test <- round(1-pchisq(abs(logl), p), decimals)

                result.df <- data.frame(Term = tt[i], LogLRT.pvalue = ll.test)

                test.df <- rbind(test.df, result.df)
            }
        }
    }

    if(!numeric) {
        test.df$LogLRT.pvalue <- format.pval(test.df$LogLRT.pvalue, digits = decimals,
                                             eps = max(as.numeric(paste0(1, "e-", decimals)), .Machine$double.eps))
    }
    else {
        test.df$LogLRT.pvalue <- round(test.df$LogLRT.pvalue, decimals)
        test.df$LogLRT.pvalue[test.df$LogLRT.pvalue==0] <- max(as.numeric(paste0(1, "e-", decimals)), .Machine$double.eps)
    }

    all.terms <- c(rand.terms, resid.terms)
    test.df <- test.df[is.element(test.df$Term, all.terms),]

    return(test.df)
}
