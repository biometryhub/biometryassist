#' Produces graph of design layout, skeletal ANOVA table and data frame with complete design
#'
#' @param design.obj An `agricolae` design object.
#' @param nrows The number of rows in the design.
#' @param ncols The number of columns in the design.
#' @param brows For RCBD only. The number of rows in a block.
#' @param bcols For RCBD only. The number of columns in a block.
#' @param byrow For split-plot only. Logical (default: `TRUE`). Provides a way to arrange plots within whole-plots when there are multiple possible arrangements.
#' @param fac.sep The separator used by `fac.names`. Used to combine factorial design levels. If a vector of 2 levels is supplied, the first separates factor levels and label, and the second separates the different factors.
#' @param fac.names Allows renaming of the `A` level of factorial designs (i.e. those using [agricolae::design.ab()]) by passing (optionally named) vectors of new labels to be applied to the factors within a list. See examples and details for more information.
#' @param plot Logical (default `TRUE`). If `TRUE`, display a plot of the generated design. A plot can always be produced later using [autoplot()].
#' @param rotation Rotate the text output as Treatments within the plot. Allows for easier reading of long treatment labels. Takes positive and negative values being number of degrees of rotation from horizontal.
#' @param size Increase or decrease the text size within the plot for treatment labels. Numeric with default value of 4.
#' @param margin Logical (default FALSE). Expand the plot to the edges of the plotting area i.e. remove white space between plot and axes.
#' @param save One of `FALSE` (default)/`"none"`, `TRUE`/`"both"`, `"plot"` or `"workbook"`. Specifies which output to save.
#' @param savename A filename for the design to be saved to. Default is the type of the design combined with "_design".
#' @param plottype The type of file to save the plot as. Usually one of `"pdf"`, `"png"`, or `"jpg"`. See [ggplot2::ggsave()] for all possible options.
#' @param return.seed Logical (default TRUE). Output the seed used in the design?
#' @param quiet Logical (default FALSE). Return the objects without printing output.
#' @param ... Additional parameters passed to [ggplot2::ggsave()] for saving the plot.
#'
#' @details If `save = TRUE` (or `"both"`), both the plot and the workbook will be saved to the current working directory, with filename given by `savename`. If one of either `"plot"` or `"workbook"` is specified, only that output is saved. If `save = FALSE` (the default, or equivalently `"none"`), nothing will be output.
#' @details `fac.names` can be supplied to provide more intuitive names for factors and their levels in factorial designs. They should be specified in a list format, for example `fac.names = list(A_names = c("a", "b", "c"), B_names = c("x", "y", "z"))`. This will result a design output with a column named `A_names` with levels `a, b, c` and another named `B_names` with levels `x, y, z`. Only the first two elements of the list will be used.
#' @details If `fac.sep` is a single element (e.g. "_"), this is used to separate all factor labels (e.g. A_1_B_1). If it is two elements (e.g. c("", "_")), the first element separates the factor names and their levels, and the second level separates the two factors (e.g. A1_B1).
#' @details `...` allows extra arguments to be passed to ggsave for output of the plot. The details of possible arguments can be found in  [ggplot2::ggsave()].
#'
#' @importFrom graphics plot
#' @importFrom ggplot2 ggsave
#' @importFrom utils write.csv
#' @importFrom ellipsis check_dots_used
#'
#' @return A list containing a data frame with the complete design, a ggplot object with plot layout, the seed (if `return.seed = TRUE`), and the `satab` object, allowing repeat output of the `satab` table via `cat(output$satab)`.
#'
#' @examples
#' library(agricolae)
#'
#' # Completely Randomised Design
#' trt <- c(1, 5, 10, 20)
#' rep <- 5
#' outdesign <- design.crd(trt = trt, r = rep, seed = 42)
#' des.out <- des_info(design.obj = outdesign, nrows = 4, ncols = 5)
#'
#' # Randomised Complete Block Design
#' trt <- LETTERS[1:11]
#' rep <- 4
#' outdesign <- design.rcbd(trt = trt, r = rep, seed = 42)
#' des.out <- des_info(
#'   design.obj = outdesign, nrows = 11,
#'   ncols = 4, brows = 11, bcols = 1
#' )
#'
#' # Latin Square Design
#' trt <- c("S1", "S2", "S3", "S4")
#' outdesign <- design.lsd(trt)
#' des.out <- des_info(design.obj = outdesign, nrows = 4, ncols = 4)
#'
#' # Factorial Design (Crossed, Completely Randomised)
#' trt <- c(3, 2) # Factorial 3 x 2
#' rep <- 3
#' outdesign <- design.ab(trt, r = rep, design = "crd")
#' des.out <- des_info(design.obj = outdesign, nrows = 6, ncols = 3)
#'
#' # Factorial Design (Crossed, Completely Randomised), renaming factors
#' trt <- c(3, 2) # Factorial 3 x 2
#' rep <- 3
#' outdesign <- design.ab(trt, r = rep, design = "crd")
#' des.out <- des_info(design.obj = outdesign, nrows = 6, ncols = 3,
#'                     fac.names = list(N = c(50, 100, 150),
#'                                      Water = c("Irrigated", "Rain-fed")))
#'
#' # Factorial Design (Nested, Latin Square)
#' trt <- c("A1", "A2", "A3", "A4", "B1", "B2", "B3")
#' outdesign <- design.lsd(trt)
#' des.out <- des_info(design.obj = outdesign, nrows = 7, ncols = 7)
#'
#' # Split plot design
#' trt1 <- c("A", "B")
#' trt2 <- 1:4
#' rep <- 4
#' outdesign <- design.split(trt1, trt2, r = rep)
#' des.out <- des_info(design.obj = outdesign, nrows = 8, ncols = 4, brows = 4, bcols = 2)
#' @export
#'
des_info <- function(design.obj,
                     nrows,
                     ncols,
                     brows = NA,
                     bcols = NA,
                     byrow = TRUE,
                     fac.names = NULL,
                     fac.sep = c("", " "),
                     plot = TRUE,
                     rotation = 0,
                     size = 4,
                     margin = FALSE,
                     save = FALSE,
                     savename = paste0(design.obj$parameters$design, "_design"),
                     plottype = "pdf",
                     return.seed = TRUE,
                     quiet = FALSE,
                     ...) {

    # Error checking of inputs
    ellipsis::check_dots_used()

    # Check brows and bcols supplied if necessary
    if(design.obj$parameters$design == "rcbd" & anyNA(c(brows, bcols))) {
        stop("Design has blocks so brows and bcols must be supplied.")
    }
    else if(design.obj$parameters$design == "factorial") {
        if(design.obj$parameters$applied == "rcbd" & anyNA(c(brows, bcols))) {
            stop("Design has blocks so brows and bcols must be supplied.")
        }

        # If factorial design, and names are supplied, use them
        n_facs <- ifelse(is.null(design.obj$book$C), 2, 3)
        if(!is.null(fac.names)) {
            if(length(fac.names) > n_facs) {
                warning("fac.names contains ", length(fac.names), " elements but only the first ", n_facs, " have been used.", call. = FALSE)
            }
            else if(length(fac.names) < n_facs) {
                warning("fac.names doesn't contain enough elements and has not been used.", call. = FALSE)
            }
            else {
                if(is.list(fac.names)) {
                    design.obj$book$A <- as.factor(design.obj$book$A)
                    design.obj$book$B <- as.factor(design.obj$book$B)

                    if(length(levels(design.obj$book$A)) == length(fac.names[[1]])) {
                        levels(design.obj$book$A) <- fac.names[[1]]
                    }
                    else {
                        warning(names(fac.names)[1], " must contain the correct number of elements. Elements have not been applied.", call. = FALSE)
                    }

                    if(length(levels(design.obj$book$B)) == length(fac.names[[2]])) {
                        levels(design.obj$book$B) <- fac.names[[2]]
                    }
                    else {
                        warning(names(fac.names)[2], " must contain the correct number of elements. Elements have not been applied.", call. = FALSE)
                    }

                    if(n_facs == 3) {
                        design.obj$book$C <- as.factor(design.obj$book$C)

                        if(length(levels(design.obj$book$C)) == length(fac.names[[3]])) {
                            levels(design.obj$book$C) <- fac.names[[3]]
                            colnames(design.obj$book)[which(colnames(design.obj$book)=="C")] <- names(fac.names)[3]
                        }
                        else {
                            warning(names(fac.names)[3], " must contain the correct number of elements. Elements have not been applied.", call. = FALSE)
                        }
                    }
                }

                colnames(design.obj$book)[which(colnames(design.obj$book)=="A")] <- names(fac.names)[1]
                colnames(design.obj$book)[which(colnames(design.obj$book)=="B")] <- names(fac.names)[2]
            }
        }
    }
    else if(design.obj$parameters$design == "split") {
        if(design.obj$parameters$applied == "rcbd" & anyNA(c(brows, bcols))) {
            stop("Design has blocks so brows and bcols must be supplied.")
        }

        # If names are supplied, use them
        if(!is.null(fac.names)) {

            if(length(fac.names) > 2) {
                warning("fac.names contains ", length(fac.names), " elements but only the first 2 have been used.", call. = FALSE)
            }
            else if(length(fac.names) < 2) {
                warning("fac.names doesn't contain enough elements and has not been used.", call. = FALSE)
            }
            else {
                if(is.list(fac.names)) {

                    design.obj$book$treatments <- as.factor(design.obj$book$treatments)
                    design.obj$book$sub_treatments <- factor(design.obj$book$sub_treatments)

                    if(length(levels(design.obj$book$treatments)) == length(fac.names[[1]])) {
                        levels(design.obj$book$treatments) <- fac.names[[1]]
                    }
                    else {
                        warning(names(fac.names)[1], " must contain the correct number of elements. Elements have not been applied.", call. = FALSE)
                    }

                    if(length(levels(design.obj$book$sub_treatments)) == length(fac.names[[2]])) {
                        levels(design.obj$book$sub_treatments) <- fac.names[[2]]
                    }
                    else {
                        warning(names(fac.names)[2], " must contain the correct number of elements. Elements have not been applied.", call. = FALSE)
                    }

                    colnames(design.obj$book)[4:5] <- names(fac.names)[1:2]
                }
                else if(is.character(fac.names)) {
                    colnames(design.obj$book)[4:5] <- fac.names[1:2]
                }
            }
        }
    }

    if(!missing(fac.sep) && length(fac.sep) == 1) {
        fac.sep <- rep(fac.sep, times = 2)
    }

    # if (return.seed) {
    #     des.seed <- design.obj$parameters$seed
    # }
    # else {
    #     des.seed <- NULL
    # }

    ifelse(design.obj$parameters$design == "factorial",
           design <- paste("factorial", design.obj$parameters$applied, sep = "_"),
           design <- design.obj$parameters$design
    )

    if (design == "crd") {
        plan <- expand.grid(row = 1:nrows, col = 1:ncols)
        des <- cbind(plan, design.obj$book)

        names(des)[names(des)=="r"] <- "rep"
        # if(any(grepl("trt", names(des)))) {
            # names(des)[names(des)=="trt"] <- "treatments"
        # }
        # else {
        names(des)[ncol(des)] <- "treatments"
        # }
        ntrt <- nlevels(as.factor(des$treatments))
    }

    if (design == "rcbd") {
        # names(design.obj$book)[names(design.obj$book)=="trt"] <- "treatments"
        # names(design.obj$book)[ncol(design.obj$book)] <- "treatments"
        ntrt <- nlevels(as.factor(design.obj$book[,ncol(design.obj$book)]))

        # Calculate direction of blocking
        xx <- c()
        rr <- nrows / brows
        cc <- ncols / bcols
        # Blocking across rows: brows == ntrt in a single column
        if (brows == ntrt) {
            des <- design.obj$book
            plan <- expand.grid(row = 1:nrows, col = 1:ncols) # 2
        }

        # Blocking incomplete rows all columns
        if (rr > 1 & cc == 1) {
            des <- design.obj$book
            plan <- expand.grid(col = 1:ncols, row = 1:nrows) # 1
        }


        # Blocking incomplete rows and incomplete columns
        if (rr > 1 & cc > 1) {
            des <- design.obj$book

            # set up empty columns in the plan data.frame
            plan <- expand.grid(row = 1:nrows, col = 1:ncols)
            plan$block <- des$block
            plan$col <- NA
            plan$row <- NA

            pp <- expand.grid(col = 1:bcols, row = 1:brows)

            i <- 1
            for (j in 1:rr) {
                for (k in 1:cc) {
                    plan$col[plan$block == i] <- pp$col + (k - 1) * bcols
                    plan$row[plan$block == i] <- pp$row + (brows * (j - 1))
                    k <- k + 1
                    i <- i + 1
                }
                j <- j + 1
            }
            plan$block <- NULL
        } # 3


        # Blocking across columns: bcols == ntrt in a single row
        if (bcols == ntrt) {
            des <- design.obj$book
            plan <- expand.grid(col = 1:ncols, row = 1:nrows)
        } # 4


        # Blocking incomplete columns all rows
        if (cc > 1 & rr == 1) {
            des <- design.obj$book

            # set up empty columns in the plan data.frame
            plan <- expand.grid(row = 1:nrows, col = 1:ncols)
            plan$block <- des$block
            plan$col <- NA
            plan$row <- NA

            pp <- expand.grid(col = 1:bcols, row = 1:brows)

            i <- 1
            for (k in 1:cc) {
                plan$col[plan$block == i] <- pp$col + (k - 1) * bcols
                plan$row[plan$block == i] <- pp$row
                k <- k + 1
                i <- i + 1
            }
            plan$block <- NULL
        } # 5

        des <- cbind(plan, des)
        names(des)[ncol(des)] <- "treatments"
    }

    if (design == "lsd") {
        des <- design.obj$book
        des$row <- as.numeric(des$row)
        des$col <- as.numeric(des$col)

        names(des)[ncol(des)] <- "treatments"
        ntrt <- nlevels(as.factor(des$treatments))
    }

    if (design == "factorial_crd") {
        treatments <- NULL
        plan <- expand.grid(row = 1:nrows, col = 1:ncols)
        des <- cbind(plan, design.obj$book, row.names = NULL)

        for (i in 3:ncol(design.obj$book)) {
            treatments <- paste(treatments, paste(colnames(design.obj$book)[i], design.obj$book[, i], sep = fac.sep[1]), sep = fac.sep[2])
        }

        if(fac.sep[2] == "") {
            des$treatments <- factor(trimws(treatments))
        }
        else {
            des$treatments <- factor(trimws(substr(treatments, 2, nchar(treatments))))
        }
        names(des)[names(des)=="r"] <- "reps"
        ntrt <- nlevels(des$treatments)
    }

    if (design == "factorial_rcbd") {
        treatments <- NULL

        for (i in 3:ncol(design.obj$book)) {
            treatments <- paste(treatments, paste(colnames(design.obj$book)[i], design.obj$book[, i], sep = fac.sep[1]), sep = fac.sep[2])
        }

        ntrt <- nlevels(as.factor(treatments))

        # Calculate direction of blocking
        xx <- c()
        rr <- nrows / brows
        cc <- ncols / bcols
        # Blocking across rows: brows == ntrt in a single column
        if (brows == ntrt) {
            des <- design.obj$book
            plan <- expand.grid(row = 1:nrows, col = 1:ncols) # 2
        }

        # Blocking incomplete rows all columns
        if (rr > 1 & cc == 1) {
            des <- design.obj$book
            plan <- expand.grid(col = 1:ncols, row = 1:nrows) # 1
        }


        # Blocking incomplete rows and incomplete columns
        if (rr > 1 & cc > 1) {
            des <- design.obj$book

            # set up empty columns in the plan data.frame
            plan <- expand.grid(row = 1:nrows, col = 1:ncols)
            plan$block <- des$block
            plan$col <- NA
            plan$row <- NA

            pp <- expand.grid(col = 1:bcols, row = 1:brows)

            i <- 1
            for (j in 1:rr) {
                for (k in 1:cc) {
                    plan$col[plan$block == i] <- pp$col + (k - 1) * bcols
                    plan$row[plan$block == i] <- pp$row + (brows * (j - 1))
                    k <- k + 1
                    i <- i + 1
                }
                j <- j + 1
            }
            plan$block <- NULL
        } # 3


        # Blocking across columns: bcols == ntrt in a single row
        if (bcols == ntrt) {
            des <- design.obj$book
            plan <- expand.grid(col = 1:ncols, row = 1:nrows)
        } # 4


        # Blocking incomplete columns all rows
        if (cc > 1 & rr == 1) {
            des <- design.obj$book

            # set up empty columns in the plan data.frame
            plan <- expand.grid(row = 1:nrows, col = 1:ncols)
            plan$block <- des$block
            plan$col <- NA
            plan$row <- NA

            pp <- expand.grid(col = 1:bcols, row = 1:brows)

            i <- 1
            for (k in 1:cc) {
                plan$col[plan$block == i] <- pp$col + (k - 1) * bcols
                plan$row[plan$block == i] <- pp$row
                k <- k + 1
                i <- i + 1
            }
            plan$block <- NULL
        } # 5

        if(fac.sep[2] == "") {
            des$treatments <- factor(trimws(treatments))
        }
        else {
            des$treatments <- factor(trimws(substr(treatments, 2, nchar(treatments))))
        }

        des <- cbind(plan, des)
    }

    if (design == "factorial_lsd") {
        treatments <- NULL
        des <- design.obj$book

        for (i in 4:ncol(design.obj$book)) {
            treatments <- paste(treatments, paste(colnames(design.obj$book)[i], design.obj$book[, i], sep = fac.sep[1]), sep = fac.sep[2])
        }

        if(fac.sep[2] == "") {
            des$treatments <- factor(trimws(treatments))
        }
        else {
            des$treatments <- factor(trimws(substr(treatments, 2, nchar(treatments))))
        }

        ntrt <- nlevels(des$treatments)
        des$row <- as.numeric(des$row)
        des$col <- as.numeric(des$col)
    }

    if (design == "split") {
        des <- design.obj$book
        spfacs <- c("plots", "splots", "block")

        trtNams <- names(des[!is.element(names(des), spfacs)])


        des$treatments <- factor(paste(des[, trtNams[1]], des[, trtNams[2]], sep = "_"))

        # Number of treatments
        ntrt <- nlevels(des$treatments)


        # Calculate direction of blocking
        xx <- c()
        rr <- nrows / brows
        cc <- ncols / bcols
        # Blocking across rows: brows == ntrt in a single column
        if (brows == ntrt) {
            plan <- expand.grid(row = 1:nrows, col = 1:ncols) # 2
        }

        # Blocking incomplete rows all columns
        if (rr > 1 & cc == 1) {
            plan <- expand.grid(col = 1:ncols, row = 1:nrows) # 1
        }

        # Blocking incomplete rows and incomplete columns
        if (rr > 1 & cc > 1) {

            # set up empty columns in the plan data.frame
            plan <- expand.grid(row = 1:nrows, col = 1:ncols)
            plan$block <- des$block
            plan$col <- NA
            plan$row <- NA

            pp <- expand.grid(col = 1:bcols, row = 1:brows)

            i <- 1
            for (j in 1:rr) {
                for (k in 1:cc) {
                    plan$col[plan$block == i] <- pp$col + (k - 1) * bcols
                    plan$row[plan$block == i] <- pp$row + (brows * (j - 1))
                    k <- k + 1
                    i <- i + 1
                }
                j <- j + 1
            }
            plan$block <- NULL
        } # 3


        # Blocking across columns: bcols == ntrt in a single row
        if (bcols == ntrt) {
            plan <- expand.grid(col = 1:ncols, row = 1:nrows)
        } # 4


        # Blocking incomplete columns all rows
        if (cc > 1 & rr == 1) {

            # set up empty columns in the plan data.frame
            plan <- expand.grid(row = 1:nrows, col = 1:ncols)
            plan$block <- des$block
            plan$col <- NA
            plan$row <- NA

            pp <- expand.grid(col = 1:bcols, row = 1:brows)

            i <- 1
            for (k in 1:cc) {
                plan$col[plan$block == i] <- pp$col + (k - 1) * bcols
                plan$row[plan$block == i] <- pp$row
                k <- k + 1
                i <- i + 1
            }
            plan$block <- NULL
        } # 5

        des <- cbind(plan, des)
        # Order by column within blocks, rather than row default
        if(!byrow) {
            des[,c("row", "col", "block")] <- des[order(des$block, des$col, des$row), c("row", "col", "block")]
        }
    }

    des$treatments <- factor(des$treatments, levels = unique(stringi::stri_sort(des$treatments, numeric = TRUE)))

    info <- list(design = des)
    class(des) <- c("design", class(des))

    if(plot) {
        info$plot.des = autoplot(des, rotation = rotation, size = size, margin = margin)
    }
    info$satab <- satab(design.obj)

    if(!quiet) {
        cat(info$satab)
        if(plot) {
            plot(info$plot.des)
        }
    }

    if(!is.logical(save)) {
        output <- tolower(save)
        if(output == "plot") {
            ggplot2::ggsave(filename = paste0(savename, ".", plottype), ...)
        }
        else if(output == "workbook") {
            write.csv(info$design, file = paste0(savename, ".csv"), row.names = FALSE)
        }
        else if(output == "both") {
            ggplot2::ggsave(filename = paste0(savename, ".", plottype), ...)
            write.csv(info$design, file = paste0(savename, ".csv"), row.names = FALSE)
        }
        else if(output == "none") {
            # Do nothing
        }
        else {
            stop("save must be one of 'none'/FALSE, 'both'/TRUE, 'plot', or 'workbook'.")
        }
    }
    else if(save) {
        ggplot2::ggsave(filename = paste0(savename, ".", plottype), ...)
        write.csv(info$design, file = paste0(savename, ".csv"), row.names = FALSE)
    }

    if (return.seed) {
        info$seed <- design.obj$parameters$seed
    }

    return(info)
}
