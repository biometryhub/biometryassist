#' Title
#'
#' @param design The data frame of the design.
#' @param type The type of buffer. One of edge, row, column, double row, double column, or block (coming soon).
#'
#' @return The original data frame, updated to include buffers
#' @keywords internal
create_buffers <- function(design, type) {
    nrow <- max(design$row)
    ncol <- max(design$col)

    # Match edge, edges or e
    if(grepl("(^edges?$|^e$)", tolower(type))) {
        row <- c(rep(1, ncol+2), rep(nrow+2, ncol+2), rep(2:(nrow+1), 2))
        col <- c(rep(1:(ncol+2), 2), rep(1, nrow), rep(ncol+2, nrow))
        n_brow <- length(row)  # Number of buffer rows
        treatments <- rep("buffer", n_brow)

        design$row <- design$row+1
        design$col <- design$col+1
    }
    # Match row, rows, r
    else if(grepl("(^rows?$|^r$)", tolower(type))) {
        row <- rep(seq(1, (2*nrow)+1, by = 2), each = ncol)
        col <- rep(seq(1, ncol),times = nrow+1)
        n_brow <- length(row)  # Number of buffer rows
        treatments <- rep("buffer", n_brow)
        design$row <- 2*design$row
    }
    # Match col, cols, column, columns or c
    else if(grepl("(^col(umn)?s?$|^c$)", tolower(type))) {
        row <- rep(seq(1, nrow), times = ncol+1)
        col <- rep(seq(1, (2*ncol)+1, by = 2), each = nrow)
        n_brow <- length(row)  # Number of buffer rows
        treatments <- rep("buffer", n_brow)
        design$col <- 2*design$col
    }
    # Match double row, double rows, or dr
    else if(grepl("(^double rows?$|^dr$)", tolower(type))) {
        row <- c(rep(seq(1, (3*nrow)-2, by = 3),
                     each = ncol),
                 rep(seq(3, (3*nrow), by = 3),
                     each = ncol))
        col <- rep(seq(1, ncol), times = 2*nrow)
        n_brow <- length(row)  # Number of buffer rows
        treatments <- rep("buffer", n_brow)
        design$row <- (3*design$row)-1
    }
    # Match double col, double cols, double column, double columns, dc
    else if(grepl("(^double col(umn)?s?$|^dc$)", tolower(type))) {
        row <- rep(seq(1, nrow), times = 2*ncol)
        col <- c(rep(seq(1, (3*ncol)-2, by = 3),
                     each = nrow),
                 rep(seq(3, (3*ncol), by = 3),
                     each = nrow))
        n_brow <- length(row)  # Number of buffer rows
        treatments <- rep("buffer", n_brow)
        design$col <- (3*design$col)-1
    }
    # Match block, blocks, or b
    else if(grepl("(^blocks?$|^b$)", tolower(type))) {
        stop("Block buffers are not yet supported.")
    }
    else {
        stop("Invalid buffer option: ", type, call. = FALSE)
    }

    buffers <- data.frame(matrix(NA, nrow = n_brow, ncol = ncol(design)))
    buffers <- setNames(buffers, names(design))
    buffers$row <- row
    buffers$col <- col
    buffers$treatments <- factor(treatments)

    design <- rbind(design, buffers)

    return(design)
}


# # Blocks
#
# nrow <- max(des.out$design$row)
# ncol <- max(des.out$design$col)
# nblocks <- max(as.numeric(des.out$design$block))
# autoplot(des.out)
#
#
# buffers <- data.frame(row = rep(1:nrow,
#                                 times = ncol+1),
#                       col = rep(seq(1, (2*ncol)+1, by = 2),
#                                 each = nrow),
#                       plots = NA, rep = NA, treatments = factor("buffer"))
#
# des.out$design$col <- 2*des.out$design$col
# des.out$design <- rbind(des.out$design, buffers)
# autoplot(des.out)
