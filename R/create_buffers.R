#' Create buffers for design plots
#'
#' @param design The data frame of the design.
#' @param type The type of buffer. One of edge, row, column, double row, double column, or block (coming soon).
#' @param blocks Does the design data frame contain blocks?
#'
#' @importFrom stats setNames aggregate
#'
#' @return The original data frame, updated to include buffers
#' @keywords internal
create_buffers <- function(design, type, blocks = FALSE) {
    nrow <- max(design$row)
    ncol <- max(design$col)

    # Match edge, edges or e
    if(grepl("(^edges?$|^e$)", tolower(type))) {
        design$row <- design$row+1
        design$col <- design$col+1

        min_row <- min(design$row)
        min_col <- min(design$col)

        row <- c(rep(1, ncol+2), rep(nrow+2, ncol+2), rep(2:(nrow+1), 2))
        col <- c(rep(1:(ncol+2), 2), rep(1, nrow), rep(ncol+2, nrow))
        n_brow <- length(row)  # Number of rows to create in the buffer dataframe
        treatments <- rep("buffer", n_brow)
    }
    # Match row, rows, r
    else if(grepl("(^rows?$|^r$)", tolower(type))) {
        design$row <- 2*design$row

        min_row <- min(design$row)
        min_col <- min(design$col)

        row <- rep(seq(min_row-1, (2*nrow)+1, by = 2), each = ncol)
        col <- rep(seq(1, ncol),times = nrow+1)
        n_brow <- length(row)  # Number of rows to create in the buffer dataframe
        treatments <- rep("buffer", n_brow)
    }
    # Match col, cols, column, columns or c
    else if(grepl("(^col(umn)?s?$|^c$)", tolower(type))) {
        design$col <- 2*design$col

        min_row <- min(design$row)
        min_col <- min(design$col)

        row <- rep(seq(1, nrow), times = ncol+1)
        col <- rep(seq(min_col-1, (2*ncol)+1, by = 2), each = nrow)
        n_brow <- length(row)  # Number of rows to create in the buffer dataframe
        treatments <- rep("buffer", n_brow)
    }
    # Match double row, double rows, or dr
    else if(grepl("(^double rows?$|^dr$)", tolower(type))) {
        design$row <- (3*design$row)-1

        min_row <- min(design$row)
        min_col <- min(design$col)

        row <- c(rep(seq(min_row-1, (3*nrow)-2, by = 3),
                     each = ncol),
                 rep(seq(min_row+1, (3*nrow), by = 3),
                     each = ncol))
        col <- seq(min_col, ncol)
        n_brow <- length(row)  # Number of rows to create in the buffer dataframe
        treatments <- rep("buffer", n_brow)
    }
    # Match double col, double cols, double column, double columns, dc
    else if(grepl("(^double col(umn)?s?$|^dc$)", tolower(type))) {
        design$col <- (3*design$col)-1

        min_row <- min(design$row)
        min_col <- min(design$col)

        row <- seq(min_row, nrow)
        col <- c(rep(seq(min_col-1, (3*ncol)-2, by = 3),
                     each = nrow),
                 rep(seq(min_col+1, (3*ncol), by = 3),
                     each = nrow))
        n_brow <- length(col)  # Number of rows to create in the buffer dataframe
        treatments <- rep("buffer", n_brow)
    }
    # Match block, blocks, or b
    else if(grepl("(^blocks?$|^b$)", tolower(type))) {
        stop("Block buffers are not yet supported.")
    }
    else {
        stop("Invalid buffer option: ", type, call. = FALSE)
    }


    buffers <- data.frame(matrix(NA, nrow = n_brow, ncol = ncol(design)))
    buffers <- stats::setNames(buffers, names(design))
    buffers$row <- row
    buffers$col <- col
    buffers$treatments <- factor(treatments)

    if(blocks) {
        blocks_df <- stats::aggregate(cbind(row, col) ~ block, data = design, FUN = max)
        blocks_df$row[blocks_df$row==max(blocks_df$row)] <- max(blocks_df$row)+1
        blocks_df$col[blocks_df$col==max(blocks_df$col)] <- max(blocks_df$col)+1
        for(i in max(as.numeric(blocks_df$block)):1) {
            buffers[buffers$row <= blocks_df$row[i]&buffers$col <= blocks_df$col[i],"block"] <- blocks_df$block[i]
        }
    }

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
