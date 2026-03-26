#' Create buffers for design plots
#'
#' @param design The data frame of the design.
#' @param type The type of buffer. One of edge, row, column, double row, or double column.
#' @param blocks Does the design data frame contain blocks?
#'
#' @importFrom stats setNames aggregate
#'
#' @returns The original data frame, updated to include buffers
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
        if (!blocks || !("block" %in% names(design))) {
            stop("Block buffers require a 'block' column in the design.", call. = FALSE)
        }

        # Ensure we can safely add a buffer level
        if (!is.factor(design$treatments)) {
            design$treatments <- factor(design$treatments)
        }
        if (!("buffer" %in% levels(design$treatments))) {
            levels(design$treatments) <- c(levels(design$treatments), "buffer")
        }

        # Per-block extents (assumes blocks form rectangular regions on the row/col grid)
        min_row <- tapply(design$row, design$block, min)
        max_row <- tapply(design$row, design$block, max)
        min_col <- tapply(design$col, design$block, min)
        max_col <- tapply(design$col, design$block, max)

        row_min_all <- min(design$row)
        row_max_all <- max(design$row)
        col_min_all <- min(design$col)
        col_max_all <- max(design$col)

        # Build gap insert counts. We insert one buffer row/col for each block edge.
        # Where two blocks are adjacent, this yields two buffer rows/cols (one per block).
        row_gaps <- (row_min_all - 1L):row_max_all
        col_gaps <- (col_min_all - 1L):col_max_all
        row_gap_counts <- integer(length(row_gaps))
        col_gap_counts <- integer(length(col_gaps))
        names(row_gap_counts) <- as.character(row_gaps)
        names(col_gap_counts) <- as.character(col_gaps)

        add_gap_counts <- function(counts, gaps) {
            gaps <- as.character(as.integer(gaps))
            idx <- match(gaps, names(counts))
            idx <- idx[!is.na(idx)]
            if (length(idx) > 0) counts[idx] <- counts[idx] + 1L
            counts
        }

        row_gap_counts <- add_gap_counts(row_gap_counts, as.integer(min_row) - 1L)
        row_gap_counts <- add_gap_counts(row_gap_counts, as.integer(max_row))
        col_gap_counts <- add_gap_counts(col_gap_counts, as.integer(min_col) - 1L)
        col_gap_counts <- add_gap_counts(col_gap_counts, as.integer(max_col))

        row_gap_prefix <- cumsum(row_gap_counts)
        col_gap_prefix <- cumsum(col_gap_counts)

        map_rows <- function(v) {
            v <- as.integer(v)
            idx <- match(as.character(v - 1L), names(row_gap_counts))
            inserted_before <- row_gap_prefix[idx]
            (v - row_min_all + 1L) + inserted_before
        }

        map_cols <- function(v) {
            v <- as.integer(v)
            idx <- match(as.character(v - 1L), names(col_gap_counts))
            inserted_before <- col_gap_prefix[idx]
            (v - col_min_all + 1L) + inserted_before
        }

        # Helper to get the inserted buffer coordinate adjacent to a block edge
        top_buffer_row <- function(min_r) {
            gap_k <- as.integer(min_r) - 1L
            n_ins <- row_gap_counts[[as.character(gap_k)]]
            base <- if (gap_k < row_min_all) 0L else map_rows(gap_k)
            base + n_ins # closest inserted row to the block interior
        }
        bottom_buffer_row <- function(max_r) {
            gap_k <- as.integer(max_r)
            base <- map_rows(gap_k)
            base + 1L # first inserted row after max_r
        }
        left_buffer_col <- function(min_c) {
            gap_k <- as.integer(min_c) - 1L
            n_ins <- col_gap_counts[[as.character(gap_k)]]
            base <- if (gap_k < col_min_all) 0L else map_cols(gap_k)
            base + n_ins
        }
        right_buffer_col <- function(max_c) {
            gap_k <- as.integer(max_c)
            base <- map_cols(gap_k)
            base + 1L
        }

        # Remap existing design coordinates to make space for the buffers
        design$row <- map_rows(design$row)
        design$col <- map_cols(design$col)

        # Create per-block buffer rings (one-cell thick)
        buffers_list <- vector("list", length(min_row))
        bnames <- names(min_row)
        for (i in seq_along(bnames)) {
            b <- bnames[i]
            rmin <- as.integer(min_row[[b]])
            rmax <- as.integer(max_row[[b]])
            cmin <- as.integer(min_col[[b]])
            cmax <- as.integer(max_col[[b]])

            rmin_new <- map_rows(rmin)
            rmax_new <- map_rows(rmax)
            cmin_new <- map_cols(cmin)
            cmax_new <- map_cols(cmax)

            r_top <- top_buffer_row(rmin)
            r_bot <- bottom_buffer_row(rmax)
            c_left <- left_buffer_col(cmin)
            c_right <- right_buffer_col(cmax)

            # Sanity: ensure ring surrounds interior
            stopifnot(r_top < rmin_new, r_bot > rmax_new, c_left < cmin_new, c_right > cmax_new)

            top <- expand.grid(row = r_top, col = c_left:c_right)
            bottom <- expand.grid(row = r_bot, col = c_left:c_right)
            left <- expand.grid(row = r_top:r_bot, col = c_left)
            right <- expand.grid(row = r_top:r_bot, col = c_right)

            ring <- unique(rbind(top, bottom, left, right))
            ring$treatments <- factor("buffer", levels = levels(design$treatments))

            # Preserve block type (factor vs numeric) from the design
            if (is.factor(design$block)) {
                ring$block <- factor(b, levels = levels(design$block))
            } else {
                ring$block <- as.numeric(b)
            }

            buffers_list[[i]] <- ring
        }

        buffers_rcb <- do.call(rbind, buffers_list)

        # Expand to match all design columns (other fields stay NA)
        buffers <- data.frame(matrix(NA, nrow = nrow(buffers_rcb), ncol = ncol(design)))
        buffers <- stats::setNames(buffers, names(design))
        buffers$row <- buffers_rcb$row
        buffers$col <- buffers_rcb$col
        buffers$treatments <- buffers_rcb$treatments
        buffers$block <- buffers_rcb$block

        design <- rbind(design, buffers)
        return(design)
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

#' Add buffers to an existing design
#'
#' @param design_obj A design object (with class "design") from the design() function
#' @param type The type of buffer to add. One of 'edge', 'row', 'column', 'double row', or 'double column'.
#' @returns The modified design object with buffers added
#' 
#' @examples
#' # Create a simple CRD design
#' des <- design(type = "crd", treatments = c("A", "B"), reps = 3, nrows = 2, ncols = 3, seed = 42)
#' 
#' # Plot the original design
#' autoplot(des)
#' 
#' # Add edge buffers to the design
#' des_buf <- add_buffers(des, type = "edge")
#' 
#' # Plot the design with buffers
#' autoplot(des_buf)
#' 
#' # Add double row buffers
#' des_row_buf <- add_buffers(des, type = "double row")
#' autoplot(des_row_buf)
#' 
#' @export
add_buffers <- function(design_obj, type) {
    stopifnot(inherits(design_obj, "design"))

    # Determine if design has blocks
    has_blocks <- any(grepl("block", tolower(names(design_obj$design))))

    # Create buffers and update the design dataframe
    design_obj$design <- create_buffers(design_obj$design, type, blocks = has_blocks)
    
    # Regenerate the plot with the updated design including buffers
    if("plot.des" %in% names(design_obj)) {
        design_obj$plot.des <- autoplot(design_obj)
    }

    return(design_obj)
}
