# Given named-integer gap-count vectors (one entry per row/col index, values =
# number of buffer slots to insert after that position), compute the remapped
# coordinates and return the updated design plus the two mapping closures.
apply_coord_remap <- function(design, row_gap_counts, col_gap_counts) {
	row_min_all <- as.integer(names(row_gap_counts)[1]) + 1L
	col_min_all <- as.integer(names(col_gap_counts)[1]) + 1L

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

	design$row <- map_rows(design$row)
	design$col <- map_cols(design$col)

	list(design = design, map_rows = map_rows, map_cols = map_cols)
}

# Empty named gap-count vector covering every gap on one axis (one entry per
# index from min-1 to max). Names are gap positions; values start at zero.
empty_gap_counts <- function(min_all, max_all) {
	gaps <- (min_all - 1L):max_all
	stats::setNames(integer(length(gaps)), as.character(gaps))
}

# Cells (row, col) whose `by` group differs from the cell immediately above
# them (a horizontal group boundary sits in the gap after row `row - 1`).
row_group_boundaries <- function(design, by) {
	top <- design[, c("row", "col", by)]
	bottom <- design[, c("row", "col", by)]
	top$row <- top$row + 1L
	pairs <- merge(top, bottom, by = c("row", "col"), suffixes = c("_a", "_b"))
	pairs[pairs[[paste0(by, "_a")]] != pairs[[paste0(by, "_b")]], c("row", "col")]
}

# Cells (row, col) whose `by` group differs from the cell immediately to their
# left (a vertical group boundary sits in the gap after col `col - 1`).
col_group_boundaries <- function(design, by) {
	left <- design[, c("row", "col", by)]
	right <- design[, c("row", "col", by)]
	left$col <- left$col + 1L
	pairs <- merge(left, right, by = c("row", "col"), suffixes = c("_a", "_b"))
	pairs[pairs[[paste0(by, "_a")]] != pairs[[paste0(by, "_b")]], c("row", "col")]
}

# TRUE when `type` selects a block-based buffer (internal boundaries or full
# surrounding rings). These require a grouping column, so `by` is defaulted to
# "block" for them; the simple types treat `by` as opt-in.
is_block_buffer_type <- function(type) {
	grepl(
		paste0(
			"(^blocks?$|^b$|^double blocks?$|^entire blocks?$|",
			"^full blocks?$|^db$|^eb$|^fb$)"
		),
		tolower(type)
	)
}

#' Create buffers for design plots
#'
#' @param design The data frame of the design.
#' @param type The type of buffer. One of edge, row, column, double row,
#' double column, blocks (internal boundaries only), or double
#' block/entire block/full block (buffers fully surrounding each block).
#' @param by The name of a grouping column. Required for `type = "block"` and the
#' `"double block"` family (buffers are placed at the boundaries of this
#' column's groups). Optional for `type = "row"`/`"col"`: when supplied, a
#' buffer is inserted only where the `by` group changes between adjacent
#' rows/columns (rather than between every pair); when `NULL`, a buffer is
#' inserted between every pair. Ignored by `edge`, `double row`, and
#' `double col`. The `edge`/`row`/`col` (and double) types still assign buffer
#' cells to blocks by position when the design has a `"block"` column.
#'
#' @importFrom stats setNames aggregate
#'
#' @returns The original data frame, updated to include buffers
#' @keywords internal
create_buffers <- function(design, type, by = NULL) {
	n_rows <- max(design$row)
	n_cols <- max(design$col)

	# Each buffer type populates `buffer_cells` (buffer positions in the
	# *post-remap* coordinate space) and, where it shifts the existing plots to
	# make room, remaps `design$row`/`design$col` itself. A single assembly step
	# at the end aligns factor levels, assigns blocks, and binds the buffer rows
	# onto the design. `do_block_assignment` requests position-based block
	# assignment for the simple types; block-based types carry their own block
	# column in `buffer_cells` instead.
	buffer_cells <- NULL
	do_block_assignment <- FALSE

	# Match edge, edges or e
	if (grepl("(^edges?$|^e$)", tolower(type))) {
		design$row <- design$row + 1
		design$col <- design$col + 1

		row <- c(
			rep(1, n_cols + 2),
			rep(n_rows + 2, n_cols + 2),
			rep(2:(n_rows + 1), 2)
		)
		col <- c(
			rep(1:(n_cols + 2), 2),
			rep(1, n_rows),
			rep(n_cols + 2, n_rows)
		)
		buffer_cells <- data.frame(row = row, col = col)
		do_block_assignment <- "block" %in% names(design)
	} else if (grepl("(^rows?$|^r$)", tolower(type))) {
		# Match row, rows, r
		if (is.null(by)) {
			# Insert a buffer row between *every* pair of adjacent rows.
			design$row <- 2 * design$row

			min_row <- min(design$row)

			row <- rep(seq(min_row - 1, (2 * n_rows) + 1, by = 2), each = n_cols)
			col <- rep(seq(1, n_cols), times = n_rows + 1)
			buffer_cells <- data.frame(row = row, col = col)
			do_block_assignment <- "block" %in% names(design)
		} else {
			# Insert a full-width buffer row only where `by` changes between
			# vertically adjacent rows (e.g. between wholeplots).
			if (!(by %in% names(design))) {
				stop(
					"`by` must name a column in the design; \"",
					by,
					"\" was not found.",
					call. = FALSE
				)
			}

			row_min_all <- min(design$row)
			row_max_all <- max(design$row)
			col_min_all <- min(design$col)
			col_max_all <- max(design$col)

			boundary_gaps <- sort(unique(
				as.integer(row_group_boundaries(design, by)$row) - 1L
			))

			row_gap_counts <- empty_gap_counts(row_min_all, row_max_all)
			col_gap_counts <- empty_gap_counts(col_min_all, col_max_all)
			if (length(boundary_gaps) > 0) {
				row_gap_counts[as.character(boundary_gaps)] <- 1L
			}

			remap <- apply_coord_remap(design, row_gap_counts, col_gap_counts)
			design <- remap$design
			map_rows <- remap$map_rows
			map_cols <- remap$map_cols

			if (length(boundary_gaps) > 0) {
				buffer_cells <- expand.grid(
					row = map_rows(boundary_gaps) + 1L,
					col = map_cols(col_min_all:col_max_all)
				)
			}
		}
	} else if (grepl("(^col(umn)?s?$|^c$)", tolower(type))) {
		# Match col, cols, column, columns or c
		if (is.null(by)) {
			# Insert a buffer column between *every* pair of adjacent columns.
			design$col <- 2 * design$col

			min_col <- min(design$col)

			row <- rep(seq(1, n_rows), times = n_cols + 1)
			col <- rep(seq(min_col - 1, (2 * n_cols) + 1, by = 2), each = n_rows)
			buffer_cells <- data.frame(row = row, col = col)
			do_block_assignment <- "block" %in% names(design)
		} else {
			# Insert a full-height buffer column only where `by` changes between
			# horizontally adjacent columns.
			if (!(by %in% names(design))) {
				stop(
					"`by` must name a column in the design; \"",
					by,
					"\" was not found.",
					call. = FALSE
				)
			}

			row_min_all <- min(design$row)
			row_max_all <- max(design$row)
			col_min_all <- min(design$col)
			col_max_all <- max(design$col)

			boundary_gaps <- sort(unique(
				as.integer(col_group_boundaries(design, by)$col) - 1L
			))

			row_gap_counts <- empty_gap_counts(row_min_all, row_max_all)
			col_gap_counts <- empty_gap_counts(col_min_all, col_max_all)
			if (length(boundary_gaps) > 0) {
				col_gap_counts[as.character(boundary_gaps)] <- 1L
			}

			remap <- apply_coord_remap(design, row_gap_counts, col_gap_counts)
			design <- remap$design
			map_rows <- remap$map_rows
			map_cols <- remap$map_cols

			if (length(boundary_gaps) > 0) {
				buffer_cells <- expand.grid(
					row = map_rows(row_min_all:row_max_all),
					col = map_cols(boundary_gaps) + 1L
				)
			}
		}
	} else if (grepl("(^double rows?$|^dr$)", tolower(type))) {
		# Match double row, double rows, or dr
		design$row <- (3 * design$row) - 1

		min_row <- min(design$row)
		min_col <- min(design$col)

		row <- c(
			rep(seq(min_row - 1, (3 * n_rows) - 2, by = 3), each = n_cols),
			rep(seq(min_row + 1, (3 * n_rows), by = 3), each = n_cols)
		)
		col <- seq(min_col, n_cols)
		# col is shorter than row and recycles across the buffer rows
		buffer_cells <- data.frame(row = row, col = col)
		do_block_assignment <- "block" %in% names(design)
	} else if (grepl("(^double col(umn)?s?$|^dc$)", tolower(type))) {
		# Match double col, double cols, double column, double columns, dc
		design$col <- (3 * design$col) - 1

		min_row <- min(design$row)
		min_col <- min(design$col)

		row <- seq(min_row, n_rows)
		col <- c(
			rep(seq(min_col - 1, (3 * n_cols) - 2, by = 3), each = n_rows),
			rep(seq(min_col + 1, (3 * n_cols), by = 3), each = n_rows)
		)
		# row is shorter than col and recycles across the buffer rows
		buffer_cells <- data.frame(row = row, col = col)
		do_block_assignment <- "block" %in% names(design)
	} else if (
		grepl(
			"(^double blocks?$|^entire blocks?$|^full blocks?$|^db$|^eb$|^fb$)",
			tolower(type)
		)
	) {
		# Match double block(s), entire block(s), full block(s), db, eb, or fb
		if (is.null(by) || !(by %in% names(design))) {
			stop(
				"Block buffers require a grouping column. Set `by` to the name ",
				"of a column in the design (e.g. \"block\").",
				call. = FALSE
			)
		}

		# Ensure we can safely add a buffer level
		if (!is.factor(design$treatments)) {
			design$treatments <- factor(design$treatments)
		}
		if (!("buffer" %in% levels(design$treatments))) {
			levels(design$treatments) <- c(levels(design$treatments), "buffer")
		}

		# Per-group extents (assumes groups form rectangular regions on the row/col grid)
		min_row <- tapply(design$row, design[[by]], min)
		max_row <- tapply(design$row, design[[by]], max)
		min_col <- tapply(design$col, design[[by]], min)
		max_col <- tapply(design$col, design[[by]], max)

		row_min_all <- min(design$row)
		row_max_all <- max(design$row)
		col_min_all <- min(design$col)
		col_max_all <- max(design$col)

		# Build gap insert counts. We insert one buffer row/col for each block edge.
		# Where two blocks are adjacent, this yields two buffer rows/cols (one per block).
		row_gap_counts <- empty_gap_counts(row_min_all, row_max_all)
		col_gap_counts <- empty_gap_counts(col_min_all, col_max_all)

		add_gap_counts <- function(counts, gaps) {
			gaps <- as.character(as.integer(gaps))
			idx <- match(gaps, names(counts))
			idx <- idx[!is.na(idx)]
			if (length(idx) > 0) {
				counts[idx] <- counts[idx] + 1L
			}
			counts
		}

		row_gap_counts <- add_gap_counts(
			row_gap_counts,
			as.integer(min_row) - 1L
		)
		row_gap_counts <- add_gap_counts(row_gap_counts, as.integer(max_row))
		col_gap_counts <- add_gap_counts(
			col_gap_counts,
			as.integer(min_col) - 1L
		)
		col_gap_counts <- add_gap_counts(col_gap_counts, as.integer(max_col))

		remap <- apply_coord_remap(design, row_gap_counts, col_gap_counts)
		design <- remap$design
		map_rows <- remap$map_rows
		map_cols <- remap$map_cols

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
			stopifnot(
				r_top < rmin_new,
				r_bot > rmax_new,
				c_left < cmin_new,
				c_right > cmax_new
			)

			top <- expand.grid(row = r_top, col = c_left:c_right)
			bottom <- expand.grid(row = r_bot, col = c_left:c_right)
			left <- expand.grid(row = r_top:r_bot, col = c_left)
			right <- expand.grid(row = r_top:r_bot, col = c_right)

			ring <- unique(rbind(top, bottom, left, right))
			ring$treatments <- factor(
				"buffer",
				levels = levels(design$treatments)
			)

			# Preserve grouping-column type (factor vs numeric) from the design
			if (is.factor(design[[by]])) {
				ring[[by]] <- factor(b, levels = levels(design[[by]]))
			} else {
				ring[[by]] <- as.numeric(b)
			}

			buffers_list[[i]] <- ring
		}

		buffers_rcb <- do.call(rbind, buffers_list)

		# Block-based type: carry the per-ring group assignment through to assembly
		buffer_cells <- buffers_rcb[, c("row", "col", by)]
	} else if (grepl("(^blocks?$|^b$)", tolower(type))) {
		# Match block, blocks, or b (internal group boundaries only)
		if (is.null(by) || !(by %in% names(design))) {
			stop(
				"Block buffers require a grouping column. Set `by` to the name ",
				"of a column in the design (e.g. \"block\").",
				call. = FALSE
			)
		}

		# Ensure we can safely add a buffer level
		if (!is.factor(design$treatments)) {
			design$treatments <- factor(design$treatments)
		}
		if (!("buffer" %in% levels(design$treatments))) {
			levels(design$treatments) <- c(levels(design$treatments), "buffer")
		}

		row_min_all <- min(design$row)
		row_max_all <- max(design$row)
		col_min_all <- min(design$col)
		col_max_all <- max(design$col)

		# Internal boundaries where adjacent plots belong to different groups.
		row_boundary_cells <- row_group_boundaries(design, by)
		col_boundary_cells <- col_group_boundaries(design, by)

		row_boundary_gaps <- sort(unique(
			as.integer(row_boundary_cells$row) - 1L
		))
		col_boundary_gaps <- sort(unique(
			as.integer(col_boundary_cells$col) - 1L
		))

		row_gap_counts <- empty_gap_counts(row_min_all, row_max_all)
		col_gap_counts <- empty_gap_counts(col_min_all, col_max_all)

		if (length(row_boundary_gaps) > 0) {
			row_gap_counts[as.character(row_boundary_gaps)] <- 1L
		}
		if (length(col_boundary_gaps) > 0) {
			col_gap_counts[as.character(col_boundary_gaps)] <- 1L
		}

		remap <- apply_coord_remap(design, row_gap_counts, col_gap_counts)
		design <- remap$design
		map_rows <- remap$map_rows
		map_cols <- remap$map_cols

		buffers_row <- data.frame(row = integer(0), col = integer(0))
		if (nrow(row_boundary_cells) > 0) {
			buffers_row <- data.frame(
				row = map_rows(as.integer(row_boundary_cells$row) - 1L) + 1L,
				col = map_cols(as.integer(row_boundary_cells$col))
			)
		}

		buffers_col <- data.frame(row = integer(0), col = integer(0))
		if (nrow(col_boundary_cells) > 0) {
			buffers_col <- data.frame(
				row = map_rows(as.integer(col_boundary_cells$row)),
				col = map_cols(as.integer(col_boundary_cells$col) - 1L) + 1L
			)
		}

		# Internal boundaries only: no block assignment for the buffer cells
		buffer_cells <- unique(rbind(buffers_row, buffers_col))
	} else {
		stop("Invalid buffer option: ", type, call. = FALSE)
	}

	# --- Assembly (shared by all buffer types) ---------------------------------
	# Nothing to add (e.g. a blocked design with no internal boundaries)
	if (is.null(buffer_cells) || nrow(buffer_cells) == 0) {
		return(design)
	}

	# Ensure the design's treatments factor can carry a "buffer" level so the
	# buffer cells bind cleanly without coercion warnings.
	if (!is.factor(design$treatments)) {
		design$treatments <- factor(design$treatments)
	}
	if (!("buffer" %in% levels(design$treatments))) {
		levels(design$treatments) <- c(levels(design$treatments), "buffer")
	}

	# Expand buffer cells to match all design columns (other fields stay NA)
	buffers <- data.frame(matrix(
		NA,
		nrow = nrow(buffer_cells),
		ncol = ncol(design)
	))
	buffers <- stats::setNames(buffers, names(design))
	buffers$row <- buffer_cells$row
	buffers$col <- buffer_cells$col
	buffers$treatments <- factor("buffer", levels = levels(design$treatments))

	if (!is.null(by) && by %in% names(buffer_cells)) {
		# Block-based types carry their own per-cell group assignment
		buffers[[by]] <- buffer_cells[[by]]
	} else if (do_block_assignment && "block" %in% names(design)) {
		# Simple types on a blocked design: assign each buffer cell to a block
		# by position (assumes rectangular, spatially-ordered blocks).
		blocks_df <- stats::aggregate(
			cbind(row, col) ~ block,
			data = design,
			FUN = max
		)
		blocks_df$row[blocks_df$row == max(blocks_df$row)] <- max(
			blocks_df$row
		) +
			1
		blocks_df$col[blocks_df$col == max(blocks_df$col)] <- max(
			blocks_df$col
		) +
			1
		for (i in max(as.numeric(blocks_df$block)):1) {
			buffers[
				buffers$row <= blocks_df$row[i] &
					buffers$col <= blocks_df$col[i],
				"block"
			] <- blocks_df$block[i]
		}
	}

	design <- rbind(design, buffers)

	return(design)
}

#' Add buffers to an existing design
#'
#' @param design_obj A design object (with class "design") from the design() function
#' @param type The type of buffer to add. One of 'edge', 'row', 'column',
#' 'double row', 'double column', 'blocks', or
#' 'double block'/'entire block'/'full block'.
#' @param by The name of a grouping column. Used by the block-based buffer types
#' (`"block"` and the `"double block"` family), and optionally by `"row"`/
#' `"column"` to insert a buffer only where the group changes between adjacent
#' rows/columns (e.g. between wholeplots in a split-plot design). Defaults to
#' `"block"` when the design has a `"block"` column, otherwise `NULL`.
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
add_buffers <- function(design_obj, type, by = NULL) {
	stopifnot(inherits(design_obj, "design"))

	# Default the grouping column to "block" for the block-based types when the
	# design has one. For row/col, `by` stays opt-in so the default remains a
	# buffer between every row/column.
	if (
		is.null(by) &&
			is_block_buffer_type(type) &&
			"block" %in% names(design_obj$design)
	) {
		by <- "block"
	}

	# Create buffers and update the design dataframe
	design_obj$design <- create_buffers(
		design_obj$design,
		type,
		by = by
	)

	# Regenerate the plot with the updated design including buffers
	if ("plot.des" %in% names(design_obj)) {
		design_obj$plot.des <- autoplot(design_obj)
	}

	return(design_obj)
}
