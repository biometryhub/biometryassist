test_that("create_buffers adds edge buffers correctly", {
	design <- data.frame(
		row = rep(1:2, each = 2),
		col = rep(1:2, 2),
		treatments = c("A", "B", "C", "D")
	)
	out <- create_buffers(design, type = "edge")
	expect_true(any(out$treatments == "buffer"))
	expect_true(all(c(1, max(out$row)) %in% out$row))
	expect_true(all(c(1, max(out$col)) %in% out$col))
})

test_that("create_buffers adds row buffers correctly", {
	design <- data.frame(
		row = rep(1:2, each = 2),
		col = rep(1:2, 2),
		treatments = c("A", "B", "C", "D")
	)
	out <- create_buffers(design, type = "row")
	expect_true(any(out$treatments == "buffer"))
	expect_true(all(seq(1, max(out$row), by = 2) %in% out$row))
})

test_that("create_buffers adds column buffers correctly", {
	design <- data.frame(
		row = rep(1:2, each = 2),
		col = rep(1:2, 2),
		treatments = c("A", "B", "C", "D")
	)
	out <- create_buffers(design, type = "col")
	expect_true(any(out$treatments == "buffer"))
	expect_true(all(seq(1, max(out$col), by = 2) %in% out$col))
})

test_that("create_buffers adds double row buffers correctly", {
	design <- data.frame(
		row = rep(1:2, each = 2),
		col = rep(1:2, 2),
		treatments = c("A", "B", "C", "D")
	)
	out <- create_buffers(design, type = "double row")
	expect_true(any(out$treatments == "buffer"))
})

test_that("create_buffers adds double column buffers correctly", {
	design <- data.frame(
		row = rep(1:2, each = 2),
		col = rep(1:2, 2),
		treatments = c("A", "B", "C", "D")
	)
	out <- create_buffers(design, type = "double col")
	expect_true(any(out$treatments == "buffer"))
})

test_that("create_buffers errors for unsupported buffer type", {
	design <- data.frame(
		row = rep(1:2, each = 2),
		col = rep(1:2, 2),
		treatments = c("A", "B", "C", "D")
	)
	expect_error(
		create_buffers(design, type = "block", by = "block"),
		"Block buffers require a grouping column"
	)
	expect_error(
		create_buffers(design, type = "db", by = "block"),
		"Block buffers require a grouping column"
	)
	# A NULL `by` is also an error for block-based types
	expect_error(
		create_buffers(design, type = "block"),
		"Block buffers require a grouping column"
	)
	expect_error(
		create_buffers(design, type = "notatype"),
		"Invalid buffer option"
	)
})

test_that("create_buffers adds block buffers correctly", {
	design <- data.frame(
		row = rep(1:4, each = 2),
		col = rep(1:2, 4),
		treatments = paste0("T", seq_len(8)),
		block = rep(c(1, 2), each = 4)
	)

	out <- create_buffers(design, type = "block", by = "block")

	expect_true(any(out$treatments == "buffer"))
	expect_true(any(is.na(out$block)))

	# Should expand rows to make room for buffers
	expect_true(max(out$row) > max(design$row))
	expect_true(max(out$col) == max(design$col))

	# No duplicated row/col coordinates
	expect_equal(nrow(out), nrow(unique(out[c("row", "col")])))
})

test_that("create_buffers adds internal block buffers along column boundaries", {
	# Two blocks split left/right to force a column boundary.
	design <- data.frame(
		row = rep(1:2, each = 2),
		col = rep(1:2, times = 2),
		treatments = paste0("T", seq_len(4)),
		block = c(1, 2, 1, 2)
	)

	out <- create_buffers(design, type = "block", by = "block")

	expect_true(any(out$treatments == "buffer"))
	# Internal boundary buffers should have NA block values
	expect_true(any(out$treatments == "buffer" & is.na(out$block)))
	# Column boundary insertion should expand columns
	expect_true(max(out$col) > max(design$col))
	# No duplicated row/col coordinates
	expect_equal(nrow(out), nrow(unique(out[c("row", "col")])))
})

test_that("create_buffers handles pure column boundary (1x2 grid)", {
	# Minimal case: one row, two columns, different blocks left/right.
	# Guarantees col_boundary_gaps is non-empty (hits col_gap_counts[...] <- 1L).
	design <- data.frame(
		row = c(1, 1),
		col = c(1, 2),
		treatments = c("A", "B"),
		block = c(1, 2)
	)

	out <- create_buffers(design, type = "block", by = "block")

	expect_true(any(out$treatments == "buffer"))
	expect_true(any(out$treatments == "buffer" & is.na(out$block)))
	expect_true(max(out$col) > max(design$col))
	expect_equal(nrow(out), nrow(unique(out[c("row", "col")])))
})

test_that("create_buffers returns design unchanged when no internal boundaries", {
	# All plots are in the same block; there are no internal boundaries, so
	# buffers_rc is empty and the function returns early.
	design <- data.frame(
		row = rep(1:2, each = 2),
		col = rep(1:2, times = 2),
		treatments = c("A", "B", "C", "D"),
		block = 1
	)

	out <- create_buffers(design, type = "block", by = "block")

	expect_false(any(out$treatments == "buffer"))
	expect_equal(out$row, design$row)
	expect_equal(out$col, design$col)
	expect_equal(out$block, design$block)
	expect_true(is.factor(out$treatments))
	expect_true("buffer" %in% levels(out$treatments))
})

test_that("create_buffers adds double/entire/full block buffers (numeric block)", {
	# 4 blocks arranged in a 2x2 grid to exercise both:
	# - blocks on the outer edge (gap_k < row_min_all/col_min_all branches)
	# - internal adjacency (double-inserted separator rows/cols)
	grid <- expand.grid(row = 1:4, col = 1:4)
	grid$block <- with(
		grid,
		ifelse(
			row <= 2 & col <= 2,
			1,
			ifelse(row <= 2 & col >= 3, 2, ifelse(row >= 3 & col <= 2, 3, 4))
		)
	)
	grid$treatments <- paste0("T", seq_len(nrow(grid)))

	type_aliases <- c(
		"double blocks",
		"entire block",
		"full blocks",
		"db",
		"eb",
		"fb"
	)
	for (tp in type_aliases) {
		out <- create_buffers(grid, type = tp, by = "block")

		expect_true(any(out$treatments == "buffer"))
		expect_false(any(is.na(out$block)))

		# Should expand both rows and cols to make room for the surrounding rings
		expect_true(max(out$row) > max(grid$row))
		expect_true(max(out$col) > max(grid$col))

		# No duplicated row/col coordinates
		expect_equal(nrow(out), nrow(unique(out[c("row", "col")])))
	}
})

test_that("create_buffers adds double/entire/full block buffers (factor block, factor treatments)", {
	grid <- expand.grid(row = 1:4, col = 1:4)
	grid$block <- factor(with(
		grid,
		ifelse(
			row <= 2 & col <= 2,
			1,
			ifelse(row <= 2 & col >= 3, 2, ifelse(row >= 3 & col <= 2, 3, 4))
		)
	))

	# Treatments already factor and already include buffer level: exercises the
	# "skip" paths for the factor/level checks.
	trt_levels <- c(paste0("T", seq_len(nrow(grid))), "buffer")
	grid$treatments <- factor(
		paste0("T", seq_len(nrow(grid))),
		levels = trt_levels
	)

	out <- create_buffers(grid, type = "double block", by = "block")
	expect_true(any(out$treatments == "buffer"))
	expect_false(any(is.na(out$block)))
	expect_true(max(out$row) > max(grid$row))
	expect_true(max(out$col) > max(grid$col))
	expect_equal(nrow(out), nrow(unique(out[c("row", "col")])))
})

test_that("create_buffers block types work with an arbitrary `by` column", {
	# `by` is not hardcoded to "block": a "wholeplot" grouping column works too.
	grid <- expand.grid(row = 1:4, col = 1:4)
	grid$wholeplot <- with(
		grid,
		ifelse(
			row <= 2 & col <= 2,
			1,
			ifelse(row <= 2 & col >= 3, 2, ifelse(row >= 3 & col <= 2, 3, 4))
		)
	)
	grid$treatments <- paste0("T", seq_len(nrow(grid)))

	# double block: surrounding rings, group carried in the `wholeplot` column
	out_db <- create_buffers(grid, type = "double block", by = "wholeplot")
	expect_true(any(out_db$treatments == "buffer"))
	expect_false(any(is.na(out_db$wholeplot)))
	expect_true(max(out_db$row) > max(grid$row))
	expect_true(max(out_db$col) > max(grid$col))
	expect_equal(nrow(out_db), nrow(unique(out_db[c("row", "col")])))

	# internal boundaries: buffer cells carry NA group, coordinates expand
	out_b <- create_buffers(grid, type = "block", by = "wholeplot")
	expect_true(any(out_b$treatments == "buffer"))
	expect_true(any(out_b$treatments == "buffer" & is.na(out_b$wholeplot)))
	expect_equal(nrow(out_b), nrow(unique(out_b[c("row", "col")])))
})

test_that("add_buffers works for design objects", {
	design_obj <- design(
		type = "crd",
		treatments = c(1, 5, 10, 20),
		reps = 5,
		nrows = 4,
		ncols = 5,
		seed = 42,
		quiet = TRUE
	)
	out <- add_buffers(design_obj, type = "edge")
	expect_s3_class(out, "design")
	expect_true(any(out$design$treatments == "buffer"))
})

test_that("add_buffers works for block buffers", {
	design_obj <- design(
		type = "rcbd",
		treatments = LETTERS[1:4],
		reps = 2,
		nrows = 4,
		ncols = 2,
		brows = 2,
		bcols = 2,
		seed = 42,
		quiet = TRUE
	)
	out <- add_buffers(design_obj, type = "block")
	expect_s3_class(out, "design")
	expect_true(any(out$design$treatments == "buffer"))
})

# ---------------------------------------------------------------------------
# Exact-coordinate characterisation tests. These lock in the precise remapping
# behaviour for each buffer type on a known 2x2 (or 4x2 blocked) design, so a
# regression in the coordinate maths is caught rather than masked by the looser
# "buffers exist / max increased" structural checks above.
# ---------------------------------------------------------------------------

# Simple 2x2 design: A=(1,1) B=(1,2) C=(2,1) D=(2,2)
simple_2x2 <- function() {
	data.frame(
		row = rep(1:2, each = 2),
		col = rep(1:2, 2),
		treatments = c("A", "B", "C", "D")
	)
}

# 4x2 design split into two row-blocks: block 1 = rows 1-2, block 2 = rows 3-4
blocked_4x2 <- function() {
	data.frame(
		row = rep(1:4, each = 2),
		col = rep(1:2, 4),
		treatments = paste0("T", 1:8),
		block = rep(c(1, 2), each = 4)
	)
}

test_that("edge buffers land at exact coordinates", {
	out <- create_buffers(simple_2x2(), type = "edge")

	# Existing plots shift inward by one; A moves from (1,1) to (2,2)
	expect_equal(out[out$treatments == "A", "row"], 2)
	expect_equal(out[out$treatments == "A", "col"], 2)

	# A one-plot border surrounds the field (rows/cols 1 and 4)
	expect_equal(min(out$row), 1)
	expect_equal(max(out$row), 4)
	expect_equal(min(out$col), 1)
	expect_equal(max(out$col), 4)

	buf <- out[out$treatments == "buffer", c("row", "col")]
	# Top-left corner is a buffer; the interior A cell is not
	expect_true(any(buf$row == 1 & buf$col == 1))
	expect_false(any(buf$row == 2 & buf$col == 2))
})

test_that("row buffers land at exact coordinates", {
	out <- create_buffers(simple_2x2(), type = "row")

	# Row coordinates double: data rows move to 2 and 4
	expect_equal(out[out$treatments == "A", "row"], 2)
	expect_equal(out[out$treatments == "C", "row"], 4)
	# Columns are untouched
	expect_equal(out[out$treatments == "A", "col"], 1)

	# Buffer rows sit at 1, 3, 5 (around and between the data rows)
	buf_rows <- sort(unique(out$row[out$treatments == "buffer"]))
	expect_equal(buf_rows, c(1, 3, 5))
})

test_that("column buffers land at exact coordinates", {
	out <- create_buffers(simple_2x2(), type = "col")

	# Column coordinates double: data cols move to 2 and 4
	expect_equal(out[out$treatments == "A", "col"], 2)
	expect_equal(out[out$treatments == "B", "col"], 4)
	# Rows are untouched
	expect_equal(out[out$treatments == "A", "row"], 1)

	buf_cols <- sort(unique(out$col[out$treatments == "buffer"]))
	expect_equal(buf_cols, c(1, 3, 5))
})

test_that("double row buffers land at exact coordinates", {
	out <- create_buffers(simple_2x2(), type = "double row")

	# Rows map as 3r - 1: data rows move to 2 and 5
	expect_equal(out[out$treatments == "A", "row"], 2)
	expect_equal(out[out$treatments == "C", "row"], 5)

	# Two buffer rows between/around the data: rows 1, 3, 4, 6
	buf_rows <- sort(unique(out$row[out$treatments == "buffer"]))
	expect_equal(buf_rows, c(1, 3, 4, 6))
})

test_that("double column buffers land at exact coordinates", {
	out <- create_buffers(simple_2x2(), type = "double col")

	# Cols map as 3c - 1: data cols move to 2 and 5
	expect_equal(out[out$treatments == "A", "col"], 2)
	expect_equal(out[out$treatments == "B", "col"], 5)

	buf_cols <- sort(unique(out$col[out$treatments == "buffer"]))
	expect_equal(buf_cols, c(1, 3, 4, 6))
})

test_that("internal block buffers land at exact coordinates", {
	out <- create_buffers(blocked_4x2(), type = "block", by = "block")

	# Block 1 (rows 1-2) is unchanged; block 2 (rows 3-4) shifts to 4-5 to make
	# room for the single inserted separator row.
	expect_equal(out[out$treatments == "T1", "row"], 1)
	expect_equal(out[out$treatments == "T3", "row"], 2)
	expect_equal(out[out$treatments == "T5", "row"], 4)
	expect_equal(out[out$treatments == "T7", "row"], 5)

	# One buffer row at row 3 spanning both columns, with NA block
	buf <- out[out$treatments == "buffer", ]
	expect_equal(sort(unique(buf$row)), 3)
	expect_equal(sort(unique(buf$col)), c(1, 2))
	expect_true(all(is.na(buf$block)))
})

test_that("double block buffers land at exact coordinates", {
	out <- create_buffers(blocked_4x2(), type = "double block", by = "block")

	# Each block gains a surrounding ring; interiors are pushed out accordingly.
	# T1 (orig 1,1) -> (2,2); T8 (orig 4,2) -> (7,3).
	expect_equal(out[out$treatments == "T1", "row"], 2)
	expect_equal(out[out$treatments == "T1", "col"], 2)
	expect_equal(out[out$treatments == "T8", "row"], 7)
	expect_equal(out[out$treatments == "T8", "col"], 3)
	expect_equal(max(out$row), 8)
	expect_equal(max(out$col), 4)

	# The top-left corner is a buffer belonging to block 1's ring
	corner <- out[out$row == 1 & out$col == 1, ]
	expect_equal(as.character(corner$treatments), "buffer")
	expect_equal(corner$block, 1)
})
