#' Export Comparison Results to an Excel Workbook
#'
#' Writes the results of [multiple_comparisons()], [pairwise_comparisons()]
#' and/or [reference_comparisons()] to a worksheet of an Excel workbook, as a
#' set of titled tables ready to share with collaborators. Each call writes one
#' sheet; call it repeatedly with different `sheet` names to build up a
#' workbook with one tab per response variable (or per analysis).
#'
#' @param x A comparison object: the output of [multiple_comparisons()] (class
#'   `mct`), [pairwise_comparisons()] or [reference_comparisons()]. Or a list
#'   of such objects, which are written one below another on the same sheet.
#'   The list names are used as the section titles; unnamed elements are given
#'   a title describing their contents (e.g. "Means by treatment").
#' @param filename Path to the `.xlsx` workbook. If the file does not exist it
#'   is created; if it does, it is opened and edited in place, keeping all
#'   other sheets. Default `"multiple_comparisons.xlsx"` in the working
#'   directory.
#' @param sheet Name of the worksheet to write. If a sheet of that name already
#'   exists (ignoring case) it is replaced, in the same tab position. The
#'   default, `NULL`, uses the response variable name recorded on the first
#'   object in `x`, falling back to `"Comparisons"`. Characters Excel does not
#'   allow in sheet names (`\ / ? * [ ] :`) are replaced with `_`, and the name
#'   is truncated to Excel's 31-character limit.
#' @param title Heading written in cell A1 of the sheet. Defaults to the sheet
#'   name as supplied (before cleaning and truncation), so a long heading can
#'   be given here with a shorter `sheet` name.
#' @param friendly_names Logical. If `TRUE` (the default) the estimate columns
#'   are renamed to be clearer to readers unfamiliar with the package output
#'   (see Details). Set `FALSE` to keep the column names exactly as the
#'   comparison functions return them.
#'
#' @details
#' Each section of the sheet consists of a title, a one-row summary of how the
#' comparisons were made, and the results as a formatted Excel table.
#'
#' The summary row records the multiplicity adjustment and significance level.
#' For [multiple_comparisons()] output it also gives the transformation (if
#' one was used) and, for Tukey's test, the largest HSD across all pairwise
#' comparisons (`Max HSD`). When the HSD varies between comparisons (e.g.
#' unequal replication, or `by` groups), the maximum is a conservative
#' threshold: any two means differing by more than it are significantly
#' different, but some smaller differences may be significant too, so the
#' letter groups remain the definitive result. The HSD is on the scale the
#' model was fitted on, i.e. the transformed scale when a transformation was
#' used, because it cannot be validly back-transformed. For
#' [reference_comparisons()] output the summary row also names the reference
#' level. Any aliased levels are listed.
#'
#' With `friendly_names = TRUE` the following columns are renamed:
#' \itemize{
#'   \item [multiple_comparisons()]: `predicted.value` becomes `estimated.mean`,
#'     or `estimated.<trans>.mean` (e.g. `estimated.log.mean`) when a
#'     transformation was used, in which case the back-transformed
#'     `PredictedValue` becomes `EstimatedMean`.
#'   \item [reference_comparisons()]: `level1` and `level2` become `treatment`
#'     and `reference`, `level1.mean` and `level2.mean` become
#'     `treatment.mean` and `reference.mean`, and `estimate` becomes
#'     `difference`.
#'   \item [pairwise_comparisons()]: `estimate` becomes `difference` (except
#'     for the `contrasts` form, where it is a general linear combination).
#' }
#'
#' Values are written at full precision; use Excel's number formatting to
#' control the number of decimal places shown. Note that the differences from
#' [pairwise_comparisons()] and [reference_comparisons()] are always on the
#' scale the model was fitted on.
#'
#' The workbook must not be open in Excel when this function is run, as Excel
#' locks the file and it cannot be saved.
#'
#' Requires the 'openxlsx2' package to be installed.
#'
#' @returns Invisibly, the path to the workbook (`filename`).
#'
#' @seealso [multiple_comparisons()], [pairwise_comparisons()],
#'   [reference_comparisons()], [export_design_to_excel()]
#'
#' @importFrom rlang %||% is_installed
#'
#' @examplesIf rlang::is_installed("openxlsx2")
#' dat.aov <- aov(Petal.Width ~ Species, data = iris)
#' mc <- multiple_comparisons(dat.aov, classify = "Species")
#' rc <- reference_comparisons(dat.aov, classify = "Species",
#'                             reference = "setosa")
#'
#' file <- tempfile(fileext = ".xlsx")
#'
#' # A single object on a sheet named after the response
#' export_comparisons_to_excel(mc, file)
#'
#' # Several objects as titled sections on one sheet
#' export_comparisons_to_excel(
#'     list("Means by species" = mc,
#'          "Each species compared with setosa" = rc),
#'     file,
#'     sheet = "Petal width"
#' )
#'
#' # Add a further sheet to the same workbook
#' dat.aov2 <- aov(Sepal.Length ~ Species, data = iris)
#' mc2 <- multiple_comparisons(dat.aov2, classify = "Species")
#' export_comparisons_to_excel(mc2, file, sheet = "Sepal length")
#'
#' @export
export_comparisons_to_excel <- function(
	x,
	filename = "multiple_comparisons.xlsx",
	sheet = NULL,
	title = NULL,
	friendly_names = TRUE
) {
	if (!rlang::is_installed("openxlsx2")) {
		stop(
			"Package 'openxlsx2' is required for Excel export but is not installed.\n",
			"Install it with: install.packages('openxlsx2')",
			call. = FALSE
		)
	}

	# Normalise `x` to a list of sections.
	if (is_comparison_object(x)) {
		x <- list(x)
	}
	if (!is.list(x) || is.data.frame(x) || length(x) == 0) {
		stop(
			"`x` must be a comparison object or a non-empty list of them.",
			call. = FALSE
		)
	}
	ok <- vapply(x, is_comparison_object, logical(1))
	if (!all(ok)) {
		stop(
			"Every element of `x` must be the output of multiple_comparisons(), ",
			"pairwise_comparisons() or reference_comparisons(). Element(s) ",
			paste(which(!ok), collapse = ", "),
			" are not.",
			call. = FALSE
		)
	}
	if (
		!is.character(filename) ||
			length(filename) != 1 ||
			is.na(filename) ||
			!nzchar(filename)
	) {
		stop("`filename` must be a single file path.", call. = FALSE)
	}
	if (!is.logical(friendly_names) || length(friendly_names) != 1) {
		stop("`friendly_names` must be TRUE or FALSE.", call. = FALSE)
	}

	if (is.null(sheet)) {
		sheet <- comparison_response(x[[1]])
		if (is.null(sheet)) sheet <- "Comparisons"
	}
	if (is.null(title)) {
		title <- sheet
	}
	sheet <- clean_sheet_name(sheet)

	titles <- names(x)
	if (is.null(titles)) {
		titles <- rep("", length(x))
	}
	sections <- lapply(seq_along(x), function(i) {
		obj <- x[[i]]
		section_title <- titles[i]
		if (is.na(section_title) || !nzchar(section_title)) {
			section_title <- default_section_title(obj)
		}
		list(
			title = section_title,
			header = comparison_header(obj),
			table = comparison_table(obj, friendly_names)
		)
	})

	# Open the existing workbook, or start a new one.
	if (file.exists(filename)) {
		wb <- tryCatch(
			openxlsx2::wb_load(filename),
			error = function(e) {
				stop(
					"Could not open existing file '",
					filename,
					"' as an Excel workbook: ",
					conditionMessage(e),
					call. = FALSE
				)
			}
		)
	} else {
		wb <- openxlsx2::wb_workbook()
	}

	# Replace an existing sheet of the same name in its original tab position.
	# Write the new sheet under a temporary name first so the workbook is never
	# left with no sheets, then remove the old one and take over its name.
	existing <- wb$get_sheet_names()
	old_pos <- match(tolower(sheet), tolower(existing))
	target <- sheet
	if (!is.na(old_pos)) {
		# Deterministic (no sample()) so the user's RNG stream is untouched.
		target <- "biometryassist_tmp"
		while (tolower(target) %in% tolower(existing)) {
			target <- paste0(target, "_")
		}
	}
	wb$add_worksheet(target)
	widths <- write_comparison_sheet(wb, target, title, sections)
	wb$set_col_widths(sheet = target, cols = seq_along(widths), widths = widths)

	if (!is.na(old_pos)) {
		wb$remove_worksheet(existing[old_pos])
		wb$set_sheet_names(old = target, new = sheet)
		n <- length(existing)
		wb$set_order(append(seq_len(n - 1), n, after = old_pos - 1))
	}

	tryCatch(
		wb$save(filename),
		error = function(e) {
			stop(
				"Could not save '",
				filename,
				"'. If it is open in Excel, close it and try again.\n",
				conditionMessage(e),
				call. = FALSE
			)
		}
	)
	message("Sheet '", sheet, "' written to: ", filename)
	return(invisible(filename))
}


#' Is an object a supported comparison result?
#' @param x Any object.
#' @returns `TRUE` for `mct`, `pairwise_comparisons` or `reference_comparisons`.
#' @keywords internal
#' @noRd
is_comparison_object <- function(x) {
	return(inherits(x, c("mct", "pairwise_comparisons", "reference_comparisons")))
}


#' Clean a string for use as an Excel worksheet name
#'
#' Replaces the characters Excel forbids, strips the leading/trailing
#' apostrophes it also rejects, and truncates to 31 characters.
#' @param x A character string.
#' @returns A valid worksheet name.
#' @keywords internal
#' @noRd
clean_sheet_name <- function(x) {
	x <- as.character(x)[1]
	# `]` must come first in the bracket expression to be taken literally.
	x <- gsub("[][\\\\/:?*]", "_", x)
	x <- gsub("[\r\n]", " ", x)
	x <- gsub("^'+|'+$", "", trimws(x))
	if (is.na(x) || !nzchar(x)) {
		x <- "Comparisons"
	}
	return(substr(x, 1, 31))
}


#' Response variable name recorded on a comparison object
#' @param x A comparison object.
#' @returns A character string, or `NULL` if none is recorded.
#' @keywords internal
#' @noRd
comparison_response <- function(x) {
	ylab <- attr(x, "ylab")
	if (is.null(ylab)) {
		return(NULL)
	}
	if (is.language(ylab)) {
		ylab <- deparse1(ylab)
	}
	ylab <- as.character(ylab)[1]
	if (is.na(ylab) || !nzchar(ylab)) {
		return(NULL)
	}
	return(ylab)
}


#' Join words as "a", "a and b", "a, b and c"
#' @param x A character vector.
#' @returns A single string.
#' @keywords internal
#' @noRd
and_list <- function(x) {
	if (length(x) <= 1) {
		return(paste(x, collapse = ""))
	}
	return(paste(paste(x[-length(x)], collapse = ", "), "and", x[length(x)]))
}


#' Default title for a section of an exported comparisons sheet
#' @param x A comparison object.
#' @returns A character string.
#' @keywords internal
#' @noRd
default_section_title <- function(x) {
	by <- attr(x, "by")
	within <- if (length(by)) {
		paste0(" (within each ", and_list(by), ")")
	} else {
		""
	}
	if (inherits(x, "mct")) {
		# The classify factor columns are those before "predicted.value".
		cols <- names(x$predictions)
		factor_cols <- cols[seq_len(match("predicted.value", cols) - 1)]
		if (!length(factor_cols)) {
			return("Means")
		}
		return(paste("Means by", and_list(factor_cols)))
	}
	if (inherits(x, "reference_comparisons")) {
		return(paste0(
			"Comparisons with ",
			attr(x, "reference") %||% "the reference level",
			within
		))
	}
	if (identical(attr(x, "comparison_type"), "contrasts")) {
		return(paste0("Contrasts", within))
	}
	return(paste0("Pairwise comparisons", within))
}


#' Transformation label for an mct object
#'
#' `NULL` when no transformation was used, otherwise the transformation name,
#' with the exponent appended for power transformations (e.g. "power-0.5").
#' @param x An `mct` object.
#' @returns A character string or `NULL`.
#' @keywords internal
#' @noRd
mct_trans_tag <- function(x) {
	trans <- attr(x, "trans")
	if (is.null(trans) || !nzchar(trans)) {
		return(NULL)
	}
	if (identical(trans, "power") && !is.null(attr(x, "power"))) {
		return(paste0("power", format(attr(x, "power"), trim = TRUE)))
	}
	return(trans)
}


#' Largest Tukey HSD across all pairwise comparisons of an mct object
#'
#' `$hsd` is a single value, a symmetric matrix (`NA` diagonal) when the HSD
#' varies by comparison, or a list of these for a `by` fit. The maximum over
#' all finite values is a conservative significance threshold.
#' @param x An `mct` object.
#' @returns A number, or `NA` if no HSD is available.
#' @keywords internal
#' @noRd
max_hsd <- function(x) {
	h <- x$hsd
	if (is.null(h)) {
		return(NA_real_)
	}
	if (is.list(h)) {
		h <- unlist(h, use.names = FALSE)
	}
	h <- suppressWarnings(as.numeric(as.matrix(h)))
	h <- h[is.finite(h)]
	if (!length(h)) {
		return(NA_real_)
	}
	return(max(h))
}


#' One-row summary written above each exported comparisons table
#' @param x A comparison object.
#' @returns A one-row data frame.
#' @keywords internal
#' @noRd
comparison_header <- function(x) {
	if (inherits(x, "mct")) {
		method <- x$comparison_method
		sig <- x$sig_level
	} else {
		method <- attr(x, "comparison_method")
		sig <- attr(x, "sig_level")
	}

	out <- list()
	if (inherits(x, "reference_comparisons")) {
		out[["Reference level"]] <- attr(x, "reference") %||% NA_character_
	}
	out[["Multiplicity adjustment"]] <- method %||% NA_character_
	out[["Significance level"]] <- sig %||% NA_real_

	if (inherits(x, "mct")) {
		tag <- mct_trans_tag(x)
		if (!is.null(tag)) {
			out[["Transformation"]] <- tag
		}
		hsd <- max_hsd(x)
		if (!is.na(hsd)) {
			label <- if (is.null(tag)) {
				"Max HSD"
			} else {
				paste0("Max HSD (", tag, " scale)")
			}
			out[[label]] <- hsd
		}
	}

	aliased <- attr(x, "aliased")
	if (length(aliased)) {
		out[["Aliased levels"]] <- paste(aliased, collapse = ", ")
	}
	return(as.data.frame(out, check.names = FALSE, stringsAsFactors = FALSE))
}


#' Results table for an exported comparisons section
#' @param x A comparison object.
#' @param friendly_names Logical; rename the estimate columns?
#' @returns A plain data frame.
#' @keywords internal
#' @noRd
comparison_table <- function(x, friendly_names) {
	if (inherits(x, "mct")) {
		tbl <- as.data.frame(x$predictions)
	} else {
		tbl <- as.data.frame(x)
	}
	# Drop the subclass and any attributes carried over from the object.
	tbl <- data.frame(tbl, check.names = FALSE, stringsAsFactors = FALSE)
	rownames(tbl) <- NULL
	if (!friendly_names) {
		return(tbl)
	}

	nm <- names(tbl)
	if (inherits(x, "mct")) {
		tag <- mct_trans_tag(x)
		if (is.null(tag)) {
			nm[nm == "predicted.value"] <- "estimated.mean"
		} else {
			nm[nm == "predicted.value"] <- paste0("estimated.", tag, ".mean")
			nm[nm == "PredictedValue"] <- "EstimatedMean"
		}
	} else if (inherits(x, "reference_comparisons")) {
		renames <- c(
			level1 = "treatment",
			level2 = "reference",
			estimate = "difference",
			level1.mean = "treatment.mean",
			level2.mean = "reference.mean"
		)
		hit <- nm %in% names(renames)
		nm[hit] <- renames[nm[hit]]
	} else if (!identical(attr(x, "comparison_type"), "contrasts")) {
		nm[nm == "estimate"] <- "difference"
	}
	names(tbl) <- nm
	return(tbl)
}


#' Write the sections of a comparisons sheet
#'
#' Writes the heading, then for each section its title, summary row and
#' results table, and works out column widths from the tables (ignoring the
#' titles, which are allowed to overflow into neighbouring cells).
#' @param wb An openxlsx2 `wbWorkbook`, modified in place.
#' @param sheet Worksheet name.
#' @param title Sheet heading for cell A1.
#' @param sections List of sections, each with `title`, `header` and `table`.
#' @returns A numeric vector of column widths.
#' @keywords internal
#' @noRd
write_comparison_sheet <- function(wb, sheet, title, sections) {
	bold <- function(row) {
		wb$add_font(
			sheet = sheet,
			dims = openxlsx2::wb_dims(row, 1),
			bold = TRUE
		)
	}
	col_widths <- function(df) {
		vapply(
			seq_along(df),
			function(j) {
				v <- df[[j]]
				content <- if (is.numeric(v)) {
					11 # Excel's "General" format shows up to ~11 characters
				} else {
					max(c(0, nchar(as.character(v))), na.rm = TRUE)
				}
				max(nchar(names(df)[j]), content)
			},
			numeric(1)
		)
	}
	widths <- numeric(0)
	merge_widths <- function(widths, new) {
		n <- max(length(widths), length(new))
		widths <- c(widths, rep(0, n - length(widths)))
		new <- c(new, rep(0, n - length(new)))
		return(pmax(widths, new))
	}

	row <- 1
	wb$add_data(sheet = sheet, x = title, start_row = row, col_names = FALSE)
	wb$add_font(
		sheet = sheet,
		dims = openxlsx2::wb_dims(row, 1),
		bold = TRUE,
		size = 14
	)
	row <- row + 2

	for (sec in sections) {
		wb$add_data(
			sheet = sheet,
			x = sec$title,
			start_row = row,
			col_names = FALSE
		)
		bold(row)
		row <- row + 1

		wb$add_data(sheet = sheet, x = sec$header, start_row = row)
		wb$add_font(
			sheet = sheet,
			dims = openxlsx2::wb_dims(row, seq_along(sec$header)),
			italic = TRUE
		)
		row <- row + nrow(sec$header) + 2

		wb$add_data_table(
			sheet = sheet,
			x = sec$table,
			start_row = row,
			table_style = "TableStyleLight9"
		)
		row <- row + nrow(sec$table) + 3

		widths <- merge_widths(widths, col_widths(sec$header))
		widths <- merge_widths(widths, col_widths(sec$table))
	}

	return(pmin(widths + 2, 50))
}
