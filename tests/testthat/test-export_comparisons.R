# Every cell value on a sheet, as a character vector, for content checks.
sheet_cells <- function(file, sheet) {
	df <- openxlsx2::wb_to_df(file, sheet = sheet, col_names = FALSE)
	vals <- unlist(lapply(df, as.character), use.names = FALSE)
	return(vals[!is.na(vals)])
}

iris_aov <- aov(Petal.Width ~ Species, data = iris)
iris_mc <- multiple_comparisons(iris_aov, classify = "Species")
iris_rc <- reference_comparisons(
	iris_aov,
	classify = "Species",
	reference = "setosa"
)

test_that("creates the default workbook, named sheet from the response", {
	skip_if_not_installed("openxlsx2")
	withr::local_dir(withr::local_tempdir())

	expect_message(
		out <- export_comparisons_to_excel(iris_mc),
		"Sheet 'Petal.Width' written to: multiple_comparisons.xlsx"
	)
	expect_equal(out, "multiple_comparisons.xlsx")
	expect_true(file.exists("multiple_comparisons.xlsx"))

	wb <- openxlsx2::wb_load("multiple_comparisons.xlsx")
	expect_equal(unname(wb$get_sheet_names()), "Petal.Width")

	cells <- sheet_cells("multiple_comparisons.xlsx", "Petal.Width")
	expect_true(all(
		c(
			"Petal.Width",
			"Means by Species",
			"Multiplicity adjustment",
			"Max HSD",
			"estimated.mean",
			"groups",
			"setosa"
		) %in%
			cells
	))
	expect_false("predicted.value" %in% cells)
})

test_that("adds sheets to an existing workbook, keeping the others", {
	skip_if_not_installed("openxlsx2")
	file <- withr::local_tempfile(fileext = ".xlsx")

	expect_message(export_comparisons_to_excel(iris_mc, file, sheet = "One"))
	expect_message(export_comparisons_to_excel(iris_rc, file, sheet = "Two"))

	wb <- openxlsx2::wb_load(file)
	expect_equal(unname(wb$get_sheet_names()), c("One", "Two"))
	expect_true("estimated.mean" %in% sheet_cells(file, "One"))
	expect_true("Reference level" %in% sheet_cells(file, "Two"))
})

test_that("an existing sheet is replaced in place, ignoring case", {
	skip_if_not_installed("openxlsx2")
	file <- withr::local_tempfile(fileext = ".xlsx")

	for (s in c("A", "B", "C")) {
		expect_message(export_comparisons_to_excel(iris_mc, file, sheet = s))
	}
	expect_message(export_comparisons_to_excel(iris_rc, file, sheet = "b"))

	wb <- openxlsx2::wb_load(file)
	expect_equal(unname(wb$get_sheet_names()), c("A", "b", "C"))
	cells <- sheet_cells(file, "b")
	expect_true("Reference level" %in% cells)
	expect_false("Max HSD" %in% cells)
	# Untouched sheets keep their content
	expect_true("Max HSD" %in% sheet_cells(file, "C"))
})

test_that("the only sheet in a workbook can be replaced", {
	skip_if_not_installed("openxlsx2")
	file <- withr::local_tempfile(fileext = ".xlsx")

	expect_message(export_comparisons_to_excel(iris_mc, file, sheet = "Only"))
	expect_message(export_comparisons_to_excel(iris_rc, file, sheet = "Only"))

	wb <- openxlsx2::wb_load(file)
	expect_equal(unname(wb$get_sheet_names()), "Only")
	expect_true("Reference level" %in% sheet_cells(file, "Only"))
})

test_that("a list is written as titled sections on one sheet", {
	skip_if_not_installed("openxlsx2")
	file <- withr::local_tempfile(fileext = ".xlsx")

	expect_message(export_comparisons_to_excel(
		list("My means" = iris_mc, iris_rc),
		file,
		sheet = "Sections",
		title = "Petal width results"
	))
	cells <- sheet_cells(file, "Sections")
	expect_true(all(
		c(
			"Petal width results",
			"My means",
			"Comparisons with setosa",
			"estimated.mean",
			"treatment",
			"reference",
			"difference"
		) %in%
			cells
	))

	tables <- openxlsx2::wb_load(file)$get_tables(sheet = "Sections")
	expect_equal(nrow(tables), 2)
})

test_that("friendly_names = FALSE keeps the package column names", {
	skip_if_not_installed("openxlsx2")
	file <- withr::local_tempfile(fileext = ".xlsx")

	expect_message(export_comparisons_to_excel(
		list(iris_mc, iris_rc),
		file,
		sheet = "Raw",
		friendly_names = FALSE
	))
	cells <- sheet_cells(file, "Raw")
	expect_true(all(c("predicted.value", "level1", "estimate") %in% cells))
	expect_false(any(c("estimated.mean", "treatment", "difference") %in% cells))
})

test_that("transformed fits name the transformation and its scale", {
	skip_if_not_installed("openxlsx2")
	file <- withr::local_tempfile(fileext = ".xlsx")

	log_aov <- aov(log(Petal.Width) ~ Species, data = iris)
	log_mc <- suppressWarnings(
		multiple_comparisons(log_aov, classify = "Species", trans = "log")
	)
	expect_message(export_comparisons_to_excel(log_mc, file, sheet = "Log"))

	cells <- sheet_cells(file, "Log")
	expect_true(all(
		c(
			"Transformation",
			"log",
			"Max HSD (log scale)",
			"estimated.log.mean",
			"EstimatedMean",
			"ApproxSE"
		) %in%
			cells
	))
})

test_that("comparison_table() renames transformed columns", {
	log_aov <- aov(log(Petal.Width) ~ Species, data = iris)
	log_mc <- suppressWarnings(
		multiple_comparisons(log_aov, classify = "Species", trans = "log")
	)
	tbl <- comparison_table(log_mc, friendly_names = TRUE)
	expect_true(all(c("estimated.log.mean", "EstimatedMean") %in% names(tbl)))
	expect_false(any(c("predicted.value", "PredictedValue") %in% names(tbl)))
})

test_that("non-Tukey adjustments have no HSD in the summary", {
	mc_holm <- multiple_comparisons(
		iris_aov,
		classify = "Species",
		adjust = "holm"
	)
	header <- comparison_header(mc_holm)
	expect_equal(header[["Multiplicity adjustment"]], "holm")
	expect_false(any(grepl("HSD", names(header))))
})

test_that("max_hsd() takes the maximum over a matrix or by-group list", {
	m <- matrix(c(NA, 1, 2, 1, NA, 3, 2, 3, NA), 3)
	expect_equal(max_hsd(list(hsd = 0.5)), 0.5)
	expect_equal(max_hsd(list(hsd = m)), 3)
	expect_equal(max_hsd(list(hsd = list(a = m, b = 4))), 4)
	expect_true(is.na(max_hsd(list(hsd = NULL))))
	expect_true(is.na(max_hsd(list(hsd = matrix(NA_real_, 2, 2)))))
})

test_that("pairwise comparisons get 'difference' except for contrasts", {
	pw <- pairwise_comparisons(
		iris_aov,
		classify = "Species",
		pairs = c("setosa-versicolor")
	)
	expect_true("difference" %in% names(comparison_table(pw, TRUE)))
	expect_equal(default_section_title(pw), "Pairwise comparisons")

	ct <- pairwise_comparisons(
		iris_aov,
		classify = "Species",
		contrasts = list(
			"setosa vs rest" = c(setosa = 1, versicolor = -0.5, virginica = -0.5)
		)
	)
	expect_true("estimate" %in% names(comparison_table(ct, TRUE)))
	expect_equal(default_section_title(ct), "Contrasts")
})

test_that("clean_sheet_name() makes valid Excel sheet names", {
	expect_equal(clean_sheet_name("a/b:c?d*e[f]g\\h"), "a_b_c_d_e_f_g_h")
	expect_equal(clean_sheet_name("'quoted'"), "quoted")
	expect_equal(clean_sheet_name(""), "Comparisons")
	expect_equal(nchar(clean_sheet_name(strrep("x", 40))), 31)
})

test_that("invalid inputs give informative errors", {
	skip_if_not_installed("openxlsx2")
	file <- withr::local_tempfile(fileext = ".xlsx")

	expect_error(
		export_comparisons_to_excel(iris, file),
		"comparison object"
	)
	expect_error(
		export_comparisons_to_excel(list(iris_mc, iris), file),
		"Element\\(s\\) 2 are not"
	)
	expect_error(
		export_comparisons_to_excel(iris_mc, c("a.xlsx", "b.xlsx")),
		"single file path"
	)

	writeLines("not a workbook", file)
	expect_error(
		export_comparisons_to_excel(iris_mc, file),
		"Could not open existing file"
	)
})

test_that("a missing openxlsx2 gives an informative error", {
	mockery::stub(
		export_comparisons_to_excel,
		"rlang::is_installed",
		function(pkg) FALSE
	)
	expect_error(
		export_comparisons_to_excel(iris_mc, "unused.xlsx"),
		"Package 'openxlsx2' is required"
	)
})

test_that("further invalid inputs give informative errors", {
	skip_if_not_installed("openxlsx2")
	file <- withr::local_tempfile(fileext = ".xlsx")

	expect_error(export_comparisons_to_excel(list(), file), "non-empty list")
	expect_error(
		export_comparisons_to_excel(iris_mc, file, friendly_names = NA_character_),
		"must be TRUE or FALSE"
	)
})

test_that("a failed save points to the file being open in Excel", {
	skip_if_not_installed("openxlsx2")
	bad <- file.path(withr::local_tempdir(), "no_such_dir", "out.xlsx")
	expect_error(
		export_comparisons_to_excel(iris_mc, bad),
		"If it is open in Excel"
	)
})

test_that("the sheet falls back to 'Comparisons' without a response label", {
	skip_if_not_installed("openxlsx2")
	file <- withr::local_tempfile(fileext = ".xlsx")

	no_ylab <- iris_mc
	attr(no_ylab, "ylab") <- NULL
	expect_message(
		export_comparisons_to_excel(no_ylab, file),
		"Sheet 'Comparisons' written"
	)
})

test_that("comparison_response() handles symbols and empty labels", {
	x <- iris_mc
	attr(x, "ylab") <- quote(yield)
	expect_equal(comparison_response(x), "yield")
	attr(x, "ylab") <- ""
	expect_null(comparison_response(x))
	attr(x, "ylab") <- NULL
	expect_null(comparison_response(x))
})

test_that("the temporary sheet name avoids an existing sheet", {
	skip_if_not_installed("openxlsx2")
	file <- withr::local_tempfile(fileext = ".xlsx")

	expect_message(export_comparisons_to_excel(
		iris_mc,
		file,
		sheet = "biometryassist_tmp"
	))
	expect_message(export_comparisons_to_excel(iris_mc, file, sheet = "Main"))
	expect_message(export_comparisons_to_excel(iris_rc, file, sheet = "Main"))

	wb <- openxlsx2::wb_load(file)
	expect_equal(
		unname(wb$get_sheet_names()),
		c("biometryassist_tmp", "Main")
	)
	expect_true("Max HSD" %in% sheet_cells(file, "biometryassist_tmp"))
	expect_true("Reference level" %in% sheet_cells(file, "Main"))
})

test_that("by fits are titled by group and use the largest HSD", {
	wb_aov <- aov(breaks ~ wool * tension, data = warpbreaks)
	mc_by <- multiple_comparisons(wb_aov, classify = "wool:tension", by = "wool")
	expect_equal(
		default_section_title(mc_by),
		"Means by wool and tension"
	)
	expect_equal(
		comparison_header(mc_by)[["Max HSD"]],
		max_hsd(mc_by)
	)

	rc_by <- reference_comparisons(
		wb_aov,
		classify = "wool:tension",
		by = "wool",
		reference = "L"
	)
	expect_equal(
		default_section_title(rc_by),
		"Comparisons with L (within each wool)"
	)
})

test_that("section titles fall back for objects missing metadata", {
	rc <- iris_rc
	attr(rc, "reference") <- NULL
	expect_equal(
		default_section_title(rc),
		"Comparisons with the reference level"
	)
	expect_true(is.na(comparison_header(rc)[["Reference level"]]))

	no_factors <- structure(
		list(predictions = data.frame(predicted.value = 1)),
		class = "mct"
	)
	expect_equal(default_section_title(no_factors), "Means")
})

test_that("power transformations are tagged with their exponent", {
	x <- structure(list(), class = "mct", trans = "power", power = -0.5)
	expect_equal(mct_trans_tag(x), "power-0.5")

	x <- iris_mc
	attr(x, "trans") <- "power"
	attr(x, "power") <- 0.5
	header <- comparison_header(x)
	expect_equal(header[["Transformation"]], "power0.5")
	expect_true("Max HSD (power0.5 scale)" %in% names(header))
	expect_true(
		"estimated.power0.5.mean" %in% names(comparison_table(x, TRUE))
	)
})

test_that("aliased levels are noted below the table, not in the summary", {
	skip_if_not_installed("openxlsx2")
	file <- withr::local_tempfile(fileext = ".xlsx")

	x <- iris_mc
	attr(x, "aliased") <- c("A", "B")
	expect_false(any(grepl("liased", names(comparison_header(x)))))
	expect_null(comparison_footnote(iris_mc))
	# Only the level names are italic, not the lead-in text
	# (unclass(): as.character() on fmt_txt returns the plain text)
	note_xml <- unclass(comparison_footnote(x))
	expect_match(note_xml, "<i/></rPr><t[^>]*>A, B</t>", fixed = FALSE)
	expect_match(note_xml, "<rPr/><t[^>]*>Aliased levels", fixed = FALSE)

	expect_message(export_comparisons_to_excel(
		list(x, iris_rc),
		file,
		sheet = "Aliased"
	))
	df <- openxlsx2::wb_to_df(
		file,
		sheet = "Aliased",
		col_names = FALSE,
		skip_empty_rows = FALSE
	)
	note <- "Aliased levels (not estimable, so not included above): A, B"
	note_row <- which(df[[1]] == note)
	expect_length(note_row, 1)
	# Directly below the last row of the 3-level predictions table
	expect_equal(df[[1]][note_row - 1], "virginica")
	# The following section still starts after a gap
	expect_true(is.na(df[[1]][note_row + 1]))
	expect_true("Comparisons with setosa" %in% df[[1]])
})
