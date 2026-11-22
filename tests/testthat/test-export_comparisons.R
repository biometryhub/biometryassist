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
