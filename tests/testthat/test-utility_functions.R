test_that("quiet supresses output", {
	expect_silent(quiet(print("Hello")))
	expect_silent(quiet(cat("Hello")))
})

test_that("aliased_note lists few levels and collapses many", {
	expect_null(aliased_note(NULL))
	expect_null(aliased_note(character(0)))
	expect_equal(aliased_note("A"), "Aliased level is: A")
	expect_equal(aliased_note(c("A", "B")), "Aliased levels are: A and B")
	expect_equal(
		aliased_note(c("A", "B", "C")),
		"Aliased levels are: A, B and C"
	)
	# more than max_show -> collapse to a count + how to retrieve them
	many <- aliased_note(LETTERS[1:7])
	expect_match(many, "^7 levels are aliased")
	expect_no_match(many, "A, B")
	expect_match(many, "attr\\(x, \"aliased\"\\)")
	# threshold is respected (6 still listed, 7 collapsed) and max_show is honoured
	expect_match(aliased_note(LETTERS[1:6]), "Aliased levels are: A, B")
})

test_that("note_ci_padjust_mismatch flags CI vs adjusted-p disagreement", {
	# CI excludes zero (conf.low > 0) but adjusted p is not significant -> mismatch
	mismatch <- data.frame(conf.low = 0.1, conf.high = 0.5, p.value = 0.2)
	expect_message(
		note_ci_padjust_mismatch(mismatch, sig = 0.05, method = "holm"),
		"per-comparison"
	)
	expect_true(suppressMessages(
		note_ci_padjust_mismatch(mismatch, sig = 0.05, method = "holm")
	))

	# agreement (CI excludes zero AND significant) -> silent
	agree <- data.frame(conf.low = 0.1, conf.high = 0.5, p.value = 0.01)
	expect_silent(note_ci_padjust_mismatch(agree, sig = 0.05, method = "holm"))
	# agreement (CI includes zero AND not significant) -> silent
	agree2 <- data.frame(conf.low = -0.1, conf.high = 0.5, p.value = 0.2)
	expect_silent(note_ci_padjust_mismatch(agree2, sig = 0.05, method = "holm"))

	# Dunnett intervals are simultaneous -> never flagged even when CI/p disagree
	expect_silent(note_ci_padjust_mismatch(
		mismatch,
		sig = 0.05,
		method = "dunnett"
	))
})

test_that("Package message prints on load", {
	rlang::local_interactive(value = TRUE)
	expect_snapshot(biometryassist:::.onAttach(pkg = "biometryassist"))
})

test_that("Output prints if crayon is not installed", {
	rlang::local_interactive(value = TRUE)

	# Force rlang::is_installed("crayon") to be FALSE.
	# We must override the function in the rlang namespace because .onAttach()
	# calls it via rlang::is_installed(), which mockery::stub() cannot intercept.
	old_is_installed <- get("is_installed", envir = asNamespace("rlang"))
	on.exit(
		assignInNamespace("is_installed", old_is_installed, ns = "rlang"),
		add = TRUE
	)
	assignInNamespace(
		"is_installed",
		function(pkg) {
			if (identical(pkg, "crayon")) {
				return(FALSE)
			}
			old_is_installed(pkg)
		},
		ns = "rlang"
	)

	# Mock compare_version to avoid available.packages() and update warnings
	testthat::local_mocked_bindings(
		compare_version = function(...) 0L,
		.package = "biometryassist"
	)

	# Use expect_message since .onAttach uses packageStartupMessage()
	# This should hit the else branch (line 45-47) which calls packageStartupMessage without crayon
	expect_message(
		biometryassist:::.onAttach(pkg = "biometryassist"),
		"biometryassist version"
	)
})

test_that("Output prints with crayon when it is installed", {
	skip_if_not_installed("crayon")
	rlang::local_interactive(value = TRUE)

	# Simply call .onAttach when crayon IS installed (default behavior)
	# This will naturally test the if(rlang::is_installed("crayon")) branch
	# Mock compare_version to avoid network calls
	local_mocked_bindings(
		compare_version = function(...) 0L,
		.package = "biometryassist"
	)

	# Should use crayon::green (lines 42-43) since crayon is installed
	expect_message(
		biometryassist:::.onAttach(pkg = "biometryassist"),
		"biometryassist version"
	)
})


test_that("Warning prints if cran version is newer", {
	rlang::local_interactive(value = TRUE)
	local_mocked_bindings(compare_version = function(...) 1L)
	expect_warning(print(biometryassist:::.onAttach(pkg = "biometryassist")))
})

test_that("handle_deprecated_param warns when old param is provided and new param is given", {
	test_fun <- function(old = 1, new = 2) {
		handle_deprecated_param("old", "new")
		old + new
	}
	expect_warning(test_fun(old = 5, new = 2), "deprecated.*use `new` instead")
})

test_that("handle_deprecated_param warns when old param is provided and no new param", {
	test_fun <- function(old = 1) {
		handle_deprecated_param("old")
		old
	}
	expect_warning(test_fun(old = 5), "deprecated")
})

test_that("handle_deprecated_param includes custom message if provided", {
	test_fun <- function(old = 1) {
		handle_deprecated_param("old", custom_msg = "This is a custom message.")
		old
	}
	expect_warning(test_fun(old = 5), "custom message")
})

test_that("handle_deprecated_param does not warn if old param is missing", {
	test_fun <- function(old = 1) {
		handle_deprecated_param("old", "new")
		42
	}
	expect_silent(test_fun())
})

# n_unique() tests ----

test_that("n_unique works with basic numeric vectors", {
	expect_equal(n_unique(c(1, 2, 3, 4, 5)), 5)
	expect_equal(n_unique(c(1, 1, 2, 2, 3)), 3)
	expect_equal(n_unique(c(1)), 1)
})

test_that("n_unique works with character vectors", {
	expect_equal(n_unique(c("a", "b", "c")), 3)
	expect_equal(n_unique(c("a", "a", "b", "b")), 2)
	expect_equal(n_unique(c("hello")), 1)
})

test_that("n_unique works with factor vectors", {
	expect_equal(n_unique(factor(c("low", "med", "high"))), 3)
	expect_equal(n_unique(factor(c("A", "A", "B", "B", "C"))), 3)
	expect_equal(n_unique(factor(c("single"))), 1)
})

test_that("n_unique handles empty vectors", {
	expect_equal(n_unique(numeric(0)), 0)
	expect_equal(n_unique(character(0)), 0)
	expect_equal(n_unique(factor(character(0))), 0)
})

test_that("n_unique handles all NA values", {
	expect_equal(n_unique(c(NA, NA, NA)), 1)
	expect_equal(n_unique(c(NA_real_, NA_real_)), 1)
	expect_equal(n_unique(c(NA_character_, NA_character_)), 1)
})

test_that("n_unique handles all NA values with na.rm=TRUE", {
	expect_equal(n_unique(c(NA, NA, NA), na.rm = TRUE), 0)
	expect_equal(n_unique(c(NA_real_, NA_real_), na.rm = TRUE), 0)
	expect_equal(n_unique(c(NA_character_, NA_character_), na.rm = TRUE), 0)
})

test_that("n_unique handles mixed NA values with na.rm=FALSE", {
	expect_equal(n_unique(c(1, 2, NA, 3, NA)), 4)
	expect_equal(n_unique(c("a", "b", NA, "c")), 4)
	expect_equal(n_unique(c(1, 1, NA, 2, NA)), 3)
})

test_that("n_unique handles mixed NA values with na.rm=TRUE", {
	expect_equal(n_unique(c(1, 2, NA, 3, NA), na.rm = TRUE), 3)
	expect_equal(n_unique(c("a", "b", NA, "c"), na.rm = TRUE), 3)
	expect_equal(n_unique(c(1, 1, NA, 2, NA), na.rm = TRUE), 2)
})

test_that("n_unique handles vectors with duplicates correctly", {
	expect_equal(n_unique(rep(1, 10)), 1)
	expect_equal(n_unique(rep("x", 5)), 1)
	expect_equal(n_unique(c(1, 2, 1, 2, 1, 2)), 2)
})
