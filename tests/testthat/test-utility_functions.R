test_that("quiet supresses output", {
    expect_output(quiet(print("Hello")), NA)
})

test_that("Package message prints on load", {
    rlang::local_interactive(value = TRUE)
    expect_snapshot(biometryassist:::.onAttach(pkg = "biometryassist"))
})

test_that("Output prints if crayon is not installed", {
    rlang::local_interactive(value = TRUE)
    mockery::stub(biometryassist:::.onAttach, "rlang::is_installed", FALSE)
    expect_output(print(biometryassist:::.onAttach(pkg = "biometryassist")))
})
