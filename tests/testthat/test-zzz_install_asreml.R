test_that("Installation works", {
    skip_on_os("windows")
    skip_on_os("mac")
    skip_on_cran()
    # skip_on_ci()
    expect_message(install_asreml(force = TRUE), "ASReml-R successfully installed!")
    expect_equal(install_asreml(), TRUE)
})

test_that("Update function works", {
    # skip_if(R.version$status == "Under development (unstable)")
    skip_on_os("windows")
    skip_on_cran()
    skip_on_ci()
    expect_message(update_asreml(), "ASReml-R successfully installed!")
})

test_that("Returns true if asreml already installed", {
    # skip_if(R.version$status == "Under development (unstable)")
    skip_on_os("windows")
    skip_on_cran()
    skip_on_ci()
    install_asreml(quiet=TRUE)
    expect_equal(install_asreml(), TRUE)
})

test_that("Prints message if asreml already installed", {
    # skip_if(R.version$status == "Under development (unstable)")
    skip_on_os("windows")
    skip_on_cran()
    skip_on_ci()
    install_asreml(quiet = TRUE)
    expect_message(install_asreml(), "ASReml-R is already installed.")
})

test_that("Quiet returns no output", {
    skip_on_os("windows")
    skip_on_cran()
    skip_on_ci()
    expect_invisible(install_asreml(quiet = TRUE))
    expect_invisible(install_asreml(quiet = TRUE, force = TRUE))
})

test_that("Force argument makes package install", {
    # skip_if(R.version$status == "Under development (unstable)")
    skip_on_os("windows")
    skip_on_cran()
    skip_on_ci()
    install_asreml(force = TRUE)
    expect_equal(requireNamespace("asreml"), TRUE)
    expect_message(install_asreml(force = TRUE), NULL)
})

test_that("keep_file = F doesn't keep file", {
    # skip_if(R.version$status == "Under development (unstable)")
    skip_on_os("windows")
    skip_on_cran()
    skip_on_ci()
    install_asreml(force = TRUE, keep_file = FALSE)
    expect_equal(length(list.files(path = c(".", tempdir()),
                                   pattern = "asreml+(([a-zA-Z0-9_.\\-])*)+(.zip|.tar.gz|.tgz)$")),
                 0)
})

test_that("keep_file = T keeps file (in temp?)", {
    skip_on_os("windows")
    skip_on_cran()
    skip_on_ci()
    # skip_if(R.version$status == "Under development (unstable)")
    install_asreml(force = TRUE, keep_file = TRUE)
    expect_gt(length(list.files(path = c(".", tempdir()),
                                pattern = "asreml+(([a-zA-Z0-9_.\\-])*)+(.zip|.tar.gz|.tgz)$")),
              0)
})


test_that("keep_file = 'data' keeps file in 'data'", {
    # skip_if(R.version$status == "Under development (unstable)")
    skip_on_os("windows")
    skip_on_cran()
    skip_on_ci()
    dir.create(paste0(tempdir(), "/data"))
    install_asreml(force = TRUE, keep_file = paste0(tempdir(), "/data"))
    expect_gt(length(list.files(path = paste0(tempdir(), "/data"),
                                pattern = "asreml+(([a-zA-Z0-9_.\\-])*)+(.zip|.tar.gz|.tgz)$")),
              0)
})

test_that("Providing a non-existant directory fails", {
    # skip_if(R.version$status == "Under development (unstable)")
    skip_on_os("windows")
    skip_on_cran()
    skip_on_ci()
    expect_warning(install_asreml(force = TRUE, keep_file = "abc"),
                   "Directory provided in keep_file does not exist. Please provide a valid path in the keep_file argument to save the package to.")
})

test_that("force and quiet work together", {
    # skip_if(R.version$status == "Under development (unstable)")
    skip_on_os("windows")
    skip_on_cran()
    skip_on_ci()
    expect_invisible(install_asreml(force = TRUE, quiet = TRUE))
})

# test_that("Old version of data.table results in upgrade to newer version", {
#     # skip_if(R.version$status == "Under development (unstable)")
#     skip_on_os("windows")
#     skip_if_not_installed("remotes")
#     skip_on_cran()
#     skip_on_ci()
#     remotes::install_version("data.table", "1.9.4", upgrade = "never")
#     install_asreml(force = TRUE, quiet = TRUE)
#     expect_true(packageVersion("data.table") > "1.9.4")
# })
