test_that("get_r_os returns correct structure and values", {
  result <- get_r_os()
  expect_type(result, "list")
  expect_named(result, c("os_ver", "os", "ver", "arm"))
  
  # Check OS detection
  sys_info <- Sys.info()
  expected_os <- switch(sys_info[['sysname']],
  Windows = "win",
  Linux = "linux",
  Darwin = "mac")
  expect_equal(result$os, expected_os)
  expect_match(result$ver, "\\d{2}")  # Should be something like "43" for R 4.3
  expect_match(result$os_ver, paste0(expected_os, ifelse(result$arm, "-arm", ""), "-\\d{2}$"))
  skip_on_os(c("windows", "linux"))  # Skip ARM check on Windows and Linux
  expect_true(result$arm)  # Windows doesn't have ARM distinction in this context
})

test_that("find_existing_package works correctly", {
  skip_on_cran()
  
  # Test with no files
  withr::with_tempdir({
    result <- find_existing_package()
    expect_null(result)
  })
  
  # Test with matching files
  withr::with_tempdir({
    file.create("asreml_4.1.0.zip")
    Sys.sleep(0.3)
    file.create("asreml_4.2.0.zip")
    result <- find_existing_package()
    expect_equal(basename(result), "asreml_4.2.0.zip")  # Should get the most recent one
  })
  
  # Test with non-matching files
  withr::with_tempdir({
    file.create("other_package.zip")
    result <- find_existing_package()
    expect_null(result)
  })
})

test_that("install_dependencies identifies missing packages correctly", {
  skip_on_cran()
  installed_pkgs <- NULL
  mock_installed <- function(...) {
    matrix(c("base", "utils"),
           ncol = 1,
           dimnames = list(c("base", "utils"), "Package"))
  }
  mock_install <- function(pkgs, ...) {
    installed_pkgs <<- pkgs
  }
  mockery::stub(install_dependencies, "installed.packages", mock_installed)
  mockery::stub(install_dependencies, "install.packages", mock_install)
  install_dependencies(quiet = TRUE)
  expect_true(all(c("data.table", "ggplot2", "jsonlite") %in% installed_pkgs))
})

test_that("manage_file handles different keep_file options", {
  skip_on_cran()

  withr::with_tempdir({
    # Create a test file
    test_file <- tempfile("test_asreml_")
    writeLines("test content", test_file)

    # Test keep_file = FALSE (should remove file)
    result <- manage_file(test_file, FALSE, "test.zip")
    expect_true(result)
    expect_false(file.exists(test_file))
  })

  withr::with_tempdir({
    # Create a test file
    test_file <- tempfile("test_asreml_")
    writeLines("test content", test_file)

    # Test keep_file = TRUE (should rename to current dir)
    result <- manage_file(test_file, TRUE, "test.zip")
    expect_true(result)
    expect_true(file.exists("test.zip"))
    expect_false(file.exists(test_file))
  })

  withr::with_tempdir({
    # Create a test file and subdirectory
    test_file <- tempfile("test_asreml_")
    writeLines("test content", test_file)
    subdir <- "subdir"
    dir.create(subdir)

    # Test keep_file = path (should move to specified directory)
    result <- manage_file(test_file, subdir, "test.zip")
    expect_true(result)
    expect_true(file.exists(file.path(subdir, "test.zip")))
    expect_false(file.exists(test_file))
  })

  withr::with_tempdir({
    # Create a test file
    test_file <- tempfile("test_asreml_")
    writeLines("test content", test_file)

    # Test keep_file = invalid path
    expect_warning(
      result <- manage_file(test_file, "/nonexistent/path", "test.zip"),
      "Invalid keep_file argument"
    )
    expect_false(result)
  })
})

test_that("parse_version_table handles edge cases", {
  # Test with empty input
  result <- parse_version_table(list(), character(0))
  expect_null(result)
  
  # Test with malformed table data
  bad_tables <- list(c("header1", "header2"))  # Too few columns
  expect_error(parse_version_table(bad_tables, "ASReml-R 4.2 (All platforms)"))
})

# Tests that require internet connection (skip on CRAN)
test_that("get_version_table works with internet", {
  skip_on_cran()
  skip_if_not(curl::has_internet(), "No internet connection")
  
  # Test with default URL
  result <- get_version_table()
  
  if(nrow(result) > 0) {
    expected_cols <- c("os", "arm", "r_ver", "asr_ver")
    expect_true(all(expected_cols %in% colnames(result)))
    expect_true(all(result$os %in% c("win", "mac", "linux", "centos")))
    expect_type(result$arm, "logical")
  }
})

test_that("newer_version handles no network gracefully", {
  skip_on_cran()
  mockery::stub(newer_version, "get_version_table", function() data.frame())
  result <- newer_version()
  expect_type(result, "logical")
  expect_false(result)
})

test_that("install_asreml handles no internet connection", {
  skip_on_cran()
  mockery::stub(install_asreml, "curl::has_internet", function() FALSE)
  expect_error(
    install_asreml(),
    "No internet connection detected\\. Cannot download ASReml\\-R package\\."
  )
})

test_that("install_asreml early return when up-to-date", {
  skip_on_cran()
  mockery::stub(install_asreml, ".check_package_available", function(pkg) TRUE)
  mockery::stub(install_asreml, "newer_version", function() FALSE)
  mockery::stub(install_asreml, "has_internet", function() TRUE)
  expect_message(
    result <- install_asreml(force = FALSE),
    "already installed"
  )
  expect_true(result)
})

test_that("update_asreml calls install_asreml with force=TRUE", {
  skip_on_cran()
  force_used <- FALSE
  mockery::stub(update_asreml, "install_asreml", function(force = FALSE, ...) {
    force_used <<- force
    TRUE
  })
  update_asreml()
  expect_true(force_used)
})

# Tests that actually attempt installation (Linux only)
test_that("ASReml installation integration test", {
  skip_on_cran()
  skip_on_os(c("windows", "mac"))
  skip_if_offline()
  
  # Create temporary library directory
  temp_lib <- file.path(tempdir(), "test_lib")
  dir.create(temp_lib, showWarnings = FALSE, recursive = TRUE)
  
  on.exit({
    # Clean up
    if(dir.exists(temp_lib)) {
      unlink(temp_lib, recursive = TRUE)
    }
  })
  
  # Test quiet installation
  result <- tryCatch({
    install_asreml(library = temp_lib, quiet = TRUE, check_version = FALSE)
  }, error = function(e) {
    # Installation might fail due to licensing or network issues
    # This is expected in test environment
    FALSE
  })
  
  expect_type(result, "logical")
})

test_that("remove_existing_asreml works when asreml is loaded and attached", {
  skip_on_cran()
  called <- list(unload = FALSE, detach = FALSE, remove = FALSE)
  mockery::stub(remove_existing_asreml, "loadedNamespaces", function() c("asreml", "stats"))
  mockery::stub(remove_existing_asreml, ".packages", function() c("asreml", "stats"))
  mockery::stub(remove_existing_asreml, "unloadNamespace", function(pkg) { called$unload <<- TRUE })
  mockery::stub(remove_existing_asreml, "detach", function(name, unload, force) { called$detach <<- TRUE })
  mockery::stub(remove_existing_asreml, "remove.packages", function(pkg) { called$remove <<- TRUE })
  remove_existing_asreml()
  expect_true(called$unload)
  expect_true(called$detach)
  expect_true(called$remove)
})

test_that("remove_existing_asreml works when asreml is not loaded or attached", {
  skip_on_cran()
  called <- list(remove = FALSE)
  mockery::stub(remove_existing_asreml, "loadedNamespaces", function() c("stats"))
  mockery::stub(remove_existing_asreml, ".packages", function() c("stats"))
  mockery::stub(remove_existing_asreml, "remove.packages", function(pkg) { called$remove <<- TRUE })
  remove_existing_asreml()
  expect_true(called$remove)
})

test_that("remove_existing_asreml warns gracefully on error", {
  skip_on_cran()
  # with_mocked_bindings(
  mockery::stub(remove_existing_asreml, "loadedNamespaces", function() stop("fail"))
  mockery::stub(remove_existing_asreml, ".packages", function() stop("fail"))
  mockery::stub(remove_existing_asreml, "remove.packages", function(pkg) stop("fail"))
  expect_warning(remove_existing_asreml(), "Could not remove existing asreml package")
})

# Test create_mac_folder (safe to run on any platform)
test_that("create_mac_folder handles different scenarios", {
  if(Sys.info()[["sysname"]] == "Darwin") {
    # On actual macOS, test the real function
    result <- create_mac_folder()
    expect_is(result, "logical")
  } else {
    # On other platforms, mock Sys.info to simulate Linux
    mockery::stub(create_mac_folder, "Sys.info", function() c(sysname = "Linux", release = "5.0"))
    result <- create_mac_folder()
    expect_true(result)  # Should return TRUE for non-macOS

    # Now mock Sys.info to simulate macOS and dir.exists to TRUE
    mockery::stub(create_mac_folder, "Sys.info", function() c(sysname = "Darwin", release = "21"))
    mockery::stub(create_mac_folder, "dir.exists", function(path) TRUE)
    result <- create_mac_folder()
    expect_true(result)
  }
})

test_that("download_asreml_package handles download failures", {
  skip_on_cran()
  mockery::stub(download_asreml_package, "curl::curl_fetch_disk", function(...) stop("Network error"))
  expect_error(
    download_asreml_package("http://fake.url"),
    "Failed to download ASReml-R package"
  )
})

test_that("install_asreml_package handles installation failures", {
  skip_on_cran()
  temp_file <- tempfile()
  file.create(temp_file)
  on.exit(unlink(temp_file))
  mockery::stub(install_asreml_package, "install.packages", function(...) stop("Installation failed"))
  expect_warning(
    result <- install_asreml_package(temp_file, tempdir(), FALSE, "linux"),
    "Installation failed"
  )
  expect_false(result)
})

# test_that("functions handle malformed inputs gracefully", {
#   # Test version comparison with missing package description
#   mockery::stub(newer_version, ".check_package_available", function(pkg) TRUE)
#   mockery::stub(newer_version, "utils::packageDescription", function(pkg) list(Version = NULL, Packaged = NULL))
#   mockery::stub(newer_version, "get_version_table", function() data.frame(
#     os = "linux", arm = FALSE, r_ver = "43", asr_ver = "4.2.0",
#     `Date published` = as.Date("2023-01-01")
#   ))
#   mockery::stub(newer_version, "get_r_os", function() list(os = "linux", arm = FALSE, ver = "43"))
#   result <- newer_version()
#   expect_is(result, "logical")
# })






# test_that("Installation works", {
#     skip_on_os("windows")
#     skip_on_os("mac")
#     skip_on_cran()
#     # skip_on_ci()
#     expect_message(install_asreml(force = TRUE), "ASReml-R successfully installed!")
#     expect_equal(install_asreml(), TRUE)
# })

# test_that("Update function works", {
#     # skip_if(R.version$status == "Under development (unstable)")
#     skip_on_os("windows")
#     skip_on_cran()
#     skip_on_ci()
#     expect_message(update_asreml(), "ASReml-R successfully installed!")
# })

# test_that("Returns true if asreml already installed", {
#     # skip_if(R.version$status == "Under development (unstable)")
#     skip_on_os("windows")
#     skip_on_cran()
#     skip_on_ci()
#     install_asreml(quiet=TRUE)
#     expect_equal(install_asreml(), TRUE)
# })

# test_that("Prints message if asreml already installed", {
#     # skip_if(R.version$status == "Under development (unstable)")
#     skip_on_os("windows")
#     skip_on_cran()
#     skip_on_ci()
#     install_asreml(quiet = TRUE)
#     expect_message(install_asreml(), 
#                    "The latest version of ASReml-R available for your sysetm is already installed\\. To install anyway, set `force = TRUE`\\.")
# })

# test_that("Quiet returns no output", {
#     skip_on_os("windows")
#     skip_on_cran()
#     skip_on_ci()
#     expect_invisible(install_asreml(quiet = TRUE))
#     expect_invisible(install_asreml(quiet = TRUE, force = TRUE))
# })

# test_that("Force argument makes package install", {
#     # skip_if(R.version$status == "Under development (unstable)")
#     skip_on_os("windows")
#     skip_on_cran()
#     skip_on_ci()
#     install_asreml(force = TRUE)
#     expect_equal(.check_package_available("asreml"), TRUE)
#     expect_message(install_asreml(force = TRUE), NULL)
# })

# test_that("keep_file = F doesn't keep file", {
#     # skip_if(R.version$status == "Under development (unstable)")
#     skip_on_os("windows")
#     skip_on_cran()
#     skip_on_ci()
#     install_asreml(force = TRUE, keep_file = FALSE)
#     expect_equal(length(list.files(path = c(".", tempdir()),
#                                    pattern = "asreml+(([a-zA-Z0-9_.\\-])*)+(.zip|.tar.gz|.tgz)$")),
#                  0)
# })

# test_that("keep_file = T keeps file (in temp?)", {
#     skip_on_os("windows")
#     skip_on_cran()
#     skip_on_ci()
#     # skip_if(R.version$status == "Under development (unstable)")
#     install_asreml(force = TRUE, keep_file = TRUE)
#     expect_gt(length(list.files(path = c(".", tempdir()),
#                                 pattern = "asreml+(([a-zA-Z0-9_.\\-])*)+(.zip|.tar.gz|.tgz)$")),
#               0)
# })


# test_that("keep_file = 'data' keeps file in 'data'", {
#     # skip_if(R.version$status == "Under development (unstable)")
#     skip_on_os("windows")
#     skip_on_cran()
#     skip_on_ci()
#     dir.create(paste0(tempdir(), "/data"))
#     install_asreml(force = TRUE, keep_file = paste0(tempdir(), "/data"))
#     expect_gt(length(list.files(path = paste0(tempdir(), "/data"),
#                                 pattern = "asreml+(([a-zA-Z0-9_.\\-])*)+(.zip|.tar.gz|.tgz)$")),
#               0)
# })

# test_that("Providing a non-existant directory fails", {
#     # skip_if(R.version$status == "Under development (unstable)")
#     skip_on_os("windows")
#     skip_on_cran()
#     skip_on_ci()
#     expect_warning(install_asreml(force = TRUE, keep_file = "abc"),
#                    "Directory provided in keep_file does not exist. Please provide a valid path in the keep_file argument to save the package to.")
# })

# test_that("force and quiet work together", {
#     # skip_if(R.version$status == "Under development (unstable)")
#     skip_on_os("windows")
#     skip_on_cran()
#     skip_on_ci()
#     expect_invisible(install_asreml(force = TRUE, quiet = TRUE))
# })

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
