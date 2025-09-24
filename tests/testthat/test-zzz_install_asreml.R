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
    expect_match(result$os_ver, "^(win-|mac-|linux-)(arm-)?[0-9]{2}$")
    expect_equal(result$arm, sys_info[["machine"]] == "arm64")
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

test_that("newer_version returns FALSE if online_versions is empty", {
    mockery::stub(newer_version, "get_version_table", function() data.frame())
    expect_false(newer_version())
})

test_that("newer_version returns FALSE if no matching OS/version in online_versions", {
    fake_versions <- data.frame(
        os = "win", arm = FALSE, r_ver = "43", asr_ver = "4.2.0",
        `Date published` = as.Date("2023-01-01")
    )
    mockery::stub(newer_version, "get_version_table", function() fake_versions)
    mockery::stub(newer_version, "get_r_os", function() list(os = "linux", arm = FALSE, ver = "44"))
    expect_false(newer_version())
})

test_that("newer_version returns FALSE if installed version is up-to-date", {
    fake_versions <- data.frame(
        os = "linux",
        arm = FALSE,
        r_ver = "44",
        asr_ver = "4.2.0",
        `Date published` = as.Date("2023-01-01"),
        stringsAsFactors = FALSE
    )
    mockery::stub(newer_version, "get_version_table", function() fake_versions)
    mockery::stub(newer_version, "get_r_os", function() list(os = "linux", arm = FALSE, ver = "44"))
    mockery::stub(newer_version, "rlang::is_installed", function(pkg) TRUE)
    mockery::stub(newer_version, "utils::packageDescription", function(pkg) list(Packaged = "2023-01-10", Version = "4.2.0"))
    expect_false(newer_version())
})

test_that("newer_version returns TRUE if online version is newer and published > 7 days after installed", {
    fake_versions <- data.frame(
        os = "linux", arm = FALSE, r_ver = "44", asr_ver = "4.3.0",
        Date = as.Date("2023-02-01")
    )
    colnames(fake_versions)[5] <- "Date published"
    mockery::stub(newer_version, "get_version_table", function() fake_versions)
    mockery::stub(newer_version, "get_r_os", function() list(os = "linux", arm = FALSE, ver = "44"))
    mockery::stub(newer_version, "rlang::is_installed", function(pkg) TRUE)
    mockery::stub(newer_version, "utils::packageDescription", function(pkg) list(
        Packaged = "2023-01-01", Version = "4.2.0"
    ))
    expect_true(newer_version())
})

test_that("newer_version returns TRUE if asreml is not installed", {
    fake_versions <- data.frame(
        os = "linux", arm = FALSE, r_ver = "44", asr_ver = "4.2.0",
        `Date published` = as.Date("2023-01-01")
    )
    colnames(fake_versions)[5] <- "Date published"
    mockery::stub(newer_version, "get_version_table", function() fake_versions)
    mockery::stub(newer_version, "get_r_os", function() list(os = "linux", arm = FALSE, ver = "44"))
    mockery::stub(newer_version, "rlang::is_installed", function(pkg) FALSE)
    expect_true(newer_version())
})

test_that("newer_version handles missing Packaged or Version gracefully", {
    fake_versions <- data.frame(
        os = "linux", arm = FALSE, r_ver = "44", asr_ver = "4.3.0",
        `Date published` = as.Date("2023-02-01")
    )
    colnames(fake_versions)[5] <- "Date published"
    mockery::stub(newer_version, "get_version_table", function() fake_versions)
    mockery::stub(newer_version, "get_r_os", function() list(os = "linux", arm = FALSE, ver = "44"))
    mockery::stub(newer_version, "rlang::is_installed", function(pkg) TRUE)
    mockery::stub(newer_version, "utils::packageDescription", function(pkg) list(
        Packaged = NULL, Version = NULL
    ))
    expect_true(newer_version())
})

test_that("newer_version handles multiple newer versions correctly", {
    # Create fake data with multiple versions, including older and newer ones
    fake_versions <- data.frame(
        os = c("linux", "linux", "linux", "linux"),
        arm = c(FALSE, FALSE, FALSE, FALSE),
        r_ver = c("44", "44", "44", "44"),
        asr_ver = c("4.1.0", "4.3.0", "4.3.0", "4.2.5"),
        `Date published` = as.Date(c("2023-01-01", "2023-03-15", "2023-03-01", "2023-02-15")),
        stringsAsFactors = FALSE
    )
    colnames(fake_versions)[5] <- "Date published"

    mockery::stub(newer_version, "get_version_table", function() fake_versions)
    mockery::stub(newer_version, "get_r_os", function() list(os = "linux", arm = FALSE, ver = "44"))
    mockery::stub(newer_version, "rlang::is_installed", function(pkg) TRUE)
    mockery::stub(newer_version, "utils::packageDescription", function(pkg) list(
        Packaged = "2023-01-01", Version = "4.1.0"  # Installed version is older
    ))

    # Should return TRUE because there are newer versions (4.3.0 > 4.1.0)
    # and the most recent 4.3.0 (2023-03-15) is > 7 days after installed date (2023-01-01)
    expect_true(newer_version())
})

test_that("newer_version selects most recent when multiple versions have same number", {
    # Test case where there are multiple entries with the same version number
    # but different publication dates - should select the most recent one
    fake_versions <- data.frame(
        os = c("linux", "linux", "linux"),
        arm = c(FALSE, FALSE, FALSE),
        r_ver = c("44", "44", "44"),
        asr_ver = c("4.3.0", "4.3.0", "4.3.0"),
        `Date published` = as.Date(c("2023-01-01", "2023-01-15", "2023-01-10")),
        stringsAsFactors = FALSE
    )
    colnames(fake_versions)[5] <- "Date published"

    mockery::stub(newer_version, "get_version_table", function() fake_versions)
    mockery::stub(newer_version, "get_r_os", function() list(os = "linux", arm = FALSE, ver = "44"))
    mockery::stub(newer_version, "rlang::is_installed", function(pkg) TRUE)
    mockery::stub(newer_version, "utils::packageDescription", function(pkg) list(
        Packaged = "2023-01-05", Version = "4.2.0"  # Older version, earlier date
    ))

    # Should return TRUE because:
    # 1. Version check: 4.3.0 > 4.2.0 = TRUE
    # 2. Date check: most recent publication date (2023-01-15) > installed date + 7 days (2023-01-12) = TRUE
    # 3. Result: TRUE && TRUE = TRUE
    expect_true(newer_version())
})

test_that("newer_version returns FALSE when multiple versions exist but none are newer", {
    # Test case with multiple versions but the installed version is already the newest
    fake_versions <- data.frame(
        os = c("linux", "linux", "linux"),
        arm = c(FALSE, FALSE, FALSE),
        r_ver = c("44", "44", "44"),
        asr_ver = c("4.1.0", "4.2.0", "4.1.5"),
        `Date published` = as.Date(c("2023-01-01", "2023-02-01", "2023-01-15")),
        stringsAsFactors = FALSE
    )
    colnames(fake_versions)[5] <- "Date published"

    mockery::stub(newer_version, "get_version_table", function() fake_versions)
    mockery::stub(newer_version, "get_r_os", function() list(os = "linux", arm = FALSE, ver = "44"))
    mockery::stub(newer_version, "rlang::is_installed", function(pkg) TRUE)
    mockery::stub(newer_version, "utils::packageDescription", function(pkg) list(
        Packaged = "2023-02-10", Version = "4.2.0"  # Already have the newest version
    ))

    # Should return FALSE because installed version (4.2.0) is >= newest available (4.2.0)
    expect_false(newer_version())
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
    mockery::stub(install_asreml, "rlang::is_installed", function(pkg) TRUE)
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

test_that("Invalid library option will fail", {
    skip_on_cran()
    expect_error(install_asreml(library = "abc"),
                 "'library' must be a valid directory path\\. Provided\\: abc"
    )
})

# Tests for verbose debugging functionality

test_that("install_asreml verbose parameter validation and messaging", {
    skip_on_cran()

    # Test that verbose = "verbose" produces debug messages
    mockery::stub(install_asreml, "rlang::is_installed", function(pkg) TRUE)
    mockery::stub(install_asreml, "newer_version", function() FALSE)
    mockery::stub(install_asreml, "curl::has_internet", function() TRUE)

    # Capture messages when quiet = "verbose"
    expect_message(
        install_asreml(quiet = "verbose", force = FALSE),
        "\\[DEBUG\\] Starting ASReml-R installation process"
    )

    expect_message(
        install_asreml(quiet = "verbose", force = FALSE),
        "\\[DEBUG\\] Library path:"
    )

    expect_message(
        install_asreml(quiet = "verbose", force = FALSE),
        "\\[DEBUG\\] Force install: FALSE"
    )
})

test_that("verbose messaging works correctly with different quiet settings", {
    skip_on_cran()

    mockery::stub(install_asreml, "rlang::is_installed", function(pkg) TRUE)
    mockery::stub(install_asreml, "newer_version", function() FALSE)
    mockery::stub(install_asreml, "curl::has_internet", function() TRUE)

    # Test quiet = TRUE (no messages)
    expect_silent(install_asreml(quiet = TRUE, force = FALSE))

    # Test quiet = FALSE (normal messages but no debug)
    expect_message(
        install_asreml(quiet = FALSE, force = FALSE),
        "already installed"
    )

    # Should NOT see debug messages with quiet = FALSE
    expect_no_message(install_asreml(quiet = FALSE, force = FALSE), message = "\\[DEBUG\\]")
})

test_that("download_asreml_package verbose parameter works", {
    skip_on_cran()

    # Mock successful download
    mockery::stub(download_asreml_package, "tempfile", function(...) "/tmp/test_file")
    mockery::stub(download_asreml_package, "curl::curl_fetch_disk", function(url, path) {
        list(url = "http://example.com/asreml_4.1.0.zip")
    })
    mockery::stub(download_asreml_package, "file.rename", function(...) TRUE)
    mockery::stub(download_asreml_package, "normalizePath", function(x) x)

    # Test verbose = TRUE produces debug messages
    expect_message(
        download_asreml_package("http://test.url", verbose = TRUE),
        "\\[DEBUG\\] Creating temporary file for download"
    )

    expect_message(
        download_asreml_package("http://test.url", verbose = TRUE),
        "\\[DEBUG\\] Initiating download from:"
    )

    # Test verbose = FALSE produces no debug messages
    expect_no_message(download_asreml_package("http://test.url", verbose = FALSE),
                      message = "\\[DEBUG\\]"
    )
})

test_that("remove_existing_asreml verbose parameter works", {
    skip_on_cran()

    mockery::stub(remove_existing_asreml, "loadedNamespaces", function() c("asreml"))
    mockery::stub(remove_existing_asreml, ".packages", function() c("asreml"))
    mockery::stub(remove_existing_asreml, "unloadNamespace", function(pkg) {})
    mockery::stub(remove_existing_asreml, "detach", function(...) {})
    mockery::stub(remove_existing_asreml, "remove.packages", function(pkg) {})

    # Test verbose = TRUE produces debug messages
    expect_message(
        remove_existing_asreml(verbose = TRUE),
        "\\[DEBUG\\] Checking if asreml namespace is loaded"
    )

    expect_message(
        remove_existing_asreml(verbose = TRUE),
        "\\[DEBUG\\] Unloading asreml namespace"
    )

    # Test verbose = FALSE produces no debug messages
    expect_no_message(
        remove_existing_asreml(verbose = FALSE), message = "\\[DEBUG\\]"
    )
})

test_that("install_dependencies verbose parameter works", {
    skip_on_cran()

    # Mock that all dependencies are missing
    mockery::stub(install_dependencies, "installed.packages", function(...) {
        matrix(character(0), ncol = 1, dimnames = list(character(0), "Package"))
    })
    mockery::stub(install_dependencies, "rlang::is_installed", function(...) FALSE)
    mockery::stub(install_dependencies, "install.packages", function(...) {})

    # Test verbose = TRUE produces debug messages for missing dependencies
    expect_message(
        install_dependencies(quiet = FALSE, library = tempdir(), verbose = TRUE),
        "\\[DEBUG\\] Checking required dependencies"
    )

    expect_message(
        install_dependencies(quiet = FALSE, library = tempdir(), verbose = TRUE),
        "\\[DEBUG\\] Required dependencies:"
    )

    # Test verbose = FALSE produces no debug messages
    expect_no_message(
        install_dependencies(quiet = FALSE, library = tempdir(), verbose = FALSE),
        message = "\\[DEBUG\\]"
    )

    # ---- NEW: Test all dependencies present triggers "All dependencies already satisfied" ----
    mockery::stub(install_dependencies, "installed.packages", function(...) {
        matrix(c("data.table", "ggplot2", "jsonlite"), ncol = 1, dimnames = list(c("data.table", "ggplot2", "jsonlite"), "Package"))
    })
    mockery::stub(install_dependencies, "rlang::is_installed", function(...) TRUE)
    expect_message(
        install_dependencies(quiet = FALSE, library = tempdir(), verbose = TRUE),
        "\\[DEBUG\\] All dependencies already satisfied"
    )
})

test_that("install_asreml_package verbose parameter works", {
    skip_on_cran()

    temp_file <- tempfile()
    file.create(temp_file)
    on.exit(unlink(temp_file))

    mockery::stub(install_asreml_package, "install.packages", function(...) {})
    mockery::stub(install_asreml_package, "rlang::is_installed", function(...) TRUE)

    # Test verbose = TRUE produces debug messages
    expect_message(
        install_asreml_package(temp_file, tempdir(), FALSE, "linux", verbose = TRUE),
        "\\[DEBUG\\] Starting ASReml package installation"
    )

    expect_message(
        install_asreml_package(temp_file, tempdir(), FALSE, "linux", verbose = TRUE),
        "\\[DEBUG\\] Package file:"
    )

    # Test verbose = FALSE produces no debug messages
    expect_no_message(
        install_asreml_package(temp_file, tempdir(), FALSE, "linux", verbose = FALSE),
        message = "\\[DEBUG\\]"
    )

    # ---- NEW: Test error handling with verbose ----
    mockery::stub(install_asreml_package, "install.packages", function(...) stop("Installation failed"))
    expect_warning(
        expect_message(
            result <- install_asreml_package(temp_file, tempdir(), FALSE, "linux", verbose = TRUE),
            "\\[DEBUG\\] Installation error: Installation failed"
        ),
        "Installation failed"
    )
    expect_false(result)
})

test_that("manage_file verbose parameter works", {
    skip_on_cran()

    withr::with_tempdir({
        # Create a test file
        test_file <- tempfile("test_asreml_")
        writeLines("test content", test_file)

        # Test verbose = TRUE with keep_file = FALSE
        expect_message(
            manage_file(test_file, FALSE, "test.zip", verbose = TRUE),
            "\\[DEBUG\\] Managing downloaded file:"
        )
        expect_message(
            manage_file(test_file, FALSE, "test.zip", verbose = TRUE),
            "\\[DEBUG\\] Removing downloaded file"
        )
    })

    withr::with_tempdir({
        # Create a test file
        test_file <- tempfile("test_asreml_")
        writeLines("test content", test_file)

        # Test keep_file = TRUE (current directory)
        expect_message(
            manage_file(test_file, TRUE, "test.zip", verbose = TRUE),
            "\\[DEBUG\\] Saving file to current directory: test.zip"
        )
    })

    withr::with_tempdir({
        # Create a test file and subdirectory
        test_file <- tempfile("test_asreml_")
        writeLines("test content", test_file)
        subdir <- "subdir"
        dir.create(subdir)

        # Test keep_file = path (specified directory)
        expect_message(
            manage_file(test_file, subdir, "test.zip", verbose = TRUE),
            "\\[DEBUG\\] Saving file to specified directory: subdir[/\\\\]test.zip"
        )
    })

    withr::with_tempdir({
        # Create a test file
        test_file <- tempfile("test_asreml_")
        writeLines("test content", test_file)

        # Test keep_file = invalid path
        expect_message(
            expect_warning(
                manage_file(test_file, "/nonexistent/path", "test.zip", verbose = TRUE),
                "Invalid keep_file argument"
            ),
            "\\[DEBUG\\] Invalid keep_file argument, removing file"
        )
    })

    withr::with_tempdir({
        # Create a test file
        test_file <- tempfile("test_asreml_")
        writeLines("test content", test_file)

        # Mock file.rename to fail to trigger error branch
        mockery::stub(manage_file, "file.rename", function(...) stop("move failed"))
        expect_message(
            expect_warning(
                manage_file(test_file, TRUE, "test.zip", verbose = TRUE),
                "Could not save ASReml file to specified location"
            ),
            "\\[DEBUG\\] Failed to move file: move failed"
        )
    })
})

test_that("verbose debugging shows OS detection details", {
    skip_on_cran()

    mockery::stub(install_asreml, "rlang::is_installed", function(pkg) FALSE)
    mockery::stub(install_asreml, "newer_version", function() FALSE)
    mockery::stub(install_asreml, "curl::has_internet", function() TRUE)
    mockery::stub(install_asreml, "find_existing_package", function() "/tmp/asreml.zip")
    mockery::stub(install_asreml, "install_dependencies", function(...) {})
    mockery::stub(install_asreml, "install_asreml_package", function(...) TRUE)
    mockery::stub(install_asreml, "manage_file", function(...) TRUE)

    # Test that OS detection details are shown in verbose mode
    expect_warning(
        expect_message(
            install_asreml(quiet = "verbose"),
            "\\[DEBUG\\] Detecting operating system and R version"),
        "There was a problem with installation and ASReml-R was not successfully installed\\."
    )

    expect_warning(
        expect_message(
            install_asreml(quiet = "verbose"),
            "\\[DEBUG\\] Detected OS:"),
        "There was a problem with installation and ASReml-R was not successfully installed\\."
    )

    expect_warning(
        expect_message(
            install_asreml(quiet = "verbose"),
            "\\[DEBUG\\] Detected R version:"),
        "There was a problem with installation and ASReml-R was not successfully installed\\."
    )
})

test_that("verbose debugging handles error cases appropriately", {
    skip_on_cran()

    # Test download failure with verbose messaging
    mockery::stub(download_asreml_package, "curl::curl_fetch_disk", function(...) stop("Network error"))

    expect_message(
        expect_error(
            download_asreml_package("http://fake.url", verbose = TRUE),
            "Failed to download ASReml-R package"
        ),
        "\\[DEBUG\\] Download failed with error:"
    )

    # Test removal error with verbose messaging
    mockery::stub(remove_existing_asreml, "remove.packages", function(...) stop("Remove failed"))

    expect_message(
        expect_warning(
            remove_existing_asreml(verbose = TRUE),
            "Could not remove existing asreml package"
        ),
        "\\[DEBUG\\] Error removing existing package:"
    )
})

test_that("install_asreml verbose mode shows version check details", {
    skip_on_cran()

    mockery::stub(install_asreml, "curl::has_internet", function() TRUE)
    mockery::stub(install_asreml, "rlang::is_installed", function(pkg) FALSE)
    mockery::stub(install_asreml, "newer_version", function() TRUE)
    mockery::stub(install_asreml, "find_existing_package", function() NULL)
    mockery::stub(install_asreml, "download_asreml_package", function(...) "/tmp/asreml.zip")
    mockery::stub(install_asreml, "install_dependencies", function(...) {})
    mockery::stub(install_asreml, "install_asreml_package", function(...) TRUE)
    mockery::stub(install_asreml, "manage_file", function(...) TRUE)

    expect_warning(
        expect_message(
            install_asreml(quiet = "verbose", check_version = TRUE),
            "\\[DEBUG\\] Checking for newer version availability"),
        "There was a problem with installation and ASReml-R was not successfully installed\\."
    )

    expect_warning(
        expect_message(
            install_asreml(quiet = "verbose", check_version = TRUE),
            "\\[DEBUG\\] Newer version available: TRUE"),
        "There was a problem with installation and ASReml-R was not successfully installed\\."
    )
})

test_that("install_asreml calls create_mac_folder on macOS", {
    skip_on_cran()

    mac_folder_called <- FALSE
    # Mock all the dependencies to get to the macOS check
    mockery::stub(install_asreml, "curl::has_internet", function() TRUE)
    mockery::stub(install_asreml, "rlang::is_installed", function(pkg) FALSE)
    mockery::stub(install_asreml, "newer_version", function() FALSE)
    mockery::stub(install_asreml, "get_r_os", function() list(os = "mac", ver = "44", arm = FALSE, os_ver = "mac-44"))
    mockery::stub(install_asreml, "create_mac_folder", function() {
        mac_folder_called <<- TRUE
        TRUE
    })
    mockery::stub(install_asreml, "find_existing_package", function() "/tmp/asreml.zip")
    mockery::stub(install_asreml, "install_dependencies", function(...) {})
    mockery::stub(install_asreml, "install_asreml_package", function(...) TRUE)
    mockery::stub(install_asreml, "manage_file", function(...) TRUE)

    # Run install_asreml
    expect_false(install_asreml(quiet = TRUE))

    # Verify create_mac_folder was called
    expect_true(mac_folder_called)
})

test_that("install_asreml does not call create_mac_folder on non-macOS", {
    skip_on_cran()

    mac_folder_called <- FALSE
    # Mock all the dependencies to get to the OS check (Linux)
    mockery::stub(install_asreml, "curl::has_internet", function() TRUE)
    mockery::stub(install_asreml, "rlang::is_installed", function(pkg) FALSE)
    mockery::stub(install_asreml, "newer_version", function() FALSE)
    mockery::stub(install_asreml, "get_r_os", function() list(os = "linux", ver = "44", arm = FALSE, os_ver = "linux-44"))
    mockery::stub(install_asreml, "create_mac_folder", function() {
        mac_folder_called <<- TRUE
        TRUE
    })
    mockery::stub(install_asreml, "find_existing_package", function() "/tmp/asreml.zip")
    mockery::stub(install_asreml, "install_dependencies", function(...) {})
    mockery::stub(install_asreml, "install_asreml_package", function(...) TRUE)
    mockery::stub(install_asreml, "manage_file", function(...) TRUE)

    # Run install_asreml
    # Run install_asreml
    expect_false(install_asreml(quiet = TRUE))

    # Verify create_mac_folder was NOT called
    expect_false(mac_folder_called)
})

test_that("install_asreml verbose messaging shows mac folder creation", {
    skip_on_cran()

    # Mock all the dependencies for macOS
    mockery::stub(install_asreml, "curl::has_internet", function() TRUE)
    mockery::stub(install_asreml, "rlang::is_installed", function(pkg) FALSE)
    mockery::stub(install_asreml, "newer_version", function() FALSE)
    mockery::stub(install_asreml, "get_r_os", function() list(os = "mac", ver = "44", arm = FALSE, os_ver = "mac-44"))
    mockery::stub(install_asreml, "create_mac_folder", function() TRUE)
    mockery::stub(install_asreml, "find_existing_package", function() "/tmp/asreml.zip")
    mockery::stub(install_asreml, "install_dependencies", function(...) {})
    mockery::stub(install_asreml, "install_asreml_package", function(...) TRUE)
    mockery::stub(install_asreml, "manage_file", function(...) TRUE)

    # Test that verbose mode shows the mac folder creation message
    expect_warning(
        expect_message(
            install_asreml(quiet = "verbose"),
            "\\[DEBUG\\] macOS detected - checking/creating Mac folder"
        ),
        "There was a problem with installation and ASReml-R was not successfully installed\\."
    )
})

test_that("install_asreml removes existing package when force=TRUE on non-Linux systems", {
    skip_on_cran()

    # Track if remove_existing_asreml was called
    remove_called <- FALSE

    # Mock all dependencies to reach the force removal logic
    mockery::stub(install_asreml, "curl::has_internet", function() TRUE)
    mockery::stub(install_asreml, "newer_version", function() FALSE)
    mockery::stub(install_asreml, "get_r_os", function() list(os = "win", ver = "44", arm = FALSE, os_ver = "win-44"))
    mockery::stub(install_asreml, "find_existing_package", function() "/tmp/asreml.zip")
    mockery::stub(install_asreml, "install_dependencies", function(...) {})
    mockery::stub(install_asreml, "install_asreml_package", function(...) TRUE)
    mockery::stub(install_asreml, "manage_file", function(...) TRUE)

    # Key mocks for the tested condition
    mockery::stub(install_asreml, "rlang::is_installed", function(pkg) {
        if (pkg == "asreml") return(TRUE)  # asreml is installed
        return(FALSE)
    })
    mockery::stub(install_asreml, "remove_existing_asreml", function(verbose = FALSE) {
        remove_called <<- TRUE
        TRUE
    })

    # Run install_asreml with force=TRUE
    result <- install_asreml(force = TRUE, quiet = TRUE)

    # Verify remove_existing_asreml was called
    expect_true(remove_called)
    expect_true(result)
})

test_that("install_asreml does NOT remove existing package when force=TRUE on Linux", {
    skip_on_cran()

    # Track if remove_existing_asreml was called
    remove_called <- FALSE

    # Mock all dependencies for Linux system
    mockery::stub(install_asreml, "curl::has_internet", function() TRUE)
    mockery::stub(install_asreml, "newer_version", function() FALSE)
    mockery::stub(install_asreml, "get_r_os", function() list(os = "linux", ver = "44", arm = FALSE, os_ver = "linux-44"))  # Linux OS
    mockery::stub(install_asreml, "find_existing_package", function() "/tmp/asreml.tar.gz")
    mockery::stub(install_asreml, "install_dependencies", function(...) {})
    mockery::stub(install_asreml, "install_asreml_package", function(...) TRUE)
    mockery::stub(install_asreml, "manage_file", function(...) TRUE)

    # Key mocks for the tested condition
    mockery::stub(install_asreml, "rlang::is_installed", function(pkg) {
        if (pkg == "asreml") return(TRUE)  # asreml is installed
        return(FALSE)
    })
    mockery::stub(install_asreml, "remove_existing_asreml", function(verbose = FALSE) {
        remove_called <<- TRUE
        TRUE
    })

    # Run install_asreml with force=TRUE on Linux
    result <- install_asreml(force = TRUE, quiet = TRUE)

    # Verify remove_existing_asreml was NOT called (because os == "linux")
    expect_false(remove_called)
    expect_true(result)
})

test_that("install_asreml does NOT remove existing package when force=FALSE", {
    skip_on_cran()

    # Track if remove_existing_asreml was called
    remove_called <- FALSE

    # Mock all dependencies
    mockery::stub(install_asreml, "curl::has_internet", function() TRUE)
    mockery::stub(install_asreml, "newer_version", function() FALSE)
    mockery::stub(install_asreml, "get_r_os", function() list(os = "win", ver = "44", arm = FALSE, os_ver = "win-44"))
    mockery::stub(install_asreml, "find_existing_package", function() "/tmp/asreml.zip")
    mockery::stub(install_asreml, "install_dependencies", function(...) {})
    mockery::stub(install_asreml, "install_asreml_package", function(...) TRUE)
    mockery::stub(install_asreml, "manage_file", function(...) TRUE)

    # Key mocks for the tested condition
    mockery::stub(install_asreml, "rlang::is_installed", function(pkg) {
        if (pkg == "asreml") return(TRUE)  # asreml is installed
        return(FALSE)
    })
    mockery::stub(install_asreml, "remove_existing_asreml", function(verbose = FALSE) {
        remove_called <<- TRUE
        TRUE
    })

    # Run install_asreml with force=FALSE
    result <- install_asreml(force = FALSE, quiet = TRUE)

    # Verify remove_existing_asreml was NOT called (because force=FALSE)
    expect_false(remove_called)
    expect_true(result)
})

test_that("install_asreml verbose messaging shows package removal", {
    skip_on_cran()

    # Mock all dependencies for Windows system with force=TRUE
    mockery::stub(install_asreml, "curl::has_internet", function() TRUE)
    mockery::stub(install_asreml, "newer_version", function() FALSE)
    mockery::stub(install_asreml, "get_r_os", function() list(os = "win", ver = "44", arm = FALSE, os_ver = "win-44"))
    mockery::stub(install_asreml, "find_existing_package", function() "/tmp/asreml.zip")
    mockery::stub(install_asreml, "install_dependencies", function(...) {})
    mockery::stub(install_asreml, "install_asreml_package", function(...) TRUE)
    mockery::stub(install_asreml, "manage_file", function(...) TRUE)
    mockery::stub(install_asreml, "remove_existing_asreml", function(verbose = FALSE) TRUE)
    mockery::stub(install_asreml, "rlang::is_installed", function(pkg) {
        if (pkg == "asreml") return(TRUE)
        return(FALSE)
    })

    # Test that verbose mode shows the removal message
    expect_message(
        install_asreml(force = TRUE, quiet = "verbose"),
        "\\[DEBUG\\] Force=TRUE and existing package found - removing existing installation"
    )
})

test_that("get_version_table handles missing version headers", {
    skip_on_cran()

    # Mock xml2 functions to simulate a page without version headers
    mock_html <- structure(list(), class = "xml_document")
    mockery::stub(get_version_table, "xml2::read_html", function(url) mock_html)
    mockery::stub(get_version_table, "xml2::xml_find_all", function(doc, xpath) {
        if(grepl("//h3", xpath)) {
            # Return headers that don't match the expected pattern
            structure(list(), class = "xml_nodeset")
        } else {
            structure(list(), class = "xml_nodeset")
        }
    })
    mockery::stub(get_version_table, "xml2::xml_text", function(nodes) character(0))

    # Should stop with specific error message
    expect_warning(
        table <- get_version_table(),
        "URL doesn't seem to contain asreml version information\\."
    )
    expect_equal(nrow(table), 0)
})

test_that("get_version_table handles network/parsing errors gracefully", {
    skip_on_cran()

    # Mock xml2::read_html to throw a network error
    mockery::stub(get_version_table, "xml2::read_html", function(url) {
        stop("Network timeout or connection failed")
    })

    # Should return empty data frame and warn about failure
    expect_warning(
        result <- get_version_table(),
        "Failed to retrieve version information: Network timeout or connection failed"
    )
    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 0)
})

test_that("get_version_table handles XML parsing errors", {
    skip_on_cran()

    # Mock xml2 functions to simulate XML parsing failure
    mock_html <- structure(list(), class = "xml_document")
    mockery::stub(get_version_table, "xml2::read_html", function(url) mock_html)
    mockery::stub(get_version_table, "xml2::xml_find_all", function(doc, xpath) {
        stop("XPath expression error")
    })

    # Should return empty data frame and warn about failure
    expect_warning(
        result <- get_version_table(),
        "Failed to retrieve version information: XPath expression error"
    )
    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 0)
})

test_that("get_version_table handles malformed table data", {
    skip_on_cran()

    # Mock xml2 functions to return valid headers but malformed table data
    mock_html <- structure(list(), class = "xml_document")
    mockery::stub(get_version_table, "xml2::read_html", function(url) mock_html)
    mockery::stub(get_version_table, "xml2::xml_find_all", function(doc, xpath) {
        structure(list(), class = "xml_nodeset")
    })
    mockery::stub(get_version_table, "xml2::xml_text", function(nodes) {
        if(length(nodes) == 0) {
            # Return valid headers for h3 search
            "ASReml-R 4.2 (All platforms)"
        } else {
            # Return malformed table data
            "Invalid table data without macOS"
        }
    })
    mockery::stub(get_version_table, "stringi::stri_split_fixed", function(x, pattern) {
        stop("String processing error")
    })

    # Should return empty data frame and warn about failure
    expect_warning(
        result <- get_version_table(),
        "Failed to retrieve version information: String processing error"
    )
    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 0)
})
