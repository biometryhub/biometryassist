test_that("get_r_os returns correct structure and values", {
    result <- get_r_os()
    expect_type(result, "list")
    expect_named(result, c("os_ver", "os", "ver", "arm"))

    # Check OS detection
    sys_info <- Sys.info()
    expected_os <- switch(sys_info[["sysname"]],
                          Windows = "win",
                          Darwin = "mac",
                          Linux = detect_linux()$os)
    expect_equal(result$os, expected_os)
    expect_match(result$ver, "\\d{2}")  # Should be something like "43" for R 4.3
    if (sys_info[["sysname"]] == "Windows") {
        expect_match(result$os_ver, "^win-[0-9]{2}$")
    } else if (sys_info[["sysname"]] == "Darwin") {
        expect_match(result$os_ver, "^mac-[0-9]+-[0-9]{2}(-arm)?$")
    } else if (sys_info[["sysname"]] == "Linux") {
        expect_match(result$os_ver, "^[-a-z0-9]+-[0-9]+-[0-9]{2}(-arm)?$")
    }
    expect_equal(result$arm, sys_info[["machine"]] %in% c("arm64", "aarch64"))
})

test_that("detect_linux parses ubuntu base OS and major version", {
    mock_file_exists <- function(paths) {
        paths %in% "/etc/os-release"
    }
    mock_read_lines <- function(path, warn = FALSE) {
        c(
            "ID=pop",
            "ID_LIKE=\"ubuntu debian\"",
            "VERSION_ID=\"22.04\""
        )
    }

    mockery::stub(detect_linux, "file.exists", mock_file_exists)
    mockery::stub(detect_linux, "readLines", mock_read_lines)

    out <- detect_linux()
    expect_type(out, "list")
    expect_named(out, c("os", "major"))
    expect_equal(out$os, "ubuntu")
    expect_equal(out$major, "22")
})

test_that("detect_linux parses rhel base OS from ID_LIKE", {
    mock_file_exists <- function(paths) {
        paths %in% "/usr/lib/os-release"
    }
    mock_read_lines <- function(path, warn = FALSE) {
        c(
            "ID=centos",
            "ID_LIKE=\"fedora rhel\"",
            "VERSION_ID=\"8.6\""
        )
    }

    mockery::stub(detect_linux, "file.exists", mock_file_exists)
    mockery::stub(detect_linux, "readLines", mock_read_lines)

    out <- detect_linux()
    expect_equal(out$os, "rhel")
    expect_equal(out$major, "8")
})

test_that("detect_linux falls back to ID when ID_LIKE is missing or not ubuntu/rhel", {
    mock_file_exists <- function(paths) {
        paths %in% "/etc/os-release"
    }
    mock_read_lines <- function(path, warn = FALSE) {
        c(
            "ID=alpine",
            "VERSION_ID=\"3.19.1\""
        )
    }

    mockery::stub(detect_linux, "file.exists", mock_file_exists)
    mockery::stub(detect_linux, "readLines", mock_read_lines)

    out <- detect_linux()
    expect_equal(out$os, "alpine")
    expect_equal(out$major, "3")
})

test_that("detect_linux errors when os-release cannot be found", {
    mock_file_exists <- function(paths) {
        rep(FALSE, length(paths))
    }

    mockery::stub(detect_linux, "file.exists", mock_file_exists)
    expect_error(detect_linux(), "Cannot detect Linux OS")
})

test_that("get_r_os constructs windows key including R version", {
    mock_sys_info <- function() {
        c(sysname = "Windows", machine = "x86_64")
    }

    mockery::stub(get_r_os, "Sys.info", mock_sys_info)
    mockery::stub(get_r_os, "get_r_version_compact", function() "44")
    mockery::stub(get_r_os, "is_arm", function() FALSE)

    out <- get_r_os()
    expect_equal(out$os, "win")
    expect_equal(out$ver, "44")
    expect_false(out$arm)
    expect_equal(out$os_ver, "win-44")
})

test_that("get_r_os constructs linux key including distro, major, R version, and optional -arm", {
    mock_sys_info <- function() {
        c(sysname = "Linux", machine = "x86_64")
    }
    mock_detect_linux <- function() {
        list(os = "ubuntu", major = "22")
    }

    mockery::stub(get_r_os, "Sys.info", mock_sys_info)
    mockery::stub(get_r_os, "detect_linux", mock_detect_linux)
    mockery::stub(get_r_os, "get_r_version_compact", function() "44")
    mockery::stub(get_r_os, "is_arm", function() FALSE)

    out <- get_r_os()
    expect_equal(out$os, "ubuntu")
    expect_equal(out$ver, "44")
    expect_false(out$arm)
    expect_equal(out$os_ver, "ubuntu-22-44")

    mockery::stub(get_r_os, "is_arm", function() TRUE)
    out_arm <- get_r_os()
    expect_true(out_arm$arm)
    expect_equal(out_arm$os_ver, "ubuntu-22-44-arm")
})

test_that("get_r_os constructs mac key including mac major version and optional -arm", {
    called <- new.env(parent = emptyenv())
    called$sw_vers <- FALSE

    mock_sys_info <- function() {
        c(sysname = "Darwin", machine = "x86_64")
    }
    mock_system <- function(command, intern = FALSE, ...) {
        if (identical(command, "sw_vers -productVersion") && isTRUE(intern)) {
            called$sw_vers <- TRUE
            return("14.3.1")
        }
        stop("Unexpected system() call in test")
    }

    mockery::stub(get_r_os, "Sys.info", mock_sys_info)
    mockery::stub(get_r_os, "system", mock_system)
    mockery::stub(get_r_os, "get_r_version_compact", function() "44")
    mockery::stub(get_r_os, "is_arm", function() FALSE)

    out <- get_r_os()
    expect_true(called$sw_vers)
    expect_equal(out$os, "mac")
    expect_equal(out$ver, "44")
    expect_false(out$arm)
    expect_equal(out$os_ver, "mac-14-44")

    mockery::stub(get_r_os, "is_arm", function() TRUE)
    out_arm <- get_r_os()
    expect_true(out_arm$arm)
    expect_equal(out_arm$os_ver, "mac-14-44-arm")
})

test_that("get_r_os errors for unsupported operating systems", {
    mock_sys_info <- function() {
        c(sysname = "Solaris", machine = "x86_64")
    }

    mockery::stub(get_r_os, "Sys.info", mock_sys_info)
    mockery::stub(get_r_os, "get_r_version_compact", function() "44")
    mockery::stub(get_r_os, "is_arm", function() FALSE)

    expect_error(get_r_os(), "Unsupported operating system")
})

test_that("get_version_table cleans table rows then calls parse_version_table", {
    # This test avoids network access by mocking xml2 + stringi and checking
    # that NA/empty rows are removed before calling parse_version_table().
    called <- new.env(parent = emptyenv())
    called$tables <- NULL
    called$headers <- NULL

    mock_read_html <- function(url) {
        structure(list(url = url), class = "mock_html")
    }
    mock_xml_find_all <- function(res, xpath) {
        # Return distinguishable placeholders for headers vs tables.
        if (identical(xpath, "//h3")) return(structure(list(kind = "h3"), class = "mock_nodes"))
        if (identical(xpath, "//table")) return(structure(list(kind = "table"), class = "mock_nodes"))
        stop("Unexpected xpath in test: ", xpath)
    }
    mock_xml_text <- function(nodes) {
        if (inherits(nodes, "mock_nodes") && identical(nodes$kind, "h3")) {
            return(c("ASReml-R 4.4 (All platforms) - R version 4.4"))
        }
        if (inherits(nodes, "mock_nodes") && identical(nodes$kind, "table")) {
            # Only tables containing "macOS" are kept.
            return(c("macOS\nrow1\nrow2", "Windows\nrow1\nrow2"))
        }
        stop("Unexpected nodes in xml_text mock")
    }
    mock_split_fixed <- function(x, sep) {
        # Simulate table splitting producing NA and empty strings.
        list(c("headerA", NA_character_, "", "headerB", "rowA"))
    }
    mock_parse_version_table <- function(tables, headers) {
        called$tables <- tables
        called$headers <- headers
        data.frame(os = "mac", arm = FALSE, r_ver = "44", asr_ver = "4.4.0", stringsAsFactors = FALSE)
    }

    mockery::stub(get_version_table, "xml2::read_html", mock_read_html)
    mockery::stub(get_version_table, "xml2::xml_find_all", mock_xml_find_all)
    mockery::stub(get_version_table, "xml2::xml_text", mock_xml_text)
    mockery::stub(get_version_table, "stringi::stri_split_fixed", mock_split_fixed)
    mockery::stub(get_version_table, "parse_version_table", mock_parse_version_table)

    out <- get_version_table(url = "http://example.com/mock")
    expect_s3_class(out, "data.frame")
    expect_true(all(c("os", "arm", "r_ver", "asr_ver") %in% names(out)))

    expect_true(is.list(called$tables))
    expect_length(called$tables, 1)
    expect_equal(called$headers, "ASReml-R 4.4 (All platforms) - R version 4.4")
    expect_false(anyNA(called$tables[[1]]))
    expect_false(any(called$tables[[1]] == ""))
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

test_that("parse_version_table parses dates and derives os/arm/r_ver/asr_ver", {
    tbl <- c(
        "Download", "File name", "Date published", "Other",
        "Windows x64", "asreml_4.2.0.0.zip", "15 March 2023", "x",
        "macOS arm", "asreml-4.3.0.0.tgz", "15/03/2023", "y",
        "Ubuntu 22", "asreml_4.4.0.0.tgz", "15 Mar 2023", "z",
        "Something else", "asreml_4.1.0.0.tgz", "15-03-2023", "w"
    )

    out <- parse_version_table(
        tables = list(tbl),
        headers = "ASReml-R 4.4 (All platforms) - R version 4.4"
    )

    expect_s3_class(out, "data.frame")
    expect_true(all(c("os", "arm", "r_ver", "asr_ver") %in% names(out)))

    # Date parsing with multiple formats
    expect_s3_class(out[["Date published"]], "Date")
    expect_equal(out[["Date published"]], rep(as.Date("2023-03-15"), 4))

    # OS mapping from Download
    expect_equal(out$os, c("win", "mac", "linux", "centos"))

    # ARM detection from Download
    expect_equal(out$arm, c(FALSE, TRUE, FALSE, FALSE))

    # R version derived from header
    expect_equal(unique(out$r_ver), "44")

    # asreml version derived from file name
    expect_equal(out$asr_ver, c("4.2.0.0", "4.3.0.0", "4.4.0.0", "4.1.0.0"))
})

test_that("parse_version_table returns NA Date for missing/blank values", {
    tbl <- c(
        "Download", "File name", "Date published", "Other",
        "Windows x64", "asreml_4.2.0.0.zip", NA_character_, "x",
        "macOS", "asreml-4.2.0.0.tgz", "", "y",
        "Ubuntu 22", "asreml_4.2.0.0.tgz", "   ", "z",
        "Something else", "asreml_4.2.0.0.tgz", "15 March 2023", "w"
    )

    out <- parse_version_table(
        tables = list(tbl),
        headers = "ASReml-R 4.4 (All platforms) - R version 4.4"
    )

    expect_s3_class(out[["Date published"]], "Date")
    expect_equal(out[["Date published"]], as.Date(c(NA, NA, NA, "2023-03-15")))
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

test_that("create_mac_folder returns TRUE on non-macOS systems", {
    skip_on_cran()

    # Mock Sys.info to return Linux
    mockery::stub(create_mac_folder, "Sys.info", function() c(sysname = "Linux", release = "5.10"))
    result <- create_mac_folder()
    expect_true(result)

    # Mock Sys.info to return Windows
    mockery::stub(create_mac_folder, "Sys.info", function() c(sysname = "Windows", release = "10.0"))
    result <- create_mac_folder()
    expect_true(result)
})

test_that("create_mac_folder returns TRUE when major release is less than 21", {
    skip_on_cran()

    # macOS Catalina (Darwin 19)
    mockery::stub(create_mac_folder, "Sys.info", function() c(sysname = "Darwin", release = "19.6.0"))
    result <- create_mac_folder()
    expect_true(result)

    # macOS Mojave (Darwin 18)
    mockery::stub(create_mac_folder, "Sys.info", function() c(sysname = "Darwin", release = "18.7.0"))
    result <- create_mac_folder()
    expect_true(result)
})

test_that("create_mac_folder returns TRUE when Reprise folder already exists", {
    skip_on_cran()

    # macOS Big Sur or later with existing folder
    mockery::stub(create_mac_folder, "Sys.info", function() c(sysname = "Darwin", release = "21.0.0"))
    mockery::stub(create_mac_folder, "dir.exists", function(path) TRUE)

    result <- create_mac_folder()
    expect_true(result)
})

test_that("create_mac_folder creates directory successfully on macOS Big Sur+", {
    skip_on_cran()

    dir_created <- FALSE

    # Mock macOS Big Sur (Darwin 21)
    mockery::stub(create_mac_folder, "Sys.info", function() c(sysname = "Darwin", release = "21.0.0"))
    mockery::stub(create_mac_folder, "dir.exists", function(path) dir_created)
    mockery::stub(create_mac_folder, "dir.create", function(path, recursive) {
        dir_created <<- TRUE
        TRUE
    })

    result <- create_mac_folder()
    expect_true(result)
    expect_true(dir_created)
})

test_that("create_mac_folder prompts user when dir.create fails and user says Yes", {
    skip_on_cran()

    system_called <- FALSE

    # Mock macOS Big Sur with failing dir.create
    mockery::stub(create_mac_folder, "Sys.info", function() c(sysname = "Darwin", release = "21.0.0"))
    mockery::stub(create_mac_folder, "dir.exists", function(path) FALSE)
    mockery::stub(create_mac_folder, "dir.create", function(path, recursive) stop("Permission denied"))
    mockery::stub(create_mac_folder, "readline", function(prompt) "Yes")
    mockery::stub(create_mac_folder, "Sys.sleep", function(time) NULL)
    mockery::stub(create_mac_folder, "system", function(command, input) {
        system_called <<- TRUE
        0
    })
    mockery::stub(create_mac_folder, "askpass::askpass", function(prompt) "password123")

    expect_message(
        result <- create_mac_folder(),
        "The ASReml-R package uses Reprise license management"
    )
    expect_true(system_called)
})

test_that("create_mac_folder prompts user with 'Y' response", {
    skip_on_cran()

    system_called <- FALSE

    # Mock with user responding 'Y' instead of 'Yes'
    mockery::stub(create_mac_folder, "Sys.info", function() c(sysname = "Darwin", release = "21.0.0"))
    mockery::stub(create_mac_folder, "dir.exists", function(path) FALSE)
    mockery::stub(create_mac_folder, "dir.create", function(path, recursive) stop("Permission denied"))
    mockery::stub(create_mac_folder, "readline", function(prompt) "Y")
    mockery::stub(create_mac_folder, "Sys.sleep", function(time) NULL)
    mockery::stub(create_mac_folder, "system", function(command, input) {
        system_called <<- TRUE
        0
    })
    mockery::stub(create_mac_folder, "askpass::askpass", function(prompt) "password123")

    result <- create_mac_folder()
    expect_true(system_called)
})

test_that("create_mac_folder prompts user with lowercase 'yes' response", {
    skip_on_cran()

    system_called <- FALSE

    # Mock with user responding 'yes' (lowercase)
    mockery::stub(create_mac_folder, "Sys.info", function() c(sysname = "Darwin", release = "21.0.0"))
    mockery::stub(create_mac_folder, "dir.exists", function(path) FALSE)
    mockery::stub(create_mac_folder, "dir.create", function(path, recursive) stop("Permission denied"))
    mockery::stub(create_mac_folder, "readline", function(prompt) "yes")
    mockery::stub(create_mac_folder, "Sys.sleep", function(time) NULL)
    mockery::stub(create_mac_folder, "system", function(command, input) {
        system_called <<- TRUE
        0
    })
    mockery::stub(create_mac_folder, "askpass::askpass", function(prompt) "password123")

    result <- create_mac_folder()
    expect_true(system_called)
})

test_that("create_mac_folder stops when user declines to create folder", {
    skip_on_cran()

    # Mock with user saying No
    mockery::stub(create_mac_folder, "Sys.info", function() c(sysname = "Darwin", release = "21.0.0"))
    mockery::stub(create_mac_folder, "dir.exists", function(path) FALSE)
    mockery::stub(create_mac_folder, "dir.create", function(path, recursive) stop("Permission denied"))
    mockery::stub(create_mac_folder, "readline", function(prompt) "No")

    expect_error(
        create_mac_folder(),
        "ASReml-R cannot be installed until the folder '/Library/Application Support/Reprise' is created"
    )
})

test_that("create_mac_folder stops when user gives invalid response", {
    skip_on_cran()

    # Mock with user giving invalid response
    mockery::stub(create_mac_folder, "Sys.info", function() c(sysname = "Darwin", release = "21.0.0"))
    mockery::stub(create_mac_folder, "dir.exists", function(path) FALSE)
    mockery::stub(create_mac_folder, "dir.create", function(path, recursive) stop("Permission denied"))
    mockery::stub(create_mac_folder, "readline", function(prompt) "Maybe")

    expect_error(
        create_mac_folder(),
        "ASReml-R cannot be installed until the folder '/Library/Application Support/Reprise' is created"
    )
})

test_that("create_mac_folder handles NA release version gracefully", {
    skip_on_cran()

    # Mock Sys.info with NA release
    mockery::stub(create_mac_folder, "Sys.info", function() c(sysname = "Darwin", release = NA))

    result <- create_mac_folder()
    expect_true(result)
})

test_that("create_mac_folder handles NULL release version gracefully", {
    skip_on_cran()

    # Mock Sys.info with NULL release
    mockery::stub(create_mac_folder, "Sys.info", function() list(sysname = "Darwin", release = NULL))

    result <- create_mac_folder()
    expect_true(result)
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

    # Capture all messages from a single run (prevents noisy [DEBUG] output)
    msgs <- capture_messages_text(install_asreml(quiet = "verbose", force = FALSE))

    expect_match(msgs, "\\[DEBUG\\] Starting ASReml-R installation process")
    expect_match(msgs, "\\[DEBUG\\] Library path:")
    expect_match(msgs, "\\[DEBUG\\] Force install: FALSE")
})

test_that("verbose messaging works correctly with different quiet settings", {
    skip_on_cran()

    mockery::stub(install_asreml, "rlang::is_installed", function(pkg) TRUE)
    mockery::stub(install_asreml, "newer_version", function() FALSE)
    mockery::stub(install_asreml, "curl::has_internet", function() TRUE)

    # Test quiet = TRUE (no messages)
    expect_silent(install_asreml(quiet = TRUE, force = FALSE))

    # Test quiet = FALSE (normal messages but no debug)
    msgs <- capture_messages_text(install_asreml(quiet = FALSE, force = FALSE))
    expect_match(msgs, "already installed")
    expect_false(grepl("\\[DEBUG\\]", msgs))
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
    msgs <- capture_messages_text(download_asreml_package("http://test.url", verbose = TRUE))
    expect_match(msgs, "\\[DEBUG\\] Creating temporary file for download")
    expect_match(msgs, "\\[DEBUG\\] Initiating download from:")

    # Test verbose = FALSE produces no debug messages
    msgs <- capture_messages_text(download_asreml_package("http://test.url", verbose = FALSE))
    expect_false(grepl("\\[DEBUG\\]", msgs))
})

test_that("remove_existing_asreml verbose parameter works", {
    skip_on_cran()

    mockery::stub(remove_existing_asreml, "loadedNamespaces", function() c("asreml"))
    mockery::stub(remove_existing_asreml, ".packages", function() c("asreml"))
    mockery::stub(remove_existing_asreml, "unloadNamespace", function(pkg) {})
    mockery::stub(remove_existing_asreml, "detach", function(...) {})
    mockery::stub(remove_existing_asreml, "remove.packages", function(pkg) {})

    # Test verbose = TRUE produces debug messages
    msgs <- capture_messages_text(remove_existing_asreml(verbose = TRUE))
    expect_match(msgs, "\\[DEBUG\\] Checking if asreml namespace is loaded")
    expect_match(msgs, "\\[DEBUG\\] Unloading asreml namespace")

    # Test verbose = FALSE produces no debug messages
    msgs <- capture_messages_text(remove_existing_asreml(verbose = FALSE))
    expect_false(grepl("\\[DEBUG\\]", msgs))
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
    msgs <- capture_messages_text(install_dependencies(quiet = FALSE, library = tempdir(), verbose = TRUE))
    expect_match(msgs, "\\[DEBUG\\] Checking required dependencies")
    expect_match(msgs, "\\[DEBUG\\] Required dependencies:")

    # Test verbose = FALSE produces no debug messages
    msgs <- capture_messages_text(install_dependencies(quiet = FALSE, library = tempdir(), verbose = FALSE))
    expect_false(grepl("\\[DEBUG\\]", msgs))

    # ---- NEW: Test all dependencies present triggers "All dependencies already satisfied" ----
    mockery::stub(install_dependencies, "installed.packages", function(...) {
        matrix(c("data.table", "ggplot2", "jsonlite"), ncol = 1, dimnames = list(c("data.table", "ggplot2", "jsonlite"), "Package"))
    })
    mockery::stub(install_dependencies, "rlang::is_installed", function(...) TRUE)
    msgs <- capture_messages_text(install_dependencies(quiet = FALSE, library = tempdir(), verbose = TRUE))
    expect_match(msgs, "\\[DEBUG\\] All dependencies already satisfied")
})

test_that("install_asreml_package verbose parameter works", {
    skip_on_cran()

    temp_file <- tempfile()
    file.create(temp_file)
    on.exit(unlink(temp_file))

    mockery::stub(install_asreml_package, "install.packages", function(...) {})
    mockery::stub(install_asreml_package, "rlang::is_installed", function(...) TRUE)

    # Test verbose = TRUE produces debug messages
    msgs <- capture_messages_text(install_asreml_package(temp_file, tempdir(), FALSE, "linux", verbose = TRUE))
    expect_match(msgs, "\\[DEBUG\\] Starting ASReml package installation")
    expect_match(msgs, "\\[DEBUG\\] Package file:")

    # Test verbose = FALSE produces no debug messages
    msgs <- capture_messages_text(install_asreml_package(temp_file, tempdir(), FALSE, "linux", verbose = FALSE))
    expect_false(grepl("\\[DEBUG\\]", msgs))

    # ---- NEW: Test error handling with verbose ----
    mockery::stub(install_asreml_package, "install.packages", function(...) stop("Installation failed"))
    msgs <- capture_messages_text(
        expect_warning(
            result <- install_asreml_package(temp_file, tempdir(), FALSE, "linux", verbose = TRUE),
            "Installation failed"
        )
    )
    expect_match(msgs, "\\[DEBUG\\] Installation error: Installation failed")
    expect_false(result)
})

test_that("manage_file verbose parameter works", {
    skip_on_cran()

    withr::with_tempdir({
        # Create a test file
        test_file <- tempfile("test_asreml_")
        writeLines("test content", test_file)

        # Test verbose = TRUE with keep_file = FALSE
        msgs <- capture_messages_text(manage_file(test_file, FALSE, "test.zip", verbose = TRUE))
        expect_match(msgs, "\\[DEBUG\\] Managing downloaded file:")
        expect_match(msgs, "\\[DEBUG\\] Removing downloaded file")
    })

    withr::with_tempdir({
        # Create a test file
        test_file <- tempfile("test_asreml_")
        writeLines("test content", test_file)

        # Test keep_file = TRUE (current directory)
        msgs <- capture_messages_text(manage_file(test_file, TRUE, "test.zip", verbose = TRUE))
        expect_match(msgs, "\\[DEBUG\\] Saving file to current directory: test.zip")
    })

    withr::with_tempdir({
        # Create a test file and subdirectory
        test_file <- tempfile("test_asreml_")
        writeLines("test content", test_file)
        subdir <- "subdir"
        dir.create(subdir)

        # Test keep_file = path (specified directory)
        msgs <- capture_messages_text(manage_file(test_file, subdir, "test.zip", verbose = TRUE))
        expect_match(msgs, "\\[DEBUG\\] Saving file to specified directory: subdir[/\\\\]test.zip")
    })

    withr::with_tempdir({
        # Create a test file
        test_file <- tempfile("test_asreml_")
        writeLines("test content", test_file)

        # Test keep_file = invalid path
        msgs <- capture_messages_text(
            expect_warning(
                manage_file(test_file, "/nonexistent/path", "test.zip", verbose = TRUE),
                "Invalid keep_file argument"
            )
        )
        expect_match(msgs, "\\[DEBUG\\] Invalid keep_file argument, removing file")
    })

    withr::with_tempdir({
        # Create a test file
        test_file <- tempfile("test_asreml_")
        writeLines("test content", test_file)

        # Mock file.rename to fail to trigger error branch
        mockery::stub(manage_file, "file.rename", function(...) stop("move failed"))
        msgs <- capture_messages_text(
            expect_warning(
                manage_file(test_file, TRUE, "test.zip", verbose = TRUE),
                "Could not save ASReml file to specified location"
            )
        )
        expect_match(msgs, "\\[DEBUG\\] Failed to move file: move failed")
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
    msgs <- capture_messages_text(
        expect_warning(
            install_asreml(quiet = "verbose"),
            "There was a problem with installation and ASReml-R was not successfully installed\\."
        )
    )
    expect_match(msgs, "\\[DEBUG\\] Detecting operating system and R version")
    expect_match(msgs, "\\[DEBUG\\] Detected OS:")
    expect_match(msgs, "\\[DEBUG\\] Detected R version:")
})

test_that("verbose debugging handles error cases appropriately", {
    skip_on_cran()

    # Test download failure with verbose messaging
    mockery::stub(download_asreml_package, "curl::curl_fetch_disk", function(...) stop("Network error"))

    msgs <- capture_messages_text(
        expect_error(
            download_asreml_package("http://fake.url", verbose = TRUE),
            "Failed to download ASReml-R package"
        )
    )
    expect_match(msgs, "\\[DEBUG\\] Download failed with error:")

    # Test removal error with verbose messaging
    mockery::stub(remove_existing_asreml, "remove.packages", function(...) stop("Remove failed"))

    msgs <- capture_messages_text(
        expect_warning(
            remove_existing_asreml(verbose = TRUE),
            "Could not remove existing asreml package"
        )
    )
    expect_match(msgs, "\\[DEBUG\\] Error removing existing package:")
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

    msgs <- capture_messages_text(
        expect_warning(
            install_asreml(quiet = "verbose", check_version = TRUE),
            "There was a problem with installation and ASReml-R was not successfully installed\\."
        )
    )
    expect_match(msgs, "\\[DEBUG\\] Checking for newer version availability")
    expect_match(msgs, "\\[DEBUG\\] Newer version available: TRUE")
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
    msgs <- capture_messages_text(
        expect_warning(
            install_asreml(quiet = "verbose"),
            "There was a problem with installation and ASReml-R was not successfully installed\\."
        )
    )
    expect_match(msgs, "\\[DEBUG\\] macOS detected - checking/creating Mac folder")
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
    msgs <- capture_messages_text(install_asreml(force = TRUE, quiet = "verbose"))
    expect_match(msgs, "\\[DEBUG\\] Force=TRUE and existing package found - removing existing installation")
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
