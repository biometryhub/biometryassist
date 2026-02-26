#' Install or update the ASReml-R package
#'
#' @description Helper functions for installing or updating the ASReml-R package, intended to reduce the difficulty of finding the correct version for your operating system and R version.
#'
#' @param library Library location to install ASReml-R. Uses first option in `.libPaths()` by default.
#' @param quiet Logical or character (default `FALSE`). Controls output verbosity. `FALSE` shows normal messages, `TRUE` suppresses messages, `"verbose"` shows detailed debugging information.
#' @param force Logical (default `FALSE`). Force ASReml-R to install. Useful for upgrading if it is already installed.
#' @param keep_file Should the downloaded asreml package file be kept? Default is `FALSE`. `TRUE` downloads to current directory. A file path can also be provided to save to another directory. See `Details` for more information.
#' @param check_version Logical (default `TRUE`). Should function check if there is a newer version of asreml available before attempting to download and install?
#'
#' @details The ASReml-R package file is downloaded from a shortlink, and if `keep_file` is `TRUE`, the package archive file will be saved in the current directory. If a valid path is provided in `keep_file`, the file will be saved to that path, but all directories are assumed to exist and will not be created. If `keep_file` does not specify an existing, valid path, an error will be shown after package installation.
#'
#' @importFrom utils install.packages installed.packages download.file remove.packages tail
#' @importFrom curl curl_fetch_disk has_internet
#' @importFrom rlang is_installed
#'
#' @export
#'
#' @returns Silently returns `TRUE` if `asreml` installed successfully or already present, `FALSE` otherwise. Optionally prints a confirmation message on success.
#'
#' @examples
#' \dontrun{
#' # Example 1: download and install asreml
#' install_asreml()
#'
#' # Example 2: install asreml and save file for later
#' install_asreml(keep_file = TRUE)
#'
#' # Example 3: install with verbose debugging
#' install_asreml(quiet = "verbose")
#' }
#'
install_asreml <- function(library = .libPaths()[1], quiet = FALSE, force = FALSE, keep_file = FALSE, check_version = TRUE) {

    # Helper function to handle verbose messaging
    verbose_msg <- function(msg) {
        if (identical(quiet, "verbose")) {
            message("[DEBUG] ", msg)
        }
    }

    # Helper function to handle normal messaging
    normal_msg <- function(msg) {
        if (!isTRUE(quiet)) {
            message(msg)
        }
    }

    verbose_msg("Starting ASReml-R installation process")
    verbose_msg(paste("Library path:", library))
    verbose_msg(paste("Force install:", force))
    verbose_msg(paste("Check version:", check_version))
    verbose_msg(paste("Keep file:", keep_file))

    # Validate library parameter
    verbose_msg("Validating library parameter")
    if (!is.character(library) || length(library) != 1 || !dir.exists(library)) {
        stop("'library' must be a valid directory path. Provided: ", library, call. = FALSE)
    }

    # Check internet connectivity
    verbose_msg("Checking internet connectivity")
    if (!curl::has_internet()) {
        stop("No internet connection detected. Cannot download ASReml-R package.", call. = FALSE)
    }
    verbose_msg("Internet connection confirmed")

    verbose_msg("Checking for newer version availability")
    new_version <- if(check_version) newer_version() else FALSE
    verbose_msg(paste("Newer version available:", new_version))

    if(rlang::is_installed("asreml") && isFALSE(new_version) && isFALSE(force)) {
        verbose_msg("Latest version already installed and force=FALSE")
        normal_msg("The latest version of ASReml-R available for your system is already installed. To install anyway, set `force = TRUE`.")
        return(invisible(TRUE))
    }

    # Get OS and R version
    verbose_msg("Detecting operating system and R version")
    os_ver <- get_r_os()
    verbose_msg(paste("Detected OS:", os_ver$os))
    verbose_msg(paste("Detected R version:", os_ver$ver))
    verbose_msg(paste("ARM architecture:", os_ver$arm))
    verbose_msg(paste("OS version string:", os_ver$os_ver))

    if(os_ver$os=="mac") {
        verbose_msg("macOS detected - checking/creating Mac folder")
        create_mac_folder()
    }

    url <- paste0("https://link.biometryhubwaite.com/", os_ver$os_ver)
    verbose_msg(paste("Download URL:", url))

    # Look for existing package file, download if not found
    verbose_msg("Looking for existing package file")
    save_file <- find_existing_package()

    if(is.null(save_file)) {
        verbose_msg("No existing package file found - downloading")
        # Download file with better error handling
        normal_msg("\nDownloading and installing ASReml-R. This may take some time, depending on internet speed...\n")
        save_file <- download_asreml_package(url, verbose = identical(quiet, "verbose"))
        verbose_msg(paste("Downloaded package to:", save_file))
    } else {
        verbose_msg(paste("Using existing package file:", save_file))
    }

    # If forcing installation, remove existing version to avoid errors on installation
    if(force && rlang::is_installed("asreml") && os_ver$os != "linux") {
        verbose_msg("Force=TRUE and existing package found - removing existing installation")
        remove_existing_asreml(verbose = identical(quiet, "verbose"))
    }

    # Install dependencies if necessary
    verbose_msg("Checking and installing dependencies")
    install_dependencies(quiet, library, verbose = identical(quiet, "verbose"))

    # Install asreml
    verbose_msg("Installing ASReml-R package")
    install_result <- install_asreml_package(save_file, library, quiet, os_ver$os, verbose = identical(quiet, "verbose"))
    verbose_msg(paste("Installation result:", install_result))

    # Handle file cleanup/retention
    verbose_msg("Managing downloaded file")
    manage_file(save_file, keep_file, basename(save_file), verbose = identical(quiet, "verbose"))

    if(install_result & rlang::is_installed("asreml")) {
        verbose_msg("Installation successful - ASReml-R is available")
        normal_msg("ASReml-R successfully installed!")
        invisible(TRUE)
    }
    else {
        verbose_msg("Installation failed - ASReml-R is not available")
        if(!isTRUE(quiet)) warning("There was a problem with installation and ASReml-R was not successfully installed.")
        invisible(FALSE)
    }
}

#' @rdname install_asreml
#' @param ... other arguments passed to `install_asreml()`
#'
#' @export
update_asreml <- function(...) {
    install_asreml(force = TRUE, ...)
}

#' Find existing asreml file
#' @keywords internal
find_existing_package <- function() {
    # More robust pattern for ASReml files
    pattern <- "^asreml[_-]?.*\\.(zip|tar\\.gz|tgz)$"
    dir_files <- list.files(pattern = pattern, ignore.case = TRUE)

    result <- NULL

    if(length(dir_files) > 0) {
        # Get the most recent file (by modification time, not alphabetically)
        file_info <- file.info(dir_files)
        latest_file <- rownames(file_info)[which.max(file_info$mtime)]
        result <- normalizePath(latest_file)
    }

    return(result)
}

#' Download asreml package file
#' @param verbose Logical for verbose output
#' @keywords internal
download_asreml_package <- function(url, verbose = FALSE) {
    if (verbose) message("[DEBUG] Creating temporary file for download")
    save_file <- tempfile("asreml_")

    result <- tryCatch({
        if (verbose) message("[DEBUG] Initiating download from: ", url)
        response <- curl::curl_fetch_disk(url = url, path = save_file)
        if (verbose) message("[DEBUG] Download completed, response URL: ", response$url)

        filename <- basename(response$url)
        if (verbose) message("[DEBUG] Extracted filename: ", filename)

        final_path <- file.path(dirname(save_file), filename)
        if (verbose) message("[DEBUG] Renaming to final path: ", final_path)

        file.rename(save_file, final_path)
        normalizePath(final_path)
    }, error = function(e) {
        if (verbose) message("[DEBUG] Download failed with error: ", e$message)
        stop("Failed to download ASReml-R package: ", e$message)
    })

    return(result)
}

#' Remove existing ASReml installation
#' @param verbose Logical for verbose output
#' @keywords internal
remove_existing_asreml <- function(verbose = FALSE) {
    tryCatch({
        if (verbose) message("[DEBUG] Checking if asreml namespace is loaded")
        if("asreml" %in% loadedNamespaces()) {
            if (verbose) message("[DEBUG] Unloading asreml namespace")
            unloadNamespace("asreml")
        }
        if (verbose) message("[DEBUG] Checking if asreml package is attached")
        if("asreml" %in% .packages()) {
            if (verbose) message("[DEBUG] Detaching asreml package")
            detach("package:asreml", unload = TRUE, force = TRUE)
        }
        if (verbose) message("[DEBUG] Removing asreml package")
        suppressMessages(remove.packages("asreml"))
        if (verbose) message("[DEBUG] Successfully removed existing asreml package")
    }, error = function(e) {
        if (verbose) message("[DEBUG] Error removing existing package: ", e$message)
        warning("Could not remove existing asreml package: ", e$message)
    })
}

#' Install required dependencies
#' @param verbose Logical for verbose output
#' @keywords internal
install_dependencies <- function(quiet, library, verbose = FALSE) {
    if (verbose) message("[DEBUG] Checking required dependencies")
    required_deps <- c("data.table", "ggplot2", "jsonlite")
    if (verbose) message("[DEBUG] Required dependencies: ", paste(required_deps, collapse = ", "))

    installed_pkgs <- rownames(installed.packages(lib.loc = library))
    if (verbose) message("[DEBUG] Currently installed packages: ", length(installed_pkgs), " packages")

    missing_deps <- setdiff(required_deps, installed_pkgs)
    if (verbose) message("[DEBUG] Missing dependencies: ", paste(missing_deps, collapse = ", "))

    # Special check for data.table version
    if (verbose) message("[DEBUG] Checking data.table version requirement (>=1.14)")
    if(!rlang::is_installed("data.table", version = "1.14")) {
        if (verbose) message("[DEBUG] data.table version requirement not met")
        missing_deps <- unique(c(missing_deps, "data.table"))
    }

    if(length(missing_deps) > 0) {
        if (verbose) message("[DEBUG] Installing missing dependencies: ", paste(missing_deps, collapse = ", "))
        if(!isTRUE(quiet)) {
            message("Installing missing dependencies: ", paste(missing_deps, collapse = ", "))
        }
        install.packages(missing_deps, lib = library, repos = "https://cloud.r-project.org")
        if (verbose) message("[DEBUG] Dependency installation completed")
    } else {
        if (verbose) message("[DEBUG] All dependencies already satisfied")
    }
}

#' Install the ASReml package
#' @param save_file Path to package file
#' @param library Library path
#' @param quiet Whether to suppress messages
#' @param os Operating system
#' @param verbose Logical for verbose output
#' @returns TRUE if successful, FALSE otherwise
#' @keywords internal
install_asreml_package <- function(save_file, library, quiet, os, verbose = FALSE) {
    if (verbose) message("[DEBUG] Starting ASReml package installation")
    if (verbose) message("[DEBUG] Package file: ", save_file)
    if (verbose) message("[DEBUG] Library path: ", library)
    if (verbose) message("[DEBUG] Operating system: ", os)
    if (verbose) message("[DEBUG] Installation type: ", if(os == "win") "binary" else "source")

    tryCatch({
        install.packages(save_file,
                         lib = library,
                         repos = NULL,
                         verbose = !isTRUE(quiet),
                         type = if(os == "win") "binary" else "source")
        if (verbose) message("[DEBUG] install.packages() completed, checking if package is available")
        result <- rlang::is_installed("asreml")
        if (verbose) message("[DEBUG] Package availability check result: ", result)
        result
    }, error = function(e) {
        if (verbose) message("[DEBUG] Installation error: ", e$message)
        if(!isTRUE(quiet)) warning("Installation failed: ", e$message)
        FALSE
    })
}

#' Get the version of R and OS
#'
#' @returns A list with the version of R and the OS in a standard format
#' @keywords internal
get_r_os <- function() {

    sys_info <- Sys.info()
    # arm Macs need a different package
    arm <- sys_info[["sysname"]] == "Darwin" && sys_info[["machine"]] == "arm64"

    os <- switch(sys_info[['sysname']],
                 Windows = "win",
                 Linux   = "linux",
                 Darwin  = "mac"
    )

    ver <- gsub("\\.", "", substr(getRversion(), 1, 3))

    os_ver <- list(os_ver = paste0(os, "-", ifelse(arm, "arm-", ""), ver),
                   os = os, ver = ver, arm = arm)
    return(os_ver)
}

#' Compare installed version of ASReml-R with available versions
#'
#' @importFrom utils packageDescription
#'
#' @returns TRUE if a newer version is available online, FALSE otherwise
#' @keywords internal
newer_version <- function(manifest) {
    os_ver <- get_r_os()

    # Find matching entries for this system
    candidates <- Filter(function(x) {
        x$os    == os_ver$os &&
            x$r_ver == os_ver$ver &&
            isTRUE(x$arm) == os_ver$arm
    }, manifest$packages)

    if (length(candidates) == 0) return(FALSE)

    # Find newest available version
    versions <- sapply(candidates, `[[`, "asr_ver")
    newest   <- max(numeric_version(versions))

    # Compare with installed
    if (rlang::is_installed("asreml")) {
        desc    <- utils::packageDescription("asreml")
        current <- numeric_version(if (is.null(desc$Version)) "0" else desc$Version)
    } else {
        current <- numeric_version("0")
    }

    return(newest > current)
}

#' Create the folder MacOS needs for licensing
#'
#' @returns logical; TRUE if folder successfully created, otherwise it will error
#' @keywords internal
#' @importFrom askpass askpass
create_mac_folder <- function() {

    get_major_release <- function() {
        rel <- Sys.info()[["release"]]
        # Extract first number before dot, or fallback to full if no dot
        if (is.null(rel) || is.na(rel)) return(NA_real_)
        as.numeric(sub("^([0-9]+).*", "\\1", rel))
    }

    reprise_path <- "/Library/Application Support/Reprise/"

    is_mac <- identical(Sys.info()[["sysname"]], "Darwin")
    major_release <- suppressWarnings(get_major_release())
    reprise_exists <- dir.exists(reprise_path)

    # Only create folder on macOS Big Sur (Darwin 20) or later
    if (!is_mac || is.na(major_release) || major_release < 21 || reprise_exists) {
        return(TRUE)
    }

    # Try to create directory
    result <- tryCatch({
        dir.create(reprise_path, recursive = TRUE)
        TRUE
    }, error = function(e) FALSE)

    if (!result) {
        message("The ASReml-R package uses Reprise license management and requires administrator privileges to create the folder '/Library/Application Support/Reprise'.")
        input <- readline("Would you like to create this folder now (Yes/No)? ")

        if (toupper(trimws(input)) %in% c("YES", "Y")) {
            message("You should now be prompted for your account password.")
            Sys.sleep(2)
            system("sudo mkdir -p '/Library/Application Support/Reprise' && sudo chmod 777 '/Library/Application Support/Reprise'",
                   input = askpass::askpass("Please enter your user account password: "))
        } else {
            stop("ASReml-R cannot be installed until the folder '/Library/Application Support/Reprise' is created with appropriate permissions.\n",
                 "Please run: sudo mkdir -p '/Library/Application Support/Reprise' && sudo chmod 777 '/Library/Application Support/Reprise'",
                 call. = FALSE)
        }
    }

    dir.exists(reprise_path)
}

#' Manage the downloaded file
#'
#' @param save_file Path to the downloaded file
#' @param keep_file Whether/where to keep the file
#' @param filename Original filename
#' @param verbose Logical for verbose output
#' @returns logical; TRUE if file successfully handled, FALSE otherwise
#' @keywords internal
manage_file <- function(save_file, keep_file, filename, verbose = FALSE) {
    if (verbose) message("[DEBUG] Managing downloaded file: ", save_file)
    if (verbose) message("[DEBUG] Keep file setting: ", keep_file)

    # Remove file if not keeping
    if(isFALSE(keep_file)) {
        if (verbose) message("[DEBUG] Removing downloaded file (keep_file=FALSE)")
        unlink(save_file)
        return(TRUE)
    }

    # Determine destination path
    if(isTRUE(keep_file)) {
        dest_path <- filename  # Current directory
        if (verbose) message("[DEBUG] Saving file to current directory: ", dest_path)
    } else if(is.character(keep_file) && length(keep_file) == 1 && dir.exists(keep_file)) {
        dest_path <- file.path(keep_file, filename)
        if (verbose) message("[DEBUG] Saving file to specified directory: ", dest_path)
    } else {
        if (verbose) message("[DEBUG] Invalid keep_file argument, removing file")
        warning("Invalid keep_file argument. File not saved.", call. = FALSE)
        unlink(save_file)
        return(FALSE)
    }

    # Try to move/copy the file
    success <- tryCatch({
        file.rename(save_file, dest_path)
        if (verbose) message("[DEBUG] Successfully moved file to: ", dest_path)
        TRUE
    }, error = function(e) {
        if (verbose) message("[DEBUG] Failed to move file: ", e$message)
        warning("Could not save ASReml file to specified location: ", e$message, call. = FALSE)
        unlink(save_file)
        FALSE
    })

    return(success)
}
