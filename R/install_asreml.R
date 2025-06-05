#' Install or update the ASReml-R package
#'
#' @description Helper functions for installing or updating the ASReml-R package, intended to reduce the difficulty of finding the correct version for your operating system and R version.
#'
#' @param library Library location to install ASReml-R. Uses first option in `.libPaths()` by default.
#' @param quiet Logical (default `FALSE`). Should package be installed quietly?
#' @param force Logical (default `FALSE`). Force ASReml-R to install. Useful for upgrading if it is already installed.
#' @param keep_file Should the downloaded asreml package file be kept? Default is `FALSE`. `TRUE` downloads to current directory. A file path can also be provided to save to another directory. See `Details` for more information.
#' @param check_version Logical (default `TRUE`). Should function check if there is a newer version of asreml available before attempting to download and install?
#'
#' @details The ASReml-R package file is downloaded from a shortlink, and if `keep_file` is `TRUE`, the package archive file will be saved in the current directory. If a valid path is provided in `keep_file`, the file will be saved to that path, but all directories are assumed to exist and will not be created. If `keep_file` does not specify an existing, valid path, an error will be shown after package installation.
#'
#' @importFrom utils install.packages installed.packages download.file remove.packages tail
#' @importFrom curl curl_fetch_disk has_internet()
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
#' }
#'
install_asreml <- function(library = .libPaths()[1], quiet = FALSE, force = FALSE, keep_file = FALSE, check_version = TRUE) {

    # Validate library parameter
    if (!is.character(library) || length(library) != 1 || !dir.exists(library)) {
        stop("'library' must be a valid directory path. Provided: ", library, call. = FALSE)
    }

    # Check internet connectivity
    if (!curl::has_internet()) {
        stop("No internet connection detected. Cannot download ASReml-R package.", call. = FALSE)
    }

    new_version <- if(check_version) newer_version() else FALSE

    if(.check_package_available("asreml") && isFALSE(new_version) && isFALSE(force)) {
        if(!quiet) message("The latest version of ASReml-R available for your system is already installed. To install anyway, set `force = TRUE`.")
        return(invisible(TRUE))
    }
    
    # Get OS and R version
    os_ver <- get_r_os()
    if(os_ver$os=="mac") {
        create_mac_folder()
    }

    url <- paste0("https://link.biometryhubwaite.com/", os_ver$os_ver)

    # Look for existing package file, download if not found
    save_file <- find_existing_package()
    
    if(is.null(save_file)) {
        # Download file with better error handling
        if(!quiet) {
            message("\nDownloading and installing ASReml-R. This may take some time, depending on internet speed...\n")
        }
        
        save_file <- download_asreml_package(url)
    }

    # If forcing installation, remove existing version to avoid errors on installation
    if(force && .check_package_available("asreml") && os_ver$os != "linux") {
        remove_existing_asreml()
    }

    # Install dependencies if necessary
    install_dependencies(quiet)

        # Install asreml
    install_result <- install_asreml_package(save_file, library, quiet, os_ver$os)

    # Handle file cleanup/retention
    manage_file(save_file, keep_file, basename(save_file))

    if(install_result & .check_package_available("asreml")) {
        if(!quiet) message("ASReml-R successfully installed!")
        invisible(TRUE)
    }
    else {
        if(!quiet) warning("There was a problem with installation and ASReml-R was not successfully installed.")
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
#' @keywords internal
download_asreml_package <- function(url) {
    save_file <- tempfile("asreml_")
    
    result <- tryCatch({
        response <- curl::curl_fetch_disk(url = url, path = save_file)
        filename <- basename(response$url)
        final_path <- file.path(dirname(save_file), filename)
        file.rename(save_file, final_path)
        normalizePath(final_path)
    }, error = function(e) {
        stop("Failed to download ASReml-R package: ", e$message)
        NULL
    })

    return(result)
}

#' Remove existing ASReml installation
#' @param os Operating system
#' @keywords internal
remove_existing_asreml <- function() {
    tryCatch({
        if("asreml" %in% loadedNamespaces()) {
            unloadNamespace("asreml")
        }
        if("asreml" %in% .packages()) {
            detach("package:asreml", unload = TRUE, force = TRUE)
        }
        suppressMessages(remove.packages("asreml"))
    }, error = function(e) {
        warning("Could not remove existing asreml package: ", e$message)
    })
}

#' Install required dependencies
#' @keywords internal
install_dependencies <- function(quiet) {
    required_deps <- c("data.table", "ggplot2", "jsonlite")
    installed_pkgs <- rownames(installed.packages(lib.loc = library))
    
    missing_deps <- setdiff(required_deps, installed_pkgs)
    
    # Special check for data.table version
    if(!rlang::is_installed("data.table", version = "1.14")) {
        missing_deps <- unique(c(missing_deps, "data.table"))
    }
    
    if(length(missing_deps) > 0) {
        if(!quiet) {
            message("Installing missing dependencies: ", paste(missing_deps, collapse = ", "))
        }
        install.packages(missing_deps, lib = library, repos = "https://cloud.r-project.org")
    }
}

#' Install the ASReml package
#' @param save_file Path to package file
#' @param library Library path
#' @param quiet Whether to suppress messages
#' @param os Operating system
#' @returns TRUE if successful, FALSE otherwise
#' @keywords internal
install_asreml_package <- function(save_file, library, quiet, os) {
    tryCatch({
        install.packages(save_file, 
                        lib = library, 
                        repos = NULL, 
                        verbose = !quiet, 
                        type = if(os == "win") "binary" else "source")
        .check_package_available("asreml")
    }, error = function(e) {
        if(!quiet) warning("Installation failed: ", e$message)
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

#' Get released versions of ASReml-R in lookup table
#'
#' @returns A list of data frames containing the version number and release date of released ASReml-R versions for comparison
#' @keywords internal
#' @importFrom xml2 read_html xml_text xml_find_all
#' @importFrom stringi stri_split_fixed
get_version_table <- function(url = "https://asreml.kb.vsni.co.uk/asreml-r-4-download-success/?site_reference=VS9AF20") {
    
    tryCatch({
        res <- xml2::read_html(url)
        
        headers <- xml2::xml_text(xml2::xml_find_all(res, "//h3"))
        headers <- headers[grepl("^ASReml-?R? 4.*\\(All platforms\\)", headers)]
        
        if(length(headers) == 0) {
            stop("URL doesn't seem to contain asreml version information.")
        }
        
        tables <- xml2::xml_text(xml2::xml_find_all(res, xpath = "//table"))
        tables <- tables[grepl("macOS", tables)]
        tables <- stringi::stri_split_fixed(tables, "\n")
        tables <- lapply(tables, function(x) x[!is.na(x) & x != ""])
        
        parse_version_table(tables, headers)
        
    }, error = function(e) {
        warning("Failed to retrieve version information: ", e$message)
        data.frame()  # Return empty data frame on error
    })
}

#' Parse version table from web scraping
#' @param tables List of table data
#' @param headers Header information
#' @returns Combined data frame of version information
#' @keywords internal
parse_version_table <- function(tables, headers) {
    fix_tables <- function(x) {
        first_row <- x[1:4]
        x <- as.data.frame(matrix(x[5:length(x)], ncol = 4, byrow = TRUE))
        colnames(x) <- first_row
        
        # Parse dates
        date_col <- grep("Date", colnames(x))
        if(length(date_col) > 0) {
            x[, date_col] <- as.Date(x[, date_col], 
                                   tryFormats = c("%d %B %Y", "%d/%m/%Y", "%d %b %Y", "%d-%m-%Y"))
        }
        x
    }
    
    for(i in seq_along(tables)) {
        tables[[i]] <- fix_tables(tables[[i]])
        tables[[i]][["os"]] <- case_when(
            grepl("Windows", tables[[i]][["Download"]], ignore.case = TRUE) ~ "win",
            grepl("macOS", tables[[i]][["Download"]], ignore.case = TRUE) ~ "mac",
            grepl("Ubuntu", tables[[i]][["Download"]], ignore.case = TRUE) ~ "linux",
            TRUE ~ "centos"
        )
        tables[[i]][["arm"]] <- grepl("arm", tables[[i]][["Download"]], ignore.case = TRUE)
        tables[[i]][["r_ver"]] <- paste0(stringi::stri_match_first_regex(headers[i], "R version (\\d?)\\.(\\d?)")[2:3], collapse = "")
        tables[[i]][["asr_ver"]] <- stringi::stri_match_first_regex(tables[[i]][["File name"]], "asreml-?_?(\\d\\.\\d?\\.\\d?\\.\\d*)")[,2]
    }
    
    do.call("rbind", tables)
}

#' Compare installed version of ASReml-R with available versions
#'
#' @importFrom utils packageDescription
#'
#' @returns TRUE if a newer version is available online, FALSE otherwise
#' @keywords internal
newer_version <- function() {
    online_versions <- get_version_table()
    
    if(nrow(online_versions) == 0) {
        return(FALSE)  # Can't check, assume no update needed
    }
    
    os_ver <- get_r_os()
    
    # Find the newest version for this system
    newest <- subset(online_versions,
                     online_versions$os == os_ver$os &
                     online_versions$arm == os_ver$arm &
                     online_versions$r_ver == os_ver$ver)
    
    if(nrow(newest) == 0) {
        return(FALSE)
    }
    
    newest <- newest[which.max(numeric_version(as.character(newest$asr_ver))), ]
    
    # Get current version info
    if(.check_package_available("asreml")) {
        asr_desc <- utils::packageDescription("asreml")
        asr_date <- as.Date(substr(asr_desc$Packaged %||% "1900-01-01", 1, 10))
        asr_ver <- asr_desc$Version %||% "0"
    } else {
        asr_date <- as.Date("1900-01-01")
        asr_ver <- "0"
    }
    
    # Check if newer version is available
    (newest$`Date published` > asr_date + 7) && 
    (numeric_version(as.character(newest$asr_ver)) > numeric_version(as.character(asr_ver)))
}

#' Create the folder MacOS needs for licensing
#'
#' @returns logical; TRUE if folder successfully created, otherwise it will error
#' @keywords internal
#' @importFrom askpass askpass
create_mac_folder <- function() {
    reprise_path <- "/Library/Application Support/Reprise/"
    
    # Only needed for macOS Big Sur and later
    if(Sys.info()[["sysname"]] != "Darwin" || 
       as.numeric(Sys.info()["release"]) < 21 || 
       dir.exists(reprise_path)) {
        return(TRUE)
    }
    
    # Try to create directory
    result <- tryCatch({
        dir.create(reprise_path, recursive = TRUE)
        TRUE
    }, error = function(e) FALSE)
    
    if(!result) {
        message("The ASReml-R package uses Reprise license management and requires administrator privileges to create the folder '/Library/Application Support/Reprise'.")
        input <- readline("Would you like to create this folder now (Yes/No)? ")
        
        if(toupper(trimws(input)) %in% c("YES", "Y")) {
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
#' @returns logical; TRUE if file successfully handled, FALSE otherwise
#' @keywords internal
manage_file <- function(save_file, keep_file, filename) {
    
    # Remove file if not keeping
    if(isFALSE(keep_file)) {
        unlink(save_file)
        return(TRUE)
    }
    
    # Determine destination path
    if(isTRUE(keep_file)) {
        dest_path <- filename  # Current directory
    } else if(is.character(keep_file) && length(keep_file) == 1 && dir.exists(keep_file)) {
        dest_path <- file.path(keep_file, filename)
    } else {
        warning("Invalid keep_file argument. File not saved.", call. = FALSE)
        unlink(save_file)
        return(FALSE)
    }
    
    # Try to move/copy the file
    success <- tryCatch({
        file.rename(save_file, dest_path)
        TRUE
    }, error = function(e) {
        warning("Could not save ASReml file to specified location: ", e$message, call. = FALSE)
        unlink(save_file)
        FALSE
    })
    
    return(success)
}