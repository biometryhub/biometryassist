#' Install or update the ASReml-R package
#'
#' @description Helper functions for installing or updating the ASReml-R package, intended to reduce the difficulty of finding the correct version for your operating system and R version.
#'
#' @param library Library location to install ASReml-R. Uses first option in `.libPaths()` by default.
#' @param quiet Logical (default `FALSE`). Should package be installed quietly?
#' @param force Logical (default `FALSE`). Force ASReml-R to install. Useful for upgrading if it is already installed.
#' @param keep_file Should the downloaded asreml package file be kept? Default is `FALSE`. `TRUE` downloads to current directory. A file path can also be provided to save to another directory. See `Details` for more information.
#'
#' @details The ASReml-R package file is downloaded from a shortlink, and if `keep_file` is `TRUE`, the package archive file will be saved in the current directory. If a valid path is provided in `keep_file`, the file will be saved to that path, but all directories are assumed to exist and will not be created. If `keep_file` does not specify an existing, valid path, an error will be shown after package installation.
#'
#' @importFrom utils install.packages installed.packages download.file remove.packages
#' @importFrom curl curl_fetch_disk
#' @importFrom rlang is_installed
#'
#' @export
#'
#' @return Silently returns `TRUE` if `asreml` installed successfully or already present, `FALSE` otherwise. Optionally prints a confirmation message on success.
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
install_asreml <- function(library = .libPaths()[1], quiet = FALSE, force = FALSE, keep_file = FALSE) {

    if(rlang::is_installed("asreml") && !newer_version() && !force) {
        if(!quiet) message("The latest version of ASReml-R available for your sysetm is already installed. To install anyway, set `force = TRUE`.")
        invisible(TRUE)
    }
    else {
        # Get OS and R version
        os_ver <- get_r_os()
        if(os_ver$os=="mac") {
          create_mac_folder()
        }

        url <- paste0("https://link.biometryhubwaite.com/", os_ver$os_ver)

        # First check if file already exists, both in the current directory and temp folder
        # Need to create a regex to check it's the correct file extension, so tests ignore .R files
        dir_files <- list.files(pattern = "asreml+(([a-zA-Z0-9_.\\-])*)+(.zip|.tar.gz|.tgz)")

        if(length(dir_files) > 0) {
            filename <- dir_files[length(dir_files)] # Get the alphabetically last one. Theoretically this should be the highest version number.
            save_file <- filename
        }

        # Can't find file, download
        else {
            if(!quiet) {
                message("\nDownloading and installing ASReml-R. This may take some time, depending on internet speed...\n")
            }
            #Create a temporary file to save the package
            save_file <- tempfile("asreml_")

            # Use curl to download the file which also gives the expanded URL
            response <- curl::curl_fetch_disk(url = url, path = save_file)

            # Extract everything after the last / as the filename
            filename <- basename(response$url)
            file.rename(save_file, paste0(tempdir(), "/", filename))
            save_file <- normalizePath(paste0(tempdir(), "/", filename))
        }

        # If forcing installation, remove existing version to avoid errors on installation
        if(force && rlang::is_installed("asreml") && os_ver$os != "linux") {
            unloadNamespace("asreml")
            if("asreml" %in% .packages()) {
                detach("package:asreml", unload = TRUE, force = TRUE)
            }
            suppressMessages(remove.packages("asreml"))
        }

        # Check dependencies are installed first
        pkgs <- rownames(installed.packages(lib.loc = library))
        deps <- setdiff(c("data.table", "ggplot2", "jsonlite"), pkgs)

        if(!rlang::is_installed("data.table", version = "1.9.6")) {
            deps <- c(deps, "data.table")
        }

        if(length(deps) > 0) {
            install.packages(deps, lib = library, repos = "https://cloud.r-project.org")
        }

        # Install asreml
        install.packages(save_file, lib = library, repos = NULL, quiet = quiet, type = ifelse(os_ver$os == "win", "binary", "source"))

        # If keep_file is true, copy asreml to current directory
        if(keep_file == TRUE) {
            result <- tryCatch(
                expr = {
                    file.rename(save_file, filename)
                },
                error = function(cond) {
                    warning("Could not copy asreml install file to current working directory", call. = FALSE)
                    file.remove(save_file)
                    return(FALSE)
                },
                warning = function(cond) {
                    warning("Could not copy asreml install file to current working directory", call. = FALSE)
                    file.remove(save_file)
                    return(FALSE)
                }
            )
        }
        else if(keep_file == FALSE) {
            file.remove(save_file)
        }
        else if(is.character(keep_file) & length(keep_file) == 1) { # Assume keep_file is a path
            if(dir.exists(keep_file)) {
                result <- tryCatch(
                    expr = {
                        file.rename(save_file, paste0(keep_file, "/", filename))
                    },
                    error = function(cond) {
                        warning("Could not copy asreml install file to provided directory.", call. = FALSE)
                        file.remove(save_file)
                        return(FALSE)
                    },
                    warning = function(cond) {
                        warning("Could not copy asreml install file to provided directory.", call. = FALSE)
                        file.remove(save_file)
                        return(FALSE)
                    }
                )
            }
            else {
                warning("Directory provided in keep_file does not exist. Please provide a valid path in the keep_file argument to save the package to.")
            }
        }
        else {
            warning("Argument keep_file should be provided as a path to a single directory or TRUE to save in current working directory. Downloaded file has not been kept.")
        }

        if(rlang::is_installed("asreml")) {
            if(!quiet) message("ASReml-R successfully installed!")
            invisible(TRUE)
        }
        else {
            if(!quiet) warning("There was a problem with installation and ASReml-R was not successfully installed.")
            invisible(FALSE)
        }
    }
}

#' @rdname install_asreml
#' @param ... other arguments passed to `install_asreml()`
#'
#' @export
update_asreml <- function(...) {
    install_asreml(force = TRUE, ...)
}


#' Get the version of R and OS
#'
#' @return A list with the version of R and the OS in a standard format
#' @keywords internal
get_r_os <- function() {

    arm <- FALSE
    # arm Macs need a different package
    if(Sys.info()[["sysname"]] == "Darwin" && Sys.info()[["machine"]] == "arm64") {
        arm <- TRUE
    }

    os <- switch(Sys.info()[['sysname']],
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
#' @return A list of data frames containing the version number and release date of released ASReml-R versions for comparison
#' @keywords internal
#' @importFrom xml2 read_html xml_text xml_find_all
#' @importFrom stringi stri_split_fixed
get_version_table <- function() {
    url <- "https://asreml.kb.vsni.co.uk/asreml-r-4-download-success/?site_reference=VS9AF20"
    res <- xml2::read_html(url)

    headers <- xml2::xml_text(xml2::xml_find_all(res, "//h3"))
    headers <- headers[grepl("^ASReml-?R? 4.*\\(All platforms\\)", headers)]

    tables <- xml2::xml_text(xml2::xml_find_all(res, xpath = "//table"))
    tables <- tables[grepl("macOS", tables)]
    tables <- stringi::stri_split_fixed(tables, "\n")
    tables <- lapply(tables, \(x) x[!is.na(x) & x != ""])

    fix_tables <- function(x) {
        first_row <- x[1:4]
        x <- as.data.frame(matrix(x[5:length(x)], ncol = 4, byrow = TRUE))
        colnames(x) <- first_row
        # Parse dates
        x[,grepl("Date", colnames(x))] <- as.Date(x[,grepl("Date", colnames(x))],
                                                  tryFormats = c("%d %B %Y", "%d/%m/%Y",
                                                                 "%d %b %Y", "%d-%m-%Y"))
        return(x)
    }

    for(i in 1:length(tables)) {
        tables[[i]] <- fix_tables(tables[[i]])
        tables[[i]]["os"] <- ifelse(grepl("Windows", x = tables[[i]][["Download"]], ignore.case = TRUE), "win",
                                    ifelse(grepl("macOS", x = tables[[i]][["Download"]], ignore.case = TRUE), "mac",
                                           ifelse(grepl("Ubuntu", x = tables[[i]][["Download"]], ignore.case = TRUE), "linux", "centos")))
        tables[[i]]["arm"] <- ifelse(grepl("arm", x = tables[[i]][["Download"]], ignore.case = TRUE), TRUE, FALSE)
        tables[[i]]["r_ver"] <- paste0(stringi::stri_match_first_regex(headers[i], "R version (\\d?)\\.(\\d?)")[2:3], collapse = "")
        tables[[i]]["asr_ver"] <- stringi::stri_match_first_regex(tables[[i]][["File name"]], "asreml-?_?(\\d\\.\\d?\\.\\d?\\.\\d*)")[,2]
    }

    tables <- do.call("rbind", tables)

    return(tables)
}


#' Compare installed version of ASReml-R with available versions
#'
#' @importFrom utils packageDescription
#'
#' @return TRUE if a newer version is available online, FALSE otherwise
#' @keywords internal
newer_version <- function() {
    online_versions <- get_version_table()
    os_ver <- get_r_os()

    newest <- subset(online_versions,
                     online_versions$os==os_ver$os &
                         online_versions$arm==os_ver$arm &
                         online_versions$r_ver==os_ver$ver &
                         numeric_version(online_versions$asr_ver)==max(numeric_version(online_versions$asr_ver)))

    if(rlang::is_installed("asreml")) {
        asr_desc <- utils::packageDescription("asreml")
        asr_date <- as.Date(substr(asr_desc$Packaged, 1, 10))
        asr_ver <- asr_desc$Version
    }
    else {
        asr_date <- as.Date("1900-01-01")
        asr_ver <- 0
    }

    if((nrow(newest)>0) && (newest$`Date published` > asr_date+7) && (numeric_version(newest$asr_ver) > numeric_version(asr_ver))) {
        output <- TRUE
    }
    else {
        output <- FALSE
    }
    return(output)
}

#' Create the folder MacOS needs for licensing
#'
#' @return logical; TRUE if folder successfully created, otherwise it will error
#' @keywords internal
#' @importFrom askpass askpass
create_mac_folder <- function() {
    # macOS needs some special set up
    if(Sys.info()[["sysname"]] == "Darwin" &&
       Sys.info()["release"] >= 21 &&
       !dir.exists("/Library/Application Support/Reprise/")) {
        result <- tryCatch(
            expr = {
                dir.create("/Library/Application Support/Reprise/", recursive = T)
            },
            error = function(cond) {
                return(FALSE)
            },
            warning = function(cond) {
                return(FALSE)
            }
        )

        if(isFALSE(result)) {
            message("The ASReml-R package uses Reprise license management and will require administrator privilege to create the folder '/Library/Application Support/Reprise' before it can be installed.")
            input <- readline("Would you like to create this folder now (Yes/No)? You will be prompted for your password if yes. ")

            if(toupper(input) %in% c("YES", "Y")) {
                system("sudo -S mkdir '/Library/Application Support/Reprise' && sudo -S chmod 777 '/Library/Application Support/Reprise'",
                       input = askpass::askpass("Please enter your user account password: "))
              cat("\n")
            }
            else {
                stop("ASReml-R cannot be installed until the folder '/Library/Application Support/Reprise' is created with appropriate permissions.
                         Please run the following command on your terminal:
                         sudo -S mkdir '/Library/Application Support/Reprise' && sudo -S chmod 777 '/Library/Application Support/Reprise'")
            }
        }
    }
    return(dir.exists("/Library/Application Support/Reprise/"))
}
