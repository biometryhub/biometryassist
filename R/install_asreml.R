#' Install or Update the ASReml-R package
#'
#' @description Helper functions for installing or updating the ASReml-R package, intended to reduce the difficulty of finding the correct version for your operating system and R version.
#'
#' @param library Library location to install ASReml-R. Uses first option in `.libPaths()` by default.
#' @param quiet Logical (default `FALSE`). Should package be installed quietly?
#' @param force Logical (default `FALSE`). Force ASReml-R to install. Useful for upgrading if it is already installed.
#' @param keep_file Should the downloaded asreml package file be kept? Default is `FALSE`. `TRUE` downloads to current directory. A file path can also be provided to save to another directory. See `Details` for more information.
#'
#' @details The ASReml-R package file is downloaded from a shortlink, and if `keep_file` is `TRUE`, the package archive file will be saved in the current directory. If a valid path is provided in `keep_file`, the file will be saved to that path, but directory is assumed to exist and will not be created. If `keep_file` does not specify an existing, valid path, an error will be shown.
#'
#' @importFrom utils install.packages installed.packages download.file remove.packages
#' @importFrom curl curl_fetch_disk
#' @importFrom withr local_file
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

    if(rlang::is_installed("asreml") & !force) {
        if(!quiet) message("ASReml-R is already installed.")
        invisible(TRUE)
    }
    else {
        if(!quiet) {
            message("\nDownloading and installing ASReml-R. This may take some time, depending on internet speed...\n")
        }
        if(force && isNamespaceLoaded("asreml")) {
            unloadNamespace("asreml")
        }

        os <- switch(Sys.info()[['sysname']],
                     Windows = "win",
                     Linux   = "linux",
                     Darwin  = "mac"
        )

        ver <- gsub("\\.", "", substr(getRversion(), 1, 3))
        url <- paste0("https://link.biometryhubwaite.com/", os, "-", ver)

        # First check if file already exists, both in the current directory and temp folder
        # Need to create a regex to check it's the correct file extension, so tests ignore .R files
        temp_files <- list.files(tempdir(), pattern = "asreml+(([a-zA-Z0-9_.\\-])*)+(.zip|.tar.gz|.tgz)")
        dir_files <- list.files(pattern = "asreml+(([a-zA-Z0-9_.\\-])*)+(.zip|.tar.gz|.tgz)")

        if(length(temp_files) > 0) {  # I don't think this will ever trigger, as I will clean up downloads from Temp
            filename <- temp_files[length(temp_files)] #Get the alphabetically last file. Theoretically should be the latest version?
            save_file <- paste0(tempdir(), "/", filename)
        }
        else if(length(dir_files) > 0) {
            filename <- dir_files[length(dir_files)] # Get the alphabetically last one. Theoretically this should be the highest version number.
            save_file <- filename
        }

        # Can't find file, download
        else {
            #Create a temporary file to save the package
            save_file <- withr::local_file(tempfile("asreml_"))

            # Use httr to GET the file which also gives the expanded URL
            response <- curl::curl_fetch_disk(url = url, path = save_file)

            # Extract everything after the last / as the filename
            filename <- basename(response$url)#, pos+1, nchar(response$url))
            file.rename(save_file, paste0(tempdir(), "/", filename))
            save_file <- paste0(tempdir(), "/", filename)
        }

        # If forcing installation, remove existing version to avoid errors on installation
        if(force && rlang::is_installed("asreml") && Sys.info()[["sysname"]] == "Windows") {
            detach("package:asreml", unload = TRUE, force = TRUE)
            suppressMessages(remove.packages("asreml", ))
        }

        # Check dependencies are installed first
        pkgs <- rownames(installed.packages(lib.loc = library))
        deps <- setdiff(c("data.table", "ggplot2", "jsonlite"), pkgs)

        if(rlang::is_installed("data.table") && packageVersion("data.table") < "1.9.6") {
            deps <- c(deps, "data.table")
        }

        if(length(deps) > 0) {
            install.packages(deps, lib = library, repos = "https://cloud.r-project.org",
                             Ncpus = ifelse(length(deps)>1,
                                            max(ceiling(parallel::detectCores()/2), # Use multiple CPUs if available
                                                ceiling(parallel::detectCores()-2)), 1))
        }

        # Install asreml
        install.packages(save_file, lib = library, repos = NULL, quiet = quiet, type = ifelse(os == "win", "binary", "source"))

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
        }
        else {
            if(!quiet) warning("There was a problem with installation and ASReml-R was not successfully installed.")
            invisible(FALSE)
        }
        invisible(TRUE)
    }
}

#' @rdname install_asreml
#' @param ... other arguments passed to `install_asreml()`
#'
#' @export
update_asreml <- function(...) {
    install_asreml(force = TRUE, ...)
}
