`%notin%` <- `%!in%` <- Negate(`%in%`)

# quiet
#' Function to suppress output if desired, especially useful for ASReml output
#'
#' @param x A function call with output to be suppressed.
#'
#' @return The invisible output of the function called.
#'
#' @keywords internal
#'
quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}

# .btenv <- new.env(parent=emptyenv())


######################################################
# Start up function
# this function is executed once the package is loaded
######################################################

#' @importFrom utils available.packages packageVersion
#' @importFrom rlang is_interactive is_installed
.onAttach <- function(library, pkg)
{
  # Create a hidden variable to only print this output every 8 hours
  # if(!exists(".btenv", where = .GlobalEnv, mode = "environment")) {
  #   .btenv <- new.env(parent=.GlobalEnv)
  #   assign("last_load", Sys.time(), envir=.btenv)
  #   last_load <- .btenv$last_load
  # }
  # else {
  #   last_load <- .btenv$last_load
  # }
  # print(last_load)
  # print(.btenv)

  # current_version <- "0.9.1"
  installed_version <- utils::packageVersion('biometryassist')

  if(rlang::is_interactive()) {# && Sys.time() > (last_load + 1)) {
    output <- paste("    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~",
                    paste("    |  ", pkg, " version ", installed_version, "                                     |",sep=""),
                    "    |  Authors: Sharon Nielsen, Sam Rogers, Annie Conway                |",
                    "    |  Developed at the University of Adelaide with funding provided    |",
                    "    |  by the Australian Grains Research and Development Corporation.   |",
                    "    |  Package website: https://biometryhub.github.io/biometryassist    |",
                    "    |                                                                   |",
                    "    |  If you have used this package in your work, please cite it.      |",
                    "    |  Type 'citation('biometryassist')' for the citation details.      |",
                    "    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n", sep = "\n")

    if(rlang::is_installed("crayon")) {
      packageStartupMessage(crayon::green(output), appendLF=TRUE)
    }
    else {
      packageStartupMessage(output, appendLF=TRUE)
    }

    # check which version is more recent
    current_version <- tryCatch(
      {
        packages <- utils::available.packages()
        ver <- packages["biometryassist","Version"]
      },
      error=function(cond) {
        NA
      }
    )

    if(!is.na(current_version) && current_version > installed_version){ # installed version < current version on CRAN
      warning("    biometryassist version ", current_version, " is now available.\n",
              "    Please update biometryassist by running\n",
              "    install.packages('biometryassist')", call. = FALSE)
    }
    else {
      output2 <- paste("    The latest version of this package is available at",
                       "    https://github.com/biometryhub/biometryassist. To update type:",
                       "    remotes::install_github('biometryhub/biometryassist')", sep = "\n")

      if(rlang::is_installed("crayon")) {
        packageStartupMessage(crayon::green(output2),appendLF=TRUE)
      }
      else {
        packageStartupMessage(output2,appendLF=TRUE)
      }
    }
  }
  invisible()
}
