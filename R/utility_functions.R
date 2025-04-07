`%notin%` <- `%!in%` <- Negate(`%in%`)

# quiet
#' Function to suppress output if desired, especially useful for ASReml output
#'
#' @param x A function call with output to be suppressed.
#'
#' @returns The invisible output of the function called.
#'
#' @keywords internal
#'
quiet <- function(x) {
    sink(tempfile())
    on.exit(sink())
    invisible(force(x))
}


######################################################
# Start up function
# this function is executed once the package is loaded
######################################################

#' @importFrom utils available.packages packageVersion compareVersion
#' @importFrom rlang is_interactive is_installed
.onAttach <- function(library, pkg)
{
    local_version <- utils::packageVersion('biometryassist')

    if(rlang::is_interactive() && !isFALSE(rlang::peek_option("biometryassist.check"))) {
        output <- paste(paste0("    ", paste0(rep("~", times = 69), collapse = "")),
                        paste("    |  ", pkg, " version ", local_version, "                                     |",sep=""),
                        "    |  Authors: Sharon Nielsen, Sam Rogers, Annie Conway                |",
                        "    |  Developed at the University of Adelaide with funding provided    |",
                        "    |  by the Australian Grains Research and Development Corporation.   |",
                        "    |  Package website: https://biometryhub.github.io/biometryassist    |",
                        "    |                                                                   |",
                        "    |  If you have used this package in your work, please cite it.      |",
                        "    |  Type 'citation('biometryassist')' for the citation details.      |",
                        paste0("    ", paste0(rep("~", times = 69), collapse = ""), "\n"), sep = "\n")

        if(is_installed("crayon")) {
            packageStartupMessage(crayon::green(output), appendLF=TRUE)
        }
        else {
            packageStartupMessage(output, appendLF=TRUE)
        }

        # check which version is more recent
        cran_version <- tryCatch(
            {
                packages <- utils::available.packages()
                ver <- packages["biometryassist","Version"]
            },
            error=function(cond) {
                NA
            }
        )

        if(compare_version(cran_version, as.character(local_version)) == 1) { # current version on CRAN newer than installed
            warning("    biometryassist version ", cran_version, " is now available.\n",
                    "    Please update biometryassist by running\n",
                    "    install.packages('biometryassist')", call. = FALSE)
        }
    }
    invisible()
}

#' Function to compare package version for mocking
#'
#' @param a,b Character strings representing package version numbers.
#'
#' @returns Numeric. `0` if the numbers are equal, `-1` if `b` is later and `1` if `a` is later
#' @keywords internal
compare_version <- function(a, b) {
    return(utils::compareVersion(as.character(a), as.character(b)))
}
