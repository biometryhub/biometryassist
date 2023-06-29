#' Create an analysis template file and open for editing
#'
#' @param filename The filename of the script
#'
#' @importFrom utils file.edit
#' @importFrom tools file_ext
#'
#' @return Returns the path to the created file invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' analysis_template(filename = "test_report")
#' }
analysis_template <- function(filename, asreml = ifelse(rlang::is_installed("asreml"), TRUE, FALSE)) {
    if(asreml) {
        file <- file.path(paste0(filename, ifelse(tools::file_ext(filename)!="R", ".R", "")))
        file.copy(system.file("analysis_template.R", package = "biometryassist"), file)
        utils::file.edit(file)
    }
    else {
        file <- file.path(paste0(filename, ifelse(tools::file_ext(filename)!="R", ".R", "")))
        file.copy(system.file("analysis_template_aov.R", package = "biometryassist"), file)
        utils::file.edit(file)
    }
    message("Replace all values given in <angle brackets>")
    invisible(file)
}
