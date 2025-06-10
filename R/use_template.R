#' Use biometryassist analysis templates
#'
#' @description
#' `use_template()` copies a pre-built analysis template from the biometryassist
#' package to your working directory and optionally opens it for editing. These
#' templates provide standardized approaches for common biometric analyses.
#'
#' @param template_name `character(1)` Name of the template file to use.
#'   Default is `"mixed_model_template.R"`. Available templates can be listed
#'   with `list_templates()`.
#' @param dest_dir `character(1)` Directory where the template should be copied.
#'   Default is the current working directory (`"."`).
#' @param open `logical(1)` Should the template file be opened in the default
#'   editor after copying? Default is `TRUE`.
#' @param overwrite `logical(1)` Should existing files be overwritten?
#'   Default is `FALSE` to prevent accidental data loss.
#' @param output_name `character`, Optional. Name for the copied file in the destination directory.
#'   If not specified, defaults to `"analysis_script.R"`.
#'   If specified, the template will be copied and renamed to this file.
#'   (The original template file is not overwritten.)
#'
#' @returns
#' The file path to the copied template (invisibly). Called primarily for
#' its side effects of copying and optionally opening the template file.
#'
#' @details
#' This function is designed to help users get started with biometryassist
#' analyses by providing tested, documented templates. The templates include:
#' - Commented code explaining each step
#' - Suggested package loading
#' - Example data structures
#' - Common analysis workflows
#'
#' If a file with the same name already exists in the destination directory,
#' the function will not overwrite it unless `overwrite = TRUE` is specified.
#' In this case, it will still open the existing file if `open = TRUE`.
#'
#' @importFrom utils file.edit
#'
#' @examples
#' \dontrun{
#' # Copy and open the default analysis template
#' use_template()
#'
#' # Copy a specific template without opening
#' use_template("anova_template.R", open = FALSE)
#'
#' # Copy to a specific directory
#' use_template("mixed_model_template.R", dest_dir = "analyses")
#'
#' # Overwrite an existing file
#' use_template("mixed_model_template.R", overwrite = TRUE)
#' }
#'
#' @seealso
#' * [list_templates()] to see available templates
#' * `vignette("analysis-workflow", package = "biometryassist")` for analysis guidance
#'
#' @export
use_template <- function(template_name = "mixed_model_template.R",
                         dest_dir = ".",
                         open = TRUE,
                         overwrite = FALSE,
                         output_name = NULL) {

    # Input validation
    if (!is.character(template_name) || length(template_name) != 1)
        stop("`template_name` must be a single character string", call. = FALSE)
    if (!is.character(dest_dir) || length(dest_dir) != 1)
        stop("`dest_dir` must be a single character string", call. = FALSE)
    if (!is.logical(open) || length(open) != 1)
        stop("`open` must be TRUE or FALSE", call. = FALSE)
    if (!is.logical(overwrite) || length(overwrite) != 1)
        stop("`overwrite` must be TRUE or FALSE", call. = FALSE)
    if (!is.null(output_name) && (!is.character(output_name) || length(output_name) != 1))
        stop("`output_name` must be a single character string", call. = FALSE)

    # Determine the template file path: use user-supplied file if it exists, otherwise use package template
   if (file.exists(template_name)) {
        template_path <- normalizePath(template_name)
    } else {
      if(template_name %!in% list_templates()) {
        warning("Template '", template_name, "' not found as a file. Using default biometryassist template instead.")
      }
        template_path <- system.file("templates", "mixed_model_template.R", package = "biometryassist")
   }

    # Create destination directory if it doesn't exist
    if (!dir.exists(dest_dir)) {
        dir.create(dest_dir, recursive = TRUE)
        message("Created directory: ", dest_dir)
    }

    # Determine output filename
    if (is.null(output_name)) {
        output_name <- "analysis_script.R"
    }

    # Create destination path
    dest_path <- file.path(dest_dir, output_name)

    # Check if file already exists
    if (file.exists(dest_path) && !overwrite) {
        message("File '", dest_path, "' already exists. Use overwrite = TRUE to replace it.")
        if (open) {
            utils::file.edit(dest_path)
        }
        return(invisible(dest_path))
    }

    # Copy template to destination
    success <- file.copy(template_path, dest_path, overwrite = overwrite)

    if (!success) {
        stop("Failed to copy template to ", dest_path, call. = FALSE)
    }

    message("Template copied to: ", dest_path)

    # Open in editor if requested
    if (open) {
        utils::file.edit(dest_path)
    }

    invisible(dest_path)
}


#' List available biometryassist templates
#'
#' @description
#' `list_templates()` returns a character vector of available analysis templates
#' included with the biometryassist package.
#'
#' @returns `character()` Vector of available template file names.
#'
#' @examples
#' # See what templates are available
#' list_templates()
#'
#' @seealso [use_template()] to copy and use a template
#' @export
list_templates <- function() {
    template_dir <- system.file("templates", package = "biometryassist")

    if (template_dir == "") {
        return(character(0))
    }

    templates <- list.files(template_dir, pattern = "\\.(R|Rmd|qmd)$",
                            ignore.case = TRUE)

    if (length(templates) == 0) {
        message("No templates found in biometryassist package")
    }

    return(templates)
}
