#' Export Experimental Design Layout to Excel
#'
#' Converts an experimental design dataframe into a spatial layout matrix
#' and exports to Excel with optional colour coding by treatment.
#'
#' @param design_df A dataframe containing experimental design with 'row' and 'col' columns
#' @param value_column Character string specifying which column to use for layout values (default: "treatments")
#' @param filename Character string for Excel filename (default: "experimental_design.xlsx")
#' @param palette colour palette for treatments. Can be a palette name (see details) or vector of colours.
#'   Set to NULL to disable colouring (default: "default")
#'
#' @return Invisibly returns the layout dataframe
#'
#' @importFrom rlang is_installed
#'
#' @details
#' This function takes an experimental design in long format (with row/col coordinates)
#' and converts it to a matrix layout that matches the spatial arrangement of the experiment,
#' then exports to Excel with formatting and optional colour coding.
#'
#' Valid palette options include:
#' \itemize{
#'   \item "default" - Spectral palette
#'   \item ColorBrewer palettes: "brbg", "piyg", "prgn", "puor", "rdbu", "rdgy", "rdylbu", "rdylgn", "spectral", "set3", "paired"
#'   \item Viridis palettes: "viridis", "magma", "inferno", "cividis", "plasma", "rocket", "mako", "turbo"
#'   \item colour blind friendly: "colour blind", "color blind", "cb" (uses viridis)
#'   \item Custom vector of colours (must match number of unique treatments)
#' }
#'
#' Requires the 'openxlsx' package to be installed.
#'
#' @examples
#' \dontrun{
#' # Export with default colours
#' export_design_to_excel(my_design, "treatments", "my_design.xlsx")
#'
#' # Export without colours
#' export_design_to_excel(my_design, "treatments", "my_design.xlsx", palette = NULL)
#'
#' # Export with custom palette
#' export_design_to_excel(my_design, "treatments", "my_design.xlsx", palette = "viridis")
#' }
#'
#' @export
export_design_to_excel <- function(design_df, value_column = "treatments",
                                   filename = "experimental_design.xlsx",
                                   palette = "default") {

    # Check if openxlsx is available
    if (!rlang::is_installed("openxlsx")) {
        stop("Package 'openxlsx' is required for Excel export but is not installed.\n",
             "Install it with: install.packages('openxlsx')")
    }

    # If design_df is a list (e.g., from a design generation function), extract the design dataframe
    if(inherits(design_df, "list")) {
        design_df <- design_df$design
    }

    # Check required columns
    required_cols <- c("row", "col", value_column)
    missing_cols <- setdiff(required_cols, names(design_df))
    if (length(missing_cols) > 0) {
        stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
    }

    # Sort by row then column to ensure correct order
    design_sorted <- design_df[order(design_df$row, design_df$col), ]

    # Get dimensions
    max_row <- max(design_df$row)
    max_col <- max(design_df$col)

    # Create matrix using vectorized approach
    layout_matrix <- matrix(design_sorted[[value_column]],
                            nrow = max_row,
                            ncol = max_col,
                            byrow = TRUE)

    # Convert to data frame
    layout_df <- as.data.frame(layout_matrix)

    # Add meaningful row and column names
    rownames(layout_df) <- paste("Row", 1:max_row)
    colnames(layout_df) <- paste("Col", 1:max_col)

    # Create workbook
    wb <- openxlsx::createWorkbook()

    # Add worksheets
    openxlsx::addWorksheet(wb, "Layout")
    openxlsx::addWorksheet(wb, "Raw_Data")

    # Write data
    openxlsx::writeData(wb, "Layout", layout_df, rowNames = TRUE)
    openxlsx::writeData(wb, "Raw_Data", design_df)

    # Apply basic formatting
    header_style <- openxlsx::createStyle(
        fontSize = 12,
        fontColour = "white",
        fgFill = "#4F81BD",
        halign = "center",
        valign = "center",
        textDecoration = "bold"
    )

    cell_style <- openxlsx::createStyle(
        halign = "center",
        valign = "center",
        border = "TopBottomLeftRight",
        borderColour = "black"
    )

    # Get dimensions for formatting
    rows <- nrow(layout_df) + 1
    cols <- ncol(layout_df) + 1

    # Apply basic styles
    openxlsx::addStyle(wb, "Layout", header_style, rows = 1, cols = 1:cols)
    openxlsx::addStyle(wb, "Layout", header_style, rows = 1:rows, cols = 1)
    openxlsx::addStyle(wb, "Layout", cell_style, rows = 2:rows, cols = 2:cols, gridExpand = TRUE)

    # Apply colour coding if palette is specified
    if (!is.null(palette)) {
        # Get unique treatments and generate colours
        unique_treatments <- sort(unique(design_df[[value_column]]))
        ntrt <- length(unique_treatments)

        colours <- setup_colour_palette(palette, ntrt)
        
        # Expand 3-digit hex codes to 6-digit and 4-digit to 8-digit for openxlsx compatibility
        colours <- gsub("^#([0-9A-Fa-f])([0-9A-Fa-f])([0-9A-Fa-f])$", "#\\1\\1\\2\\2\\3\\3", colours)  # 3-digit -> 6-digit
        colours <- gsub("^#([0-9A-Fa-f])([0-9A-Fa-f])([0-9A-Fa-f])([0-9A-Fa-f])$", "#\\1\\1\\2\\2\\3\\3\\4\\4", colours)  # 4-digit -> 8-digit
        # Remove transparency from 8-digit hex codes
        colours <- gsub("^(#[0-9A-Fa-f]{6})[0-9A-Fa-f]{2}$", "\\1", colours)  # 8-digit -> 6-digit

        # Create colour mapping
        colour_map <- setNames(colours, unique_treatments)

        # Apply colours to cells
        for (i in 2:rows) {
            for (j in 2:cols) {
                cell_value <- layout_df[i-1, j-1]
                if (!is.na(cell_value)) {
                    colour <- colour_map[[as.character(cell_value)]]
                    coloured_style <- openxlsx::createStyle(
                        halign = "center",
                        valign = "center",
                        border = "TopBottomLeftRight",
                        borderColour = "black",
                        fgFill = colour,
                        fontColour = ifelse(.is_light_colour(colour), "black", "white")
                    )
                    openxlsx::addStyle(wb, "Layout", coloured_style, rows = i, cols = j)
                }
            }
        }
    }

    # Auto-size columns
    openxlsx::setColWidths(wb, "Layout", cols = 1:cols, widths = "auto")

    # Save workbook
    openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)

    message("Excel file saved as: ", filename)
    return(invisible(layout_df))
}
