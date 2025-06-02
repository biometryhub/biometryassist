#' Create Excel Layout from Experimental Design
#'
#' Converts an experimental design dataframe into a spatial layout matrix
#' and optionally exports to Excel format with colour coding.
#'
#' @param design_df A dataframe containing experimental design with 'row' and 'col' columns
#' @param value_column Character string specifying which column to use for layout values (default: "treatments")
#' @param filename Character string for Excel filename (optional, if provided will export to Excel)
#' @param export_excel Logical, whether to export to Excel (default: FALSE)
#' @param palette colour palette for Excel export. Can be a palette name (e.g., "default", "spectral", "viridis")
#'                or a vector of custom colours. Only used when exporting to Excel. (default: "default")
#'
#' @return A dataframe representing the spatial layout, or invisibly returns layout if exporting to Excel
#'
#' @details
#' This function takes an experimental design in long format (with row/col coordinates)
#' and converts it to a matrix layout that matches the spatial arrangement of the experiment.
#'
#' If export_excel is TRUE or filename is provided, the function will attempt to export
#' to Excel format with colour coding based on the specified palette. This requires the
#' 'openxlsx' package to be installed.
#'
#' @examples
#' \dontrun{
#' # Create layout matrix only
#' layout <- design_to_excel_layout(my_design, "treatments")
#'
#' # Export to Excel with default colours
#' design_to_excel_layout(my_design, "treatments",
#'                        filename = "my_design.xlsx", export_excel = TRUE)
#'
#' # Export to Excel with viridis palette
#' design_to_excel_layout(my_design, "treatments",
#'                        filename = "my_design.xlsx", export_excel = TRUE,
#'                        palette = "viridis")
#'
#' # Export with custom colours
#' design_to_excel_layout(my_design, "treatments",
#'                        filename = "my_design.xlsx", export_excel = TRUE,
#'                        palette = c("#FF0000", "#00FF00", "#0000FF"))
#' }
#'
#' @export
design_to_excel_layout <- function(design_df, value_column = "treatments",
                                   filename = NULL, export_excel = FALSE,
                                   palette = "default") {


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
                            byrow = FALSE)

    # Convert to data frame
    layout_df <- as.data.frame(layout_matrix)

    # Add meaningful row and column names
    rownames(layout_df) <- paste("Row", 1:max_row)
    colnames(layout_df) <- paste("Col", 1:max_col)

    # Check if Excel export is requested
    if (export_excel || !is.null(filename)) {
        # Check if openxlsx is available
        if (!requireNamespace("openxlsx", quietly = TRUE)) {
            stop("Package 'openxlsx' is required for Excel export but is not installed.\n",
                 "Install it with: install.packages('openxlsx')")
        }

        # Set default filename if not provided
        if (is.null(filename)) {
            filename <- "experimental_design.xlsx"
        }

        # Export to Excel with colours
        .export_to_excel(design_df, layout_df, filename, value_column, palette)

        message("Excel file saved as: ", filename)
        return(invisible(layout_df))
    }

    return(layout_df)
}

# Internal function for Excel export with colours (not exported)
.export_to_excel <- function(design_df, layout_df, filename, value_column, palette) {

    # Get unique treatments and setup colours
    unique_treatments <- sort(unique(design_df[[value_column]]))
    ntrt <- length(unique_treatments)
    colours <- setup_colour_palette(palette, ntrt)

    # Create colour mapping
    colour_map <- setNames(colours, unique_treatments)

    # Create workbook
    wb <- openxlsx::createWorkbook()

    # Add worksheets
    openxlsx::addWorksheet(wb, "Layout")
    openxlsx::addWorksheet(wb, "Raw_Data")

    # Write data
    openxlsx::writeData(wb, "Layout", layout_df, rowNames = TRUE)
    openxlsx::writeData(wb, "Raw_Data", design_df)

    # Create base styles
    header_style <- openxlsx::createStyle(
        fontSize = 12,
        fontColour = "white",
        fgFill = "#4F81BD",
        halign = "center",
        valign = "center",
        textDecoration = "bold"
    )

    # Get dimensions for formatting
    rows <- nrow(layout_df) + 1
    cols <- ncol(layout_df) + 1

    # Apply header styles
    openxlsx::addStyle(wb, "Layout", header_style, rows = 1, cols = 1:cols)
    openxlsx::addStyle(wb, "Layout", header_style, rows = 1:rows, cols = 1)

    # Create and apply coloured styles for each treatment
    for (i in 1:length(unique_treatments)) {
        treatment <- unique_treatments[i]
        colour <- colours[i]

        # Create style for this treatment
        treatment_style <- openxlsx::createStyle(
            halign = "center",
            valign = "center",
            border = "TopBottomLeftRight",
            borderColour = "black",
            fgFill = colour,
            fontColour = ifelse(.is_light_colour(colour), "black", "white")
        )

        # Find cells with this treatment value
        for (row in 1:nrow(layout_df)) {
            for (col in 1:ncol(layout_df)) {
                if (layout_df[row, col] == treatment) {
                    # Add 1 to row and col to account for headers
                    openxlsx::addStyle(wb, "Layout", treatment_style,
                                       rows = row + 1, cols = col + 1)
                }
            }
        }
    }

    # Auto-size columns
    openxlsx::setColWidths(wb, "Layout", cols = 1:cols, widths = "auto")

    # Save workbook
    openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)
}
