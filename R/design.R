#' Create a complete experimental design with graph of design layout and skeletal ANOVA table
#'
#' @param type The design type. One of `crd`, `rcbd`, `lsd`, `split`, `strip`, or a crossed factorial specified as `crossed:<base>` where `<base>` is one of `crd`, `rcbd`, or `lsd`.
#' @param treatments A vector containing the treatment names or labels.
#' @param reps The number of replicates. Ignored for Latin Square Designs.
#' @param nrows The number of rows in the design.
#' @param ncols The number of columns in the design.
#' @param brows For RCBD, split-plot and strip-plot designs. The number of rows in a block.
#' @param bcols For RCBD, split-plot and strip-plot designs. The number of columns in a block.
#' @param byrow For split-plot and strip-plot designs. Logical (default `TRUE`). Controls the within-block arrangement when there are multiple valid layouts.
#' @param sub_treatments A vector of treatments for the sub-plot factor (required for `split` and `strip`).
#' @param fac.names Allows renaming of the `A` level of factorial designs by passing (optionally named) vectors of new labels to be applied to the factors within a list. See examples and details for more information.
#' @param fac.sep The separator used by `fac.names`. Used to combine factorial design levels. If a vector of 2 levels is supplied, the first separates factor levels and label, and the second separates the different factors.
#' @param buffer A string specifying the buffer plots to include for plotting. Default is `NULL` (no buffers plotted). Other options are "edge" (outer edge of trial area), "rows" (between rows), "columns" (between columns), "double row" (a buffer row each side of a treatment row) or "double column" (a buffer row each side of a treatment column). "blocks" (a buffer around each treatment block) will be implemented in a future release.
#' @param plot Logical (default `TRUE`). If `TRUE`, display a plot of the generated design. A plot can always be produced later using [autoplot()].
#' @param rotation Rotate the text output as Treatments within the plot. Allows for easier reading of long treatment labels. Takes positive and negative values being number of degrees of rotation from horizontal.
#' @param size Increase or decrease the text size within the plot for treatment labels. Numeric with default value of 4.
#' @param margin Logical (default `FALSE`). Expand the plot to the edges of the plotting area i.e. remove white space between plot and axes.
#' @param save One of `FALSE` (default)/`"none"`, `TRUE`/`"both"`, `"plot"` or `"workbook"`. Specifies which output to save.
#' @param savename A file name for the design to be saved to. Default is the type of the design combined with "_design".
#' @param plottype The type of file to save the plot as. Usually one of `"pdf"`, `"png"`, or `"jpg"`. See [ggplot2::ggsave()] for all possible options.
#' @param seed Logical (default `TRUE`). If `TRUE`, return the seed used to generate the design. If a numeric value, use that value as the seed for the design.
#' @param quiet Logical (default `FALSE`). Hide the output.
#' @param ... Additional parameters passed to [ggplot2::ggsave()] for saving the plot.
#'
#' @details Supported designs are Completely Randomised (`crd`), Randomised Complete Block (`rcbd`), Latin Square (`lsd`), split-plot (`split`), strip-plot (`strip`), and crossed factorial designs via `crossed:<base>` where `<base>` is `crd`, `rcbd`, or `lsd` (e.g. `crossed:crd`).
#' @details If `save = TRUE` (or `"both"`), both the plot and the workbook will be saved to the current working directory, with filename given by `savename`. If one of either `"plot"` or `"workbook"` is specified, only that output is saved. If `save = FALSE` (the default, or equivalently `"none"`), nothing will be output.
#' @details `fac.names` can be supplied to provide more intuitive names for factors and their levels in factorial and split plot designs. They can be specified in a list format, for example `fac.names = list(A_names = c("a", "b", "c"), B_names = c("x", "y", "z"))`. This will result a design output with a column named `A_names` with levels `a, b, c` and another named `B_names` with levels `x, y, z`. Labels can also be supplied as a character vector (e.g. `c("A", "B")`) which will result in only the treatment column names being renamed. Only the first two elements of the list will be used, except in the case of a 3-way factorial design.
#' @details `...` allows extra arguments to be passed to `ggsave()` for output of the plot. The details of possible arguments can be found in  [ggplot2::ggsave()].
#'
#' @importFrom graphics plot
#' @importFrom ggplot2 ggsave
#' @importFrom utils write.csv
#' @importFrom rlang check_dots_used
#'
#' @export
#'
#' @returns A list containing a data frame with the complete design (`$design`), a ggplot object with plot layout (`$plot.des`), the seed (`$seed`, if `return.seed = TRUE`), and the `satab` object (`$satab`), allowing repeat output of the `satab` table via `cat(output$satab)`.
#'
#' @examples
#' # Completely Randomised Design
#' des.out <- design(type = "crd", treatments = c(1, 5, 10, 20),
#'                   reps = 5, nrows = 4, ncols = 5, seed = 42)
#'
#' # Randomised Complete Block Design
#' des.out <- design("rcbd", treatments = LETTERS[1:11], reps = 4,
#'                   nrows = 11, ncols = 4, brows = 11, bcols = 1, seed = 42)
#'
#' # Latin Square Design
#' # Doesn't require reps argument
#' des.out <- design(type = "lsd", c("S1", "S2", "S3", "S4"),
#'                   nrows = 4, ncols = 4, seed = 42)
#'
#' # Factorial Design (Crossed, Completely Randomised)
#' des.out <- design(type = "crossed:crd", treatments = c(3, 2),
#'                   reps = 3, nrows = 6, ncols = 3, seed = 42)
#'
#' # Factorial Design (Crossed, Completely Randomised), renaming factors
#' des.out <- design(type = "crossed:crd", treatments = c(3, 2),
#'                   reps = 3, nrows = 6, ncols = 3, seed = 42,
#'                   fac.names = list(N = c(50, 100, 150),
#'                                    Water = c("Irrigated", "Rain-fed")))
#'
#' # Factorial Design (Crossed, Randomised Complete Block Design),
#' # changing separation between factors
#' des.out <- design(type = "crossed:rcbd", treatments = c(3, 2),
#'                   reps = 3, nrows = 6, ncols = 3,
#'                   brows = 6, bcols = 1,
#'                   seed = 42, fac.sep = c(":", "_"))
#'
#' # Factorial Design (Nested, Latin Square)
#' trt <- c("A1", "A2", "A3", "A4", "B1", "B2", "B3")
#' des.out <- design(type = "lsd", treatments = trt,
#'                   nrows = 7, ncols = 7, seed = 42)
#'
#' # Split plot design
#' des.out <- design(type = "split", treatments = c("A", "B"), sub_treatments = 1:4,
#'                   reps = 4, nrows = 8, ncols = 4, brows = 4, bcols = 2, seed = 42)
#'
#' # Alternative arrangement of the same design as above
#' des.out <- design(type = "split", treatments = c("A", "B"), sub_treatments = 1:4,
#'                   reps = 4, nrows = 8, ncols = 4, brows = 4, bcols = 2,
#'                   byrow = FALSE, seed = 42)
#'
#' # Strip plot design
#' des.out <- design(type = "strip", treatments = c("A", "B", "C"), sub_treatments = 1:4,
#'                   reps = 4, nrows = 12, ncols = 4, brows = 3, bcols = 4, seed = 42)
#'
design <- function(type,
                   treatments,
                   reps,
                   nrows,
                   ncols,
                   brows = NA,
                   bcols = NA,
                   byrow = TRUE,
                   sub_treatments = NULL,
                   fac.names = NULL,
                   fac.sep = c("", " "),
                   buffer = NULL,
                   plot = TRUE,
                   rotation = 0,
                   size = 4,
                   margin = FALSE,
                   save = FALSE,
                   savename = paste0(type, "_design"),
                   plottype = "pdf",
                   seed = TRUE,
                   quiet = FALSE,
                   ...) {

    # Error checking of inputs
    rlang::check_dots_used()
    validate_design_inputs(nrows, ncols, brows, bcols, size, seed)

    # Normalise fac.sep
    if (!missing(fac.sep) && length(fac.sep) == 1) {
        fac.sep <- rep(fac.sep, times = 2)
    }

    # Parse and validate design type
    parsed_type <- parse_design_type(type)

    # Create the agricolae design object
    outdesign <- create_agricolae_design(
        parsed_type, treatments, reps, sub_treatments, seed
    )

    # Validate design dimensions
    dim <- nrows * ncols
    trs <- calculate_total_plots(parsed_type, treatments, reps, sub_treatments)
    validate_dimensions(dim, trs)

    # Update savename if factorial
    if (parsed_type$is_factorial) {
        savename <- gsub(":", "_", savename)
    }

    # Get design information
    design_info <- get_design_info(outdesign)

    # Validate block parameters
    validate_block_params(design_info, brows, bcols)

    # Normalise agricolae book column names so downstream code can rely on
    # predictable treatment column names.
    outdesign$book <- normalise_agricolae_book(outdesign$book, design_info)

    # Apply factor names if this is a factorial or split design
    if (design_info$is_factorial) {
        outdesign$book <- apply_factor_names(outdesign$book, fac.names, "factorial")
    } else if (design_info$type %in% c("split", "strip")) {
        outdesign$book <- apply_factor_names(outdesign$book, fac.names, "split")
    }

    # Build the design based on type
    des <- build_design(outdesign, design_info$type, nrows, ncols,
                        brows, bcols, fac.sep, byrow)

    # Sort treatments naturally
    des$treatments <- factor(des$treatments,
                             levels = unique(stringi::stri_sort(des$treatments, numeric = TRUE)))

    # Create output list
    output <- list(design = des)
    class(des) <- c("design", class(des))

    # Add buffers if requested
    if (!is.null(buffer)) {
        has_blocks <- any(grepl("block", tolower(names(des))))
        des <- create_buffers(des, type = buffer, blocks = has_blocks)
        output$design <- des
    }

    # Create plot
    if (plot) {
        output$plot.des <- autoplot(des, rotation = rotation, size = size, margin = margin)
    }

    # Create SATAB
    output$satab <- satab(outdesign)

    # Print output if not quiet
    if (!quiet) {
        print.satab(output$satab)
        if (plot) {
            plot(output$plot.des)
        }
    }

    # Handle saving
    handle_save(save, savename, plottype, output, ...)

    # Add seed if requested
    if (seed) {
        output$seed <- outdesign$parameters$seed
    }

    class(output) <- c("design", class(output))

    return(output)
}



#' Produce a graph of design layout, skeletal ANOVA table and data frame with complete design
#'
#' @description
#' **Deprecated**: `des_info()` has been superseded by [design()]. Please use [design()] instead.
#' This function will be removed in a future version.
#'
#' @param design.obj An `agricolae` design object.
#' @param nrows The number of rows in the design.
#' @param ncols The number of columns in the design.
#' @param brows For RCBD only. The number of rows in a block.
#' @param bcols For RCBD only. The number of columns in a block.
#' @param byrow For split-plot only. Logical (default: `TRUE`). Provides a way to arrange plots within whole-plots when there are multiple possible arrangements.
#' @param fac.sep The separator used by `fac.names`. Used to combine factorial design levels. If a vector of 2 levels is supplied, the first separates factor levels and label, and the second separates the different factors.
#' @param buffer The type of buffer. One of edge, row, column, double row, double column, or block (coming soon).
#' @param fac.names Allows renaming of the `A` level of factorial designs (i.e. those using [agricolae::design.ab()]) by passing (optionally named) vectors of new labels to be applied to the factors within a list. See examples and details for more information.
#' @param plot Logical (default `TRUE`). If `TRUE`, display a plot of the generated design. A plot can always be produced later using [autoplot()].
#' @param rotation Rotate the text output as Treatments within the plot. Allows for easier reading of long treatment labels. Takes positive and negative values being number of degrees of rotation from horizontal.
#' @param size Increase or decrease the text size within the plot for treatment labels. Numeric with default value of 4.
#' @param margin Logical (default FALSE). Setting to `TRUE` will add a margin (white space) between plot and axes.
#' @param save One of `FALSE` (default)/`"none"`, `TRUE`/`"both"`, `"plot"` or `"workbook"`. Specifies which output to save.
#' @param savename A filename for the design to be saved to. Default is the type of the design combined with "_design".
#' @param plottype The type of file to save the plot as. Usually one of `"pdf"`, `"png"`, or `"jpg"`. See [ggplot2::ggsave()] for all possible options.
#' @param return.seed Logical (default TRUE). Output the seed used in the design?
#' @param quiet Logical (default FALSE). Return the objects without printing output.
#' @param ... Additional parameters passed to [ggplot2::ggsave()] for saving the plot.
#'
#' @returns A list containing a data frame with the complete design, a ggplot object with plot layout, the seed (if `return.seed = TRUE`), and the `satab` object, allowing repeat output of the `satab` table via `cat(output$satab)`.
#'
#' @keywords internal
#' @export
des_info <- function(design.obj,
                     nrows,
                     ncols,
                     brows = NA,
                     bcols = NA,
                     byrow = TRUE,
                     fac.names = NULL,
                     fac.sep = c("", " "),
                     buffer = NULL,
                     plot = TRUE,
                     rotation = 0,
                     size = 4,
                     margin = FALSE,
                     save = FALSE,
                     savename = paste0(design.obj$parameters$design, "_design"),
                     plottype = "pdf",
                     return.seed = TRUE,
                     quiet = FALSE,
                     ...) {

    .Deprecated("design",
                msg = "des_info() is deprecated and will be removed in a future version. Please use design() instead.")

    # Validate inputs
    rlang::check_dots_used()
    validate_design_inputs(nrows, ncols, brows, bcols, size, seed = TRUE)

    # Normalise fac.sep
    if (!missing(fac.sep) && length(fac.sep) == 1) {
        fac.sep <- rep(fac.sep, times = 2)
    }

    # Get design information from the existing agricolae object
    design_info <- get_design_info(design.obj)

    # Validate block parameters
    validate_block_params(design_info, brows, bcols)

    # Update savename if factorial
    if (design_info$is_factorial) {
        savename <- gsub(":", "_", savename)
    }

    # Normalise agricolae book column names so downstream code can rely on
    # predictable treatment column names.
    design.obj$book <- normalise_agricolae_book(design.obj$book, design_info)

    # Build the design based on type (before applying factor names)
    des <- build_design(design.obj, design_info$type, nrows, ncols,
                        brows, bcols, fac.sep, byrow)

    # Sort treatments naturally
    des$treatments <- factor(des$treatments,
                             levels = unique(stringi::stri_sort(des$treatments, numeric = TRUE)))

    # Apply factor names AFTER building and sorting
    # This renames the factor columns but preserves the treatments column
    if (design_info$is_factorial && !is.null(fac.names)) {
        # For factorial designs, rename the A, B, C columns
        if (is.list(fac.names)) {
            factor_cols <- c("A", "B", "C")[1:length(fac.names)]
            for (i in seq_along(fac.names)) {
                if (factor_cols[i] %in% names(des)) {
                    # Apply level names (ensure factor first)
                    des[[factor_cols[i]]] <- as.factor(des[[factor_cols[i]]])
                    if (nlevels(des[[factor_cols[i]]]) == length(fac.names[[i]])) {
                        levels(des[[factor_cols[i]]]) <- fac.names[[i]]
                    }
                    # Rename column
                    names(des)[names(des) == factor_cols[i]] <- names(fac.names)[i]
                }
            }
        }
    } else if ((design_info$type == "split" || design_info$type == "strip") && !is.null(fac.names)) {
        # For split designs, rename the treatment columns
        if (is.list(fac.names)) {
            # In split designs, `treatments` is the combined whole+sub treatment.
            # Apply fac.names to the whole-plot and subplot factor columns.
            trt_cols <- c("wp_treatments", "sub_treatments")
            for (i in seq_along(fac.names)) {
                if (i <= 2 && trt_cols[i] %in% names(des)) {
                    # Apply level names
                    des[[trt_cols[i]]] <- as.factor(des[[trt_cols[i]]])
                    if (nlevels(des[[trt_cols[i]]]) == length(fac.names[[i]])) {
                        levels(des[[trt_cols[i]]]) <- fac.names[[i]]
                    }
                    # Rename column
                    names(des)[names(des) == trt_cols[i]] <- names(fac.names)[i]
                }
            }
        } else if (is.character(fac.names) && length(fac.names) >= 2) {
            # Just rename the columns
            names(des)[names(des) == "wp_treatments"] <- fac.names[1]
            names(des)[names(des) == "sub_treatments"] <- fac.names[2]
        }
    }

    # Create output list
    output <- list(design = des)
    class(des) <- c("design", class(des))

    # Add buffers if requested
    if (!is.null(buffer)) {
        has_blocks <- any(grepl("block", tolower(names(des))))
        des <- create_buffers(des, type = buffer, blocks = has_blocks)
        output$design <- des
    }

    # Create plot
    if (plot) {
        output$plot.des <- autoplot(des, rotation = rotation, size = size, margin = margin)
    }

    # Create SATAB
    output$satab <- satab(design.obj)

    # Print output if not quiet
    if (!quiet) {
        print.satab(output$satab)
        if (plot) {
            plot(output$plot.des)
        }
    }

    # Handle saving
    handle_save(save, savename, plottype, output, ...)

    # Add seed if requested
    if (return.seed) {
        output$seed <- design.obj$parameters$seed
    }

    class(output) <- c("design", class(output))

    return(output)
}


# Build Functions ---------------------------------------------------------

#' Create Agricolae Design Object
#'
#' Calls the appropriate agricolae design function
#' @importFrom agricolae design.ab design.crd design.rcbd design.lsd design.split design.strip
#'
#' @noRd
create_agricolae_design <- function(parsed_type, treatments, reps,
                                    sub_treatments, seed) {
  seed_value <- if (is.numeric(seed)) seed else 0

  if (parsed_type$is_factorial) {
    if (length(treatments) > 3) {
      stop("Crossed designs with more than three treatment factors are not supported",
           call. = FALSE)
    }

    outdesign <- agricolae::design.ab(
      trt = treatments,
      r = reps,
      design = parsed_type$base,
      seed = seed_value
    )
  } else {
    outdesign <- switch(parsed_type$base,
                        crd = agricolae::design.crd(
                          trt = treatments,
                          r = reps,
                          seed = seed_value
                        ),
                        rcbd = agricolae::design.rcbd(
                          trt = treatments,
                          r = reps,
                          seed = seed_value
                        ),
                        lsd = {
                          if (!missing(reps)) {
                            message("Number of replicates is not required for Latin Square designs and has been ignored")
                          }
                          agricolae::design.lsd(
                            trt = treatments,
                            seed = seed_value
                          )
                        },
                        split = {
                          if (is.null(sub_treatments) || anyNA(sub_treatments)) {
                            stop("sub_treatments are required for a split plot design", call. = FALSE)
                          }
                          agricolae::design.split(
                            trt1 = treatments,
                            trt2 = sub_treatments,
                            r = reps,
                            seed = seed_value
                          )
                        },
                        strip = {
                          if (is.null(sub_treatments) || anyNA(sub_treatments)) {
                            stop("sub_treatments are required for a strip plot design", call. = FALSE)
                          }
                          agricolae::design.strip(
                            trt1 = treatments,
                            trt2 = sub_treatments,
                            r = reps,
                            seed = seed_value
                          )
                        },
                        stop("Unknown design type: ", parsed_type$base, call. = FALSE)
    )
  }

  return(outdesign)
}

#' Build Design Data Frame
#'
#' Creates the complete design data frame based on design type
#' @noRd
build_design <- function(design.obj, design_type, nrows, ncols,
                         brows, bcols, fac.sep, byrow) {

    des <- switch(design_type,
                  crd = build_crd(design.obj$book, nrows, ncols),
                  rcbd = build_rcbd(design.obj$book, nrows, ncols, brows, bcols),
                  lsd = build_lsd(design.obj$book),
                  factorial_crd = build_factorial_crd(design.obj$book, nrows, ncols, fac.sep),
                  factorial_rcbd = build_factorial_rcbd(design.obj$book, nrows, ncols,
                                                        brows, bcols, fac.sep),
                  factorial_lsd = build_factorial_lsd(design.obj$book, fac.sep),
                  split = build_split(design.obj$book, nrows, ncols, brows, bcols, byrow),
                  strip = build_strip(design.obj$book, nrows, ncols, brows, bcols, byrow),
                  stop("Unsupported design type: ", design_type, call. = FALSE)
    )

    return(des)
}

#' Build CRD Design
#' @noRd
build_crd <- function(design_book, nrows, ncols) {
    plan <- expand.grid(row = 1:nrows, col = 1:ncols)
    des <- cbind(plan, design_book)

    names(des)[names(des) == "r"] <- "reps"

    des
}

#' Build RCBD Design
#' @noRd
build_rcbd <- function(design_book, nrows, ncols, brows, bcols) {
    ntrt <- n_unique(design_book$treatments)

    plan <- calculate_block_layout(nrows, ncols, brows, bcols, ntrt, design_book$block)
    des <- cbind(plan, design_book)

    des
}

#' Build LSD Design
#' @noRd
build_lsd <- function(design_book) {
    des <- design_book
    des$row <- as.numeric(des$row)
    des$col <- as.numeric(des$col)

    des
}

#' Build Factorial CRD Design
#' @noRd
build_factorial_crd <- function(design_book, nrows, ncols, fac.sep) {
    # Factorial designs need an explicit combined `treatments` column.
    design_book$treatments <- construct_factorial_labels(design_book, 3, fac.sep)

    # Reuse the standard CRD layout builder
    build_crd(design_book, nrows, ncols)
}

#' Build Factorial RCBD Design
#' @noRd
build_factorial_rcbd <- function(design_book, nrows, ncols, brows, bcols, fac.sep) {
    # Factorial designs need an explicit combined `treatments` column.
    design_book$treatments <- construct_factorial_labels(design_book, 3, fac.sep)

    # Reuse the standard RCBD layout builder.
    build_rcbd(design_book, nrows, ncols, brows, bcols)
}

#' Build Factorial LSD Design
#' @noRd
build_factorial_lsd <- function(design_book, fac.sep) {
    # Factorial designs need an explicit combined `treatments` column.
    design_book$treatments <- construct_factorial_labels(design_book, 4, fac.sep)

    # Reuse the standard LSD builder.
    build_lsd(design_book)
}

#' Build Split Plot Design
#' @noRd
build_split <- function(design_book, nrows, ncols, brows, bcols, byrow) {
    # Prepare split design structure
    des <- prepare_split_design(design_book)

    # Get treatment column names
    spfacs <- c("plots", "block", "wholeplots", "subplots")
    trtNams <- names(des[!is.element(names(des), spfacs)])

    # Create combined treatment factor
    des$wp_treatments <- des[[trtNams[1]]]
    des$treatments <- factor(paste(des[[trtNams[1]]], des[[trtNams[2]]], sep = "_"))
    ntrt <- nlevels(des$treatments)

    # Calculate layout
    plan <- calculate_block_layout(nrows, ncols, brows, bcols, ntrt, des$block)
    des <- cbind(plan, des)

    # Order by column within blocks if requested
    if (!byrow) {
        des[, c("row", "col", "block")] <- des[order(des$block, des$col, des$row),
                                               c("row", "col", "block")]
    }

    des
}

#' Build Strip Plot Design
#' @noRd
build_strip <- function(design_book, nrows, ncols, brows, bcols, byrow) {
    strip_info <- validate_strip_inputs(design_book, nrows, ncols, brows, bcols)

    # Determine row-strip and column-strip treatment levels from the design book.
    # For classic strip-plot designs, one treatment is applied to each row and
    # one (different) treatment is applied to each column within each block.
    row_levels <- strip_info$row_levels
    col_levels <- strip_info$col_levels

    # Create a deterministic tiling of blocks across the whole layout.
    # Block numbering is row-major across block tiles.
    cc <- strip_info$cc

    plan <- expand.grid(row = 1:nrows, col = 1:ncols)
    plan$block <- ((plan$row - 1L) %/% brows) * cc + ((plan$col - 1L) %/% bcols) + 1L

    # Within-block indices identify the row-strip and column-strip.
    wholeplots <- ((plan$row - 1L) %% brows) + 1L
    subplots <- ((plan$col - 1L) %% bcols) + 1L

    # Assign treatments so that within each block:
    # - all plots in the same within-block row share the row treatment
    # - all plots in the same within-block column share the column treatment
    wp_treatments <- vector(mode = "character", length = nrow(plan))
    sp_treatments <- vector(mode = "character", length = nrow(plan))

    for (blk in sort(unique(plan$block))) {
        idx <- which(plan$block == blk)
        row_map <- sample(row_levels, size = brows, replace = FALSE)
        col_map <- sample(col_levels, size = bcols, replace = FALSE)
        wp_treatments[idx] <- row_map[wholeplots[idx]]
        sp_treatments[idx] <- col_map[subplots[idx]]
    }

    des <- cbind(
        plan,
        plots = seq_len(nrow(plan)),
        wholeplots = wholeplots,
        subplots = subplots,
        wp_treatments = wp_treatments,
        sub_treatments = sp_treatments
    )

    des$treatments <- factor(paste(des$wp_treatments, des$sub_treatments, sep = "_"))

    # Order by column within blocks if requested (keeps treatments fixed,
    # but swaps the coordinates to provide the alternative arrangement).
    if (!byrow) {
        des[, c("row", "col", "block")] <- des[order(des$block, des$col, des$row),
                                                 c("row", "col", "block")]
    }

    des
}

#' Normalise Agricolae Book Column Names
#'
#' Agricolae design functions sometimes name treatment columns based on the
#' expression passed to `trt`/`trt1`/`trt2` (e.g. `"c(1, 5, 10, 20)"`).
#' This helper standardises those columns to `treatments` / `sub_treatments`
#' so downstream code can rely on stable names.
#' @noRd
normalise_agricolae_book <- function(design_book, design_info) {
    if (is.null(design_book) || !is.data.frame(design_book)) {
        return(design_book)
    }

    if (isTRUE(design_info$is_factorial)) {
        return(design_book)
    }

    rename_col <- function(from, to) {
        if (from %in% names(design_book) && !to %in% names(design_book)) {
            names(design_book)[names(design_book) == from] <<- to
        }
    }

    struct_cols_common <- c("plots", "block", "r", "row", "col")

    if (identical(design_info$type, "split") || identical(design_info$base, "split") ||
        identical(design_info$type, "strip") || identical(design_info$base, "strip")) {
        # Preferred explicit names
        rename_col("trt1", "treatments")
        rename_col("trt2", "sub_treatments")

        # Fallback: infer from position among non-structural columns
        struct_cols <- c(struct_cols_common, "splots", "wplots", "wholeplots", "subplots")
        candidates <- setdiff(names(design_book), struct_cols)

        has_main <- "treatments" %in% names(design_book)
        has_sub <- "sub_treatments" %in% names(design_book)

        if (!has_main && !has_sub && length(candidates) >= 2) {
            main_col <- candidates[length(candidates) - 1]
            sub_col <- candidates[length(candidates)]
            names(design_book)[names(design_book) == main_col] <- "treatments"
            names(design_book)[names(design_book) == sub_col] <- "sub_treatments"
        } else {
            if (!has_main && length(candidates) >= 1) {
                main_col <- candidates[length(candidates)]
                names(design_book)[names(design_book) == main_col] <- "treatments"
                candidates <- setdiff(candidates, main_col)
            }

            has_sub <- "sub_treatments" %in% names(design_book)
            if (!has_sub && length(candidates) >= 1) {
                sub_col <- candidates[length(candidates)]
                if (sub_col != "treatments") {
                    names(design_book)[names(design_book) == sub_col] <- "sub_treatments"
                }
            }
        }

        return(design_book)
    }

    # Non-split, non-factorial designs: ensure a single `treatments` column.
    rename_col("trt", "treatments")

    if (!"treatments" %in% names(design_book)) {
        candidates <- setdiff(names(design_book), struct_cols_common)
        if (length(candidates) >= 1) {
            trt_col <- candidates[length(candidates)]
            names(design_book)[names(design_book) == trt_col] <- "treatments"
        }
    }

    design_book
}
