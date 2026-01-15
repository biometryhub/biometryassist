#' Create a complete experimental design with graph of design layout and skeletal ANOVA table
#'
#' @param type The type of design. Supported design types are `crd`, `rcbd`, `lsd`, `crossed:<type>` where `<type>` is one of the previous types, and `split`. See Details for more information.
#' @param treatments A vector containing the treatment names or labels.
#' @param reps The number of replicates. Ignored for Latin Square Designs.
#' @param nrows The number of rows in the design.
#' @param ncols The number of columns in the design.
#' @param brows For RCBD and Split Plot designs. The number of rows in a block.
#' @param bcols For RCBD and Split Plot designs. The number of columns in a block.
#' @param byrow For split-plot only. Logical (default `TRUE`). Provides a way to arrange plots within whole-plots when there are multiple possible arrangements.
#' @param sub_treatments A vector of treatments for sub-plots in a split plot design.
#' @param fac.names Allows renaming of the `A` level of factorial designs (i.e. those using [agricolae::design.ab()]) by passing (optionally named) vectors of new labels to be applied to the factors within a list. See examples and details for more information.
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
#' @details The designs currently supported by `type` are Completely Randomised designs (`crd`), Randomised Complete Block designs (`rcbd`), Latin Square Designs (`lsd`), Factorial with crossed structure (use `crossed:<type>` where `<type>` is one of the previous types e.g. `crossed:crd`) and Split Plot designs (`split`). Nested factorial designs are supported through manual setup, see Examples.
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
  
  # Normalize fac.sep
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
  
  # Apply factor names if this is a factorial or split design
  if (design_info$is_factorial) {
    outdesign$book <- apply_factor_names(outdesign$book, fac.names, "factorial")
  } else if (design_info$type == "split") {
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
  
  # Error checking of inputs
  rlang::check_dots_used()
  
  # Normalize fac.sep
  if (!missing(fac.sep) && length(fac.sep) == 1) {
    fac.sep <- rep(fac.sep, times = 2)
  }
  
  # Get design information
  design_info <- get_design_info(design.obj)
  
  # Validate block parameters
  validate_block_params(design_info, brows, bcols)
  
  # Apply factor names if this is a factorial or split design
  if (design_info$is_factorial) {
    design.obj$book <- apply_factor_names(design.obj$book, fac.names, "factorial")
  } else if (design_info$type == "split") {
    design.obj$book <- apply_factor_names(design.obj$book, fac.names, "split")
  }
  
  # Build the design based on type
  des <- build_design(design.obj, design_info$type, nrows, ncols, 
                      brows, bcols, fac.sep, byrow)
  
  # Sort treatments naturally
  des$treatments <- factor(des$treatments, 
                          levels = unique(stringi::stri_sort(des$treatments, numeric = TRUE)))
  
  # Create output list
  info <- list(design = des)
  class(des) <- c("design", class(des))
  
  # Add buffers if requested
  if (!is.null(buffer)) {
    has_blocks <- any(grepl("block", tolower(names(des))))
    des <- create_buffers(des, type = buffer, blocks = has_blocks)
    info$design <- des
  }
  
  # Create plot
  if (plot) {
    info$plot.des <- autoplot(des, rotation = rotation, size = size, margin = margin)
  }
  
  # Create SATAB
  info$satab <- satab(design.obj)
  
  # Print output if not quiet
  if (!quiet) {
    print.satab(info$satab)
    if (plot) {
      plot(info$plot.des)
    }
  }
  
  # Handle saving
  handle_save(save, savename, plottype, info, ...)
  
  # Add seed if requested
  if (return.seed) {
    info$seed <- design.obj$parameters$seed
  }
  
  return(info)
}


# Helper Functions --------------------------------------------------------

#' Validate Design Inputs
#' @noRd
validate_design_inputs <- function(nrows, ncols, brows, bcols, size, seed) {
  if (!is.na(brows) && brows > nrows) {
    stop("brows must not be larger than nrows", call. = FALSE)
  }
  
  if (!is.na(bcols) && bcols > ncols) {
    stop("bcols must not be larger than ncols", call. = FALSE)
  }
  
  if (!is.numeric(size)) {
    stop("size must be numeric", call. = FALSE)
  }
  
  if ((!is.logical(seed) || is.na(seed)) && !is.numeric(seed)) {
    stop("seed must be numeric or TRUE/FALSE", call. = FALSE)
  }
}

#' Parse Design Type
#'
#' Parses user input to determine base design and whether factorial
#' @noRd
parse_design_type <- function(type) {
  type_lower <- tolower(type)
  
  # Check if factorial (crossed)
  if (substr(type_lower, 1, 7) == "crossed") {
    type_split <- unlist(strsplit(type_lower, ":"))
    
    if (length(type_split) < 2) {
      stop("Crossed designs must be specified as 'crossed:<type>'", call. = FALSE)
    }
    
    base_type <- type_split[2]
    
    if (base_type %!in% c("crd", "rcbd", "lsd")) {
      stop("Crossed designs of type '", base_type, "' are not supported", call. = FALSE)
    }
    
    return(list(
      base = base_type,
      is_factorial = TRUE,
      full_type = paste0("factorial_", base_type)
    ))
  }
  
  # Non-factorial designs
  valid_types <- c("crd", "rcbd", "lsd", "split")
  if (type_lower %!in% valid_types) {
    stop("Designs of type '", type, "' are not supported", call. = FALSE)
  }
  
  return(list(
    base = type_lower,
    is_factorial = FALSE,
    full_type = type_lower
  ))
}

#' Create Agricolae Design Object
#'
#' Calls the appropriate agricolae design function
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
      stop("Unknown design type: ", parsed_type$base, call. = FALSE)
    )
  }
  
  return(outdesign)
}

#' Calculate Total Number of Plots
#' @noRd
calculate_total_plots <- function(parsed_type, treatments, reps, sub_treatments) {
  if (parsed_type$is_factorial) {
    if (parsed_type$base == "lsd") {
      return(prod(treatments)^2)
    } else {
      return(prod(treatments) * reps)
    }
  }
  
  switch(parsed_type$base,
    crd = length(treatments) * reps,
    rcbd = length(treatments) * reps,
    lsd = length(treatments)^2,
    split = length(treatments) * length(sub_treatments) * reps,
    stop("Unknown design type")
  )
}

#' Validate Design Dimensions
#' @noRd
validate_dimensions <- function(dim, trs) {
  if (dim > trs) {
    warning("Area provided is larger than treatments applied. Please check inputs.", 
            call. = FALSE)
  }
  
  if (dim < trs) {
    warning("Area provided is smaller than treatments applied. Please check inputs.", 
            call. = FALSE)
  }
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
    stop("Unknown design type: ", design_type, call. = FALSE)
  )
  
  return(des)
}

#' Build CRD Design
#' @noRd
build_crd <- function(design_book, nrows, ncols) {
  plan <- expand.grid(row = 1:nrows, col = 1:ncols)
  des <- cbind(plan, design_book)
  
  names(des)[names(des) == "r"] <- "rep"
  names(des)[ncol(des)] <- "treatments"
  
  des
}

#' Build RCBD Design
#' @noRd
build_rcbd <- function(design_book, nrows, ncols, brows, bcols) {
  ntrt <- nlevels(as.factor(design_book[, ncol(design_book)]))
  
  plan <- calculate_block_layout(nrows, ncols, brows, bcols, ntrt, design_book$block)
  des <- cbind(plan, design_book)
  
  names(des)[ncol(des)] <- "treatments"
  
  des
}

#' Build LSD Design
#' @noRd
build_lsd <- function(design_book) {
  des <- design_book
  des$row <- as.numeric(des$row)
  des$col <- as.numeric(des$col)
  
  names(des)[ncol(des)] <- "treatments"
  
  des
}

#' Build Factorial CRD Design
#' @noRd
build_factorial_crd <- function(design_book, nrows, ncols, fac.sep) {
  plan <- expand.grid(row = 1:nrows, col = 1:ncols)
  des <- cbind(plan, design_book, row.names = NULL)
  
  des$treatments <- construct_factorial_labels(design_book, 3, fac.sep)
  names(des)[names(des) == "r"] <- "reps"
  
  des
}

#' Build Factorial RCBD Design
#' @noRd
build_factorial_rcbd <- function(design_book, nrows, ncols, brows, bcols, fac.sep) {
  treatments <- construct_factorial_labels(design_book, 3, fac.sep)
  ntrt <- nlevels(as.factor(treatments))
  
  plan <- calculate_block_layout(nrows, ncols, brows, bcols, ntrt, design_book$block)
  
  design_book$treatments <- treatments
  des <- cbind(plan, design_book)
  
  des
}

#' Build Factorial LSD Design
#' @noRd
build_factorial_lsd <- function(design_book, fac.sep) {
  des <- design_book
  
  des$treatments <- construct_factorial_labels(design_book, 4, fac.sep)
  des$row <- as.numeric(des$row)
  des$col <- as.numeric(des$col)
  
  des
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
  des$treatments <- factor(paste(des[, trtNams[1]], des[, trtNams[2]], sep = "_"))
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

#' Handle Save Operations
#' @noRd
handle_save <- function(save, savename, plottype, info, ...) {
  if (!is.logical(save)) {
    output <- tolower(save)
  } else if (save) {
    output <- "both"
  } else {
    output <- "none"
  }
  
  if (output == "plot") {
    ggplot2::ggsave(filename = paste0(savename, ".", plottype), ...)
  } else if (output == "workbook") {
    write.csv(info$design, file = paste0(savename, ".csv"), row.names = FALSE)
  } else if (output == "both") {
    ggplot2::ggsave(filename = paste0(savename, ".", plottype), ...)
    write.csv(info$design, file = paste0(savename, ".csv"), row.names = FALSE)
  } else if (output != "none") {
    stop("save must be one of 'none'/FALSE, 'both'/TRUE, 'plot', or 'workbook'.", 
         call. = FALSE)
  }
}