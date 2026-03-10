#' Produces a skeletal ANOVA table
#'
#' @param design.obj A modified `agricolae` design object as output from des_info.
#'
#' @returns Prints skeletal ANOVA table to console output.
#'
#' @keywords internal
satab <- function(design.obj) {
  design_info <- get_design_info(design.obj)
  design_book <- design.obj$book

  # Get ANOVA structure for this design
  anova_structure <- get_anova_structure(design_info$type, design_book)

  # Format and return
  format_satab(anova_structure, design_info$type)
}

#' Get ANOVA Structure for Design
#'
#' Calculates degrees of freedom and source names
#' @noRd
get_anova_structure <- function(design_type, design_book) {
  switch(design_type,
    crd = anova_crd(design_book),
    rcbd = anova_rcbd(design_book),
    lsd = anova_lsd(design_book),
    factorial_crd = anova_factorial_crd(design_book),
    factorial_rcbd = anova_factorial_rcbd(design_book),
    factorial_lsd = anova_factorial_lsd(design_book),
    split = anova_split(design_book),
    strip = anova_strip(design_book),
    stop("Unknown design type: ", design_type, call. = FALSE)
  )
}

#' ANOVA structure for Strip Plot
#' @noRd
anova_strip <- function(design_book) {
  if (!"block" %in% names(design_book)) {
    stop("Expected a 'block' column in strip plot design", call. = FALSE)
  }

  # Treatment columns are the columns that aren't structural columns.
  structural_cols <- c("plots", "block", "wholeplots", "wplots", "subplots", "splots")
  trt_cols <- setdiff(names(design_book), structural_cols)

  if (length(trt_cols) != 2) {
    stop("Expected 2 treatment columns in strip plot design, found ",
         length(trt_cols), call. = FALSE)
  }

  trtAname <- trt_cols[1]
  trtBname <- trt_cols[2]

  r <- n_unique(design_book$block)
  a <- n_unique(design_book[[trtAname]])
  b <- n_unique(design_book[[trtBname]])

  blkdf <- r - 1
  totdf <- nrow(design_book) - 1
  trtAdf <- a - 1
  trtBdf <- b - 1
  trtABdf <- trtAdf * trtBdf

  # Typical strip-plot error strata dfs
  errAdf <- (r - 1) * trtAdf
  errBdf <- (r - 1) * trtBdf
  errABdf <- (r - 1) * trtABdf

  list(
    sources = c(
      "Block stratum",
      trtAname, paste0(trtAname, " Residual"),
      trtBname, paste0(trtBname, " Residual"),
      paste(trtAname, trtBname, sep = ":"),
      "Interaction Residual",
      "Total"
    ),
    df = c(
      blkdf,
      trtAdf, errAdf,
      trtBdf, errBdf,
      trtABdf,
      errABdf,
      totdf
    ),
    strata = list(
      block = 1,
      stripA = 2:3,
      stripB = 4:5,
      interaction = 6:7
    ),
    names = c(trtAname, trtBname)
  )
}

#' ANOVA structure for CRD
#' @noRd
anova_crd <- function(design_book) {
  trt_name <- names(design_book)[3]
  totdf <- nrow(design_book) - 1
  trtdf <- n_unique(design_book[, 3]) - 1
  errdf <- totdf - trtdf

  list(
    sources = c(trt_name, "Residual", "Total"),
    df = c(trtdf, errdf, totdf),
    strata = NULL
  )
}

#' ANOVA structure for RCBD
#' @noRd
anova_rcbd <- function(design_book) {
  trt_name <- names(design_book)[3]
  blkdf <- n_unique(design_book$block) - 1
  totdf <- nrow(design_book) - 1
  trtdf <- n_unique(design_book[, 3]) - 1
  errdf <- totdf - trtdf - blkdf

  list(
    sources = c("Block stratum", trt_name, "Residual", "Total"),
    df = c(blkdf, trtdf, errdf, totdf),
    strata = list(block = 1, main = 2:3)
  )
}

#' ANOVA structure for LSD
#' @noRd
anova_lsd <- function(design_book) {
  trt_name <- names(design_book)[4]
  rowdf <- n_unique(design_book$row) - 1
  coldf <- n_unique(design_book$col) - 1
  totdf <- nrow(design_book) - 1
  trtdf <- n_unique(design_book[, 4]) - 1
  errdf <- totdf - trtdf - coldf - rowdf

  list(
    sources = c("Row", "Column", trt_name, "Residual", "Total"),
    df = c(rowdf, coldf, trtdf, errdf, totdf),
    strata = NULL
  )
}

#' ANOVA structure for Factorial CRD
#' @noRd
anova_factorial_crd <- function(design_book) {
  # Get factor columns robustly (agricolae books don't always include a combined
  # treatment column).
  structural_cols <- c(
    "plots", "plot", "r", "rep", "reps",
    "block", "row", "col",
    "wholeplots", "wplots", "subplots", "splots",
    "treatments"
  )
  trt_names <- names(design_book)[!names(design_book) %in% structural_cols]
  totdf <- nrow(design_book) - 1

  # Calculate df for each factor
  trtdf <- sapply(trt_names, function(name) {
    n_unique(design_book[[name]]) - 1
  })

  # Interaction df
  interaction_name <- paste(trt_names, collapse = ":")
  interaction_df <- prod(trtdf)

  # Residual df
  errdf <- totdf - sum(trtdf) - interaction_df

  list(
    sources = c(trt_names, interaction_name, "Residual", "Total"),
    df = c(trtdf, interaction_df, errdf, totdf),
    strata = NULL
  )
}

#' ANOVA structure for Factorial RCBD
#' @noRd
anova_factorial_rcbd <- function(design_book) {
  # Get factor columns robustly (agricolae books don't always include a combined
  # treatment column).
  structural_cols <- c(
    "plots", "plot", "r", "rep", "reps",
    "block", "row", "col",
    "wholeplots", "wplots", "subplots", "splots",
    "treatments"
  )
  trt_names <- names(design_book)[!names(design_book) %in% structural_cols]
  totdf <- nrow(design_book) - 1
  blkdf <- n_unique(design_book$block) - 1

  # Calculate df for each factor
  trtdf <- sapply(trt_names, function(name) {
    n_unique(design_book[[name]]) - 1
  })

  # Interaction df
  interaction_name <- paste(trt_names, collapse = ":")
  interaction_df <- prod(trtdf)

  # Residual df
  errdf <- totdf - sum(trtdf) - interaction_df - blkdf

  list(
    sources = c("Block stratum", trt_names, interaction_name, "Residual", "Total"),
    df = c(blkdf, trtdf, interaction_df, errdf, totdf),
    strata = list(block = 1, main = 2:(2 + length(trtdf) + 1))
  )
}

#' ANOVA structure for Factorial LSD
#' @noRd
anova_factorial_lsd <- function(design_book) {
  rowdf <- n_unique(design_book$row) - 1
  coldf <- n_unique(design_book$col) - 1
  totdf <- nrow(design_book) - 1

  # Get factor columns robustly (agricolae books don't always include a combined
  # treatment column).
  structural_cols <- c(
    "plots", "plot", "r", "rep", "reps",
    "block", "row", "col",
    "wholeplots", "wplots", "subplots", "splots",
    "treatments"
  )
  trt_names <- names(design_book)[!names(design_book) %in% structural_cols]

  # Calculate df for each factor
  trtdf <- sapply(trt_names, function(name) {
    n_unique(design_book[[name]]) - 1
  })

  # Interaction df
  interaction_name <- paste(trt_names, collapse = ":")
  interaction_df <- prod(trtdf)

  # Residual df
  errdf <- totdf - sum(trtdf) - interaction_df - rowdf - coldf

  list(
    sources = c("Row", "Column", trt_names, interaction_name, "Residual", "Total"),
    df = c(rowdf, coldf, trtdf, interaction_df, errdf, totdf),
    strata = NULL
  )
}

#' ANOVA structure for Split Plot
#' @noRd
anova_split <- function(design_book) {
  # Get column names dynamically
  # Structure: plots, block, wholeplots, subplots, trtA, trtB, (treatments - added later)

  # Find the block column
  blkdf <- n_unique(design_book$block) - 1
  totdf <- nrow(design_book) - 1

  # Find subplots column (could be 'subplots' or 'splots')
  subplot_col <- intersect(c("subplots", "splots"), names(design_book))
  
  if (length(subplot_col) == 0) {
    stop("Cannot find subplot column in design book", call. = FALSE)
  }
  subplot_col <- subplot_col[1]

  numwplots <- nrow(design_book) / n_unique(design_book[[subplot_col]])

  # Find treatment columns - they're the columns that aren't structural columns
  structural_cols <- c("plots", "block", "wholeplots", "wplots", "subplots", "splots")
  trt_cols <- setdiff(names(design_book), structural_cols)

  if (length(trt_cols) != 2) {
    stop("Expected 2 treatment columns in split plot design, found ",
         length(trt_cols), call. = FALSE)
  }

  # Whole-plot treatment is first, subplot treatment is second
  trtAname <- trt_cols[1]
  trtBname <- trt_cols[2]

  trtAdf <- n_unique(design_book[[trtAname]]) - 1
  trtBdf <- n_unique(design_book[[trtBname]]) - 1
  trtABdf <- trtAdf * trtBdf

  wpresdf <- (numwplots - 1) - blkdf - trtAdf
  errdf <- totdf - trtAdf - trtBdf - trtABdf - blkdf - wpresdf

  list(
    sources = c("Block stratum", trtAname, "Whole plot Residual",
                trtBname, paste(trtAname, trtBname, sep = ":"),
                "Subplot Residual", "Total"),
    df = c(blkdf, trtAdf, wpresdf, trtBdf, trtABdf, errdf, totdf),
    strata = list(
      block = 1,
      wholeplot = 2:3,
      subplot = 4:6
    ),
    names = c(trtAname, trtBname)
  )
}

#' Format SATAB Output
#'
#' Creates formatted string output for ANOVA table
#' @noRd
format_satab <- function(anova_structure, design_type) {
  # Special formatting for split plot
  if (design_type == "split") {
    return(format_satab_split(anova_structure))
  }
  else if (design_type == "strip") {
    return(format_satab_strip(anova_structure))
  }

  # Standard formatting
  output <- c(
    paste0(format("Source of Variation", width = 40), "df", "\n"),
    "=============================================\n"
  )

  sources <- anova_structure$sources
  df <- anova_structure$df

  for (i in seq_along(sources)) {
    # Add separator after block stratum
    if (!is.null(anova_structure$strata) && i == 1) {
      output <- c(output, paste0(format(sources[i], width = 40), df[i], "\n"))
      output <- c(output, "---------------------------------------------\n")
    } else if (sources[i] == "Total") {
      output <- c(output, "=============================================\n")
      output <- c(output, paste0(format(sources[i], width = 40), df[i], "\n"))
    } else {
      output <- c(output, paste0(format(sources[i], width = 40), df[i], "\n"))
    }
  }

  class(output) <- c("satab", class(output))
  return(output)
}

#' Format SATAB for Split Plot (special case)
#' @noRd
format_satab_split <- function(anova_structure) {
  sources <- anova_structure$sources
  df <- anova_structure$df
  names <- anova_structure$names

  # Determine width based on df magnitude
  width1 <- ifelse(df[1] > 9, 44, 45)
  width2 <- ifelse(df[2] > 9, 35, 36)
  width3 <- ifelse(df[3] > 9, 44, 45)
  width4 <- ifelse(df[4] > 9, 35, 36)
  width5 <- ifelse(df[5] > 9, 35, 36)
  width6 <- ifelse(df[6] > 9, 44, 45)
  width7 <- ifelse(df[7] > 9, 44, 45)

  output <- c(
    paste0(format("Source of Variation", width = 45), "df", "\n"),
    "==================================================\n",
    paste0(format(sources[1], width = width1), df[1], "\n"),
    "--------------------------------------------------\n",
    "Whole plot stratum\n",
    paste0(format(" ", width = 9), format(sources[2], width = width2), df[2], "\n"),
    paste0(format(sources[3], width = width3), df[3], "\n"),
    "==================================================\n",
    "Subplot stratum\n",
    paste0(format(" ", width = 9), format(sources[4], width = width4), df[4], "\n"),
    paste0(format(" ", width = 9), format(sources[5], width = width5), df[5], "\n"),
    paste0(format(sources[6], width = width6), df[6], "\n"),
    "==================================================\n",
    paste0(format("Total", width = width7), df[7], "\n")
  )

  class(output) <- c("satab", class(output))
  return(output)
}


#' Format SATAB for Split Plot (special case)
#' @noRd
format_satab_strip <- function(anova_structure) {
  sources <- anova_structure$sources
  df <- anova_structure$df
  names <- anova_structure$names
  
  # Determine width based on df magnitude
  width1 <- ifelse(df[1] > 9, 44, 45)
  width2 <- ifelse(df[2] > 9, 35, 36)
  width3 <- ifelse(df[3] > 9, 44, 45)
  width4 <- ifelse(df[4] > 9, 35, 36)
  width5 <- ifelse(df[5] > 9, 44, 45)
  width6 <- ifelse(df[6] > 9, 35, 36)
  width7 <- ifelse(df[7] > 9, 44, 45)
  width8 <- ifelse(df[8] > 9, 44, 45)
  
  output <- c(
    paste0(format("Source of Variation", width = 45), "df", "\n"),
    "==================================================\n",
    paste0(format(sources[1], width = width1), df[1], "\n"),
    "--------------------------------------------------\n",
    "Row strip stratum\n",
    paste0(format(" ", width = 9), format(sources[2], width = width2), df[2], "\n"),
    paste0(format(sources[3], width = width3), df[3], "\n"),
    "==================================================\n",
    "Column strip stratum\n",
    paste0(format(" ", width = 9), format(sources[4], width = width4), df[4], "\n"),
    paste0(format(sources[5], width = width5), df[5], "\n"),
    "==================================================\n",
    "Observational unit stratum\n",
    paste0(format(" ", width = 9), format(sources[6], width = width6), df[6], "\n"),
    paste0(format(sources[7], width = width7), df[7], "\n"),
    "==================================================\n",
    paste0(format("Total", width = width8), df[8], "\n")
  )
  
  class(output) <- c("satab", class(output))
  return(output)
}


#' @noRd
#' @method print satab
#' @export
print.satab <- function(x, ...) {
  stopifnot(inherits(x, "satab"))
  cat(as.character(x), sep = "")
  invisible(x)
}
