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
    stop("Unknown design type: ", design_type, call. = FALSE)
  )
}

#' ANOVA structure for CRD
#' @noRd
anova_crd <- function(design_book) {
  trt_name <- names(design_book)[3]
  totdf <- nrow(design_book) - 1
  trtdf <- length(unique(design_book[, 3])) - 1
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
  blkdf <- length(unique(design_book$block)) - 1
  totdf <- nrow(design_book) - 1
  trtdf <- length(unique(design_book[, 3])) - 1
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
  rowdf <- length(unique(design_book$row)) - 1
  coldf <- length(unique(design_book$col)) - 1
  totdf <- nrow(design_book) - 1
  trtdf <- length(unique(design_book[, 4])) - 1
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
  trt_names <- names(design_book)[3:(ncol(design_book) - 1)]
  totdf <- nrow(design_book) - 1
  
  # Calculate df for each factor
  trtdf <- sapply(trt_names, function(name) {
    length(unique(design_book[[name]])) - 1
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
  trt_names <- names(design_book)[3:(ncol(design_book) - 1)]
  totdf <- nrow(design_book) - 1
  blkdf <- length(unique(design_book$block)) - 1
  
  # Calculate df for each factor
  trtdf <- sapply(trt_names, function(name) {
    length(unique(design_book[[name]])) - 1
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
  rowdf <- length(unique(design_book$row)) - 1
  coldf <- length(unique(design_book$col)) - 1
  totdf <- nrow(design_book) - 1
  
  trt_names <- names(design_book)[4:(ncol(design_book) - 1)]
  
  # Calculate df for each factor
  trtdf <- sapply(trt_names, function(name) {
    length(unique(design_book[[name]])) - 1
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
  blkdf <- length(unique(design_book$block)) - 1
  totdf <- nrow(design_book) - 1
  numwplots <- nrow(design_book) / length(unique(design_book$subplots))
  
  trtAname <- names(design_book)[5]
  trtBname <- names(design_book)[6]
  
  trtAdf <- length(unique(design_book[, 5])) - 1
  trtBdf <- length(unique(design_book[, 6])) - 1
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
  width1 <- ifelse(df[1] > 10, 44, 45)
  width2 <- ifelse(df[2] > 10, 35, 36)
  width3 <- ifelse(df[4] > 10, 35, 36)
  width4 <- ifelse(df[5] > 10, 35, 36)
  width5 <- ifelse(df[7] > 10, 44, 45)
  
  output <- c(
    paste0(format("Source of Variation", width = 45), "df", "\n"),
    "==================================================\n",
    paste0(format(sources[1], width = width1), df[1], "\n"),
    "--------------------------------------------------\n",
    "Whole plot stratum\n",
    paste0(format(" ", width = 9), format(sources[2], width = width2), df[2], "\n"),
    paste0(format(sources[3], width = 45), df[3], "\n"),
    "==================================================\n",
    "Subplot stratum\n",
    paste0(format(" ", width = 9), format(sources[4], width = width3), df[4], "\n"),
    paste0(format(" ", width = 9), format(sources[5], width = width4), df[5], "\n"),
    paste0(format(" ", width = 9), format(sources[6], width = 35), df[6], "\n"),
    "==================================================\n",
    paste0(format("Total", width = width5), df[7], "\n")
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