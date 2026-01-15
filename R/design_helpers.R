#' Design Metadata Registry
#'
#' Central registry of design types and their requirements
#' @noRd
design_registry <- function() {
  list(
    crd = list(
      agricolae_func = "design.crd",
      needs_blocks = FALSE,
      needs_reps = TRUE,
      treatment_col = 3,
      has_factorial = TRUE
    ),
    rcbd = list(
      agricolae_func = "design.rcbd",
      needs_blocks = TRUE,
      needs_reps = TRUE,
      treatment_col = 3,
      has_factorial = TRUE
    ),
    lsd = list(
      agricolae_func = "design.lsd",
      needs_blocks = FALSE,
      needs_reps = FALSE,
      treatment_col = 4,
      has_factorial = TRUE
    ),
    split = list(
      agricolae_func = "design.split",
      needs_blocks = TRUE,
      needs_reps = TRUE,
      treatment_col = NA,  # handled differently
      has_factorial = FALSE
    )
  )
}

#' Calculate Block Layout Plan
#'
#' Generates row/column positions for blocked designs
#' @param nrows Total number of rows
#' @param ncols Total number of columns
#' @param brows Rows per block
#' @param bcols Columns per block
#' @param ntrt Number of treatments
#' @param block_vec Vector of block assignments
#' @return Data frame with row, col positions
#' @noRd
calculate_block_layout <- function(nrows, ncols, brows, bcols, ntrt, block_vec = NULL) {
  rr <- nrows / brows
  cc <- ncols / bcols
  
  # Blocking across rows: brows == ntrt in a single column
  if (brows == ntrt) {
    plan <- expand.grid(row = 1:nrows, col = 1:ncols)
    return(plan)
  }
  
  # Blocking incomplete rows all columns
  if (rr > 1 & cc == 1) {
    plan <- expand.grid(col = 1:ncols, row = 1:nrows)
    return(plan)
  }
  
  # Blocking across columns: bcols == ntrt in a single row
  if (bcols == ntrt) {
    plan <- expand.grid(col = 1:ncols, row = 1:nrows)
    return(plan)
  }
  
  # Blocking incomplete rows and incomplete columns
  if (rr > 1 & cc > 1) {
    plan <- expand.grid(row = 1:nrows, col = 1:ncols)
    if (!is.null(block_vec)) {
      plan$block <- block_vec
    }
    plan$col <- NA
    plan$row <- NA
    
    pp <- expand.grid(col = 1:bcols, row = 1:brows)
    
    i <- 1
    for (j in 1:rr) {
      for (k in 1:cc) {
        plan$col[plan$block == i] <- pp$col + (k - 1) * bcols
        plan$row[plan$block == i] <- pp$row + (brows * (j - 1))
        k <- k + 1
        i <- i + 1
      }
    }
    plan$block <- NULL
    return(plan)
  }
  
  # Blocking incomplete columns all rows
  if (cc > 1 & rr == 1) {
    plan <- expand.grid(row = 1:nrows, col = 1:ncols)
    if (!is.null(block_vec)) {
      plan$block <- block_vec
    }
    plan$col <- NA
    plan$row <- NA
    
    pp <- expand.grid(col = 1:bcols, row = 1:brows)
    
    i <- 1
    for (k in 1:cc) {
      plan$col[plan$block == i] <- pp$col + (k - 1) * bcols
      plan$row[plan$block == i] <- pp$row
      k <- k + 1
      i <- i + 1
    }
    plan$block <- NULL
    return(plan)
  }
  
  # Default fallback
  expand.grid(row = 1:nrows, col = 1:ncols)
}

#' Construct Factorial Treatment Labels
#'
#' Combines factor levels into treatment labels
#' @param design_book The design book data frame
#' @param start_col First column containing factor levels
#' @param fac.sep Vector of separators (factor-level, between-factors)
#' @return Factor vector of treatment labels
#' @noRd
construct_factorial_labels <- function(design_book, start_col, fac.sep = c("", " ")) {
  treatments <- NULL
  
  for (i in start_col:ncol(design_book)) {
    treatments <- paste(
      treatments,
      paste(colnames(design_book)[i], design_book[, i], sep = fac.sep[1]),
      sep = fac.sep[2]
    )
  }
  
  if (fac.sep[2] == "") {
    return(factor(trimws(treatments)))
  } else {
    return(factor(trimws(substr(treatments, 2, nchar(treatments)))))
  }
}

#' Apply Factor Names to Design
#'
#' Renames factors and their levels in factorial or split designs
#' @param design_book The design book data frame
#' @param fac.names List or vector of new names
#' @param design_type Type of design ("factorial" or "split")
#' @return Modified design_book
#' @noRd
apply_factor_names <- function(design_book, fac.names, design_type = "factorial") {
  if (is.null(fac.names)) {
    return(design_book)
  }
  
  if (design_type == "factorial") {
    n_facs <- ifelse(is.null(design_book$C), 2, 3)
    factor_cols <- c("A", "B", if (n_facs == 3) "C" else NULL)
    
    if (length(fac.names) > n_facs) {
      warning("fac.names contains ", length(fac.names), 
              " elements but only the first ", n_facs, " have been used.", 
              call. = FALSE)
    } else if (length(fac.names) < n_facs) {
      warning("fac.names doesn't contain enough elements and has not been used.", 
              call. = FALSE)
      return(design_book)
    }
    
    if (is.list(fac.names)) {
      for (i in seq_along(factor_cols)) {
        col_name <- factor_cols[i]
        design_book[[col_name]] <- as.factor(design_book[[col_name]])
        
        if (length(levels(design_book[[col_name]])) == length(fac.names[[i]])) {
          levels(design_book[[col_name]]) <- fac.names[[i]]
          colnames(design_book)[colnames(design_book) == col_name] <- names(fac.names)[i]
        } else {
          warning(names(fac.names)[i], 
                  " must contain the correct number of elements. Elements have not been applied.", 
                  call. = FALSE)
        }
      }
    }
    
  } else if (design_type == "split") {
    factor_cols <- c("treatments", "sub_treatments")
    
    if (length(fac.names) > 2) {
      warning("fac.names contains ", length(fac.names), 
              " elements but only the first 2 have been used.", 
              call. = FALSE)
    } else if (length(fac.names) < 2) {
      warning("fac.names doesn't contain enough elements and has not been used.", 
              call. = FALSE)
      return(design_book)
    }
    
    if (is.list(fac.names)) {
      for (i in 1:2) {
        col_name <- factor_cols[i]
        design_book[[col_name]] <- as.factor(design_book[[col_name]])
        
        if (length(levels(design_book[[col_name]])) == length(fac.names[[i]])) {
          levels(design_book[[col_name]]) <- fac.names[[i]]
        } else {
          warning(names(fac.names)[i], 
                  " must contain the correct number of elements. Elements have not been applied.", 
                  call. = FALSE)
        }
      }
      colnames(design_book)[colnames(design_book) %in% factor_cols] <- names(fac.names)[1:2]
      
    } else if (is.character(fac.names)) {
      colnames(design_book)[colnames(design_book) %in% factor_cols] <- fac.names[1:2]
    }
  }
  
  return(design_book)
}

#' Get Design Type Information
#'
#' Determines the actual design type and whether it's factorial
#' @param design_obj Agricolae design object
#' @return List with design type and whether factorial
#' @noRd
get_design_info <- function(design_obj) {
  base_design <- design_obj$parameters$design
  
  if (base_design == "factorial") {
    applied <- design_obj$parameters$applied
    list(
      type = paste0("factorial_", applied),
      is_factorial = TRUE,
      base = applied
    )
  } else {
    list(
      type = base_design,
      is_factorial = FALSE,
      base = base_design
    )
  }
}

#' Validate Block Parameters
#'
#' Checks if block parameters are provided when needed
#' @param design_info Design info from get_design_info()
#' @param brows Rows per block
#' @param bcols Columns per block
#' @noRd
validate_block_params <- function(design_info, brows, bcols) {
  needs_blocks <- design_info$base %in% c("rcbd", "split")
  
  if (needs_blocks && anyNA(c(brows, bcols))) {
    stop("Design has blocks so brows and bcols must be supplied.", call. = FALSE)
  }
}

#' Prepare Split Plot Design
#'
#' Adds wholeplot and subplot columns to split design
#' @param design_book The design book
#' @return Modified design book
#' @noRd
prepare_split_design <- function(design_book) {
  numsp <- max(as.numeric(design_book$splots))
  lenblk <- as.vector(table(design_book$block)[1])
  numwp <- lenblk / numsp
  
  design_book$wplots <- rep(
    rep(1:numwp, each = numsp),
    max(as.numeric(design_book$block))
  )
  
  design_book <- design_book[, c(1, 3, 6, 2, 4, 5)]
  colnames(design_book)[colnames(design_book) == "wplots"] <- "wholeplots"
  colnames(design_book)[colnames(design_book) == "splots"] <- "subplots"
  
  design_book
}