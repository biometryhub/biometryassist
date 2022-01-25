#' Produces a skeletal ANOVA table
#'
#' @param design.obj An `agricolae` design object.
#'
#' @return Prints skeletal ANOVA table to console output.
#'
#' @keywords internal
#'
satab <- function(design.obj) {
  des <- design.obj$parameters$design

  ifelse(des == "factorial",
    design <- paste("factorial", design.obj$parameters$applied, sep = "_"),
    design <- des
  )

  design.obj <- design.obj$book

  if (design != "split") {
    output <- paste0(format("Source of Variation", width = 40), "df", "\n")
    output <- c(output, paste0("=============================================\n"))
  }

  if (design == "crd") {
    trt <- names(design.obj)[3]
    totdf <- nrow(design.obj) - 1
    trtdf <- length(unique(design.obj[, 3])) - 1
    errdf <- totdf - trtdf

    output <- c(output, paste0(format(trt, width = 40), trtdf, "\n"))
    output <- c(output, paste0(format("Residual", width = 40), errdf, "\n"))
    output <- c(output, paste0("=============================================\n"))
    output <- c(output, paste0(format("Total", width = 40), totdf, "\n"))
  }

  if (design == "rcbd") {
    trt <- names(design.obj)[3]
    blkdf <- length(unique(design.obj$block)) - 1
    totdf <- nrow(design.obj) - 1
    trtdf <- length(unique(design.obj[, 3])) - 1
    errdf <- totdf - trtdf - blkdf

    output <- c(output, paste0(format("Block stratum", width = 40), blkdf, "\n"))
    output <- c(output, paste0("---------------------------------------------\n"))
    output <- c(output, paste0(format(trt, width = 40), trtdf, "\n"))
    output <- c(output, paste0(format("Residual", width = 40), errdf, "\n"))
    output <- c(output, paste0("=============================================\n"))
    output <- c(output, paste0(format("Total", width = 40), totdf, "\n"))
  }

  if (design == "factorial_crd") {
    trt <- names(design.obj)[3:length(names(design.obj))]
    totdf <- nrow(design.obj) - 1
    trtdf <- c()
    for (i in 1:length(trt)) {
      dd <- length(unique(design.obj[[trt[i]]])) - 1
      trtdf <- c(trtdf, dd)
    }
    trtABdf <- trtdf[1] * trtdf[2]
    errdf <- totdf - sum(trtdf) - trtABdf
    for (i in 1:length(trt)) {
      output <- c(output, paste0(format(trt[i], width = 40), trtdf[i], "\n"))
    }
    output <- c(output, paste0(format(paste0(names(design.obj)[3:length(names(design.obj))], collapse = ":"), width = 40), trtABdf, "\n"))
    output <- c(output, paste0(format("Residual", width = 40), errdf, "\n"))
    output <- c(output, paste0("=============================================\n"))
    output <- c(output, paste0(format("Total", width = 40), totdf, "\n"))
  }


  if (design == "factorial_rcbd") {
    trt <- names(design.obj)[3:length(names(design.obj))]
    totdf <- nrow(design.obj) - 1
    trtdf <- c()
    for (i in 1:length(trt)) {
      dd <- length(unique(design.obj[[trt[i]]])) - 1
      trtdf <- c(trtdf, dd)
    }
    trtABdf <- trtdf[1] * trtdf[2]
    blkdf <- length(unique(design.obj$block)) - 1
    output <- c(output, paste0(format("Block stratum", width = 40), blkdf, "\n"))
    output <- c(output, paste0("---------------------------------------------\n"))
    errdf <- totdf - sum(trtdf) - trtABdf - blkdf
    for (i in 1:length(trt)) {
      output <- c(output, paste0(format(trt[i], width = 40), trtdf[i], "\n"))
    }
    output <- c(output, paste0(format(paste0(names(design.obj)[3:length(names(design.obj))], collapse = ":"), width = 40), trtABdf, "\n"))
    output <- c(output, paste0(format("Residual", width = 40), errdf, "\n"))
    output <- c(output, paste0("=============================================\n"))
    output <- c(output, paste0(format("Total", width = 40), totdf, "\n"))
  }

  if (design == "lsd") {
    trt <- names(design.obj)[4]
    rowdf <- length(unique(design.obj$row)) - 1
    coldf <- length(unique(design.obj$col)) - 1
    totdf <- nrow(design.obj) - 1
    trtdf <- length(unique(design.obj[, 4])) - 1
    errdf <- totdf - trtdf - coldf - rowdf

    output <- c(output, paste0(format("Row", width = 40), rowdf, "\n"))
    output <- c(output, paste0(format("Column", width = 40), coldf, "\n"))
    output <- c(output, paste0(format(trt, width = 40), trtdf, "\n"))
    output <- c(output, paste0(format("Residual", width = 40), errdf, "\n"))
    output <- c(output, paste0("=============================================\n"))
    output <- c(output, paste0(format("Total", width = 40), totdf, "\n"))
  }



  if (design == "split") {
    blkdf <- length(unique(design.obj$block)) - 1
    totdf <- nrow(design.obj) - 1
    numwplots <- nrow(design.obj) / length(unique(design.obj$splots))
    sp.facWdf <- length(unique(design.obj[, 4])) - 1
    wpresdf <- (numwplots - 1) - blkdf - sp.facWdf

    trtAdf <- length(unique(design.obj[, 4])) - 1
    trtBdf <- length(unique(design.obj[, 5])) - 1
    trtABdf <- trtAdf * trtBdf
    errdf <- totdf - trtAdf - trtBdf - trtABdf - blkdf - wpresdf

    output <- paste0(format("Source of Variation", width = 45), "df", "\n")
    output <- c(output, paste0("==================================================\n"))
    output <- c(output, paste0(format("Block stratum", width = ifelse(blkdf>10, 44, 45)), blkdf, "\n"))
    output <- c(output, paste0("--------------------------------------------------\n"))
    output <- c(output, paste0("Whole plot stratum", "\n"))
    output <- c(output, paste0(format(" ", width = 9), format(names(design.obj)[4], width = ifelse(trtAdf>10, 35, 36)), trtAdf, "\n"))
    output <- c(output, paste0(format("Whole plot Residual", width = 45), wpresdf, "\n"))
    output <- c(output, paste0("==================================================\n"))
    output <- c(output, paste0("Subplot stratum", "\n"))
    output <- c(output, paste0(format(" ", width = 9), format(names(design.obj)[5], width = ifelse(trtBdf>10, 35, 36)), trtBdf, "\n"))
    output <- c(output, paste0(format(" ", width = 9), format(paste(names(design.obj)[4], names(design.obj)[5], sep = ":"), width = ifelse(trtABdf>10, 35, 36)), trtABdf, "\n"))
    output <- c(output, paste0(format(" ", width = 9), format("Subplot Residual", width = 35), errdf, "\n"))
    output <- c(output, paste0("==================================================\n"))
    output <- c(output, paste0(format("Total", width = ifelse(totdf>10, 44, 45)), totdf, "\n"))
  }

  if (design == "factorial_lsd") {
    rowdf <- length(unique(design.obj$row)) - 1
    coldf <- length(unique(design.obj$col)) - 1
    totdf <- nrow(design.obj) - 1
    trtAdf <- length(unique(design.obj[,4])) - 1
    trtBdf <- length(unique(design.obj[,5])) - 1
    trtABdf <- trtAdf * trtBdf
    errdf <- totdf - trtAdf - trtBdf - trtABdf - rowdf - coldf

    output <- c(output, paste0(format("Row", width = 40), rowdf, "\n"))
    output <- c(output, paste0(format("Column", width = 40), coldf, "\n"))
    output <- c(output, paste0(format(names(design.obj)[4], width = 40), trtAdf, "\n"))
    output <- c(output, paste0(format(names(design.obj)[5], width = 40), trtBdf, "\n"))
    output <- c(output, paste0(format(paste0(names(design.obj)[4:length(names(design.obj))], collapse = ":"), width = 40), trtABdf, "\n"))
    output <- c(output, paste0(format("Residual", width = 40), errdf, "\n"))
    output <- c(output, paste0("=============================================\n"))
    output <- c(output, paste0(format("Total", width = 40), totdf, "\n"))
  }
  return(output)
}
