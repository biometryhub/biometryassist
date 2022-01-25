#' Variogram plots for spatial models.
#'
#' Produces variogram plots for checking spatial trends.
#'
#' @param model.obj An `asreml` model object.
#' @param row A row variable.
#' @param column A column variable.
#' @param horizontal Logical (default `TRUE`). The direction the plots are arranged. The default `TRUE` places the plots above and below, while `FALSE` will place them side by side.
#' @param palette A string specifying the colour scheme to use for plotting. The default value (`"default"`) is equivalent to `"rainbow"`. Colour blind friendly palettes can also be provided via options `"colour blind"` (or `"color blind"`, both equivalent to `"viridis"`), `"magma"`, `"inferno"`, `"plasma"` or `"cividis"`. The `"Spectral"` palette from [scales::brewer_pal()] is also possible.
#'
#' @return A ggplot2 object.
#'
#' @importFrom akima interp interp2xyz
#' @importFrom grDevices rainbow
#' @importFrom lattice wireframe
#' @importFrom cowplot plot_grid
#' @importFrom ggplot2 ggplot geom_tile coord_equal geom_contour scale_fill_gradientn theme_bw scale_x_continuous scale_y_continuous theme labs
#'
#' @references S. P. Kaluzny, S. C. Vega, T. P. Cardoso, A. A. Shelly, "S+SpatialStats: User’s Manual for Windows® and UNIX®" _Springer New York_, 2013, p. 68, https://books.google.com.au/books?id=iADkBwAAQBAJ.
#' @references A. R. Gilmour, B. R. Cullis, A. P. Verbyla, "Accounting for Natural and Extraneous Variation in the Analysis of Field Experiments." _Journal of Agricultural, Biological, and Environmental Statistics 2, no. 3_, 1997, pp. 269–93, https://doi.org/10.2307/1400446.
#'
#' @examples
#' \dontrun{
#' library(asreml)
#' oats <- asreml::oats
#' oats <- oats[order(oats$Row, oats$Column),]
#' model.asr <- asreml(yield ~ Nitrogen + Variety + Nitrogen:Variety,
#'                     random = ~ Blocks + Blocks:Wplots,
#'                     residual = ~ ar1(Row):ar1(Column),
#'                     data = oats)
#' variogram(model.asr)
#' }
#' @export

variogram <- function(model.obj, row = NA, column = NA, horizontal = TRUE, palette = "default") {

    if(!(inherits(model.obj, "asreml"))) {
        stop("model.obj must be an asreml model object")
    }

    aa <- vario_df(model.obj)
    xnam <- names(aa)[2]
    ynam <- names(aa)[1]
    fld <- akima::interp(y = aa[,1], x = aa[,2], z = aa$gamma)
    gdat <- akima::interp2xyz(fld, data.frame = TRUE)

    a <- ggplot2::ggplot(gdat, ggplot2::aes(x = y, y = x, z = z, fill = z)) +
        ggplot2::geom_tile(alpha = 0.6) +
        ggplot2::coord_equal() +
        ggplot2::geom_contour(color = "white", alpha = 0.5) +
        ggplot2::theme_bw(base_size = 8) +
        ggplot2::scale_y_continuous(expand = c(0, 0), breaks = seq(1, max(gdat$x), 2)) +
        ggplot2::scale_x_continuous(expand = c(0, 0), breaks = seq(1, max(gdat$y), 2)) +
        ggplot2::theme(legend.position = "none", aspect.ratio = 0.3) +
        ggplot2::labs(y = paste(xnam, "Lag", sep = " "), x = paste(ynam, "Lag", sep = " "))

    if(tolower(palette) == "rainbow" | tolower(palette) == "default") {
        a <- a + ggplot2::scale_fill_gradientn(colours = grDevices::rainbow(100))

        b <- lattice::wireframe(z ~ y * x, data = gdat, aspect = c(61/87, 0.4),
                                scales = list(cex = 0.5, arrows = FALSE),
                                shade = TRUE, colorkey = FALSE,
                                par.settings = list(axis.line = list(col = 'transparent')),
                                xlab = list(label = paste(ynam, "Lag", sep = " "), cex = .8, rot = 20),
                                ylab = list(label = paste(xnam, "Lag", sep = " "), cex = .8, rot = -18),
                                zlab = list(label = NULL, cex.axis = 0.5))
    }
    else if(any(grepl("(colou?r([[:punct:]]|[[:space:]]?)blind)|cb|viridis", palette, ignore.case = T))) {
        a <- a + ggplot2::scale_fill_gradientn(colours = scales::viridis_pal(option = "viridis")(50))

        # Create the lattice plot
        b <- lattice::wireframe(z ~ y * x, data = gdat, aspect = c(61/87, 0.4),
                                scales = list(cex = 0.5, arrows = FALSE),
                                drape = TRUE, colorkey = FALSE,
                                par.settings = list(axis.line = list(col = 'transparent')),
                                xlab = list(label = paste(ynam, "Lag", sep = " "), cex = .8, rot = 20),
                                ylab = list(label = paste(xnam, "Lag", sep = " "), cex = .8, rot = -18),
                                zlab = list(label = NULL, cex.axis = 0.5),
                                col.regions = scales::viridis_pal(option = "viridis")(100))
    }
    else if(tolower(palette) %in% c("magma", "inferno", "cividis", "plasma")) {
        a <- a + ggplot2::scale_fill_gradientn(colours = scales::viridis_pal(option = palette)(50))

        # Create the lattice plot
        b <- lattice::wireframe(z ~ y * x, data = gdat, aspect = c(61/87, 0.4),
                                scales = list(cex = 0.5, arrows = FALSE),
                                drape = TRUE, colorkey = FALSE,
                                par.settings = list(axis.line = list(col = 'transparent')),
                                xlab = list(label = paste(ynam, "Lag", sep = " "), cex = .8, rot = 20),
                                ylab = list(label = paste(xnam, "Lag", sep = " "), cex = .8, rot = -18),
                                zlab = list(label = NULL, cex.axis = 0.5),
                                col.regions = scales::viridis_pal(option = palette)(100))
    }

    else if(palette %in% c("Spectral")) {
        a <- a + ggplot2::scale_fill_gradientn(colours = scales::brewer_pal(palette = palette)(11))

        # Create the lattice plot
        b <- lattice::wireframe(z ~ y * x, data = gdat, aspect = c(61/87, 0.4),
                                scales = list(cex = 0.5, arrows = FALSE),
                                drape = TRUE, colorkey = FALSE,
                                par.settings = list(axis.line = list(col = 'transparent')),
                                xlab = list(label = paste(ynam, "Lag", sep = " "), cex = .8, rot = 20),
                                ylab = list(label = paste(xnam, "Lag", sep = " "), cex = .8, rot = -18),
                                zlab = list(label = NULL, cex.axis = 0.5),
                                col.regions = scales::brewer_pal(palette = palette)(11))
    }
    else {
        stop("Invalid value for palette.")
    }

    # if(isTRUE(horizontal)) {
        output <- cowplot::plot_grid(b, a, nrow = 2, scale = c(2, 1))
    # }
    # else if(isFALSE(horizontal)) {
    #     output <- cowplot::plot_grid(b, a, ncol = 2, nrow = 1, scale = c(1, 1))
    # }
    # else {
    #     stop("horizontal must be either TRUE or FALSE")
    # }

    return(output)
}

#' Calculate the variogram data frame for a model
#'
#' @param model.obj An asreml model
#'
#' @return A data frame with the variogram for a model. The data frame contains the spatial coordinaties (typically row and column), the $gamma$ for that position and the number of points with the separation.
#' @keywords internal
#'
#'
#' @examples
#' \dontrun{
#' library(asreml)
#' oats <- asreml::oats
#' oats <- oats[order(oats$Row, oats$Column),]
#' model.asr <- asreml(yield ~ Nitrogen + Variety + Nitrogen:Variety,
#'                     random = ~ Blocks + Blocks:Wplots,
#'                     residual = ~ ar1(Row):ar1(Column),
#'                     data = oats)
#' vario_df(model.asr)
#' }
#'
vario_df <- function(model.obj, Row, Col) {
    # The 'z' value for the variogram is the residuals
    # Need to be able to pull out the x/y from the model object

    dims <- unlist(strsplit(names(model.obj$R.param[1]), ":"))
    Row <- as.numeric(model.obj$mf[[dims[1]]])
    Column <- as.numeric(model.obj$mf[[dims[2]]])
    Resid <- residuals(model.obj)

    nrows <- max(Row)
    ncols <- max(Column)

    vario <- expand.grid(Row = 0:(nrows-1), Column = 0:(ncols-1))

    # Ignore the 0, 0 case (gamma=0, counted row*cols times)
    gammas <- rep(0, nrows*ncols)
    nps <- rep(nrows*ncols, nrows*ncols)

    for (index in 2:nrow(vario)) {
        i <- vario[index, 'Row']
        j <- vario[index, 'Column']

        gamma <- 0
        np <- 0
        for (val_index in 1:nrows) {
            # val <- vals[val_index, ]

            # Deliberate double-counting so that offset handling is easy
            # (so e.g. we compute distance from (1,1)->(2,3), and then again
            # later from (2,3)->(1,1)).
            for (offset in unique(list(c(i, j), c(-i, j), c(i, -j), c(-i, -j)))) {
                row <- Row[val_index] + offset[1]
                col <- Column[val_index] + offset[2]

                if (0 < row && row <= nrows && 0 < col && col <= ncols) {
                    other <- which(Row == row & Column == col)
                    gamma <- gamma + (Resid[val_index]-Resid[other])^2
                    np <- np + 1
                }
            }
        }
        # Since we double-counted precisely, halve to get the correct answer.
        np <- np / 2
        gamma <- gamma / 2

        if (np > 0) {
            gamma <- gamma / (2*np)
        }

        gammas[index] <- gamma
        nps[index] <- np
    }
    vario <- cbind(vario, data.frame(gamma = gammas, np = nps))
    colnames(vario) <- c(dims, "gamma", "np")
    class(vario) <- c("variogram", "data.frame")
    return(vario)
}
