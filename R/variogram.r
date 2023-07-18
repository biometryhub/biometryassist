#' Variogram plots for spatial models.
#'
#' Produces variogram plots for checking spatial trends.
#'
#' @param model.obj An `asreml` model object.
#' @param row A row variable.
#' @param column A column variable.
#' @param horizontal Logical (default `TRUE`). The direction the plots are arranged. The default `TRUE` places the plots above and below, while `FALSE` will place them side by side.
#' @param palette A string specifying the colour scheme to use for plotting. The default value (`"default"`) is equivalent to `"rainbow"`. Colour blind friendly palettes can also be provided via options `"colo(u)r blind"` (both equivalent to `"viridis"`), `"magma"`, `"inferno"`, `"plasma"`, `"cividis"`, `"rocket"`, `"mako"` or `"turbo"`. The `"Spectral"` palette from [scales::brewer_pal()] is also possible.
#'
#'
#' @return A `ggplot2` object.
#'
#' @importFrom pracma interp2
#' @importFrom grDevices rainbow
#' @importFrom lattice wireframe
#' @importFrom cowplot plot_grid
#' @importFrom ggplot2 ggplot geom_tile coord_equal geom_contour scale_fill_gradientn theme_bw scale_x_continuous scale_y_continuous theme labs
#'
#' @references S. P. Kaluzny, S. C. Vega, T. P. Cardoso, A. A. Shelly, "S+SpatialStats: User’s Manual for Windows® and UNIX®" _Springer New York_, 2013, p. 68, https://books.google.com.au/books?id=iADkBwvario_pointsQBAJ.
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

    if(attr(model.obj$formulae$residual,"term.labels") == "units") {
        stop("Residual term must include spatial component.")
    }

    vario_points <- vario_df(model.obj, row, column)

    if("groups" %in% colnames(vario_points)) {
        groups <- unique(vario_points$groups)
        n_groups <- length(groups)
    }
    else {
        groups <- 1
        n_groups <- 1
        vario_points$groups <- 1
    }
    output <- list()

    for(i in seq_along(groups)) {
        points <- vario_points[vario_points$groups == groups[i],]
        orig_row <- TRUE
        orig_col <- TRUE

        if(missing(row) | is.na(row) | is.null(row)) {
            row <- names(points)[2]
            orig_row <- FALSE
        }
        if(missing(column) | is.na(column) | is.null(column)) {
            column <- names(points)[1]
            orig_col <- FALSE
        }
        row_vals <- unique(points[,2]) # x
        col_vals <- unique(points[,1]) # y


        z <- matrix(points$gamma, nrow = length(row_vals), byrow = TRUE)

        interp_rows <- seq(min(row_vals), max(row_vals), length = 40)
        interp_cols <- seq(min(col_vals), max(col_vals), length = 40)
        gdat <- expand.grid(x = interp_rows, y = interp_cols)

        pr <- pracma::interp2(x = col_vals, y = row_vals, Z = z, xp = gdat$y, yp = gdat$x)
        pr <- matrix(pr, nrow = length(interp_rows), byrow = F)
        gdat <- cbind(gdat, z = as.vector(pr))

        a <- ggplot2::ggplot(gdat, ggplot2::aes(x = y, y = x, z = z)) +
            ggplot2::geom_tile(alpha = 0.6, ggplot2::aes(fill = z)) +
            ggplot2::coord_equal() +
            ggplot2::geom_contour(color = "white", alpha = 0.5) +
            ggplot2::theme_bw(base_size = 8) +
            ggplot2::scale_y_continuous(expand = c(0, 0), breaks = seq(1, max(gdat$x), 2)) +
            ggplot2::scale_x_continuous(expand = c(0, 0), breaks = seq(1, max(gdat$y), 2)) +
            ggplot2::theme(legend.position = "none", aspect.ratio = 0.3) +
            ggplot2::labs(y = paste(row, "Lag", sep = " "), x = paste(column, "Lag", sep = " "))

        if(tolower(palette) == "rainbow" | tolower(palette) == "default") {
            a <- a + ggplot2::scale_fill_gradientn(colours = grDevices::rainbow(100))

            b <- lattice::wireframe(z ~ y * x, data = gdat, aspect = c(61/87, 0.4),
                                    scales = list(cex = 0.5, arrows = FALSE),
                                    drape = TRUE, colorkey = FALSE,
                                    par.settings = list(axis.line = list(col = 'transparent')),
                                    xlab = list(label = paste(column, "Lag", sep = " "), cex = .8, rot = 20),
                                    ylab = list(label = paste(row, "Lag", sep = " "), cex = .8, rot = -18),
                                    zlab = list(label = NULL, cex.axis = 0.5),
                                    col.regions = grDevices::rainbow(100))
        }
        else if(any(grepl("(colou?r([[:punct:]]|[[:space:]]?)blind)|cb|viridis", palette, ignore.case = T))) {
            a <- a + ggplot2::scale_fill_gradientn(colours = scales::viridis_pal(option = "viridis")(50))

            # Create the lattice plot
            b <- lattice::wireframe(z ~ y * x, data = gdat, aspect = c(61/87, 0.4),
                                    scales = list(cex = 0.5, arrows = FALSE),
                                    drape = TRUE, colorkey = FALSE,
                                    par.settings = list(axis.line = list(col = 'transparent')),
                                    xlab = list(label = paste(column, "Lag", sep = " "), cex = .8, rot = 20),
                                    ylab = list(label = paste(row, "Lag", sep = " "), cex = .8, rot = -18),
                                    zlab = list(label = NULL, cex.axis = 0.5),
                                    col.regions = scales::viridis_pal(option = "viridis")(100))
        }
        else if(tolower(trimws(palette)) %in% c("magma", "inferno", "cividis", "plasma", "rocket", "mako", "turbo")) {
            a <- a + ggplot2::scale_fill_gradientn(colours = scales::viridis_pal(option = palette)(50))

            # Create the lattice plot
            b <- lattice::wireframe(z ~ y * x, data = gdat, aspect = c(61/87, 0.4),
                                    scales = list(cex = 0.5, arrows = FALSE),
                                    drape = TRUE, colorkey = FALSE,
                                    par.settings = list(axis.line = list(col = 'transparent')),
                                    xlab = list(label = paste(column, "Lag", sep = " "), cex = .8, rot = 20),
                                    ylab = list(label = paste(row, "Lag", sep = " "), cex = .8, rot = -18),
                                    zlab = list(label = NULL, cex.axis = 0.5),
                                    col.regions = scales::viridis_pal(option = palette)(100))
        }

        else if(tolower(trimws(palette)) %in% c("spectral")) {
            a <- a + ggplot2::scale_fill_gradientn(colours = scales::brewer_pal(palette = palette)(11))

            # Create the lattice plot
            b <- lattice::wireframe(z ~ y * x, data = gdat, aspect = c(61/87, 0.4),
                                    scales = list(cex = 0.5, arrows = FALSE),
                                    drape = TRUE, colorkey = FALSE,
                                    par.settings = list(axis.line = list(col = 'transparent')),
                                    xlab = list(label = paste(column, "Lag", sep = " "), cex = .8, rot = 20),
                                    ylab = list(label = paste(row, "Lag", sep = " "), cex = .8, rot = -18),
                                    zlab = list(label = NULL, cex.axis = 0.5),
                                    col.regions = scales::brewer_pal(palette = palette)(11))
        }
        else {
            stop("Invalid value for palette.")
        }
        output[[i]] <- cowplot::plot_grid(b, a, nrow = 2, scale = c(2, 1))
        if(!orig_row) {
            row <- NA
        }
        if(!orig_col) {
            column <- NA
        }
    }

    if(n_groups > 1) {
        names(output) <- groups
        for (j in seq_along(output)) {
            title <- cowplot::ggdraw() + cowplot::draw_label(groups[j],
                                           fontface = 'bold',
                                           x = 0.1,
                                           hjust = 0)
            output[[j]] <- cowplot::plot_grid(title, output[[j]], nrow = 2, rel_heights = c(0.1, 1))
        }
        return(output)
    }
    else {
        return(output[[1]])
    }
}

#' Calculate the variogram data frame for a model
#'
#' @param model.obj An asreml model
#'
#' @return A data frame with the variogram for a model. The data frame contains the spatial coordinates (typically row and column), the `gamma` for that position and the number of points with the separation.
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
vario_df <- function(model.obj, Row = NA, Column = NA) {
    # The 'z' value for the variogram is the residuals
    # Need to be able to pull out the x/y from the model object

    if(length(names(model.obj$R.param)) > 1) {
        # More than one residual component (e.g. from dsum())
        # Do we need to check the names match in all the levels? Probably...
        if(!is.null(attr(model.obj$formulae$residual,"specials")$dsum)) {
            dsum_col <- as.character(model.obj$formulae$residual[[2]][[2]][[2]][[3]])
        }
        levs <- names(model.obj$R.param)
        dims <- setdiff(names(model.obj$R.param[[1]]), "variance")

    }
    else {
        dims <- unlist(strsplit(names(model.obj$R.param[1]), ":"))
        levs <- 1
    }

    output <- data.frame()

    for(level in seq_along(levs)) {

        model_frame <- model.obj$mf
        if(length(levs) > 1) {
            model_frame <- subset(model_frame, model_frame[,dsum_col]==levs[level])
        }

        if(missing(Row) || is.na(Row) || is.null(Row)) {
            Row <- as.numeric(model_frame[[dims[1]]])
        }
        if(missing(Column) || is.na(Column) || is.null(Column)) {
            Column <- as.numeric(model_frame[[dims[2]]])
        }

        nrows <- max(Row)
        ncols <- max(Column)

        Resid <- residuals(model.obj)[model_frame$units]

        vario <- expand.grid(Row = 0:(nrows-1), Column = 0:(ncols-1))

        # Ignore the 0, 0 case (gamma=0, counted row*cols times)
        gammas <- rep(0, nrows*ncols)
        nps <- rep(nrows*ncols, nrows*ncols)

        for (index in 2:nrow(vario)) {
            i <- vario[index, 'Row']
            j <- vario[index, 'Column']

            gamma <- 0
            np <- 0
            for (val_index in 1:nrow(vario)) {

                # Deliberate double-counting so that offset handling is easy
                # (so e.g. we compute distance from (1,1)->(2,3), and then again
                # later from (2,3)->(1,1)).
                for (offset in unique(list(c(i, j), c(-i, j), c(i, -j), c(-i, -j)))) {
                    row <- Row[val_index] + offset[1]
                    col <- Column[val_index] + offset[2]

                    if (0 < row && row <= nrows && 0 < col && col <= ncols && !is.na(Resid[val_index])) {
                        other <- Resid[Row == row & Column == col]

                        if (!is.na(other)) {
                            gamma <- gamma + (Resid[val_index] - other)^2
                            np <- np + 1
                        }
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
        nps[1] <- nps[1]-sum(is.na(Resid))
        vario <- cbind(vario, data.frame(gamma = gammas, np = nps, groups = levs[level]))
        output <- rbind(output, vario)
        Row <- NULL
        Column <- NULL
    }
    colnames(output) <- c(dims, "gamma", "np", "groups")
    class(output) <- c("variogram", "data.frame")

    if(length(levs)==1 && levs[1]==1) {
        output <- subset(output, select = -groups)
    }

    return(output)
}
