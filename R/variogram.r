#' Display variogram plots for spatial models
#'
#' Produces variogram plots for checking spatial trends.
#'
#' @param model.obj An `asreml` model object.
#' @param row A row variable.
#' @param column A column variable.
#' @param horizontal Logical (default `TRUE`). The direction the plots are arranged. The default `TRUE` places the plots above and below, while `FALSE` will place them side by side.
#' @param palette A string specifying the colour scheme to use for plotting. The default value (`"default"`) is equivalent to `"rainbow"`. Colour blind friendly palettes can also be provided via options `"colo(u)r blind"` (both equivalent to `"viridis"`), `"magma"`, `"inferno"`, `"plasma"`, `"cividis"`, `"rocket"`, `"mako"` or `"turbo"`. The `"Spectral"` palette from [scales::brewer_pal()] is also possible.
#' @param onepage Logical (default FALSE). If TRUE and there are multiple groups,
#'   combines up to 6 plots onto a single page using a grid layout.
#'
#' @returns A `ggplot2` object.
#'
#' @importFrom pracma interp2
#' @importFrom grDevices rainbow
#' @importFrom lattice wireframe
#' @importFrom ggplot2 ggplot geom_tile coord_equal geom_contour scale_fill_gradientn theme_bw scale_x_continuous scale_y_continuous theme labs
#' @importFrom patchwork wrap_elements wrap_plots plot_layout
#' @importFrom grid grid.grabExpr
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

variogram <- function(model.obj, row = NA, column = NA, horizontal = TRUE,
palette = "rainbow", onepage = FALSE) {

    if(!(inherits(model.obj, "asreml"))) {
        stop("model.obj must be an asreml model object", call. = FALSE)
    }

    if(attr(model.obj$formulae$residual,"term.labels") == "units") {
        stop("Residual term must include spatial component.", call. = FALSE)
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
            ggplot2::geom_contour(colour = "white", alpha = 0.5) +
            ggplot2::theme_bw(base_size = 8) +
            ggplot2::scale_y_continuous(expand = c(0, 0), breaks = seq(1, max(gdat$x), 2)) +
            ggplot2::scale_x_continuous(expand = c(0, 0), breaks = seq(1, max(gdat$y), 2)) +
            ggplot2::theme(legend.position = "none", aspect.ratio = 0.3) +
            ggplot2::labs(y = paste(row, "Lag", sep = " "), x = paste(column, "Lag", sep = " "))

        # First adjust the lattice spacing
        oldpar <- lattice::trellis.par.get()
        on.exit(lattice::trellis.par.set(oldpar), add = TRUE)

        lattice::trellis.par.set(
            layout.heights = list(
                top.padding = 0,
                main.key.padding = 0,
                key.axis.padding = 0,
                axis.xlab.padding = 0,
                xlab.key.padding = 0,
                key.sub.padding = 0,
                bottom.padding = 0
            ),
            layout.widths = list(
                left.padding = 0,
                right.padding = 0,
                axis.key.padding = 0,
                key.right = 0,
                key.left = 0
            )
        )

        col_regions <- setup_colour_palette(palette, n = 100)

        # Create the lattice plot
        b <- lattice::wireframe(
            z ~ y * x,
            data = gdat,
            aspect = c(61/87, 0.45),
            scales = list(cex = 0.5, arrows = FALSE),
            drape = TRUE,
            colorkey = FALSE,
            zlim = range(gdat$z, finite = TRUE),
            par.settings = list(axis.line = list(col = "transparent")),
            xlab = list(label = paste(column, "Lag"), cex = .8, rot = 20),
            ylab = list(label = paste(row, "Lag"), cex = .8, rot = -30),
            zlab = list(label = NULL, cex.axis = 0.5), 
            col.regions = col_regions
        )


        # b <- lattice:::update.trellis(
        #     base_wireframe,
            
        # )

        a <- a + ggplot2::scale_fill_gradientn(colours = col_regions)

        lattice_grob <- grid::grid.grabExpr(
            print(b, newpage = FALSE)
        )

        lg <- patchwork::wrap_elements(full = lattice_grob)

        pw <- (lg / a) +
            patchwork::plot_layout(heights = c(2, 1)) &
            ggplot2::theme(plot.margin = grid::unit(c(0, 0, 0, 0), "pt"))

        output[[i]] <- pw
        class(output[[i]]) <- c("variogram_plot", class(output[[i]]))

        if(!orig_row) {
            row <- NA
        }
        if(!orig_col) {
            column <- NA
        }
    }

    if(n_groups > 1) {

        # Add titles to all plots
        titled_plots <- list()
        for(j in seq_along(output)) {
            title_grob <- grid::textGrob(groups[j], gp = grid::gpar(fontface = 'bold'),
                                         hjust = 0, x = 0.1)
                title <- patchwork::wrap_elements(full = title_grob)
                titled_plots[[j]] <- patchwork::wrap_plots(
                    title,
                    output[[j]],
                    ncol = 1,
                    heights = c(1, 20)
                )
            class(titled_plots[[j]]) <- c("variogram_plot", class(titled_plots[[j]]))
        }
        names(titled_plots) <- groups

        if(onepage) {
            # Calculate number of pages needed
            n_pages <- ceiling(n_groups/6)
            pages <- vector("list", n_pages)

            for(page in 1:n_pages) {
                # Get index range for current page
                start_idx <- (page-1)*6 + 1
                end_idx <- min(page*6, n_groups)

                # Calculate grid dimensions for current page
                n_plots_on_page <- end_idx - start_idx + 1
                n_cols <- min(3, n_plots_on_page)
                n_rows <- ceiling(n_plots_on_page/3)

                # Create combined plot for current page
                    pages[[page]] <- patchwork::wrap_plots(
                        titled_plots[start_idx:end_idx],
                        ncol = n_cols,
                        nrow = n_rows
                    )
                class(pages[[page]]) <- c("variogram_plot", class(pages[[page]]))
            }

            return(pages)
        }
        else {
            return(titled_plots)
        }
    } else {
        return(output[[1]])
    }
}

#' Calculate the variogram data frame for a model
#'
#' @param model.obj An asreml model
#'
#' @returns A data frame with the variogram for a model. The data frame contains the spatial coordinates (typically row and column), the `gamma` for that position and the number of points with the separation.
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

    if(length(names(model.obj$R.param)) > 1) {
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

        # Create a matrix of residuals indexed by Row and Column
        resid_matrix <- matrix(NA, nrow = nrows, ncol = ncols)
        for(k in seq_along(Row)) {
            if(!is.na(Resid[k])) {
                resid_matrix[Row[k], Column[k]] <- Resid[k]
            }
        }

        # Generate all lag combinations
        vario <- expand.grid(Row = 0:(nrows-1), Column = 0:(ncols-1))
        n_lags <- nrow(vario)

        # Pre-allocate results
        gammas <- numeric(n_lags)
        nps <- numeric(n_lags)

        # Vectorized computation for all lags
        for(index in 2:n_lags) {
            row_lag <- vario[index, 'Row']
            col_lag <- vario[index, 'Column']

            # Calculate for all four symmetric offsets and combine
            gamma_total <- 0
            n_total <- 0

            # Offset combinations
            offset_list <- list(
                c(row_lag, col_lag),
                c(-row_lag, col_lag),
                c(row_lag, -col_lag),
                c(-row_lag, -col_lag)
            )

            # Remove duplicates (e.g., when row_lag or col_lag is 0)
            offset_list <- unique(offset_list)

            for(offset in offset_list) {
                dr <- offset[1]
                dc <- offset[2]

                # Determine valid row and column ranges
                if(dr >= 0) {
                    row_from <- 1:(nrows - dr)
                    row_to <- (1 + dr):nrows
                } else {
                    row_from <- (1 - dr):nrows
                    row_to <- 1:(nrows + dr)
                }

                if(dc >= 0) {
                    col_from <- 1:(ncols - dc)
                    col_to <- (1 + dc):ncols
                } else {
                    col_from <- (1 - dc):ncols
                    col_to <- 1:(ncols + dc)
                }

                # Extract sub-matrices
                mat_from <- resid_matrix[row_from, col_from, drop = FALSE]
                mat_to <- resid_matrix[row_to, col_to, drop = FALSE]

                # Compute squared differences where both values exist
                valid_pairs <- !is.na(mat_from) & !is.na(mat_to)
                sq_diff <- (mat_from - mat_to)^2

                # Sum the valid squared differences
                gamma_total <- gamma_total + sum(sq_diff[valid_pairs], na.rm = TRUE)
                n_total <- n_total + sum(valid_pairs)
            }

            # Account for double counting
            n_total <- n_total / 2
            gamma_total <- gamma_total / 2

            # Store results
            if(n_total > 0) {
                gammas[index] <- gamma_total / (2 * n_total)
            } else {
                gammas[index] <- 0
            }
            nps[index] <- n_total
        }

        # Handle the (0,0) case
        nps[1] <- nrows * ncols - sum(is.na(Resid))
        gammas[1] <- 0

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


#' #' @export
#' print.variogram_plot <- function(x, ...) {
#'     if (inherits(x, "patchwork")) {
#'         print(x)
#'     } else {
#'         grid::grid.newpage()
#'         grid::grid.draw(x)
#'     }
#'     invisible(x)
#' }
