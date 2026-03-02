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

variogram <- function(model.obj,
                      row = NA,
                      column = NA,
                      third = NA,
                      depth_only = FALSE,
                      palette = "rainbow") {

    if(!(inherits(model.obj, "asreml"))) {
        stop("model.obj must be an asreml model object", call. = FALSE)
    }

    vario_points <- vario_df(model.obj,
                             row = row,
                             column = column,
                             third = third,
                             depth_only = depth_only)

    col_regions <- setup_colour_palette(palette, n = 100)

    if(depth_only) {

        p <- ggplot2::ggplot(vario_points,
                             ggplot2::aes(x = lag,
                                          y = gamma)) +
            ggplot2::geom_line() +
            ggplot2::geom_point() +
            ggplot2::theme_bw(base_size = 10) +
            ggplot2::labs(
                x = paste(attr(vario_points,"third_name"), "Lag"),
                y = expression(gamma)
            )

        class(p) <- c("variogram_plot", class(p))
        return(p)
    }

    # ---- 2D case ----

    row_vals <- unique(vario_points$row_lag)
    col_vals <- unique(vario_points$col_lag)

    p <- ggplot2::ggplot(vario_points,
                         ggplot2::aes(x = col_lag,
                                      y = row_lag,
                                      fill = gamma)) +
        ggplot2::geom_tile() +
        ggplot2::coord_equal() +
        ggplot2::scale_fill_gradientn(colours = col_regions) +
        ggplot2::theme_bw(base_size = 8) +
        ggplot2::labs(
            x = paste(attr(vario_points,"col_name"), "Lag"),
            y = paste(attr(vario_points,"row_name"), "Lag"),
            fill = expression(gamma)
        )

    if(!is.null(attr(vario_points,"third_name"))) {
        p <- p + ggplot2::facet_wrap(~ third_level)
    }

    class(p) <- c("variogram_plot", class(p))
    return(p)
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
vario_df <- function(model.obj,
                     row = NA,
                     column = NA,
                     third = NA,
                     depth_only = FALSE) {

    model_frame <- model.obj$mf
    Resid <- residuals(model.obj)[model_frame$units]

    dims <- unlist(strsplit(names(model.obj$R.param[1]), ":"))

    coord_from_arg <- function(arg, arg_name, default_col) {

        if(missing(arg) || is.null(arg) ||
           (length(arg)==1 && is.na(arg))) {
            arg <- model_frame[[default_col]]
        }

        if(is.character(arg) && length(arg)==1) {
            arg <- model_frame[[arg]]
        }

        arg_int <- as.integer(as.numeric(arg))
        if(min(arg_int, na.rm=TRUE)==0)
            arg_int <- arg_int + 1L

        arg_int
    }

    row_vec <- coord_from_arg(row,"row",dims[1])
    col_vec <- coord_from_arg(column,"column",dims[2])

    third_vec <- NULL
    third_name <- NULL

    if(!missing(third) && !is.null(third) &&
       !(length(third)==1 && is.na(third))) {

        third_vec <- coord_from_arg(third,"third",dims[3])
        third_name <- if(is.character(third)) third else dims[3]
    }

    # -------------------------
    # DEPTH-ONLY VARIOGRAM
    # -------------------------

    if(depth_only) {

        if(is.null(third_vec))
            stop("depth_only = TRUE requires a third axis.")

        max_lag <- max(third_vec, na.rm=TRUE) - 1
        lags <- 0:max_lag

        gammas <- numeric(length(lags))
        nps <- numeric(length(lags))

        for(i in seq_along(lags)) {

            h <- lags[i]

            gamma_total <- 0
            n_total <- 0

            for(r in unique(row_vec)) {
                for(c in unique(col_vec)) {

                    idx <- which(row_vec==r & col_vec==c)

                    if(length(idx) < 2) next

                    d_vals <- third_vec[idx]
                    res_vals <- Resid[idx]

                    for(j in seq_along(idx)) {

                        match_idx <- which(d_vals ==
                                               d_vals[j] + h)

                        if(length(match_idx)==1) {

                            diff <- res_vals[j] -
                                res_vals[match_idx]

                            gamma_total <- gamma_total +
                                diff^2

                            n_total <- n_total + 1
                        }
                    }
                }
            }

            if(n_total > 0) {
                gammas[i] <- gamma_total /
                    (2*n_total)
            } else {
                gammas[i] <- NA
            }

            nps[i] <- n_total
        }

        df <- data.frame(lag = lags,
                         gamma = gammas,
                         np = nps)

        attr(df,"third_name") <- third_name
        class(df) <- c("variogram","data.frame")

        return(df)
    }

    # -------------------------
    # 2D VARIOGRAM
    # -------------------------

    nrows <- max(row_vec, na.rm=TRUE)
    ncols <- max(col_vec, na.rm=TRUE)

    if(is.null(third_vec)) {

        third_levels <- 1
        third_vec <- rep(1,length(row_vec))

    } else {

        third_levels <- sort(unique(third_vec))
    }

    output <- data.frame()

    for(level in third_levels) {

        idx <- which(third_vec==level)

        resid_matrix <- matrix(NA,
                               nrow=nrows,
                               ncol=ncols)

        for(k in idx) {
            resid_matrix[row_vec[k],
                         col_vec[k]] <- Resid[k]
        }

        vario <- expand.grid(row_lag = 0:(nrows-1),
                             col_lag = 0:(ncols-1))

        gammas <- numeric(nrow(vario))
        nps <- numeric(nrow(vario))

        for(i in 2:nrow(vario)) {

            dr <- vario$row_lag[i]
            dc <- vario$col_lag[i]

            gamma_total <- 0
            n_total <- 0

            for(r in 1:(nrows-dr)) {
                for(c in 1:(ncols-dc)) {

                    v1 <- resid_matrix[r,c]
                    v2 <- resid_matrix[r+dr,
                                       c+dc]

                    if(!is.na(v1) &&
                       !is.na(v2)) {

                        gamma_total <- gamma_total +
                            (v1-v2)^2

                        n_total <- n_total + 1
                    }
                }
            }

            if(n_total > 0)
                gammas[i] <- gamma_total /
                (2*n_total)
            else
                gammas[i] <- NA

            nps[i] <- n_total
        }

        vario$gamma <- gammas
        vario$np <- nps
        vario$third_level <- level

        output <- rbind(output,vario)
    }

    attr(output,"row_name") <- dims[1]
    attr(output,"col_name") <- dims[2]

    if(!is.null(third_name))
        attr(output,"third_name") <- third_name

    class(output) <- c("variogram","data.frame")

    return(output)
}


