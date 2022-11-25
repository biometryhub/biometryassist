#' Field layout
#'
#' Easily create plots of field layouts.
#'
#' @param data A data frame with the field values.
#' @param row_var
#' @param col_var
#' @param other_var
#'
#' @return A ggplot2 plot
#' @export
#'
#' @examples
field_layout <- function(data, row_var, col_var, other_var){

    other_var_fac <- is.factor(data[[other_var]])

    if(other_var_fac){
        tt <- as.data.frame(tapply(as.numeric(data[[other_var]]), list(data[[row_var]], data[[col_var]]),
                                   mean, na.rm = TRUE))
        newtt <- expand.grid(row = 1:length(rownames(tt)), col = 1:length(names(tt)))
        newtt$mean_dat = factor(unlist(tt))
    }

    else {
        tt <- as.data.frame(tapply(as.numeric(data[[other_var]]), list(data[[row_var]], data[[col_var]]),
                                   mean, na.rm = TRUE))
        newtt <- expand.grid(row = 1:length(rownames(tt)), col = 1:length(names(tt)))
        newtt$mean_dat = unlist(tt)
    }

    output <- ggplot(data = newtt, aes(y = row, x = col, fill = mean_dat)) +
        geom_tile(colour = "black") +
        theme_bw() +
        labs(x = col_var, y = row_var, fill = other_var) +
        ggplot2::scale_x_continuous(expand = c(0, 0), breaks = seq(1, max(newtt$col), 1)) +
        ggplot2::scale_y_continuous(expand = c(0, 0), trans = scales::reverse_trans(), breaks = seq(1, max(newtt$row), 1))

    return(output)
}


