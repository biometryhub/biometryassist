equivalent_ggplot2 <- function(x, y) {
    # Create temporary files that will be automatically cleaned up when the function exits
    # Determine file extension based on svglite availability
    fileext <- ifelse(rlang::is_installed("svglite"), ".svg", ".png")
    tmp1 <- withr::local_tempfile(fileext = fileext)
    tmp2 <- withr::local_tempfile(fileext = fileext)

    # Save the ggplot2 objects to the temporary SVG files
    suppressMessages(ggplot2::ggsave(tmp1, plot = x))
    suppressMessages(ggplot2::ggsave(tmp2, plot = y))

    # Compare the MD5 checksums of the two files
    tools::md5sum(tmp1) == tools::md5sum(tmp2)
}

logit <- function (p, percents = range.p[2] > 1, adjust)
{
    range.p <- range(p, na.rm = TRUE)
    if(percents) {
        if(range.p[1] < 0 || range.p[1] > 100)
            stop("p must be in the range 0 to 100")
        p <- p/100
        range.p <- range.p/100
    }
    else if(range.p[1] < 0 || range.p[1] > 1)
        stop("p must be in the range 0 to 1")
    a <- if(missing(adjust)) {
        if(isTRUE(all.equal(range.p[1], 0)) || isTRUE(all.equal(range.p[2],
                                                                1)))
            0.025
        else 0
    }
    else adjust
    if(missing(adjust) && a != 0)
        warning(paste("proportions remapped to (", a, ", ",
                      1 - a, ")", sep = ""))
    a <- 1 - 2 * a
    log((0.5 + a * (p - 0.5))/(1 - (0.5 + a * (p - 0.5))))
}


# Helper function for ggplot2 version variants in visual tests
ggplot2_variant <- function() {
  if (packageVersion("ggplot2") <= "3.5.2") {
    "ggplot2-old"
  } else {
    "ggplot2-new"
  }
}