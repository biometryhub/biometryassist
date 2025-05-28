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
