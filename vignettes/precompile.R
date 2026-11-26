# Precompile vignettes whose code needs ASReml-R, which CRAN does not have.
#
# The source lives in <name>.qmd.orig (excluded from the package build) and is
# knitted here into <name>.qmd, which contains only static output and figures,
# so the vignette builds on CRAN without running any asreml code.
#
# Run from the package root, with a licensed ASReml-R installed:
#   Rscript vignettes/precompile.R
# then commit the regenerated .qmd and vignettes/figures/.

withr::with_dir("vignettes", {
	# Start from an empty figures/ folder so figures from removed or renamed
	# chunks don't linger. figures/ is only used by precomputed vignettes.
	unlink("figures", recursive = TRUE)
	# dpi = 96 matches Quarto's HTML default. fig.cap = "" stops knitr writing
	# "plot of chunk <label>" as alt text, which Quarto would show as a caption.
	knitr::opts_chunk$set(fig.path = "figures/", dpi = 96, fig.cap = "")
	knitr::knit("variogram.qmd.orig", output = "variogram.qmd")
})
