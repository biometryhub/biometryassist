# Pre-compute the vignettes.
#
# The executable sources live in `vignettes/*.qmd.orig` and are knitted here
# into plain `vignettes/*.qmd` with the results embedded, so what ships has no R
# chunks and `R CMD build`/`check` only do a markdown-to-HTML pass.
#
# Run from the package root after changing any `.qmd.orig`, then commit the
# regenerated `.qmd`, `vignettes/figures/` and `vignette-hashes.txt`:
#
#     Rscript tools/precompute-vignettes.R

pkgload::load_all(quiet = TRUE)

`%||%` <- function(x, y) if (is.null(x)) y else x

vig_dir <- "vignettes"
fig_dir <- "figures"
orig <- list.files(vig_dir, pattern = "\\.qmd\\.orig$", full.names = FALSE)
if (!length(orig)) {a
  stop("No .qmd.orig sources found in vignettes/")
}

# Must come before any `knit_hooks$set()` below: `knit()` installs the markdown
# hooks only while the hooks are still untouched, so customising one first left
# every chunk unfenced and joined onto the next.
knitr::render_markdown()

# Quarto marks a figure cross-referenceable with `#| label: fig-*`. knitr would
# collapse the chunk to a bare `![](path)` and the label - and any `@fig-`
# reference to it - would be lost, so emit Quarto's figure syntax instead.
knitr::knit_hooks$set(plot = function(x, options) {
  cap <- options$fig.cap %||% ""
  label <- options$label %||% ""
  attrs <- character(0)
  if (grepl("^(fig|tbl)-", label)) {
    attrs <- c(attrs, paste0("#", label))
  }
  if (!is.null(options$fig.align) && nzchar(options$fig.align)) {
    attrs <- c(attrs, sprintf('fig-align="%s"', options$fig.align))
  }
  suffix <- if (length(attrs)) {
    sprintf("{%s}", paste(attrs, collapse = " "))
  } else {
    ""
  }
  return(sprintf("\n![%s](%s)%s\n", cap, x, suffix))
})

withr::with_dir(vig_dir, {
  dir.create(fig_dir, showWarnings = FALSE)
  # knitr defaults to pdf outside an HTML context; these vignettes render to
  # HTML, where a pdf figure will not display
  # `cache: true` chunks would otherwise write `vignettes/cache/`, which ends up
  # in the tarball; keep it in tools/, which is not shipped
  knitr::opts_chunk$set(
    fig.path = paste0(fig_dir, "/"),
    cache.path = "../tools/vignette-cache/",
    dev = "png",
    dpi = 96
  )
  
  for (f in orig) {
    out <- sub("\\.orig$", "", f)
    message("Rendering ", f, " -> ", out)
    t0 <- Sys.time()
    knitr::knit(f, output = out, quiet = TRUE)
    message(sprintf(
      "  %.1fs",
      as.numeric(difftime(Sys.time(), t0, units = "secs"))
    ))
  }
})

# Record what each `.qmd` was generated from, so staleness is detectable. Kept
# in tools/ rather than vignettes/ so it does not ship in the tarball. CR
# bytes are stripped so a CRLF working tree hashes the same as an LF checkout;
# `check-vignettes-current.R` must strip them the same way.
hashes <- vapply(
  file.path(vig_dir, orig),
  function(f) {
    bytes <- readBin(f, "raw", file.size(f))
    tmp <- tempfile()
    on.exit(unlink(tmp), add = TRUE)
    writeBin(bytes[bytes != as.raw(13L)], tmp)
    return(unname(tools::md5sum(tmp)[[1]]))
  },
  character(1)
)
writeLines(
  sprintf("%s  %s", hashes, orig),
  file.path("tools", "vignette-hashes.txt")
)

message(
  "\nDone. Commit the regenerated .qmd files, ",
  vig_dir,
  "/",
  fig_dir,
  "/ and tools/vignette-hashes.txt"
)
