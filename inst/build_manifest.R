# tools/build_manifest.R
# Scrapes VSNi download page and writes manifest.json
# Requires VSNI_URL environment variable to be set

vsni_url <- Sys.getenv("VSNI_URL")
if (nchar(vsni_url) == 0) stop("VSNI_URL environment variable not set")

# ---- Scraping (moved wholesale from package) -------------------------

parse_version_table_html <- function(table_node, header) {

    rows <- xml2::xml_find_all(table_node, ".//tr")

    header_cells <- xml2::xml_text(
        xml2::xml_find_all(rows[[1]], ".//th")
    )

    out <- lapply(rows[-1], function(r) {

        cells <- xml2::xml_find_all(r, ".//td")

        vals <- xml2::xml_text(cells)

        # find download link
        link_node <- xml2::xml_find_first(r, ".//a")

        href <- xml2::xml_attr(link_node, "href")

        vals <- c(vals, Download_URL = href)

        vals
    })

    df <- as.data.frame(do.call(rbind, out),
                        stringsAsFactors = FALSE)

    colnames(df)[seq_along(header_cells)] <- header_cells

    # ---- your existing metadata extraction ----

    df$r_ver <- paste0(
        stringi::stri_match_first_regex(
            header,
            "R version (\\d?)\\.(\\d?)"
        )[2:3],
        collapse = ""
    )

    parsed <- lapply(df$Download, parse_download_os,
                     r_ver = df$r_ver[1])

    df$os     <- vapply(parsed, `[[`, "", "os")
    df$os_ver <- vapply(parsed, `[[`, "", "os_ver")
    df$arm    <- vapply(parsed, `[[`, FALSE, "arm")

    df$asr_ver <- stringi::stri_match_first_regex(
        df$`File name`,
        "asreml-?_?(\\d\\.\\d?\\.\\d?\\.\\d*)"
    )[,2]

    df$`Date published` <- as.Date(df$`Date published`, tryFormats = c("%d %B %Y", "%Y-%m-%d", "%Y/%m/%d"))

    return(df)
}

parse_download_os <- function(desc, r_ver) {

    desc_lower <- tolower(desc)
    arm        <- grepl("arm64|aarch64", desc_lower)

    if (grepl("windows", desc_lower)) {
        return(list(
            os     = "win",
            os_ver = paste0("win-", r_ver),
            arm    = FALSE
        ))
    }

    if (grepl("mac", desc_lower)) {

        ver_match <- regmatches(desc_lower,
                                regexpr("(?<=macos )[0-9]+", desc_lower, perl = TRUE))
        ver <- if (length(ver_match) && nchar(ver_match)) ver_match else NULL

        os_ver <- paste0(
            "mac-",
            if (!is.null(ver)) paste0(ver, "-") else "",
            r_ver,
            if (arm) "-arm" else ""
        )

        return(list(os = "mac", os_ver = os_ver, arm = arm))
    }

    if (grepl("ubuntu", desc_lower)) {

        ver <- regmatches(desc_lower,
                          regexpr("(?<=ubuntu )[0-9]+", desc_lower, perl = TRUE))
        ver <- if (length(ver)) ver else "unknown"

        return(list(
            os     = "ubuntu",
            os_ver = paste0("ubuntu-", ver, "-", r_ver),
            arm    = FALSE
        ))
    }

    if (grepl("rocky", desc_lower)) {

        ver <- regmatches(desc_lower,
                          regexpr("(?<=rocky linux )[0-9]+", desc_lower, perl = TRUE))
        ver <- if (length(ver)) ver else "unknown"

        return(list(
            os     = "rocky",
            os_ver = paste0("rocky-", ver, "-", r_ver),
            arm    = FALSE
        ))
    }

    if (grepl("centos", desc_lower)) {

        ver <- regmatches(desc_lower,
                          regexpr("(?<=centos )[0-9]+", desc_lower, perl = TRUE))
        ver <- if (length(ver)) ver else "unknown"

        return(list(
            os     = "centos",
            os_ver = paste0("centos-", ver, "-", r_ver),
            arm    = FALSE
        ))
    }

    if (grepl("redhat|red hat|rhel", desc_lower)) {

        ver <- regmatches(desc_lower,
                          regexpr("[0-9]+", desc_lower, perl = TRUE))
        ver <- if (length(ver)) ver else "unknown"

        return(list(
            os     = "redhat",
            os_ver = paste0("redhat-", ver, "-", r_ver),
            arm    = FALSE
        ))
    }

    list(
        os     = "linux",
        os_ver = paste0("linux-", r_ver),
        arm    = FALSE
    )
}

get_version_table <- function(url) {
    tryCatch({
        res <- xml2::read_html(url)
        headers <- xml2::xml_text(xml2::xml_find_all(res, "//h3"))
        headers <- headers[grepl("^ASReml-?R? 4.*\\(All platforms\\)", headers)]
        if (length(headers) == 0) stop("No version info found at URL")
        tables <- xml2::xml_find_all(res, "//table")
        tables <- tables[grepl("macOS", xml2::xml_text(tables))]
        parsed <- Map(parse_version_table_html, tables, headers)
        do.call(rbind, parsed)
    }, error = function(e) stop("Scraping failed: ", e$message))
}

# ---- Build manifest --------------------------------------------------

base_url <- "https://link.biometryhubwaite.com/"

message("Fetching version table from VSNi...")
vt <- get_version_table(vsni_url)

if (nrow(vt) == 0) stop("Version table is empty")

message("Building manifest entries...")
entries <- lapply(seq_len(nrow(vt)), function(i) {
    row <- vt[i, ]
    slug <- row$os_ver
    list(
        asr_ver  = row$asr_ver,
        r_ver    = row$r_ver,
        os       = row$os,
        os_ver   = sub(paste0("^", row$os, "-"), "",
                       sub(paste0("-", row$r_ver, ".*$"), "", row$os_ver)),
        arm      = isTRUE(row$arm),
        published = format(row$`Date published`, "%Y-%m-%d"),
        slug     = slug,
        url      = paste0(base_url, slug)
    )
})

manifest <- list(
    updated  = format(Sys.Date(), "%Y-%m-%d"),
    packages = entries
)

out_path <- "inst/manifest.json"
jsonlite::write_json(manifest, out_path, pretty = TRUE, auto_unbox = TRUE)
message("Manifest written to ", out_path, " with ", length(entries), " entries")
