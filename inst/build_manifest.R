# inst/build_manifest.R
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

packages_to_df <- function(packages) {

    keep <- c("asr_ver", "r_ver", "os", "os_ver", "arm", "published", "slug", "url")

    if (is.null(packages)) {
        df <- data.frame(stringsAsFactors = FALSE)
    } else if (is.data.frame(packages)) {
        df <- packages
    } else if (is.list(packages)) {
        if (length(packages) == 0) {
            df <- data.frame(stringsAsFactors = FALSE)
        } else {
            df <- do.call(
                rbind,
                lapply(packages, function(x) as.data.frame(x, stringsAsFactors = FALSE))
            )
        }
    } else {
        stop("Unsupported packages type: ", paste(class(packages), collapse = ", "))
    }

    df <- as.data.frame(df, stringsAsFactors = FALSE)

    # Ensure consistent columns + order
    missing <- setdiff(keep, names(df))
    for (m in missing)
        df[[m]] <- NA
    df <- df[, keep, drop = FALSE]

    # Normalize types that might differ between read/write
    df$arm <- as.logical(df$arm)

    # Stable ordering for comparison
    df <- df[order(df$slug), , drop = FALSE]
    row.names(df) <- NULL

    df
}


# ---- Functions for updating links ------------------------------------

get_latest_asreml_downloads <- function(vt) {

    groups <- split(
        vt,
        list(vt$os, vt$arm, vt$r_ver),
        drop = TRUE
    )

    latest <- lapply(groups, function(x) {

        x <- x[order(
            numeric_version(x$asr_ver),
            x$`Date published`
        ), , drop = FALSE]

        tail(x, 1)
    })

    do.call(rbind, latest)
}


get_shortio_links <- function(api_key, domain_id) {

    url <- "https://api.short.io/api/links"

    response <- httr::GET(
        url,
        query = list(domain_id = as.numeric(domain_id)),
        httr::add_headers(Authorization = api_key),
        httr::content_type("application/octet-stream"),
        httr::accept("application/json")
    )

    if (httr::status_code(response) >= 300) {
        stop(
            "Short.io request failed: ",
            httr::content(response, "text", encoding = "UTF-8")
        )
    }

    dat <- httr::content(response, as = "parsed", simplifyVector = TRUE)

    as.data.frame(dat$links, stringsAsFactors = FALSE)
}


shortio_update_link <- function(
        api_key,
        link_id,
        domain_id,
        target_url,
        title = NULL,
        tags = NULL
) {

    body <- list(
        domain_id  = domain_id,
        originalURL = target_url
    )

    if (!is.null(title))
        body$title <- title

    if (!is.null(tags))
        body$tags <- tags

    res <- httr::POST(
        paste0("https://api.short.io/links/", link_id),
        httr::add_headers(Authorization = api_key),
        httr::content_type_json(),
        body = body,
        encode = "json"
    )

    httr::stop_for_status(res)

    invisible(TRUE)
}


shortio_create_link <- function(
        api_key,
        domain,
        path,
        target_url,
        title = NULL,
        tags = NULL
) {

    body <- list(
        domain      = domain,
        originalURL = target_url,
        path        = path
    )

    if (!is.null(title))
        body$title <- title

    if (!is.null(tags))
        body$tags <- tags

    res <- httr::POST(
        "https://api.short.io/links",
        httr::add_headers(Authorization = api_key),
        httr::content_type_json(),
        body = body,
        encode = "json"
    )

    if (httr::status_code(res) >= 300) {
        cat(httr::content(res, "text"), "\n")
        stop("Short.io create failed")
    }

    httr::content(res, as = "parsed", simplifyVector = TRUE)
}

shortio_set_tags <- function(api_key, link_id, domain_id, tags) {

    res <- httr::POST(
        paste0("https://api.short.io/links/", link_id),
        httr::add_headers(Authorization = api_key),
        httr::content_type_json(),
        body = list(
            domain_id = as.numeric(domain_id),
            tags = tags
        ),
        encode = "json"
    )

    httr::stop_for_status(res)

    invisible(TRUE)
}

sync_asreml_shortlinks <- function(
        vt,
        api_key,
        domain,
        domain_id,
        dry_run = Sys.getenv("CI") != "true"
) {

    expected <- build_expected_asreml_links(vt)

    current <- get_shortio_links(api_key, domain_id)[, c("id","path","originalURL","title")]

    merged <- merge(expected, current, by="path", all.x=TRUE)

    results <- vector("list", nrow(merged))

    for (i in seq_len(nrow(merged))) {

        row <- merged[i,]

        tags <- c(
            row$os,
            paste0(substr(row$r_ver,1,1),".",substr(row$r_ver,2,2))
        )

        action <- "skip"

        if (is.na(row$id)) {

            action <- "create"
            message("CREATE ", row$path)

            if (!dry_run) {

                created <- shortio_create_link(
                    api_key,
                    domain,
                    row$path,
                    row$target,
                    title = row$title,
                    tags = c(row$os, row$r_ver)
                )

                shortio_set_tags(
                    api_key,
                    created$id,
                    domain_id,
                    tags
                )
            }

        } else if (!identical(row$target, row$originalURL)) {

            action <- "update"
            message("UPDATE ", row$path)

            if (!dry_run)
                shortio_update_link(
                    api_key,
                    row$id,
                    domain_id,
                    row$target,
                    title = row$title,
                    tags = c(row$os, row$r_ver)
                )

        } else {

            message("SKIP   ", row$path)
        }

        results[[i]] <- data.frame(
            path   = row$path,
            action = action,
            tags   = paste(tags, collapse=","),
            stringsAsFactors = FALSE
        )
    }

    do.call(rbind, results)
}



build_expected_asreml_links <- function(vt) {

    latest <- get_latest_asreml_downloads(vt)

    r_pretty <- paste0(
        substr(latest$r_ver, 1, 1),
        ".",
        substr(latest$r_ver, 2, 2)
    )

    data.frame(
        path   = latest$os_ver,
        target = latest$Download_URL,
        title  = latest$os_ver,   # ← slug as title
        os     = latest$os,
        r_ver  = r_pretty,
        stringsAsFactors = FALSE
    )
}

# ---- Build manifest --------------------------------------------------

base_url <- "https://link.biometryhubwaite.com/"

message("Fetching version table from VSNi...")
vt <- get_version_table(vsni_url)

if (nrow(vt) == 0) stop("Version table is empty")

# ---- filter supported R versions ----
vt <- vt[numeric_version(vt$r_ver) >= numeric_version("40"), ]


message("Syncing shortlinks...")
sync_asreml_shortlinks(
    vt,
    api_key   = Sys.getenv("SHORTIO_API_KEY"),
    domain    = "link.biometryhubwaite.com",
    domain_id = as.numeric(Sys.getenv("SHORTIO_DOMAIN_ID")),
    dry_run   = Sys.getenv("CI") != "true"
)

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

entries <- entries[order(
    vapply(entries, `[[`, "", "slug")
)]

manifest <- list(
    schema_version = 1,
    updated  = format(Sys.Date(), "%Y-%m-%d"),
    packages = entries
)

out_path <- "inst/manifest.json"

old_manifest <- NULL

if (file.exists(out_path)) {
    old_manifest <- jsonlite::read_json(out_path, simplifyVector = TRUE)
}

manifest_changed <- TRUE

if (!is.null(old_manifest)) {

    old_schema <- old_manifest$schema_version
    new_schema <- manifest$schema_version

    old_pkgs <- packages_to_df(old_manifest$packages)
    new_pkgs <- packages_to_df(manifest$packages)

    # ignore timestamp; compare schema + canonicalized packages
    manifest_changed <- !(
        identical(as.integer(old_schema), as.integer(new_schema)) &&
            identical(old_pkgs, new_pkgs)
    )
}

if (manifest_changed) {

    message("Manifest content changed — writing file")

    jsonlite::write_json(
        manifest,
        out_path,
        pretty = TRUE,
        auto_unbox = TRUE
    )
    message("Manifest written to ", out_path, " with ", length(entries), " entries")

} else {

    message("No manifest changes (ignoring updated date)")
}
