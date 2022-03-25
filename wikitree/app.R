# IMPORTS


library(dplyr)
library(rvest)
library(shiny)


# CONSTANTS


RMGET_LIST <- NULL
PARSE_LIST <- NULL

PATTERN_TAXON <- "^Domain|Kingdom|Phylum|Class|Order|Family|Genus|Species"

FORMAT_GRAPH <- gsub("^\\s+", "", "
strict digraph {
layout=%s\noverlap=%s\nrankdir=%s
pad=0
node [shape=record style=filled fillcolor=white color=none fillcolor=skyblue]
%s
%s
}\n")

OPTIONS_LAYOUT <- c("dot", "neato", "twopi", "fdp")
OPTIONS_OVERLAP <- c("true", "false", "scale", "scalex", "scaley", "scalexy", "ortho")
OPTIONS_RANKDIR <- c("LR", "BT", "RL", "TB")


# FUNCTIONS


join <- function(..., sep = "", collapse = "") {
    paste(..., sep = sep, collapse = collapse)
}

slicer <- function(vec, start=NULL, end=NULL) {
    if (is.null(start)) { start <- 1 }
    if (is.null(end)) { end <- length(vec) }
    while (start < 0) { start <- start + length(vec) }
    while (end < 0) { end <- end + length(vec) }
    vec[start:end]
}


# SCRAPING


rmget <- function(urls, dname) {
    # make the folder if it does not exist
    if (!dir.exists(dname)) {
        dir.create(dname, recursive = TRUE)
    }

    # format filepaths from input URLs
    fnames <- gsub("[^0-9A-z]", "", urls)
    fpaths <- sprintf("%s%s", file.path(dname, fnames), ".html")

    # filter URLs that don't have filepaths yet
    df <- data.frame(url = urls, fpath = fpaths)
    df_save <- df[!file.exists(fpaths), ]

    # write HTML to file
    if (nrow(df_save) != 0) {
        resps_save <- crul::Async$new(df_save$url)$get()
        for (i in 1:length(resps_save)) {
            content <- resps_save[[i]]$content
            fpath <- df_save$fpath[i]
            writeBin(content, fpath)
            RMGET_LIST[[fpath]] <<- rvest::read_html(content)
        }
    }

    # load from memory, or from file
    lapply(df$fpath, function(fpath) {
        if (fpath %in% names(RMGET_LIST)) {
            html <- RMGET_LIST[[fpath]]
        } else {
            html <- rvest::read_html(fpath)
            RMGET_LIST[[fpath]] <<- html
        }
    })
}


# PARSING


parse_h1 <- function(html) {
    html %>%
        html_node("h1") %>%
        html_text2()
}

parse_biota <- function(html) {
    html %>%
        html_node("table.biota")
}

parse_biota_th <- function(biota) {
    biota %>%
        html_node("th") %>%
        html_text2() %>%
        gsub("\\n.+$", "", .)
}

parse_biota_img <- function(biota) {
    biota %>%
        html_node("img") %>%
        html_attr("src") %>%
        gsub("^//", "https://", .) %>%
        gsub("^/", "https://en.wikipedia.org/", .)
}

parse_biota_td <- function(biota) {
    mat <- biota %>%
        html_nodes("tr") %>%
        lapply(function(x) {
            td <- html_nodes(x, "td")
            if (length(td) != 2) {
                return()
            }
            text <- html_text2(td)
            if (!endsWith(text[1], ":")) {
                return()
            }
            text <- stringr::str_extract(text, "^([^:\\[](?:\\.|-)*)+(?:\\b|\\s)*?")
            text <- stringr::str_extract(text, "^([^\\n])+")
            text
        }) %>%
        do.call(what = rbind)

    if (1 < nrow(mat)) {
        for (i in 2:nrow(mat)) {
            abb <- gsub("\\b([A-z])\\w+?\\b", "\\1\\\\.", mat[i - 1, 2])
            mat[i, 2] <- gsub(abb, mat[i - 1, 2], mat[i, 2])
        }
    }

    apply(mat, 1, toString)
}

parse <- function(html) {
    biota <- parse_biota(html)
    if (is.na(biota)) {
        return()
    }

    list(
        h1 = parse_h1(html),
        th = parse_biota_th(biota),
        img = parse_biota_img(biota),
        td = parse_biota_td(biota)
    )
}

scrape_and_parse <- function(terms, dname, crawl = 1) {
    for (epoch in 1:crawl) {
        if (epoch != 1) {
            new <- do.call(c, lapply(PARSE_LIST[terms], function(lst) {
                gsub("^.+, ", "", lst$td)
            }))
            terms <- union(terms, new)
        }

        index <- (terms %in% names(PARSE_LIST))
        terms_sel <- terms[!index]

        if (!identical(terms_sel, character(0))) {
            urls <- sprintf(
                "https://en.wikipedia.org/w/index.php?search=%s",
                gsub("\\s+", "%20", terms_sel)
            )

            htmls <- rmget(urls, dname)
            biota <- lapply(htmls, parse)
            for (i in 1:length(terms_sel)) {
                if (is.null(biota[[i]])) {
                    next
                }
                PARSE_LIST[[terms_sel[i]]] <<- biota[[i]]
            }
        }
    }

    snp <- PARSE_LIST[terms]
    snp <- snp[!is.na(names(snp))]
    snp <- snp[!is.null(snp)]
    snp
}

simplify_network <- function(snp, terms) {
    snp_sel <- snp[terms[terms %in% names(snp)]]

    last <- vapply(snp_sel, function(lst) {
        tail(lst$td, 1)
    }, character(1))

    common <- NULL
    if (1 < length(snp_sel)) {
        for (i in 1:(length(snp_sel) - 1)) {
            for (j in (i + 1):length(snp_sel)) {
                new <- tail(intersect(snp_sel[[i]]$td, snp_sel[[j]]$td), 1)
                if (is.null(new) || identical(new, character(0))) {
                    next
                }
                common[[new]] <- new
            }
        }
    }

    lapply(snp, function(lst) {
        if (any(lst$td %in% last)) {
            return(lst)
        }

        is_last <- (lst$td %in% last)
        is_comm <- (lst$td %in% common)
        is_dkpc <- grepl(PATTERN_TAXON, lst$td)
        lst$td <- lst$td[is_last | is_comm | is_dkpc]
        lst
    })
}


# GRAPHING


form_nodes <- function(snp) {
    nodes <- c(
        do.call(c, lapply(snp, function(lst) {
            sprintf(
                '"%s" [label=< <b>%s </b><br/><i>%s</i> > fillcolor=dodgerblue]',
                tail(lst$td, 1), lst$th, gsub("^.+, ", "", tail(lst$td, 1))
            )
        })),
        do.call(c, lapply(snp, function(lst) {
            sprintf(
                '"%s" [label=< <i>%s</i> >]',
                head(lst$td, -1), gsub("^.+, ", "", head(lst$td, -1))
            )
        }))
    )

    nodes[!duplicated(vapply(nodes, function(x) {
        stringr::str_extract(x, '"(.+?)"')
    }, character(1)))]
}

form_edges <- function(snp, rooted) {
    mat_ind <- do.call(rbind, lapply(snp, function(lst) {
        if (1 < length(lst$td)) {
            if (rooted) {
                new <- cbind(c("Vitae", slicer(lst$td, end = -1)), lst$td)
            } else {
                new <- cbind(slicer(lst$td, end = -1), slicer(lst$td, 2))
            }
            cbind(new, c(rep(0, nrow(new) - 1), 1))
        } else if (rooted) {
            c("Vitae", lst$td, 1)
        }
    }))

    mat <- mat_ind[, 1:2, drop = FALSE]
    terms <- mat[mat_ind[, 3] == "1", 2]

    new_mat <- NULL
    for (term in terms) {

        lst <- NULL
        new_lst <- NULL
        lst[[term]] <- term
        while (!identical(lst, new_lst)) {
            new_lst <- lst

            for (key in names(lst)) {
                path <- lst[[key]]

                child <- head(path, 1)
                parents <- mat[mat[, 2] == child, 1]
                if (identical(parents, character(0))) {
                    next
                }

                lst[[key]] <- NULL
                for (parent in parents) {
                    lst[[parent]] <- c(parent, path)
                }
            }
        }

        lengths <- vapply(lst, length, numeric(1))
        longest <- which(lengths == max(lengths))[1]

        vec <- lst[[longest]]

        new <- cbind(slicer(vec, end = -1), slicer(vec, 2))
        if (rooted) {
            new_mat <- rbind(new_mat, new)
        } else {
            new_mat <- rbind(new_mat, new)
        }
    }

    edges <- sprintf('"%s"->"%s"', new_mat[, 1], new_mat[, 2])
    edges[!duplicated(edges)]
}

form_graph <- function(
        terms, dname,
        crawl = 1,
        simplify = TRUE, rooted = TRUE,
        layout = OPTIONS_LAYOUT,
        overlap = OPTIONS_OVERLAP,
        rankdir = OPTIONS_RANKDIR) {

    simplify <- as.logical(simplify)
    layout <- match.arg(layout)
    overlap <- match.arg(overlap)
    rankdir <- match.arg(rankdir)

    snp <- scrape_and_parse(terms, dname, crawl)
    if (simplify) {
        snp <- simplify_network(snp, terms)
    }

    nodes <- form_nodes(snp)
    edges <- form_edges(snp, rooted)

    if (rooted) {
        nodes <- c('"Vitae" [label=< <i>Vitae</i> >]', nodes)
    }

    sprintf(
        FORMAT_GRAPH,
        layout, overlap, rankdir,
        join(nodes, collapse = "\n"),
        join(edges, collapse = "\n")
    )
}


# APP


ui <- fluidPage(
    br(),
    sidebarLayout(
        sidebarPanel(h4("Wikipedia Trees", style = "text-align:center"), br(), tabsetPanel(type = "tabs",
            tabPanel("Input",
                textAreaInput("text_in", NULL, "horse\nsheep\ngoat\ncow", "100%", "70vh", resize = "both")
            ),
            tabPanel("Config", br(),
                checkboxInput("simplify", "Simplify?", TRUE),
                checkboxInput("rooted", "Rooted tree?", TRUE),
                sliderInput("crawl", "Recursive Search", 1, 5, 1),
                selectInput("layout", "Layout", OPTIONS_LAYOUT),
                selectInput("overlap", "Overlap", OPTIONS_OVERLAP),
                selectInput("rankdir", "Rankdir", OPTIONS_RANKDIR)
            )
        ), width = 2),
        mainPanel(tabsetPanel(type = "pills",
            tabPanel("SVG",
                DiagrammeR::grVizOutput("plot", height = "80vh")
            ),
            tabPanel("HTML",
            ),
            tabPanel("Text",
            ),
            tabPanel("CSV",
            )
        ), width = 10)
    )
)

server <- function(input, output) {
    output$plot <- DiagrammeR::renderDiagrammeR({
        dname <- file.path("/tmp", "html")
        if (!dir.exists(dname)) {
            dir.create(dname, recursive = TRUE)
        }

        terms <- trimws(unlist(strsplit(input$text_in, "\n")))
        terms <- terms[terms != ""]

        DiagrammeR::grViz(form_graph(
            terms, dname,
            as.integer(input$crawl),
            as.logical(input$simplify), as.logical(input$rooted),
            input$layout, input$overlap, input$rankdir
        ))
    })
}

shinyApp(ui, server)
