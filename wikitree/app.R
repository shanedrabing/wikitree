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
layout=%s\nsplines=%s\noverlap=%s\nrankdir=%s
pad=0
node [shape=record style=filled fillcolor=white color=none fillcolor=skyblue]
%s
%s
}\n")

CSS <- trimws(which = "left", "
/* Credits */

/* Ilya Pestov: Original, https://codepen.io/Pestov/pen/BLpgm */
/* Paul Smirnov: Horizontal, https://codepen.io/paulsmirnov/pen/dyyOLwa */
/* Shane Drabing: Changed styling, added image hover effects */

/* Variables */

:root {
    --border-radius: 1px;
    --border-width: 2px;
    --border-padding: 10px;
    --transition-speed: 0.5s;
    --box-color: skyblue;
    --border-color: black;
    --text-color: black;
    --box-color-hover: red;
    --border-color-hover: red;
    --text-color-hover: white;
}

/* Now the CSS */

* {
    margin: 0;
    padding: 0;
}

.tree {
    display: -webkit-box;
    display: -ms-flexbox;
    display: flex;
    -webkit-box-pack: start;
    -ms-flex-pack: start;
    justify-content: flex-start;
    padding-top: 5vh;
    padding-bottom: 20vh;
}

.tree ul {
    padding-left: var(--border-padding);
    position: relative;
    transition: all var(--transition-speed);
    -webkit-transition: all var(--transition-speed);
    -moz-transition: all var(--transition-speed);
    display: -webkit-box;
    display: -ms-flexbox;
    display: flex;
    -webkit-box-orient: vertical;
    -webkit-box-direction: normal;
    -ms-flex-direction: column;
    flex-direction: column;
    -webkit-box-pack: center;
    -ms-flex-pack: center;
    justify-content: center;
}

.tree li {
    text-align: center;
    list-style-type: none;
    position: relative;
    padding: var(--border-radius) 0 var(--border-radius) var(--border-padding);
    display: -webkit-box;
    display: -ms-flexbox;
    display: flex;
    transition: all var(--transition-speed);
    -webkit-transition: all var(--transition-speed);
    -moz-transition: all var(--transition-speed);
}

/* We will use ::before and ::after to draw the connectors */

.tree li::before, .tree li::after {
    content: '';
    position: absolute;
    left: 0;
    bottom: 50%;
    border-left: var(--border-width) solid var(--border-color);
    width: var(--border-padding);
    height: 50%;
}

.tree li::after {
    bottom: auto;
    top: 50%;
    border-top: var(--border-width) solid var(--border-color);
}

/* We need to remove left-right connectors from elements without
any siblings */

.tree li:only-child::after, .tree li:only-child::before {
    display: none;
}

/* Remove space from the top of single children */

.tree li:only-child {
    padding-left: 0;
}

/* Remove left connector from first child and
right connector from last child */

.tree li:first-child::before, .tree li:last-child::after {
    border: 0 none;
}

/* Adding back the vertical connector to the last nodes */

.tree li:last-child::before {
    border-bottom: var(--border-width) solid var(--border-color);
    border-radius: 0 0 var(--border-radius) 0;
    -webkit-border-radius: 0 0 var(--border-radius) 0;
    -moz-border-radius: 0 0 var(--border-radius) 0;
}

.tree li:first-child::after {
    border-radius: 0 0 0 var(--border-radius);
    -webkit-border-radius: 0 0 0 var(--border-radius);
    -moz-border-radius: 0 0 0 var(--border-radius);
}

/* Time to add downward connectors from parents */

.tree ul ul::before {
    content: '';
    position: absolute;
    left: 0;
    top: 50%;
    border-top: var(--border-width) solid var(--border-color);
    width: var(--border-padding);
    height: 0;
}

/* Box styles */

.tree li div {
    cursor: pointer;
    border: 0 solid var(--border-color);
    padding: 4px 8px;
    text-decoration: none;
    color: var(--text-color);
    background-color: var(--box-color);
    font-family: arial, verdana, tahoma;
    font-size: 12px;
    display: flex-end;
    -ms-flex-item-align: center;
    -ms-grid-row-align: center;
    align-self: center;
    border-radius: var(--border-radius);
    -webkit-border-radius: var(--border-radius);
    -moz-border-radius: var(--border-radius);
    transition: all var(--transition-speed);
    -webkit-transition: all var(--transition-speed);
    -moz-transition: all var(--transition-speed);
}

#blue {
    background-color: dodgerblue;
}

/* Time for some hover effects */
/* We will apply the hover effect the the lineage of the element also */

.tree li div:hover, .tree li div:hover+ul li div, #blue:hover, .tree li div:hover+ul li div#blue {
    background: var(--box-color-hover);
    color: var(--text-color-hover);
    border: 0 solid var(--border-color-hover);
}

/* Connector styles on hover */

.tree li div:hover+ul li::after, .tree li div:hover+ul li::before, .tree li div:hover+ul::before, .tree li div:hover+ul ul::before {
    border-color: var(--border-color-hover);
}

/* Image hover effects */

a {
  color: inherit;
  text-decoration: none;
}

/* Dynamic mode */

img {
    transition: all 0.75s;
    -webkit-transition: all 0.75s;
    -moz-transition: all 0.75s;
    transition-timing-function: ease-out;
    transition-delay: 75ms;

    width: auto;
    height: auto;
    max-width: 0px;
    max-height: 0px;
    display: block;
    visibility: hidden;
}

div.parent:hover img {
    transition: all 0.5s;
    transition-delay: 15ms;
    max-width: 40vw;
    max-height: 30vh;
    visibility: visible;
}
")

DEFAULT_INPUT <- "fly\nspider\nbird\ncat\ndog\ngoat\ncow\nhorse"

OPTIONS_LAYOUT <- c("dot", "neato", "twopi", "fdp")
OPTIONS_SPLINES <- c("true", "false", "ortho", "polyline", "curved")
OPTIONS_OVERLAP <- c("true", "false", "scale", "ortho")
OPTIONS_RANKDIR <- c("LR", "BT", "RL", "TB")


# FUNCTIONS


join <- function(..., sep = "", collapse = "") {
    paste(..., sep = sep, collapse = collapse)
}

adjacent <- function(x) {
    cbind(slicer(x, end = -1), slicer(x, 2))
}

dedupe <- function(x) {
    if (!is.null(ncol(x))) {
        return(x[!duplicated(x), ])
    }
    x[!duplicated(x)]
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


parse_canonical <- function(html) {
    html %>%
        html_node("head link[rel='canonical']") %>%
        html_attr("href")
}

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
        gsub("(?:\\n|\\[).+$", "", .)
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

    if (is.null(mat)) {
        return()
    }

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

    lst <- list(
        url = parse_canonical(html),
        h1 = parse_h1(html),
        th = parse_biota_th(biota),
        img = parse_biota_img(biota),
        td = parse_biota_td(biota)
    )

    if (any(sapply(lst, is.null))) {
        return()
    }

    lst
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

    snp_sim <- lapply(snp, function(lst) {
        # is_last <- (lst$td %in% last)
        # if (any(is_last)) {
        #     return(lst)
        # }

        is_comm <- (lst$td %in% common)
        is_dkpc <- grepl(PATTERN_TAXON, lst$td)
        lst$td <- lst$td[is_comm | is_dkpc]
        lst
    })

    index <- !duplicated(lapply(snp_sim, "[[", "td"))
    snp_sim[index]
}

# GRAPHING


form_nodes <- function(snp) {
    nodes <- c(
        do.call(c, lapply(snp, function(lst) {
            sprintf(
                '"%s" [label=< <b>%s</b><br/><i>%s</i> > fillcolor=dodgerblue]',
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
        out <- NULL
        if (1 < length(lst$td)) {
            if (rooted) {
                new <- cbind(c("Vitae", slicer(lst$td, end = -1)), lst$td)
            } else {
                new <- cbind(slicer(lst$td, end = -1), slicer(lst$td, 2))
            }
            out <- cbind(new, c(rep(0, nrow(new) - 1), 1))
        } else if (0 < length(lst$td) && rooted) {
            out <- matrix(c("Vitae", lst$td, 1), ncol = 3)
        }
        out
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
        splines = OPTIONS_SPLINES,
        overlap = OPTIONS_OVERLAP,
        rankdir = OPTIONS_RANKDIR) {

    simplify <- as.logical(simplify)
    layout <- match.arg(layout)
    splines <- match.arg(splines)
    overlap <- match.arg(overlap)
    rankdir <- match.arg(rankdir)

    snp <- scrape_and_parse(terms, dname, crawl)
    if (simplify) {
        snp <- simplify_network(snp, terms)
    }

    if (all(is.na(names(snp)))) {
        return("digraph {}")
    }

    nodes <- form_nodes(snp)
    edges <- form_edges(snp, rooted)

    if (rooted) {
        nodes <- c('"Vitae" [label=< <i>Vitae</i> >]', nodes)
    }

    sprintf(
        FORMAT_GRAPH,
        layout, splines, overlap, rankdir,
        join(nodes, collapse = "\n"),
        join(edges, collapse = "\n")
    )
}


# HTML


tag <- function(name, ..., attrs = NULL, cap = TRUE, esc = TRUE) {
    escchar <- ifelse(esc, "\n", "")
    attrs <- join(sprintf(" %s='%s'", names(attrs), attrs))
    start <- sprintf("<%s%s>", name, attrs)
    middle <- sprintf("%s%s", trimws(join(...)), ifelse(join(...) != "", escchar, ""))
    end <- ifelse(cap, sprintf("</%s>%s", name, escchar), "")
    sprintf("%s%s%s%s", start, escchar, middle, end)
}

html_list <- function(node, edges, lookup) {
    children <- edges[edges[, 1] == node, 2]

    kids <- common <- img <- ""
    if (0 < length(children)) {
        kids <- tag("ul", sapply(children, html_list, edges, lookup))
    }

    rank_taxon <- unlist(strsplit(node, ", "))
    taxon <- gsub("\\b([A-z])(\\w+\\s)", "\\1. ", rank_taxon[2])
    if (is.na(taxon)) {
        taxon <- rank_taxon[1]
    }

    if (node %in% names(lookup)) {
        lst <- lookup[[node]]
        common <- lst$th
        img <- tag("img", attrs = c(src = lst$img, loading = "lazy"), cap = FALSE)
        href <- lst$url
    }

    div <- tag("div", tag("i", taxon))
    if (img != "") {
        name <- join(tag("b", common), tag("i", taxon), sep = "<br/>")
        a <- tag("a", img, attrs = c(href = href, target = "_blank"))
        div <- tag("div", name, a, attrs = c(class = "parent", onclick = "", id = "blue"))
    }

    tag("li", div, kids)
}

html_page <- function(snp) {
    edges <- form_edges(snp, TRUE)
    edges <- stringr::str_match(edges, '"(.+?)"->"(.+?)"')[, 2:3]

    lookup <- NULL
    for (key in names(snp)) {
        lst <- snp[[key]]
        if (0 < length(lst$td)) {
            lookup[[tail(lst$td, 1)]] <- lst
        }
    }

    meta <- tag("meta", attrs = c(charset = "UTF-8"))
    head <- tag("head", meta, tag("style", CSS))
    vita <- html_list("Vitae", edges, lookup)
    tree <- tag("ul", vita)
    body <- tag("body", tag("div", tree, attrs = c(class = "tree")))
    html <- tag("html", head, body)
    page <- sprintf("<!DOCTYPE html>\n%s\n", html)

    page
}


# TEXT


text_list <- function(node, edges, lookup, a = "", b = "", k = 2) {
    children <- edges[edges[, 1] == node, 2]
    n <- length(children)

    rank_taxon <- unlist(strsplit(node, ", "))
    taxon <- common <- ""

    rank <- sprintf("%s:", node)
    if (1 < length(rank_taxon)) {
        rank <- sprintf("%s:", rank_taxon[1])
        taxon <- gsub("\\b([A-z])(\\w+\\s)", "\\1. ", rank_taxon[2])
    }

    if (node %in% names(lookup)) {
        lst <- lookup[[node]]
        common <- sprintf("(%s)", lst$th)
        img <- tag("img", attrs = c(src = lst$img, loading = "lazy"), cap = FALSE)
        href <- lst$url
    }

    name <- trimws(join(rank, taxon, common, sep = " "))

    if (0 < n) {
        sprintf("%s%s\n%s", a, name, join(sapply(1:n, function(i) {
            if (i != n) {
                text_list(
                    children[i], edges, lookup,
                    sprintf("%s\u251c%s ", b, join(rep("\u2500", k))),
                    sprintf("%s\u2502%s ", b, join(rep(" ", k))), k
                )
            } else {
                text_list(
                    children[i], edges, lookup,
                    sprintf("%s\u2514%s ", b, join(rep("\u2500", k))),
                    sprintf("%s %s ", b, join(rep(" ", k))), k
                )
            }
        }), collapse = "\n"))
    } else {
        sprintf("%s%s", a, name)
    }
}

text_page <- function(snp) {
    edges <- form_edges(snp, TRUE)
    edges <- stringr::str_match(edges, '"(.+?)"->"(.+?)"')[, 2:3]

    lookup <- NULL
    for (key in names(snp)) {
        lst <- snp[[key]]
        if (0 < length(lst$td)) {
            lookup[[tail(lst$td, 1)]] <- lst
        }
    }

    text_list("Vitae", edges, lookup)
}


# APP


ui <- fluidPage(
    br(),
    sidebarLayout(
        sidebarPanel(h3("Wikipedia Trees", style = "text-align:center"), br(), tabsetPanel(type = "tabs",
            tabPanel("Input",
                textAreaInput("text_in", NULL, DEFAULT_INPUT, "100%", "60vh", resize = "vertical")
            ),
            tabPanel("Config",
                 br(), p("General Options", style = "text-align:center;font-weight:bold;"),
                checkboxInput("simplify", "Simplify?", TRUE),
                fixedRow(column(4, p("Recursive Search")), column(8,
                    sliderInput("crawl", NULL, 1, 5, 1, animate = FALSE)
                )),
                br(), p("GraphViz Options", style = "text-align:center;font-weight:bold;"),
                checkboxInput("rooted", "Rooted tree?", TRUE),
                fixedRow(column(4, p("Layout")), column(8,
                    selectInput("layout", NULL, OPTIONS_LAYOUT)
                )),
                fixedRow(column(4, p("Splines")), column(8,
                    selectInput("splines", NULL, OPTIONS_SPLINES),
                )),
                fixedRow(column(4, p("Overlap")), column(8,
                    selectInput("overlap", NULL, OPTIONS_OVERLAP),
                )),
                fixedRow(column(4, p("Rankdir")), column(8,
                    selectInput("rankdir", NULL, OPTIONS_RANKDIR)
                ))
            )
        ), width = 2),
        column(tabsetPanel(type = "pills",
            tabPanel("SVG", br(), DiagrammeR::grVizOutput("plot", "95%", "80vh")),
            tabPanel("HTML", br(), htmlOutput("html")),
            tabPanel("Text", br(), verbatimTextOutput("text")),
            tabPanel("Table", br(), tableOutput("table"))
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
        terms <- unique(tolower(terms[terms != ""]))

        DiagrammeR::grViz(form_graph(
            terms, dname,
            input$crawl,
            input$simplify, input$rooted,
            input$layout, input$splines, input$overlap, input$rankdir
        ))
    })

    output$html <- renderText({
        dname <- file.path("/tmp", "html")
        if (!dir.exists(dname)) {
            dir.create(dname, recursive = TRUE)
        }

        terms <- trimws(unlist(strsplit(input$text_in, "\n")))
        terms <- unique(tolower(terms[terms != ""]))

        snp <- scrape_and_parse(terms, dname, crawl = input$crawl)
        if (input$simplify) {
            snp <- simplify_network(snp, terms)
        }

        html_page(snp)
    })

    output$text <- reactive({
        dname <- file.path("/tmp", "html")
        if (!dir.exists(dname)) {
            dir.create(dname, recursive = TRUE)
        }

        terms <- trimws(unlist(strsplit(input$text_in, "\n")))
        terms <- unique(tolower(terms[terms != ""]))

        snp <- scrape_and_parse(terms, dname, crawl = input$crawl)
        if (input$simplify) {
            snp <- simplify_network(snp, terms)
        }

        text_page(snp)
    })

    output$table <- renderTable({
        dname <- file.path("/tmp", "html")
        if (!dir.exists(dname)) {
            dir.create(dname, recursive = TRUE)
        }

        terms <- trimws(unlist(strsplit(input$text_in, "\n")))
        terms <- unique(tolower(terms[terms != ""]))

        snp <- scrape_and_parse(terms, dname, crawl = input$crawl)
        if (input$simplify) {
            snp <- simplify_network(snp, terms)
        }

        mat <- do.call(rbind, lapply(snp, function(lst) {
            setNames(sapply(lst, join, collapse = "|"), names(lst))
        }))

        mat[, c("h1", "th", "td", "url", "img")]
    }, spacing = "xs", striped = TRUE, bordered = TRUE)
}

shinyApp(ui, server)
