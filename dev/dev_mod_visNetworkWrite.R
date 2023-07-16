library(igraph)
library(visNetwork)
g <- igraph::make_graph(~ A-+B:C,
                        B-+D:E, C-+F:G
)

# g <- igraph::make_tree(30, 3)
g_length <- length(V(g))
e_length <- length(E(g))
V(g)$name <- seq(g_length) |> as.character() |> paste0(".name")
V(g)$attr_1 <- sample(seq(10), g_length, replace = T)
V(g)$attr_2 <- sample(LETTERS, g_length, replace = T)
E(g)$attr1 <- sample(LETTERS, e_length, replace = T)
E(g)$attr2 <- sample(seq(10), e_length, replace = T)
# E(g)$name <- seq(e_length) |> as.character()
E(g)$id <- seq(e_length) |> as.character() |> paste0(".id") # this attribute won't
#' get recognised

g <- tidygraph::as_tbl_graph(g)


# export_formats <- c("edgelist", "pajek", "ncol", "lgl", "graphml", "dimacs", "gml", "dot",
#                     "leda")
# export_files <- as.character(glue::glue("dev/dev_output/export_as_{format}.{format}", format = export_formats))
# for(i in seq(length(export_files))) {
#   try(write_graph(g, export_files[i], format = export_formats[i]))
# }
#
# list.files("dev/dev_output", full.names = T) |>
#   purrr::walk(unlink)
#
# gml_file = export_files[7]
# read_graph(gml_file, format="gml")


##' The Optimal formate to export is probably gml.
##' DOT is good igraph won't able to read them immediately
##'
# This Return Error
g <- igraph::make_tree(40, 3, mode = "out")
nV <- length(V(g))
nE <- length(E(g))
V(g)$names <- sample(letters, nV, replace = T)
V(g)$attr1 <- sample(seq(10), nV, replace = T)
V(g)$attr2 <- sample(LETTERS, nV, replace = T)
V(g)$title <- pasteNodeDetails(g)
if(interactive()) {
  library(shiny)

  ui <- fluidPage(
    mod_visNetInteraction_ui("id"),
    mod_visNetModification_ui("id")
  )

  server <- function(input, output, session) {
    mod_visNetInteraction_server("id",
                                 reactive(g),
                                 v_ignore = c('title')
                                 )
    mod_visNetModification_server("id", reactive(g))
  }
  options(shiny.autoreload = T)
  shinyApp(ui, server)
}

library(shiny)
if(interactive()) {
  ui <- fluidPage(
    div(
      style = 'display:inline-block;vertical-align:top; width = 150px',
        style = 'display:inline-block;vertical-align:top; width = 150px',
        shiny::selectizeInput("id",
                            label = "select city",
                            choices = c("Washinton", "NYC", "Exeter")),
        style = 'display:inline-block;vertical-align:center; width = 150px',
        shiny::actionButton("id",
                          "",
                          icon = icon('search'))
    )
  )

  server <- function(input, output, session) {

  }
  shinyApp(ui, server)
}

