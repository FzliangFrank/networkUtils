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
##' DOT is good igraph won't able to read them immediatly
##'
if(interactive()) {
  library(shiny)

  ui <- fluidPage(
    mod_visNetworkWrite_ui("id")
  )

  server <- function(input, output, session) {
    mod_visNetworkWrite_server("id", reactive(g))
  }
  options(shiny.autoreload = T)
  shinyApp(ui, server)
}


