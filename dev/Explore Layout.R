library(igraph)
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

library(shiny)
library(shinyWidgets)
library(visNetwork)
ui <- fluidPage(
  searchInput("lo", "layout", NULL),
  visNetworkOutput("vis")
)

server <- function(input, output, session) {
  output$vis = renderVisNetwork({
    req(input$lo)
    G = tryCatch({
      if(input$lo == "" || is.null(input$lo)) stop()
      visIgraph(g, layout = input$lo)
    }, error = function() {
      message("graph errored")
      visIgraph(g)
    })
    return(G)
  })
}

shinyApp(ui, server)
