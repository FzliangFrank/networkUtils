pkgload::load_all()
library(shiny)
require(igraph)

g <- igraph::make_graph(~ A-+B:C,
                        B-+D:E, C-+F:G
                        )

# g <- igraph::make_tree(30, 3)
g_length <- length(V(g))
e_length <- length(E(g))
V(g)$name <- seq(g_length) |> as.character()
V(g)$attr_1 <- sample(seq(g_length), g_length, replace = T)
V(g)$attr_2 <- sample(LETTERS, g_length, replace = T)
V(g)$attr_3 <- runif(g_length) * 100 |> round(2)
E(g)$attr1 <- sample(LETTERS, e_length, replace = T)
E(g)$attr2 <- sample(seq(e_length), e_length, replace = T)
E(g)$attr3 <- runif(e_length)
# vertex_attr_names(g) |>
#   lapply(\(x) vertex_attr(g, name = x)) |>
#   purrr::reduce(paste)

vertex_attr(g) |>
  purrr::imap(~paste(.y, ":", .x)) |>
  purrr::reduce(paste, sep = "<br>")

if(interactive()) {
  library(reactlog)

  # tell shiny to log all reactivity
  reactlogReset()
  reactlog_enable()
  ui <- fluidPage(
    mod_visNetworkReadDisplay_ui("id"),
    mod_visNetworkReadControler_ui("id")
  )
  server <- function(input, output, session) {
    gg <- reactiveValues()
    observe({
      gg$g = g
      print(gg$g)
    })
    mod_visNetworkReadDisplay_server("id", reactive(gg$g))
    mod_visNetworkReadControler_server("id", reactive(gg$g))
  }
  shinyApp(ui, server, options=list(display.mode='showcase'))
}
shiny::reactlogShow()


