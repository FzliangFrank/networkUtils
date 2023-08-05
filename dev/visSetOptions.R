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
library(visNetwork)
G = visIgraph(g, layout = 'layout_in_circle')
G |>
  visIgraphLayout('layout_on_grid')
l = igraph::make_ring(10)
l |>
  visIgraph() |>
  visIgraphLayout('layout_with_lgl')
