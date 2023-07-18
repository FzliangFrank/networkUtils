g0 = igraph::make_tree(20, 4)
V(g0) |> as_ids()

g1 = igraph::make_tree(20, 4)
V(g1)$name = seq(length(g1))
V(g1) |> as_ids()

g2 = igraph::make_tree(20, 4)
V(g2)$id = seq(length(g2))
V(g2) |> as_ids()

g3 = igraph::make_tree(20, 4)
V(g3)$id = seq(length(g3)) |> as.character()
V(g3) |> as_ids()

g4 = igraph::make_tree(20, 4)
V(g4)$name = seq(length(g4))
V(g4)$id = seq(length(g4))
V(g4) |> as_ids()
visNetwork::visIgraph(g4)

g5 = igraph::make_tree(20, 4)
(g5)$id = seq(length(g5))
V(g5)$name = seq(length(g5))
V(g5) |> as_ids()
visNetwork::visIgraph(g4)
