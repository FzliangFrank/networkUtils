devtools::load_all()
set.seed(1)
g = make_tree(12, 3)
V(g)$attr <- sample(letters, length(g))
V(g)$name <- paste0(seq(length(g)), ".", V(g)$attr)
E(g)$attr <- runif(length(E(g)))
E(g)$name <- paste0(seq(length(E(g))), ".", 'id')
search_idx(g, "attr == 'y'")
search_idx(g, '9.w')
search_idx(g, '1.id', search_in = 'edges')
# if tidygraph is loaded in environment this will work
library(tidygraph)
search_idx(g, 'node_is_root()')
