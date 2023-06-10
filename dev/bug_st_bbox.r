# replicating bug

# Create Demo Graph
g <- igraph::make_tree(20, 3, mode = "out")
nV <- length(V(g))
nE <- length(E(g))
V(g)$label <- sample(letters, nV, replace = T) # fix this latter NAME needs to not identical
V(g)$attr1_int <- sample(seq(10), nV, replace = T)
V(g)$attr2_letter <- sample(LETTERS, nV, replace = T)
V(g)$attr3_numb <- runif(nV) * 100
sp_df = data.frame(x = runif(nV), y= runif(nV)) |> sf::st_as_sf(coords=c('x', 'y'))
V(g)$attr4_geom <- sp_df$geom
E(g)$attr1 <- runif(nE) * 100

# Below code cause bug
g = g + vertex(13)



# Leads me to this issue on github: https://github.com/luukvdmeer/sfnetworks/issues/29
# The usage of add_vertex do not always work on sf object

# if exist a list attribute in vertex?
(vertex_attr(g) |> purrr::map_chr(~class(.x)[2]))

purrr::detect_index(vertex_attr(g), ~.x |> inherits('sfc'))
names(vertex_attr(g))

id = "placeholder"

# work out for one
g = create_demo_graph()

schema = purrr::imap(vertex_attr(g), ~ class(.x))
sf_attr_i = purrr::detect_index(schema, ~'sfc' %in% .x)
sf_attr_name = names(schema)[sf_attr_i]

if(sf_attr_i != 0) {
  sf_attr = vertex_attr(g, sf_attr_name)
  g = delete_vertex_attr(g, sf_attr_name)
  g = g + vertex(id)
  set_vertex_attr(g, name = sf_attr_name, value = sf_attr[length(V(g))])
}

tidygraph::as_tbl_graph(g)
