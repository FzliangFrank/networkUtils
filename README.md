
<!-- README.md is generated from README.Rmd. Please edit that file -->

# NetworkUtils

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

Empowered by shiny and `visNetwork`, this is a font-end development to
interactively view and modify any graph based network structure.

## Installation

You can install the development version of `networkUtils` from
[GitHub](https://github.com/) with:

``` r
remotes::install_github("FzliangFrank/networkUtils")
```

## Introduction

This package is designed for whoever need editing network for their
work. There are two main features:

- let you edit a network, interact via a GUI;
- support for spatial data (spatial points);

Let’s say in you have a network, each node is fixed within a specific
location on a map. You want to delete a node that is too far away from
center. Take it out of you network. But you do not sure if taken it out
will have some unintended consequence, (disconnecting a node will
disconnect any other off-stream branch).

You need visual of what happen if I delete this, what happens if I do
something.

`networkUtil` package makes it possible.

![](readme_img/Screenshot%202023-06-10%20at%2016.56.19.png) Selecting it
in side bar will select corresponding node on the network. In this
instance it tells me deleting this node will disconnect three other
nodes!

All the four method of deleting and editing node from package
`visNetwork` are supported.

![](readme_img/Screenshot%202023-06-10%20at%2017.23.15.png) \### Key
take away.

You need two shiny module made it possible:

- `mod_visNetModification_server(id)` (ui) in charge of the logic to
  editing networkgraph. This function return reactive graph.
- `mod_visNetInteraction_server(id)` (ui) in charge of all the dynamic
  UI and selection by node.

Make sure the id matches. You can build your own shinyApp using these
two module.

``` r
library(shiny)
library(igraph)
ui <- fluidPage(
  column(4,
         mod_visNetInteraction_ui("id")
         ),
  column(8, 
         mod_visNetModification_ui("id")
         )  
)

server <- function(input, output, session) {
  myGraph = reactive({
    g = igraph::make_tree(30, 2)
    V(g)$attr1 = sample(LETTERS, length(V(g)), replace = T)
    V(g)$id = seq(length(V(g)))
    g
  })
  sessionGraph = mod_visNetModification_server("id", myGraph) # Output is reactive value
  mod_visNetInteraction_server("id", 
                               reactive(sessionGraph$Current)
                               )
  observe({
    print(class(myGraph))
    print(class(myGraph()))
  })
}
shinyApp(ui, server)
```

Note always pass igraph as reacitve expression. The output of
mod_visNetModification_server is always a list of two:

- `$Current` for the graph you are interacting with now
- `$Main` for graph you are happy with (one after you press save)

This is a basic version control.

### Siny App Build Using this Module

``` r
library(networkUtils)
run_simpleNetworkUtilApp()
```

[Simple Network Util
App](https://frank-the-tank.shinyapps.io/networkutils/?_ga=2.2760560.1153125338.1680970415-1261124081.1680970415)
is avaiable on shiny.io

## Trivia

### To-do List

**Change Management & API readyness**

- [ ] Accept a record of list in function `modify_graph()`.This can the
  gives you ability to write api calll,
- [ ] Ability to trigger side effect on function `modify_graph_i()`. For
  example track what has been change, and what has been.
- [ ] Arguments in two module to return reactive log.

**More Supporting Features**

- [x] `mod_visNetInteraction_` should scope much more data type.
- [ ] visualize line geometry as an input to select edge.
- [x] specialized shiny app
- [x] When scoping namespace within `visNetwork` own function, session
  id do not get appended automatically. This might get fixed in the
  future. For production reasons, freeze current visNetwork package in
  namespace
