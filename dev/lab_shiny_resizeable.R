library(shinyjqui)
library(visNetwork)
library(shiny)
library(htmltools)
library(bs4Dash)
devtools::load_all()



visNetId = function(id) {
  a_id=paste0(id,'-','visNetworkId')
}
maximize_helper=function(id, offset = 180) {
  HTML(sprintf(r'(
  // Access JS Object
  var target=document.getElementById('%s');
  var cardBody=target.parentElement
  var card = cardBody.parentElement
  var cardHeader = card.getElementsByClassName('card-header')[0]
  var cardTool = cardHeader.getElementsByClassName('card-tools')[0]
  var maximizeBtn = cardTool.querySelectorAll('[data-card-widget="maximize"]')[0]

  console.log("find btn", maximizeBtn)
  let maximized=false
  const ogHeight = target.offsetHeight

  maximizeBtn.addEventListener('click', function(){
      maximized = !maximized
      console.log('button is clicked')
      if(maximized) {
          console.log('try to maiximise target')
          const h = window.innerHeight - %i
          target.style.height = h + 'px'
      } else {
          console.log('try to reset target')
          target.style.height = ogHeight + 'px'
      }
  })
  )',id, offset))
}


ui <- dashboardPage(
  sidebar=dashboardSidebar(),
  header = dashboardHeader(title="test"),
  body=dashboardBody(
    box(width=12,
      # shinyjqui::jqui_resizable(
      id = 'map',
      visNetworkOutput("default", width='100%'),
      maximizable = T
    ),
    box(width=12,maximizable = T,
        mod_visNetModification_ui('id'),
        tags$script(maximize_helper(visNetId('id')))
    )
  )
)

server <- function(input, output) {
  output$default <- renderVisNetwork({
    # generate dot plot
    g = igraph::make_tree(12,3)
    visIgraph(g) |>
      visOptions(clickToUse = T)
  })
  G = reactive({
    igraph::make_tree(12,3)
  })
  mod_visNetModification_server('id', G)
}
shinyApp(ui, server)

