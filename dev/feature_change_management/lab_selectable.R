
library(shiny)
library(visNetwork)
library(igraph)
library(bs4Dash)
devtools::load_all()

ccl = make_ring(10, T)
log_timeline_item = function(chglog_1) {
  if(chglog_1 |> is.null()) return(NULL)
  chglog_1$change -> x
  chglog_1$time -> time
  content = jsonlite::toJSON(x, auto_unbox = T, simplifyMatrix = T) |>
    jsonlite::prettify()

  if(is.null(x) || is.na(x) || length(x) == 0) {
    return(timelineStart())
  } else if(x$cmd == 'addNode') {
    icon = icon("circle-plus")
    title = "add node"
    color = 'success'
  } else if (x$cmd == 'addEdge') {
    icon = icon('ruler')
    title = "add edge"
    color = 'info'
    content = p('add edge from:', x$from, 'to', x$to)
  } else if (x$cmd == 'deleteElements') {
    icon = icon("trash")
    title = sprintf("deleted %i element", length(x$nodes) + length(x$edges))
    color = 'maroon'
  } else if (x$cmd == 'editEdge') {
    icon = icon("pencil")
    color = 'indigo'
    title = "edge edited"
    content = paste(x$from, '-->',x$to)
  } else {
    stop("unrecognised command")
  }
  timelineItem(
    icon = icon,
    time = time,
    title = title,
    color = color,
    content
  )
}
# basic UI
# dashboard UI
ui <- bs4Dash::dashboardPage(
  header = dashboardHeader(
    title = 'Graph Change Tracker',
    tags$head(
      tags$script('window.onresize = function() {network.fit();}')
    )
  ),
  sidebar = dashboardSidebar(),
  controlbar = dashboardControlbar(
    controlbarMenu(
      type = 'hidden',
      controlbarItem(
        title = 'Find Node',
        icon = icon('magnifying-glass-arrow-right'),
        mod_visNetInteraction_ui('visNet'),
      )
    ),
    width = 300
  ),
  body = dashboardBody(
      fluidRow(
        sortable(
        width = 6,
          box(
            width = 12,
            style = 'overflow-y: scroll;',
            title = 'Network',
            maximizable = T,
            height = 200,
            mod_visNetModification_ui('visNet')
          ),
        ),
        sortable(
          width = 6,
          box(
            width = 12,
            title = 'Time Line',
            div(
              style = 'overflow: scroll',
              height = '300px',
              uiOutput("timeline_ui")
            )
          )
        )
      ),
      box(
        title = "Dev Log",
        verbatimTextOutput('dev')
      )
  )
)

server <- function(input, output, session) {
  # output$visNet <- renderVisNetwork({
  #   visIgraph(ccl)
  # })
  # observe({
  #   visNetworkProxy('visNet') |>
  #     visGetNodes()
  #   print(input$visNet_nodes)
  # })
  graph = reactive(ccl)
  G = mod_visNet_server('visNet', graph)
  clicked_node = reactive({
    G$click_node
  })
  change_log = reactiveValues(log=NULL)
  # create a series of change logs based on whenever a
  # change happens.
  observe({
    req(G$editing)
    req(!is.null(G$editing$cmd) || is.na(G$editing))
    change_log$log = isolate(change_log$log) |>
      append(list(list(
        time = Sys.time(),
        change = G$editing
      )))
  })
  output$dev <- renderPrint({
    print(clicked_node())
    print(change_log$log)
    print(change_log$log |>
            jsonlite::toJSON(auto_unbox = T, simplifyMatrix = T) |>
            jsonlite::prettify())
  })
  observe({
    change_log$log |>
      jsonlite::toJSON(flatten=T) |>
      jsonlite::write_json('dev/feature_change_management/change_log.json')
  })
  output$save = downloadHandler(
    filename = function() {
      paste(Sys.time(), '.json')
    },
    content = function(file) {
      change_log$log |>
        jsonlite::toJSON() |>
        jsonlite::write_json(file)
    }
  )
  output$timeline_ui <- renderUI({
    timelineBlock(
      width = 12,
      purrr::map(
        change_log$log,
        log_timeline_item
        )
    )
  })
}
shinyApp(ui, server)


## How to Use Demo Time Line -------------------------------
if(interactive()){
  library(shiny)
  library(bs4Dash)

  shinyApp(
    ui = bs4DashPage(
      header = dashboardHeader(),
      sidebar = dashboardSidebar(),
      controlbar = dashboardControlbar(),
      footer = dashboardFooter(),
      title = "test",
      body = dashboardBody(
        box(
          height = '300px',
          style = 'overflow-y: scroll',
          title = "Timeline",
          uiOutput('timeline_ui')
        )
      )
    ),
    server = function(input, output) {
      output$timeline_ui <- renderUI({

        myList=list(
          itemA = list(
            name = "item A.",
            time = '2023-01-31'
          ),
          itemB = list(
            name = "item B.",
            time = '2023-02-31'
          )
        )
        timelineBlock(
          width = 12,
          reversed = TRUE,
          timelineEnd(color = "danger"),
          timelineLabel("10 Feb. 2014", color = "pink"),
          timelineItem(
            elevation = 4,
            title = "Item 1",
            icon = icon("gears"),
            color = "olive",
            time = "now",
            footer = "Here is the footer",
            "This is the body"
          ),
          timelineItem(
            title = "Item 2",
            border = FALSE,
            time = Sys.time()
          ),
          timelineLabel("3 Jan. 2014", color = "lightblue"),
          timelineItem(
            elevation = 2,
            title = "Item 3",
            icon = icon("paint-brush"),
            status = "orange",
            timelineItemMedia(image = "https://via.placeholder.com/150x100"),
            timelineItemMedia(image = "https://via.placeholder.com/150x100")
          ),
          timelineStart(color = "secondary")
        )
      })
    }
  )
}
if(interactive()) {
  library(purrr)
  change_list = jsonlite::read_json('dev/feature_change_management/change_log.json')[[1]] |>
    jsonlite::parse_json(
      flatten=T,
      auto_unbox=T
    )
  change_list |>
    purrr::detect(~.x$change$cmd == 'deleteElements')

}

