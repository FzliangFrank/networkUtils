
library(shiny)
library(visNetwork)
library(igraph)
library(bs4Dash)
devtools::load_all()

ccl = make_ring(10, T)
# basic UI
ui <- fluidPage(
  column(
    8,
    mod_visNetModification_ui('visNet'),
    verbatimTextOutput('dev'),
    actionButton('write', "Download Changelog"),
    downloadButton('save', "Save Changelog")
  ),
  column(
    4,
    mod_visNetInteraction_ui('visNet')
  )
)
# dashboard UI
ui <- bs4Dash::dashboardPage(
  header = dashboardHeader(
    title = 'Graph Change Tracker'
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
    sortable(
      width = 12,
      fluidRow(
        box(
          title = 'Network',
          mod_visNetModification_ui('visNet'),
          actionButton('write', "Download Changelog"),
          downloadButton('save', "Download Changelog")
        ),
        box(
          title = 'Time Line',
          uiOutput("timeline_ui")
        )
      ),
      box(
        title = "Dev Log",
        verbatimTextOutput('dev')
      )
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
    req(!is.null(G$editing))
    req(G$editing != '')
    change_log$log = isolate(change_log$log) |>
      append(list(list(
        time = Sys.time(),
        change = G$editing
      )))
  })
  output$dev <- renderPrint({
    print(clicked_node())
    print(change_log$log |>
            jsonlite::toJSON(auto_unbox = T, simplifyMatrix = T) |>
            jsonlite::prettify())
  })
  observe({
    change_log$log |>
      jsonlite::toJSON() |>
      jsonlite::write_json('change_log.json')
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
        ~ timelineItem(
          time = .x$time,
          title = .x$change$cmd
        )
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
        # timelineBlock(
        #   width = 12,
        #   # lapply(myList, \(x) timelineItem(
        #   #   title = x$name,
        #   #   time = x$time
        #   # ))
        #   purrr::map(myList, ~ timelineItem(
        #     title = .x$name,
        #     time = .x$time
        #   ))
        # )
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

jsonlite::read_json('dev/change_log.json')
