#' fileUploader UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyWidgets pickerInput updatePickerInput
mod_fileUploader_ui <- function(id, label = "Upload a File",...){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    fileInput(ns("file"), label = label, accept = c(".xlsx", ".csv"), ...),
    div(
      id = ns("info"), class = "simpleDiv",
      "Either a .xlsx excel fille with two sheets or a .csv format with at least two column"
    ),
    pickerInput(ns("pickEdgeSheet"), labe = "Select Edge Sheet",
                choices = NULL,
                selected = NULL,
                options = list(`live-search` = T)
    ),
    pickerInput(ns("pickNodeSheet"), labe = "Select Node Sheet",
                choices = NULL,
                selected = NULL,
                options = list(`live-search` = T)
                ),
    div(actionButton(ns("go1"), label = "Next"), align = "right"),
    pickerInput(ns("from_col"), label = "From:",
                choices = NULL,
                selected = NULL,
                options = list(`live-search` = T)),
    pickerInput(ns("to_col"), label = "To:",
                choices = NULL,
                selected = NULL,
                options = list(`live-search` = T)),
    pickerInput(ns("id_col"), label = "Node id",
                choices = NULL,
                selected = NULL,
                options = list(`live-search` = T)
    ),
    div(actionButton(ns("go2"), label = "Comfirm"), align = "right")
    # accordion(
    #   id = "def",
    #   accordionItem(
    #     title = "Node Definition",
    #     solidHeader = F,
    #
    #   ),
    #   accordionItem(
    #     title = "Edge Definition",
    #     solidHeader = F,
    #
    #   )
    # )
  )
}

#' fileUploader Server Functions
#'
#' @noRd
mod_fileUploader_server <- function(id, fn_ingest, fn_valide){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    observe({
      shinyjs::hideElement(id = "pickEdgeSheet")
      shinyjs::hideElement(id = "pickNodeSheet")
      shinyjs::hideElement(id = "go1")
      shinyjs::hideElement(id = "id_col")
      shinyjs::hideElement(id = "from_col")
      shinyjs::hideElement(id = "to_col")
      shinyjs::hideElement(id = "go2")
    })
    data = reactiveValues(
      wb = NULL,
      node = NULL,
      edge = NULL,
      graph = NULL
    )
    observe({
      req(input$file)
      data$wb = NULL
      data$node = NULL
      data$edge = NULL
      data$graph = NULL
      shinyjs::hideElement(id = "pickEdgeSheet")
      shinyjs::hideElement(id = "pickNodeSheet")
      shinyjs::hideElement(id = "go1")
      shinyjs::hideElement(id = "id_col")
      shinyjs::hideElement(id = "from_col")
      shinyjs::hideElement(id = "to_col")
      shinyjs::hideElement(id = "go2")
      shinyjs::hideElement(id = "info")
      updatePickerInput(session = session, "pickEdgeSheet",selected = NULL)
      updatePickerInput(session = session, "pickNodeSheet",selected = NULL)
      updatePickerInput(session = session, "id_col", selected = NULL)
      updatePickerInput(session = session, "from_col", selected = NULL)
      updatePickerInput(session = session, "to_col", selected = NULL)
      ext = tools::file_ext(input$file$datapath)
      if(ext == "xlsx") {
        message("a excel file uploaded")
        data$wb = openxlsx::loadWorkbook(input$file$datapath)
        wb = data$wb
        sheets = openxlsx::sheets(wb)
        shiny::validate(need(length(sheets) >= 1,  "Need at least one sheet!"))
        updatePickerInput(session = session,
                          inputId = "pickEdgeSheet",
                          choices = c(sheets))
        updatePickerInput(session = session,
                          inputId = "pickNodeSheet",
                          choices = c("", sheets),
                          selected = "")
        if("edge" %in% gsub("s$", "" , sheets |> tolower()) ) {
          edge_n = which(gsub("s$", "" ,tolower(sheets)) == "edge")
          updatePickerInput(session = session, "pickEdgeSheet",
                            selected = sheets[edge_n])
        }
        if("node" %in% gsub("s$", "" ,tolower(sheets))) {
          node_n = which(gsub("s$", "" ,tolower(sheets)) == "node")
          updatePickerInput(session = session, "pickNodeSheet",
                            selected = sheets[node_n])
        }
        shinyjs::showElement(id = "pickNodeSheet")
        shinyjs::showElement(id = "pickEdgeSheet")
        shinyjs::showElement(id = "go1")
      } else if(ext == "csv") {
        message("a csv uploaded")
        data$edge = readr::read_csv(input$file$datapath)
      }
    })
    observeEvent(input$go1, {
      req(input$pickEdgeSheet)
      message("writing edge data")
      data$edge = try(openxlsx::read.xlsx(data$wb, sheet = input$pickEdgeSheet))
      if(!is.null(input$pickNodeSheet) & input$pickNodeSheet != "") data$node = try(openxlsx::read.xlsx(data$wb, sheet = input$pickNodeSheet))
      shinyjs::hideElement(id = "pickEdgeSheet")
      shinyjs::hideElement(id = "pickNodeSheet")
      shinyjs::hideElement(id = "go1")
    })
    observe({
      if(!is.null(data$edge)) {
        columns = names(data$edge)
        updatePickerInput(session = session, inputId = "from_col", choices = columns)
        updatePickerInput(session = session, inputId = "to_col", choices = columns)
        if("from" %in% columns) updatePickerInput(session = session, inputId = "from_col", selected = "from")
        if("to" %in% columns) updatePickerInput(session = session, inputId = "to_col", selected = "to")
        shinyjs::showElement(id = "from_col")
        shinyjs::showElement(id = "to_col")
        shinyjs::showElement(id = "go2")
      }
      if(!is.null(data$node)) {
        columns = names(data$node)
        updatePickerInput(session = session, inputId = "id_col", choices = columns)
        shinyjs::toggleElement(id = "id_col")
      }
    })
    graph_rct = eventReactive(input$go2, {
      req(input$from_col)
      req(input$to_col)
      edges = data$edge
      nodes = data$node
      edges = edges |> dplyr::relocate(!!input$from_col, !!input$to_col)
      if(!is.null(input$id_col) && input$id_col != "" && !is.null(nodes)) {
        if(
          (nodes[[input$id_col]] |> length()) != (nodes[[input$id_col]] |> unique() |> length())
          ) {
          bs4Dash::toast("Error", "Node id not unique!",
                         session = session,
                         options = list(
                           class = "bg-reed",
                           autohide = T
                         ))
          shiny::validate("Node id not unique")
        }
        if(
          !all(unique(c(edges[[input$from_col]], edges[[input$to_col]])) %in% nodes[[input$id_col]])
        ) {
          bs4Dash::toast("Error", "Node id not matched to Edge!",
                         session = session,
                         options = list(
                           class = "bg-red",
                           autohide = T
                         ))
          shiny::validate("Un matched column")
        }
        nodes = nodes |> dplyr::relocate(!!input$id_col)
      }

      data$graph = tryCatch({
        igraph::graph_from_data_frame(d = edges, directed = TRUE, vertices = nodes)
      }, error = function(e) {
        shinyWidgets::sendSweetAlert(
          session = session,
          type = "danger",
          title = "Opps!",
          text = "Something wrong converting file "
                       )
      })
      data$graph
    })
    return(graph_rct)
  })
}

## To be copied in the UI
# mod_fileUploader_ui("fileUploader_1")

## To be copied in the server
# mod_fileUploader_server("fileUploader_1")
