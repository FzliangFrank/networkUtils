#' dynamic_attr_selector UI Function
#'
#' @description
#' The dynamic attribute selector take a input vector and
#' based on attributes of vector, render UI based on input data type
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param name a custom label a head
#' @param dataInput a reactive structured data
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @export
mod_dynamic_attr_selector_ui <- function(id, name="an"){
  ns <- NS(id)
  tagList(
    selectInput(ns("AttrName"), sprintf("Specify %s Attribute:",name), NULL),
    uiOutput(ns("AttrUi")),
    div(actionButton(ns("reset"), "Clear",#style="background-color: #BB6464; color: #FFFFFF"
                     ), align="left")
  )
}

#' dynamic_attr_selector Server Functions
#'
#' @export
mod_dynamic_attr_selector_server <- function(
    id,
    dataInput,
    domain = getDefaultReactiveDomain()
    ){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    dataNames = reactiveVal()
    idx = reactiveVal()
    bool_FUN=reactiveVal(value=any) # for putting
    name_placeholder ="<Select Something>"

    observe({
      # This is the Module
      shinyjs::hide(ns("reset"))
      data_names=dataInput() |> names()
      updateSelectInput(inputId="AttrName",choices= c(name_placeholder, data_names))
      dataNames(data_names)
    })
    observeEvent(input$reset,{
      message("Try reset")
      updateSelectInput(inputId="AttrName",selected=name_placeholder)
      idx(NULL)
    })

    Attr=reactive({
      req(input$AttrName)
      if(input$AttrName != name_placeholder) {
        shinyjs::show(ns("reset"))
        f= which(dataNames() == input$AttrName)
        dataInput()[[f]]
      } else if(input$AttrName == name_placeholder) {
        shinyjs::hide(ns("reset"))
      } else {
        NULL
      }
    })
    output$AttrUi <- renderUI({
      golem::print_dev('Creating node UI..')
      AttrLabel=""
      if(Attr() |> is.null()) {
        p("no attribute selected")
      } else {
        if(typeof(Attr())=="double") {
          sliderInput(ns("Attr"), AttrLabel,
                      min=blurry_range(Attr())[1], max=blurry_range(Attr())[2],
                      value=c(quantile(Attr(), 0.33, na.rm = TRUE), quantile(Attr(), 0.66, na.rm = TRUE))
          )
        } else if(inherits(Attr(), "sfc")) {
          plotOutput(ns("AttrPlot"), brush=ns("Attr"),
                     inline=F,
                     height='200px')
        } else if(typeof(Attr()) == "list") {
          flattend_list = purrr::flatten(Attr()) |> as.character()
          tagList(
            selectizeInput(ns("Attr"),
                         AttrLabel,
                         choices=flattend_list,
                         selected=flattend_list[1],
                         multiple=T
                         ),
            shinyWidgets::prettyToggle(
              ns("bool"),
              label_on="All",
              label_off="Any",
              status_off="success",
              status_on="danger",
              value=F
            )
          )
        } else {
          selectizeInput(ns('Attr'),
                         AttrLabel,
                         choices=Attr(),
                         selected=Attr()[1]
          )
        }
      }
    })
    observe(label="spatial data ploter", {# this step has to be after ui is rendered
      Attrs = Attr()
      if(inherits(Attrs, "sfc_POINT")) {
        Attrs = Attrs |> purrr::discard(sf::st_is_empty)
        output$AttrPlot <- renderPlot({
          sf::st_as_sf(Attrs) |> plotPPPdensity()
        }, bg='transparent')
      } else if(inherits(Attr, "sfc")) {
        output$AttrPlot <- renderPlot({
          sf::st_as_sf(Attrs) |> plot()
        })
      } else if(typeof(Attr)== "list") {
        req(input$bool)
        if(input$bool) {
          bool_FUN(all)
        } else {
          bool_FUN(any)
        }
      }
    })
    observe(label = "Matching Index", {
      req(input$Attr)
      req(Attr())
      # DEBUG
      # golem::print_dev(sprintf("Enter node selector: %s", typeof(Attr() )))
      # cur_attr_name = isolate(input$AttrName)
      if(typeof(Attr()) =="double") {
        req(length(input$Attr) == 2) #whenever UI render this notify
        inbond=input$Attr[1]
        outbond=input$Attr[2]
        idxMatched <- which(Attr() >= inbond & Attr() <= outbond)
      } else if(Attr() |> inherits("sfc_POINT")) {
        req(input$Attr |> inherits('list'))
        req(input$Attr$xmin)
        req(input$Attr$xmin)
        message("geometry selected")
        cur = input$Attr
        brash_area = sf::st_bbox(c(xmin =cur$xmin, xmax = cur$xmax,
                                   ymin=cur$ymin, ymax =cur$ymax)) |>
          sf::st_as_sfc()
        idxMatched = sf::st_intersects(sf::st_as_sf(Attr()), brash_area, sparse=F) |> which()
      } else if(Attr() |> inherits("character")) {
        if(length(input$Attr) == 1) {
          req(length(input$Attr) == 1 )
          idxMatched <- which(as.character(Attr()) == input$Attr)
        } else if(length(input$Attr) > 1) {
          idxMatched <- which(as.character(Attr()) %in% input$Attr)
        }
      } else if (typeof(Attr())=="list") {

          terms = input$Attr
          idxMatched <- which(purrr::map_lgl(Attr(), ~ bool_FUN()(terms %in% .x)) )

      } else {
        message('unrecognised input')
        idxMatched = NULL
      }
      idx(idxMatched)
    })
    #-------
    return(idx)
  })
}

## To be copied in the UI
# mod_dynamic_attr_selector_ui("dynamic_attr_selector_1")

## To be copied in the server
# mod_dynamic_attr_selector_server("dynamic_attr_selector_1")
