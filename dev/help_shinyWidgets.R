# shinyWidgets::demoVirtualSelect()
# # install.packages("shinydashboard")
# shinyWidgetsGallery()
if (interactive()) {
  library(shiny)
  library(shinyjs)
  shinyApp(
    ui = fluidPage(
      shinyjs::useShinyjs(),  # Set up shinyjs
      actionButton("btn", "Click me"),
      hidden(
        p(id = "element", "I was born invisible")
      )
    ),
    server = function(input, output) {
      observeEvent(input$btn, {
        show("element")
      })
    }
  )
}

if (interactive()) {
  library(shiny)

  shinyApp(
    ui = fluidPage(
      useShinyjs(),  # Set up shinyjs
      actionButton("btn", "Click me"),
      textInput("text", "Text")
    ),
    server = function(input, output) {
      observeEvent(input$btn, {
        # Change the following line for more examples
        toggle("text")
      })
    }
  )
}
