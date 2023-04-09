library(shiny)
pkgload::load_all()
ui <- fluidPage(
  mod_fileUploader_ui("id")
)

server <- function(input, output, session) {
  mod_fileUploader_server("id")
}

shinyApp(ui, server)
