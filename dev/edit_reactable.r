library(shiny)
library(reactable)
library(dplyr)
# library(crosstalk)

ui <- fluidPage(
  actionButton(inputId = "edit",
              label = "Edit"),
  reactableOutput("table")
)

server <- function(input, output, session) {

  data <- mtcars
  data$model <- rownames(data)
  rownames(data) <- NULL
  data <- data |> select(model, everything())

  values <- reactiveValues(dataframe = data,
                           modal_closed = NULL)

  selected_row <- reactive({
    getReactableState("table")$selected
  })

  observeEvent(input$edit, {
    if (!is.null(selected_row())) {
      labels <- colnames(values$dataframe)
      values$modal_closed = FALSE
      # This model will open a serverside window..
      showModal(
        modalDialog(
        title = "Edit Values",
        p(values$dataframe[selected_row(), ]),
        textInput(inputId = labels[1],
                  label = labels[1],
                  value = values$dataframe[selected_row(), labels[1]]),
        textInput(inputId = labels[2],
                  label = labels[2],
                  value = values$dataframe[selected_row(), labels[2]]),
        textInput(inputId = labels[3],
                  label = labels[3],
                  value = values$dataframe[selected_row(), labels[3]]),
        easyClose = FALSE,
        footer = actionButton("save", "Save") # guessing this needs to be saved
        )
      )
    }
  })

  observeEvent(input$save, {
    values$modal_closed <- TRUE
    removeModal()
  })

  observeEvent(values$modal_closed, {
    if (values$modal_closed == TRUE) {
      values$dataframe[selected_row(), "model"] <- input$model
      values$dataframe[selected_row(), "cyl"] <- input$cyl
      values$dataframe[selected_row(), "mpg"] <- input$mpg
    }
  })

  reactable_table <- reactive({
    reactable(values$dataframe,
              selection = "single")
  })

  output$table <- renderReactable(
    reactable_table()
  )
}

shinyApp(ui, server)
