library(shiny)
library(ggplot2)
# This shiny module is used specifically for the purpose of any sub-graph structure
# connect panel a to b. This is usefully for any engineer I think to do whatever
# specification they want; Example of Instruct an engineer to connect one to another;

# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(
      HTML("
        #plot {
          cursor: crosshair;
        }
      ")
    ),
    tags$script('
      $(document).on("shiny:connected", function() {
        var clickCount = 0;
        var cursorStyles = ["crosshair", "pointer", "auto"];

        Shiny.addCustomMessageHandler("updateCursor", function(message) {
          var cursorStyle = cursorStyles[message - 1];
          $("#plot").css("cursor", cursorStyle);
                });

        $("#plot").on("click", function() {
          clickCount++;
          var cursorIndex = clickCount % 3;
          Shiny.setInputValue("clickCount", cursorIndex);
        });
      });
    ')
  ),
  shinyWidgets::switchInput("enableDrawing", "Enable Drawing", value = FALSE, onLabel = "On", offLabel = "Off"),
  actionButton("edit", "Edit Drawing"),
  actionButton("clearBtn", "Clear Drawing"),
  plotOutput("plot", click = "plot_click")
)

# Define server
server <- function(input, output, session) {
  # Initialize the data for drawing
  # From commercial point of view I need user to specify what they want to map
  # on an X and Y plain? Also perhaps I need them to change linedraw ()
  # Further more x y needs to be dymamically mapped back to data structure;
  data <- reactiveValues(dots = data.frame(x = numeric(0), y = numeric(0)),
                         lines = data.frame(
                           # FROM
                           # in a splicing schedule y would be mapped to fiber index
                           # x would be mapped to initial position of whatever;
                           x1 = numeric(0), y1 = numeric(0),
                           # TO
                           x2 = numeric(0), y2 = numeric(0)))

  # Update the plot with the dots and lines
  output$plot <- renderPlot({
    ggplot() +
      geom_point(data = data$dots, aes(x, y), alpha = 0.3) +
      ggforce::geom_diagonal(data = data$lines, aes(x = x1, y = y1, xend = x2, yend = y2), size = 1) +
      coord_cartesian(xlim = c(0, 6), ylim = c(0, 8))
  })

  # Track clicks and draw lines
  clickCount <- reactiveVal(0)
  clickPoints <- reactiveValues(x = numeric(0), y = numeric(0))
  clickConditions = reactive({
    input$enableDrawing
    # another condition is input$plot_click$x needs to be within defined axis
  })

  observeEvent(input$plot_click, {
    if(clickConditions()) {
      session$sendCustomMessage("updateCursor", 2)
      clickCount(clickCount() + 1)
      addDot(input$plot_click$x, input$plot_click$y)
      if (clickCount() %% 2 != 0) {
        clickPoints$x <- input$plot_click$x
        clickPoints$y <- input$plot_click$y
        session$sendCustomMessage("updateCursor", 1)
      } else {
        addLine(clickPoints$x, clickPoints$y, input$plot_click$x, input$plot_click$y)
        session$sendCustomMessage("updateCursor", 2)
      }
    }
  })

  # Clear the drawing
  observeEvent(input$clearBtn, {
    data$dots <- data.frame(x = numeric(0), y = numeric(0))
    data$lines <- data.frame(x1 = numeric(0), y1 = numeric(0), x2 = numeric(0), y2 = numeric(0))
  })

  # Function to add a line to the drawing data
  addLine <- function(x1, y1, x2, y2) {
    x1 = round(x1); x2 = round(x2); y1 = round(y1); y2 = round(y2)
    data$lines <- rbind(data$lines, data.frame(x1 = x1, y1 = y1, x2 = x2, y2 = y2))
    # translate in the FAS sheet
    # x1 is from_position , x2 is to_position, y1 is fibre id_from, y2 is fibre id_to
    # x1, x2 in your plot should be fixaged.
  }

  # Function to add a dot to the drawing data
  addDot <- function(x, y) {
    x = round(x); y = round(y) # snap to grid
    data$dots <- rbind(data$dots, data.frame(x = x, y = y))
  }

  # Alternative Function That Dynamically Snap Something
  #...

}

# Run the app
shinyApp(ui, server)
