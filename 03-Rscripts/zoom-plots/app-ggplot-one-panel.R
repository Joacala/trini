# https://shiny.rstudio.com/gallery/plot-interaction-zoom.html

library(ggplot2)
library(shiny)

ui <- fluidPage(
  fluidRow( # to arrange elements in rows
           h4("Brush and double-click to zoom"), # heading level 4
           plotOutput("plot1",
                      width = 400,
                      height = 400, # add a space to plot the output
                      dblclick = "plot1_dblclick", #  double-clicking
                      brush = brushOpts( # Brushing is clicking and dragging a selection box
                        id = "plot1_brush",
                        resetOnNew = TRUE # see info in the server
                      )
                      )
           )
)


server <- function(input, output, session) {
  # Single zoomable plot
  ranges <- reactiveValues(x = NULL, y = NULL) # creates a list of reactive values to manipulate programatically
  
  output$plot1 <- renderPlot({ # output object 1 + reactive function 1 (i.e. the render function that makes objects to display)
    ggplot(USArrests, aes(UrbanPop, Murder)) + # aprende a utilizar ggplot que ya es hora!
      geom_point() +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE) # I use the reactive values previously created
  })
  
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observeEvent(input$plot1_dblclick, { # triggers code to run on the server. double click and run the code I specify later
    brush <- input$plot1_brush
    if (!is.null(brush)) { # check if there's a brush on the plot.
      ranges$x <- c(brush$xmin, brush$xmax) # If so, zoom to the brush bounds;
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else { # if not, reset the zoom (i.e. zoom out double click without brushing)
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
}



shinyApp(ui, server)