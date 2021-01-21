### add and delete points from a scaterplot


library(shiny)

ui <- fluidPage(
  plotOutput("plot1", dblclick = dblclickOpts(id = "plot_click2"),click = "plot_click")
)

server <- function(input, output) {
  
  # For storing which rows have been excluded
  rv=reactiveValues(m=data.frame(x=res.s$x,y=res.s$y))
  
  output$plot1 <- renderPlot({
    # Plot the kept and excluded points as two separate data sets
    plot(imc,ylim=c(1000,0))
    points(rv$m$y~rv$m$x,col=4,pch="_",cex=1)
  })
  
  # remove points that are double clicked
  observeEvent(input$plot_click2, {
    np <- nearPoints(rv$m, input$plot_click2, xvar = "x", yvar = "y", allRows = TRUE, maxpoints=1)
    rv$m <- rv$m[!np$selected_,]
  })
  
  # add points that are clicked
  observeEvent(input$plot_click, {
    rv$m <- rbind(rv$m,unlist(input$plot_click))
  })
}


