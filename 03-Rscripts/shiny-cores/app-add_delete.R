### add and delete points from a scaterplot

library(shiny)

visual.cor <- function(res.s,imc, path){
  
  ui <- fluidPage(

    fluidRow(
      div(id="container",
          height = 500,
          width = 500,
          actionButton("reset", "Reset"),
          actionButton("save", "Save"),
          style="position:relative;",
          div(plotOutput("plot2",
                         height = 500,
                         width = 500),
              style="position:absolute; top:10; left:0;"),
          div(plotOutput("plot1",
                         height = 500,
                         width = 500,
                         dblclick = dblclickOpts(id = "plot_click2"),
                         click = "plot_click"),
              style="position:absolute; top:10; left:0;")
      )
    )
  )

  server <- function(input, output) {
  
    rv=reactiveValues(m=data.frame(x=res.s$x,y=res.s$y))

    output$plot1 <- renderPlot({
      par(bg="transparent")
      plot(rv$m$y~rv$m$x,col=4,pch=3,cex=1.5,ylim=c(1000,0),xlim=c(-600,600),axes=T, yaxs="i", xaxs="i")
    })
    
    output$plot2 <- renderPlot({
      plot(imc,ylim=c(1000,0),xlim=c(-600,600),asp="varying")
    })
  
   observeEvent(input$plot_click2, {
      np <- nearPoints(rv$m, input$plot_click2, xvar = "x", yvar = "y", allRows = TRUE, maxpoints=1)
      rv$m <- rv$m[!np$selected_,]
    })
  
    observeEvent(input$plot_click, {
     rv$m <- rbind(rv$m,unlist(input$plot_click))
    })
  
    observeEvent(input$reset, {
      rv$m <- data.frame(x=res.s$x,y=res.s$y)
    })
  
    observeEvent(input$save, {
      write.csv(rv$m,path)
    })
  
  }
  

  shinyApp(ui, server)

}


path <- "C:\\Users\\F541U\\Desktop\\proyectos\\Julen\\data_shiny.csv" 
visual.cor(res.s,imc, path)


