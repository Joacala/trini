### add and delete points from a scaterplot
imc <- load.image("C:\\Users\\F541U\\Desktop\\proyectos\\Julen\\Pinu_gorria_02_Asier.png")


library(shiny)

visual.cor <- function(res.s,imc, path){
  
  
  ui <- basicPage(
   plotOutput("plot1", dblclick = dblclickOpts(id = "plot_click2"),click = "plot_click"),
   plotOutput("plot2"),
   actionButton("reset", "Reset"),
   actionButton("save", "Save")
   )
  
  ui <- fluidPage(
    
    # Overlapping images in 2 divs inside a "container"
    fluidRow(
      div(id="container",
          height = dim(imc)[2],
          width = dim(imc)[1],
          actionButton("reset", "Reset"),
          actionButton("save", "Save"),
          style="position:relative;",
          div(tags$img(src="C:\\Users\\F541U\\Desktop\\proyectos\\Julen\\Pinu_gorria_02_Asier.png",
                       style=paste0("width:",dim(imc)[1],";height:",dim(imc)[2],";")),
              style="position:absolute; top:0; left:0;"),
          div(plotOutput("plot1", 
                         height = dim(imc)[2],
                         width = dim(imc)[1],
                         dblclick = dblclickOpts(id = "plot_click2"),
                         click = "plot_click"),
              style="position:absolute; top:0; left:0;")
      )
    )
  )

  server <- function(input, output) {
  
    rv=reactiveValues(m=data.frame(x=res.s$x,y=res.s$y))

    output$plot2 <- renderPlot({
    plot(imc,ylim=c(1000,0))# aquí habría que jugar con el zoom
    })
    
    output$plot1 <- renderPlot({
      points(rv$m$y~rv$m$x,col=4,pch="_",cex=1.5)
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

ui <- basicPage(
  plotOutput("plot1", dblclick = dblclickOpts(id = "plot_click2"),click = "plot_click"),
  actionButton("reset", "Reset"),
  actionButton("save", "Save")
)
dim(imc)

fluidPage(
  
  # Overlapping images in 2 divs inside a "container"
  fluidRow(
    div(id="container",
        height = dim(imc)[2],
        width = dim(imc)[1],
        style="position:relative;",
        div(tags$img(src='jaime_pino.jpe',
                     style=paste0("width:",dim(img)[2],";height:",dim(img)[2],";")),
            # plotOutput("plot",
            #              height = dim(img)[1],
            #              width = dim(img)[2],
            #              click = "image_cl1"),
            style="position:absolute; top:0; left:0;"),
        div(plotOutput("plot1",
                       height = dim(imc)[2],
                       width = dim(imc)[1],
                       click = "image_click"),
            style="position:absolute; top:0; left:0;")
    )
  )
)

