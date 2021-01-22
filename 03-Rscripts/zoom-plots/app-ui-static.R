#imc <- load.image("03-Rscripts/zoom-plots/www/Pinu_gorria_02_Asier.png")

library(shiny)

ui <- fluidPage(
  
  # Overlapping images in 2 divs inside a "container"
  fluidRow(
    div(id="container",
        height = dim(imc)[2],
        width = dim(imc)[1],
        style="position:relative;",
        div(tags$img(src="Pinu_gorria_02_Asier.png",
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
  
  output$plot1 <- renderPlot({
    plot(rv$m$y~rv$m$x,col=4,pch="_",cex=1.5)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
