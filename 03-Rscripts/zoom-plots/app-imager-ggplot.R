library(imager)
library(shiny)
library(ggplot2)
library(ggpubr)

# img <- load.image("03-Rscripts/zoom-plots/www/Captura.PNG")
# img <- load.image("03-Rscripts/zoom-plots/www/jaime_pino.png")
# img <- load.image("03-Rscripts/zoom-plots/www/Pinu_gorria_02_Asier.png")

# ABRIR PRIMERO LA IMAGEN CON IMAGER
#### UI ####
ui <- fluidPage(
  # Overlapping images in 2 divs inside a "container"
  fluidRow(
    div(id="container",
        height = dim(img)[1],
        width = dim(img)[2],
        style="position:relative;",
        div(tags$img(src='Pinu_gorria_02_Asier.png',
                     style=paste0("width:",dim(img)[2],";height:",dim(img)[2],";")),
            style="position:absolute; top:0; left:0;"),
        div(plotOutput("plot1",
                       height = dim(img)[2],
                       width = dim(img)[1],
                       click = "image_click"),
            style="position:absolute; top:0; left:0;")
    )
  )
)



### SERVER ####
server <- function(input, output, session) {
  
  output$plot1 <- renderPlot({
      ggplot(USArrests, aes(UrbanPop, Murder)) + # aprende a utilizar ggplot que ya es hora!
        geom_point() +
      theme(
        panel.background = element_rect(fill = "transparent", colour = NA), # bg of the panel
        plot.background = element_rect(fill = "transparent", colour = NA), # bg of the plot
        panel.grid.major = element_blank(), # get rid of major grid
        panel.grid.minor = element_blank(), # get rid of minor grid
        legend.background = element_rect(fill = "transparent", colour = NA), # get rid of legend bg
        legend.box.background = element_rect(fill = "transparent", colour = NA) # get rid of legend panel bg
      )

  },
  bg="transparent")
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

