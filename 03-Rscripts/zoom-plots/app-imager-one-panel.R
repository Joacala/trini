# https://jfiksel.github.io/2017-02-26-cropping_images_with_a_shiny_app/

library(imager)
library(shiny)

ui <- fluidPage(
  titlePanel("Zoom Image"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose Image",
                accept=c(".png",
                         ".jpg")),
      textInput("imgName", "Image Name", ""),
     ),
    mainPanel(
      plotOutput("plot1", click="plot1_click",
                 dblclick = "plot1_dblclick",
                 brush = brushOpts(
                   id = "plot1_brush",
                   resetOnNew = TRUE
                 ))
    )
  )
)



server <- function(input, output, session) {
  
  ### Function to read in images
  read.image <- function(image.file){
    im <- load.image(image.file)
    if(dim(im)[4] > 3){
      im <- imappend(channels(im, 1:3), 'c')
    }
    im
  }
  
  ### Generic function for plotting the image
  app.plot <- function(im){
    if(is.null(im)){
      return(NULL)
    }
    if(is.null(ranges$x) | is.null(ranges$y)){
      plot(im, xaxt='n', yaxt='n', ann=FALSE)
    }else{
      plot(im, xaxt='n', yaxt='n', ann=FALSE,
           xlim=ranges$x,  ylim = c(ranges$y[2], ranges$y[1])) # ylim: importante para que no le de la vuelta a la imagen al hacer zoom
    }
  }
  
  
  ### Set ranges for zooming
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  ### Code to zoom in on brushed area when double clicking for plot 1
  observeEvent(input$plot1_dblclick, {
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  v <- reactiveValues(
    originalImage = NULL,
    imageName = NULL
  )
  
  ### Read in image
  ### Automatically set image name to file name
  observeEvent(input$file1, {
    v$originalImage <- read.image(input$file1$datapath)
    v$imageName <- gsub("(.jpg|.png)","", input$file1$name)
    updateTextInput(session, inputId = "imgName", label = NULL, value = v$imageName)
    output$plot1 <- renderPlot({
      app.plot(v$originalImage)
    })
  })
  
}


shinyApp(ui, server)
