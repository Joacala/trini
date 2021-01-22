library(shinyjs)
library(imager)
library(shiny)

ui <- fluidPage(
  titlePanel("Zoom Image (upper image controls low image)"),
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose Image",
                accept=c(".png",
                         ".jpg")),
      textInput("imgName", "Image Name", "")
    ),
    mainPanel(
      plotOutput("plot1", height = 200,
                 brush = brushOpts(
                   id = "plot1_brush",
                   resetOnNew = FALSE
                 )
                 ),
      plotOutput("plot2", height = 300)
      )
    )
  )

rsize.per <- -10
server <- function(input, output, session) {
  
  ### function to read & resize in images
  read.image <- function(image.file){
    im <- load.image(image.file)
    if(dim(im)[4] > 3){
      im <- imappend(channels(im, 1:3), 'c')
    }
  }

  
  ### function for plotting the static image
  app.plot.sta <- function(im, x, y){ # negative arguments are interpreted as percentages
    if(is.null(im)){
      return(NULL)
    }
    if(is.null(ranges$x) | is.null(ranges$y)){ # when loading
      im <- resize(im, x, y) # x = width; y =heigh
      plot(im, xaxt='n', yaxt='n', ann=FALSE)
    }else{ # when zooming in
      im <- resize(im, x, y) # x = width; y =heigh
      plot(im, xaxt='n', yaxt='n', ann=FALSE) 
    }
  }
  
  ### function for plotting the dynamic image
    app.plot.dyn <- function(im, x, y){
    if(is.null(im)){
      return(NULL)
    }
    if(is.null(ranges$x) | is.null(ranges$y)){
      #im <- resize(im, x, y) # x = width; y =heigh
      plot(im, xaxt='n', yaxt='n', ann=FALSE)
    }else{
      #im <- resize(im, x, y) # x = width; y =heigh
      plot(im, xaxt='n', yaxt='n', ann=FALSE,
           xlim=ranges$x,  ylim = c(ranges$y[2], ranges$y[1])) # ylim: importante para que no le de la vuelta a la imagen al hacer zoom
    }
  }
  
  ### Set ranges for zooming
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observe({
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin * abs(rsize.per), brush$xmax * abs(rsize.per))
      ranges$y <- c(brush$ymin * abs(rsize.per), brush$ymax * abs(rsize.per))
      
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
      app.plot.sta(v$originalImage, y = rsize.per, x = rsize.per)
    })
    output$plot2 <- renderPlot({
      app.plot.dyn(v$originalImage)
    })
  })
  
}

shinyApp(ui, server)  
  
  

