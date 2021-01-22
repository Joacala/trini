library(shiny)
library(ggplot2)

install.packages("shiny")
kk
ui <- fluidPage("Hello World")
server <- function(input, output) {}
shinyApp(ui = ui, server = server)


ui <- fluidPage(
  sliderInput(inputId = "num",
              label = "Choose a number",
              value = 25, min = 1, max = 100)
)
server <- function(input, output) {}
shinyApp(server = server, ui = ui)

ui <- fluidPage(
  sliderInput(inputId = "num",
              label = "Choose a number",
              value = 25, min = 1, max = 100),
  plotOutput("hist")
)
server <- function(input, output) {}
shinyApp(ui = ui, server = server)


ui <- fluidPage(
  sliderInput(inputId = "num",
              label = "Choose a number",
              value = 25, min = 1, max = 100),
  plotOutput("hist")
)
server <- function(input, output) {
  output$hist <- renderPlot({
    hist(rnorm(input$num))
  })
}
shinyApp(ui = ui, server = server)


library(shiny)

ui <- basicPage(
  plotOutput("plot1", click = "plot_click"),
  verbatimTextOutput("info")
)

server <- function(input, output) {
  output$plot1 <- renderPlot({
    plot(mtcars$wt, mtcars$mpg)
  })
  
  output$info <- renderText({
    paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
  })
}

shinyApp(ui, server)

ui <- basicPage(
  plotOutput("plot1",
             click = "plot_click",
             dblclick = "plot_dblclick",
             hover = "plot_hover",
             brush = "plot_brush"
  ),
  verbatimTextOutput("info")
)

server <- function(input, output) {
  output$plot1 <- renderPlot({
    plot(mtcars$wt, mtcars$mpg)
  })
  
  output$info <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
    }
    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1), 
             " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
    }
    
    paste0(
      "click: ", xy_str(input$plot_click),
      "dblclick: ", xy_str(input$plot_dblclick),
      "hover: ", xy_str(input$plot_hover),
      "brush: ", xy_range_str(input$plot_brush)
    )
  })
}

shinyApp(ui, server)


library(shiny)

ui <- basicPage(
  plotOutput("plot1", click = "plot_click"),
  verbatimTextOutput("info")
)

server <- function(input, output) {
  output$plot1 <- renderPlot({
    plot(imc,ylim=c(1000,0))
    points(res.s[,2]~res.s[,1],col=4,pch="_",cex=0.05)
    for(i in 1:(nrow(res.s)-1)){
      segments(res.s[i,1], res.s[i,2], res.s[i+1,1], res.s[i+1,2],col=4,lwd=0.1,lty=3)
      text(res.s[i,1]+4,(res.s[i,3])/2+res.s[i,2],round(res.s[i,3],2),cex=0.1)
    }
    lines(gs[[1]], 1:length(gs[[1]]),lwd=0.1,col=2)
  })
  
  output$info <- renderText({
    paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
  })
}

shinyApp(ui, server)


library(ggplot2)
library(Cairo)   # For nicer ggplot2 output when deployed on Linux

ui <- fluidPage(
  fluidRow(
    column(width = 6,
           plotOutput("plot1", height = 350,
                      click = "plot1_click",
                      brush = brushOpts(
                        id = "plot1_brush"
                      )
           ),
           actionButton("exclude_toggle", "Toggle points"),
           actionButton("exclude_reset", "Reset")
    )
  )
)

server <- function(input, output) {
  # For storing which rows have been excluded
  vals <- reactiveValues(
    keeprows = rep(TRUE, nrow(mtcars))
  )
  
  output$plot1 <- renderPlot({
    # Plot the kept and excluded points as two separate data sets
    keep    <- mtcars[ vals$keeprows, , drop = FALSE]
    exclude <- mtcars[!vals$keeprows, , drop = FALSE]
    
    ggplot(keep, aes(wt, mpg)) + geom_point() +
      geom_smooth(method = lm, fullrange = TRUE, color = "black") +
      geom_point(data = exclude, shape = 21, fill = NA, color = "black", alpha = 0.25) +
      coord_cartesian(xlim = c(1.5, 5.5), ylim = c(5,35))
  })
  
  # Toggle points that are clicked
  observeEvent(input$plot1_click, {
    res <- nearPoints(mtcars, input$plot1_click, allRows = TRUE)
    
    vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  # Toggle points that are brushed, when button is clicked
  observeEvent(input$exclude_toggle, {
    res <- brushedPoints(mtcars, input$plot1_brush, allRows = TRUE)
    
    vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  # Reset all points
  observeEvent(input$exclude_reset, {
    vals$keeprows <- rep(TRUE, nrow(mtcars))
  })
  
}

shinyApp(ui, server)


library(shiny)
y <- 1:100
x <- 1:100
ui <- basicPage(
  actionButton("submit","submit"),
  plotOutput("plot1", click = "plot_click"),
  verbatimTextOutput("info"),
  tableOutput('table')
)

server <- function(input, output) {
  click_saved <- reactiveValues(singleclick = NULL)
  observeEvent(eventExpr = input$plot_click, handlerExpr = { click_saved$singleclick <- input$plot_click })
  rv=reactiveValues(m=data.frame(x=0,y=0))
  output$plot1 <- renderPlot({
    plot(x,y, type='l')
    points(rv$m$x[-1],rv$m$y[-1])
  })
  
  output$info <- renderText({
    paste0(unlist(click_saved$singleclick))
  })
  
  
  observeEvent(input$submit, {
    if (input$submit > 0) {
      rv$m <- rbind(rv$m,unlist(click_saved$singleclick))
    }
  })
  
  output$table <- renderTable({
    if (is.null(rv$m)) {return()}
    print(rv$m)
  }, 'include.rownames' = FALSE
  , 'include.colnames' = TRUE
  )
  
}

shinyApp(ui, server)



library(ggplot2)
library(jpeg)
library(grid)
library(shiny)

#### pre-run setup ####

# initiate a ggplot theme for use in plotting
# (just getting rid of everything so we only see the image itself)
theme_empty <- theme_bw()
theme_empty$line <- element_blank()
theme_empty$rect <- element_blank()
theme_empty$strip.text <- element_blank()
theme_empty$axis.text <- element_blank()
theme_empty$plot.title <- element_blank()
theme_empty$axis.title <- element_blank()

# set the image input file
image.file <- "C:\\Users\\F541U\\Desktop\\proyectos\\Julen\\jaime_pino.png"

img <- load.image(image.file)

#load.image("C:\\Users\\F541U\\Desktop\\proyectos\\Julen\\jaime_pino.png")
## set up a function for loading an image file as a grob ---------------------
# grob_image <- function(file) {
#   grid::rasterGrob( jpeg::readJPEG(file), interpolate = TRUE )
# }

## load the image as a a grob ---------------------
# img <- grob_image(image.file)

#### UI ####
ui <- fluidPage(
  
  # Overlapping images in 2 divs inside a "container"
  fluidRow(
    div(id="container",
        height = dim(img)[1],
        width = dim(img)[2],
        actionButton("reset", "Reset"),
        actionButton("save", "Save"),
        style="position:relative;",
        div(tags$img(src="C:\\Users\\F541U\\Desktop\\proyectos\\Julen\\jaime_pino.png",
                     style=paste0("width:",dim(img)[2],";height:",dim(img)[1],";")),
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




### SERVER ####
server <- function(input, output, session) {
  
  ## get clicked point coordinates -----------------------
  xy_coord <- reactive(c(input$image_click$x,input$image_click$y))
  
  ## add the new points to the dataframe -----------------
  xy_clicks <- shinySignals::reducePast(xy_coord,
                                        function(x,y){
                                          df <- x
                                          nn <- nrow(df)
                                          
                                          # add values in case of click
                                          if(length(y)>0){
                                            df[nn+1,1 ] <- y[1]
                                            df[nn+1,2 ] <- y[2]
                                          }
                                          return(df)
                                        },
                                        init=data.frame(x_coord=numeric(0),
                                                        y_coord=numeric(0)))
  
  ## render plot of the jpeg image --------------------------------------
  # output$plot <- renderPlot({
  #   ggplot()+
  #     geom_blank(data = data.frame(x = c(0, dim(img$raster)[2])
  #                                  , y = c(0, dim(img$raster)[1])),
  #                mapping = aes(x = x, y = y))+
  #     theme_empty +
  #     annotation_custom(grob = img)
  # })
  
  # alternative for plot of the jpeg image
  # output$plot <- renderPlot({
  #   # plot_jpeg("survey.jpg")
  # })
  
  
  ## re-render the plot with the new data -------------------------
  output$plot1 <- renderPlot({
    ggplot() +
      geom_blank(data = data.frame(x = c(0,dim(img)[2])
                                   ,y = c(0,dim(img)[1])),
                 mapping = aes(x = x,
                               y = y))+
      theme_empty+
      geom_point(data = xy_clicks(),
                 mapping = aes(x = x_coord,
                               y = y_coord),
                 colour = "red")+
      coord_cartesian(xlim = c(0,dim(img)[2]),
                      ylim= c(0,dim(img)[1]))
    
  },
  bg="transparent")
  
}


## uncomment and add verbatimTextOutput("txt") in UI to see the xy_clicks() dataframe
# output$txt <- renderPrint(xy_clicks())

# Run the application 
shinyApp(ui = ui, server = server)
