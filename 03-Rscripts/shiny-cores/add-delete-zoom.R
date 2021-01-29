library(imager)
library(shiny)
#library(shinyKnobs)
library(keys)

im <- resize(imc, -40, -40)
res <- res.s*0.4
visual.cor <- function(res.s,imc, rsize.per, path ){
 
  im <- resize(imc, rsize.per, rsize.per) # x = width; y =heigh
  ui <- fluidPage(
    useKeys(),
    fluidRow(
      column(6,
      div(id="container",
          height = 600,
          width = 600,
             style="position:relative;",
          div(plotOutput("plot2",
                         height = 600,
                         width = 600),
              style="position:absolute; top:0; left:0;"),
          div(plotOutput("plot1",
                         height = 600,
                         width = 600,
                         dblclick = dblclickOpts(id = "plot_click2"),
                         click = "plot_click"),
               style="position:absolute; top:0; left:0;"))),
      column(3,offset = 0,

      plotOutput("plot3",
                     height = 400,
                     width = 400,
                         brush = brushOpts(
                           id = "plot1_brush",
                           fill="",
                           resetOnNew = FALSE)),
      actionButton("reset", "Reset"),
      actionButton("save", "Save"),
          )
       )
      
    )
  
  server <- function(input, output) {
    
    addKeys("down", "down")
    addKeys("up", "up")
    addKeys("left", "left")
    addKeys("right", "right")
    
    ### function for plotting the dynamic image
    app.plot.img <- function(imc){
      if(is.null(imc)){
        return(NULL)
      }
      if(is.null(ranges$x) | is.null(ranges$y)){
        plot(imc ,xlim=c(dim(imc)[2]/2*-1,dim(imc)[2]/2),ylim=c(dim(imc)[2],0),asp="varying")
      }else{
        plot(imc, xlim=ranges$x,  ylim = c(ranges$y[2], ranges$y[1]),asp="varying") # ylim: importante para que no le de la vuelta a la imagen al hacer zoom
      }
    }
 
    app.plot.sct <- function(rv){
      if(is.null(rv)){
        return(NULL)
      }
      if(is.null(ranges$x) | is.null(ranges$y)){
        par(bg="transparent")
        plot(rv$m$y~rv$m$x,col=4,pch=3,cex=1.5,
             yaxs="i", xaxs="i",
             xlim=c(dim(imc)[2]/2*-1,dim(imc)[2]/2),ylim=c(dim(imc)[2],0),xlab="",ylab="")
      }else{
        par(bg="transparent")
        plot(rv$m$y~rv$m$x,col=4,pch=3,cex=1.5, 
             yaxs="i", xaxs="i",
             xlim=ranges$x,  ylim = c(ranges$y[2], ranges$y[1]),xlab="",ylab="") # ylim: importante para que no le de la vuelta a la imagen al hacer zoom
      }
    }
    
    
    rv=reactiveValues(m=data.frame(x=res.s$x,y=res.s$y))
    ranges <- reactiveValues(x = NULL, y = NULL)
    
    output$plot1 <- renderPlot({
      app.plot.sct(rv)
    })
    
    output$plot2 <- renderPlot({
      app.plot.img(imc) 
    })
    
    output$plot3 <- renderPlot({
      plot(im,xlim=c(dim(im)[2]/2*-1,dim(im)[2]/2),ylim=c(dim(im)[2],0),asp="varying")
      rect(ranges$x[1]/abs(rsize.per), ranges$y[2]/abs(rsize.per), ranges$x[2]/abs(rsize.per), ranges$y[1]/abs(rsize.per))
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
    
    observeEvent(input$plot1_brush, {
        ranges$x <- c(input$plot1_brush$xmin * abs(rsize.per), input$plot1_brush$xmax * abs(rsize.per))
        ranges$y <- c(input$plot1_brush$ymin * abs(rsize.per), input$plot1_brush$ymax * abs(rsize.per))
    })
    
    observeEvent(input$up,{
      if(is.null(ranges$y)){
      }else{
        ran <- (ranges$y[2]-ranges$y[1])
        ranges$y[1] <- ranges$y[1]-ran +(ran*0.1)
        ranges$y[2] <- ranges$y[2]-ran +(ran*0.1)
        
      }
    })
    
    observeEvent(input$down,{
      if(is.null(ranges$y)){
      }else{
        ran <- (ranges$y[2]-ranges$y[1])
        ranges$y[1] <- ranges$y[1]+ran -(ran*0.1)
        ranges$y[2] <- ranges$y[2]+ran -(ran*0.1)
      }
    })
    
    observeEvent(input$left,{
      if(is.null(ranges$y)){
      }else{
        ran <- (ranges$y[2]-ranges$y[1])
        ranges$y[1] <- ranges$y[1] + abs(ranges$y[1]*0.1) 
        ranges$y[2] <- ranges$y[2] - abs(ranges$y[2]*0.1)
        
        ranx <- (ranges$x[2]-ranges$x[1])
        ranges$x[1] <- ranges$x[1]+ abs(ranges$x[1]*0.1)
        ranges$x[2] <- ranges$x[2]- abs(ranges$x[2]*0.1)
        
      }
    })
    
    observeEvent(input$right,{
      if(is.null(ranges$y)){
      }else{
        ran <- (ranges$y[2]-ranges$y[1])
        ranges$y[1] <- ranges$y[1] - abs(ranges$y[1]*0.1) 
        ranges$y[2] <- ranges$y[2] + abs(ranges$y[2]*0.1)
        
        ranx <- (ranges$x[2]-ranges$x[1])
        ranges$x[1] <- ranges$x[1]- abs(ranges$x[1]*0.1)
        ranges$x[2] <- ranges$x[2]+ abs(ranges$x[2]*0.1)
        
      }
    })
    
    
  }
  

  1/1.5
  
  
  
  shinyApp(ui, server)
  
}


rsize.per <- -10
 
path <- "C:/Users/F541U/Desktop/proyectos/Julen/kk.csv"
visual.cor(res,im, rsize.per, path)






