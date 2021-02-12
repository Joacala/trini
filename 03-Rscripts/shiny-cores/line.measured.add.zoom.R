### set measure line
library(imager)
library(shiny)
#library(shinyKnobs)
library(keys)


line.measured <- function(res.s,imc,rsize.per, gs){
  
  im <- resize(imc, rsize.per, rsize.per) # x = width; y =heigh
  gst <- (gs*(dim(imc)[1]/2))/max(gs)
  
  
  
  ui <- fluidPage(
    useKeys(),
    titlePanel("Tree ring enlightened examination - TREE"),
    
    
    sidebarLayout(
      
    mainPanel(
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
                                click = "plot_click",
                                dblclick = dblclickOpts(id = "plot_click2")),
                     style="position:absolute; top:0; left:0;"))),
      column(3,offset = 0,
             
             plotOutput("plot3",
                        height = 300,
                        width = 300,
                        brush = brushOpts(
                          id = "plot1_brush",
                          fill="",stroke="",
                         resetOnNew = FALSE)),
      )
    )
    ),
    sidebarPanel(
      tabsetPanel(id="tabs",
                  
                  tabPanel("Line measured",value="line",
                           actionButton("undo", "Undo"),
                           textInput("file_l", "File name", value = "lines.csv"),
                           actionButton("save_l", "Save")),
                  tabPanel("Peak score",value="score.p",
                           sliderInput("score","", min = round(min(gst),2), max = 0, round=-2,step=0.01,
                                       value = 0)),
                  tabPanel("Correction", value="corr",
                           actionButton("rese", "Reset"),
                           textInput("file_p", "File name", value = "points.csv"),
                           actionButton("save_p", "Save")
                           
                  )

          )
      )
      )
    )

  
  
  server <- function(input, output) {
    
    addKeys("down", "down")
    addKeys("up", "up")
    addKeys("left", "left")
    addKeys("right", "right")
    addKeys("undok","ctrl+z")
    
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
      if(input$tabs=="line"){
       if(is.null(ranges$x) ){
        par(bg="transparent")
        plot(rv$m$y1~rv$m$x1,col=1,pch=3,cex=1.5,
             yaxs="i", xaxs="i",
             xlim=c(dim(imc)[2]/2*-1,dim(imc)[2]/2),ylim=c(dim(imc)[2],0),xlab="",ylab="")
          #if(sf$r==0){
          points(rv$m$y2~rv$m$x2,pch=3,cex=1.5,col=2)
          segments(rv$m$x1,rv$m$y1,rv$m$x2,rv$m$y2)
          #}
        }else{
        par(bg="transparent")
        plot(rv$m$y1~rv$m$x1,col=1,pch=3,cex=1.5, 
             yaxs="i", xaxs="i",
             xlim=ranges$x,  ylim = c(ranges$y[2], ranges$y[1]),xlab="",ylab="") 
          #if(sf$r==0){
          points(rv$m$y2~rv$m$x2,pch=3,cex=1.5,col=2)
          segments(rv$m$x1,rv$m$y1,rv$m$x2,rv$m$y2)
          #}
        }}
      if(input$tabs=="corr"){
        if(is.null(r)){
          return(NULL)
        }
        if(is.null(ranges$x) | is.null(ranges$y)){
          par(bg="transparent")
          plot(r$m$y~r$m$x,col=4,pch=3,cex=1.5,
               yaxs="i", xaxs="i",
               xlim=c(dim(imc)[2]/2*-1,dim(imc)[2]/2),ylim=c(dim(imc)[2],0),xlab="",ylab="")
        }else{
          par(bg="transparent")
          plot(r$m$y~r$m$x,col=4,pch=3,cex=1.5, 
               yaxs="i", xaxs="i",
               xlim=ranges$x,  ylim = c(ranges$y[2], ranges$y[1]),xlab="",ylab="") # ylim: importante para que no le de la vuelta a la imagen al hacer zoom
        }
      }
      if(input$tabs=="score.p"){
        if(is.null(r)){
          return(NULL)
        }
        if(is.null(ranges$x) | is.null(ranges$y)){
          par(bg="transparent")
          plot(r$m$y~r$m$x,col=4,pch="",cex=1.5,
               yaxs="i", xaxs="i",
               xlim=c(dim(imc)[2]/2*-1,dim(imc)[2]/2),ylim=c(dim(imc)[2],0),xlab="",ylab="")
              lines(gst, 1:length(gst),lwd=0.1,col="darkblue")
              abline(v=input$score)
        }else{
          par(bg="transparent")
          plot(r$m$y~r$m$x,col=4,pch="",cex=1.5,
               yaxs="i", xaxs="i",
               xlim=ranges$x,  ylim = c(ranges$y[2], ranges$y[1]),xlab="",ylab="") # ylim: importante para que no le de la vuelta a la imagen al hacer zoom
               lines(gst, 1:length(gst),lwd=0.1,col="darkblue")
               abline(v=input$score)
        }
      }
    }
    
    
    rv=reactiveValues(m=data.frame(x1=NA,y1=NA,x2=NA,y2=NA))
    r=reactiveValues(m=data.frame(x=res.s$x,y=res.s$y))
    sf=reactiveValues(r=c(0))
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
    
    
    observeEvent(input$plot1_brush, {
      ranges$x <- c(input$plot1_brush$xmin * abs(rsize.per), input$plot1_brush$xmax * abs(rsize.per))
      ranges$y <- c(input$plot1_brush$ymin * abs(rsize.per), input$plot1_brush$ymax * abs(rsize.per))
    })
    
    observeEvent(input$up,{
      if(is.null(ranges$y)){
      }else{
        ran <- (ranges$y[2]-ranges$y[1])
        ranges$y[1] <- ranges$y[1]-ran +(ran*0.5)
        ranges$y[2] <- ranges$y[2]-ran +(ran*0.5)
        
      }
    })
    
    observeEvent(input$down,{
      if(is.null(ranges$y)){
      }else{
        ran <- (ranges$y[2]-ranges$y[1])
        ranges$y[1] <- ranges$y[1]+ran -(ran*0.5)
        ranges$y[2] <- ranges$y[2]+ran -(ran*0.5)
      }
    })
    
    observeEvent(input$left,{
      if(is.null(ranges$y)){
      }else{
        
        py <- abs(ranges$y[2]-ranges$y[1])
        px <- abs(ranges$x[2]-ranges$x[1])
        ranges$y[1] <- ranges$y[1]+ py*0.1 
        ranges$y[2] <- ranges$y[2]- py*0.1
        ranges$x[1] <- ranges$x[1]+ px*0.1
        ranges$x[2] <- ranges$x[2]- px*0.1
        
      }
    })
    
    observeEvent(input$right,{
      if(is.null(ranges$y)){
      }else{
        
        py <- abs(ranges$y[2]-ranges$y[1])
        px <- abs(ranges$x[2]-ranges$x[1])
        ranges$y[1] <- ranges$y[1]- py*0.1 
        ranges$y[2] <- ranges$y[2]+ py*0.1
        ranges$x[1] <- ranges$x[1]- px*0.1
        ranges$x[2] <- ranges$x[2]+ px*0.1
        
      }
    })
    observeEvent(input$plot_click, {
      if(input$tabs=="line"){
      #¸if(input$plot_click$x[1]>0 & input$plot_click$x[2]<dim(imc)[1] & input$plot_click$y[1]>0 & input$plot_click$y[2]<dim(imc)[2]){
      if(sf$r == 0){
        rv$m[nrow(rv$m)+1,1] <- input$plot_click$x
        rv$m[nrow(rv$m),2] <- input$plot_click$y
        sf$r <- sf$r + 1
      }else{
        rv$m[nrow(rv$m),3] <- input$plot_click$x
        rv$m[nrow(rv$m),4] <- input$plot_click$y
        sf$r <- 0
      }}#}
      if(input$tabs=="corr"){
        r$m <- rbind(r$m,unlist(input$plot_click))
      }
    })
    dim(imc)
    observeEvent(input$plot_click2, {
      np <- nearPoints(r$m, input$plot_click2, xvar = "x", yvar = "y", allRows = TRUE, maxpoints=1)
      r$m <- r$m[!np$selected_,]
    })
    
    observeEvent(input$undo, {
      if(nrow(rv$m)>0){
      if(is.na(rv$m[nrow(rv$m),3])){
        rv$m <- rv$m[-nrow(rv$m),]
        sf$r <- 0
      }else{
        rv$m[nrow(rv$m),3:4] <- NA
        sf$r <- 1
      }}
    })
    
    observeEvent(input$undok, {
      if(input$tabs=="line"){
      if(nrow(rv$m)>0){
      if(is.na(rv$m[nrow(rv$m),3])){
        rv$m <- rv$m[-nrow(rv$m),]
        sf$r <- 0
      }else{
        rv$m[nrow(rv$m),3:4] <- NA
        sf$r <- 1
      }}}
      if(input$tabs=="corr"){
        if(nrow(r$m)>0){
        r$m <- r$m[-nrow(r$m),]
        }
      }
    })
    
     observeEvent(input$rese, {
       r$m <- data.frame(x=res.s$x,y=res.s$y)
     })
     
    observeEvent(input$save_l, {
      write.csv(rv$m,input$file_l)
    })
    
    observeEvent(input$save_p, {
      write.csv(r$m,input$file_p)
    })
    
  }
  

  
  shinyApp(ui, server)
  
}

path <- "C:/Users/F541U/Desktop/proyectos/Julen/"
rsize.per <- -10
line.measured(res.s, imc,rsize.per, gs)


getwd()

