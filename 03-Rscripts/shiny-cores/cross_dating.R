library(shiny)
library(keys)


ui <- (fluidPage(
  useKeys(),
      plotOutput(outputId = "plot",
                 click = "plot_click",
                 dblclick = dblclickOpts(id = "plot_click2"),
                 ),
    hr(),
    fluidRow(
      column(3,
      tabPanel("Load data 1",value="load_data_1",
               textInput("load_file_1", "Load ring data 1", value = "name.csv"),
               textInput("sep_1", "Separator", value = ";"),
               actionButton("load_1", "Load")),
      numericInput(inputId = "Ncore1", 
                   label = "Number of cores master 1", 
                   value = 1,
                   step = 1)
      ),
      column(4,
      tabPanel("Load data 2",value="load_data_2",
               textInput("load_file_2", "Load ring data 2", value = "name.csv"),
               textInput("sep_2", "Separator", value = ";"),
               actionButton("load_2", "Load")),
      numericInput(inputId = "Ncore2", 
                   label = "Number of cores master 2", 
                   value = 1,
                   step = 1)
      ),
      column(5,
      radioButtons(
        inputId = "sel_core",
        label = "Select core",
        choices = list("1" = "1",
                       "2" = "2")),
      actionButton("mean", "Mean"),
      textInput("name", "Name master", value = "name.csv"),
      actionButton("save", "Save master")
    ))
  )
)


server <- function(input, output) {
  
  addKeys("down", "down")
  addKeys("up", "up")
  addKeys("left", "left")
  addKeys("right", "right")
  addKeys("o", "o")
  addKeys("i", "i")
  addKeys("p", "p")
  
  ring.data.1 = reactiveValues(m=NULL)
  ring.data.2 = reactiveValues(m=NULL)
  mean.data = reactiveValues(m=NULL)
  ranges = reactiveValues(x = NULL, y = NULL)
 
  
  output$plot <- renderPlot({
    
    if(!c(is.null(ring.data.1$m) | is.null(ring.data.2$m))){
      
      if(is.null(ranges$x)){
        ranges$x <- c(min(c(ring.data.1$m$year,ring.data.2$m$year),na.rm=T), max(c(ring.data.1$m$year,ring.data.2$m$year),na.rm=T))
        ranges$y <- c(min(c(ring.data.1$m$distance,ring.data.2$m$distance),na.rm=T), max(c(ring.data.1$m$distance,ring.data.2$m$distance),na.rm=T))
      }
      
      plot(ring.data.1$m$year, ring.data.1$m$distance, xlim=ranges$x, ylim=ranges$y, col=1, pch=19, xlab="Year", ylab="Ring width",xaxt="n")
      lines(ring.data.1$m$year, ring.data.1$m$distance, col=1)
      points(ring.data.2$m$year, ring.data.2$m$distance, col=2, pch=19)
      lines(ring.data.2$m$year, ring.data.2$m$distance, col=2)
      axis(1, at = seq(round(ranges$x[1]),round(ranges$x[2]), by=5), seq(round(ranges$x[1]),round(ranges$x[2]), by=5))
    }
    if(!is.null(mean.data$m)){
      lines(mean.data$m$year, mean.data$m$distance, col=4, lwd=2, lty=2)
    }
  })
 
  observeEvent(input$load_1, {
    dat <- tryCatch( expr = {
      read.csv(input$load_file_1,sep = input$sep_1,check.names=F)
    },
    error = function(e){showNotification("The file does not exist", duration = 10, type="error")
      NULL
    })
    if(!is.null(dat)){
      if(ncol(dat)<2){
        showNotification("Please verify the separator is correct", duration = 10, type="error")
      }else{
        ring.data.1$m=NULL 
        ring.data.1$m <- dat[,c("year","distance(cm)")]
        colnames(ring.data.1$m)[2] <- "distance"
        ring.data.1$m <- ring.data.1$m[!is.na(rowSums(ring.data.1$m)),]
      }
    }
  })
  
  observeEvent(input$load_2, {
    dat <- tryCatch( expr = {
      read.csv(input$load_file_2,sep = input$sep_2,check.names=F)
    },
    error = function(e){showNotification("The file does not exist", duration = 10, type="error")
      NULL
    })
    if(!is.null(dat)){
      if(ncol(dat)<2){
        showNotification("Please verify the separator is correct", duration = 10, type="error")
      }else{
        ring.data.2$m=NULL 
        ring.data.2$m <- dat[,c("year","distance(cm)")]
        colnames(ring.data.2$m)[2] <- "distance"
        ring.data.2$m <- ring.data.2$m[!is.na(rowSums(ring.data.2$m)),]
      }
    }
  })
  
  observeEvent(input$plot_click, {
    if(input$sel_core == "1"){
      ring.data.1$m[nrow(ring.data.1$m)+1,"year"] <- round(as.numeric(input$plot_click$x))
      ring.data.1$m[nrow(ring.data.1$m),"distance"] <- as.numeric(input$plot_click$y)
      ring.data.1$m <- ring.data.1$m[order(ring.data.1$m$year),]
    }
    if(input$sel_core == "2"){
      ring.data.2$m[nrow(ring.data.2$m)+1,"year"] <- round(as.numeric(input$plot_click$x))
      ring.data.2$m[nrow(ring.data.2$m),"distance"] <- as.numeric(input$plot_click$y)
      ring.data.2$m <- ring.data.2$m[order(ring.data.2$m$year),]
    }
  })
  
  observeEvent(input$plot_click2, {
    if(input$sel_core == "1"){
        np <- nearPoints(ring.data.1$m, input$plot_click2, xvar = "year", yvar = "distance", allRows = TRUE, maxpoints=1)
        ring.data.1$m <- ring.data.1$m[!np$selected_,]
    }
    if(input$sel_core == "2"){
      np <- nearPoints(ring.data.2$m, input$plot_click2, xvar = "year", yvar = "distance", allRows = TRUE, maxpoints=1)
      ring.data.2$m <- ring.data.2$m[!np$selected_,]
    }
  })
  
  observeEvent(input$mean, {
    year.id <- sort(unique(c(ring.data.1$m$year, ring.data.2$m$year)))
    distance.1 <- ring.data.1$m[match(year.id, ring.data.1$m$year),"distance"]
    distance.2 <- ring.data.2$m[match(year.id, ring.data.2$m$year),"distance"]
    total <- input$Ncore1 + input$Ncore2
    mean <- apply(cbind(distance.1, distance.2), 1,function(x){
      if(sum(is.na(x)) == 0){
        x[1]*(input$Ncore1/total) + x[2]*(input$Ncore2/total)
      }else{
        x[!is.na(x)]
      }
     }
    )
    mean.data$m <- data.frame(year= year.id, distance= mean)  
  })
  observeEvent(input$save, {
    write.csv(mean.data$m, input$name, row.names=F)
  })
  
  observeEvent(input$up,{
    ran <- (ranges$y[2]-ranges$y[1])
    ranges$y[1] <- ranges$y[1]+ran -(ran*0.9)
    ranges$y[2] <- ranges$y[2]+ran -(ran*0.9)
  })
  
  observeEvent(input$down,{
      ran <- (ranges$y[2]-ranges$y[1])
      ranges$y[1] <- ranges$y[1]-ran +(ran*0.9)
      ranges$y[2] <- ranges$y[2]-ran +(ran*0.9)
  })
  
  observeEvent(input$right,{
      ran <- (ranges$x[2]-ranges$x[1])
      ranges$x[1] <- ranges$x[1]+ran -(ran*0.9)
      ranges$x[2] <- ranges$x[2]+ran -(ran*0.9)
    })

  observeEvent(input$left,{
      ran <- (ranges$x[2]-ranges$x[1])
      ranges$x[1] <- ranges$x[1]-ran +(ran*0.9)
      ranges$x[2] <- ranges$x[2]-ran +(ran*0.9)
  })
  
  observeEvent(input$i,{
      py <- abs(ranges$y[2]-ranges$y[1])
      px <- abs(ranges$x[2]-ranges$x[1])
      ranges$y[1] <- ranges$y[1]+ py*0.1 
      ranges$y[2] <- ranges$y[2]- py*0.1
      ranges$x[1] <- ranges$x[1]+ px*0.1
      ranges$x[2] <- ranges$x[2]- px*0.1
  })
  
  observeEvent(input$o,{
      py <- abs(ranges$y[2]-ranges$y[1])
      px <- abs(ranges$x[2]-ranges$x[1])
      ranges$y[1] <- ranges$y[1]- py*0.1 
      ranges$y[2] <- ranges$y[2]+ py*0.1
      ranges$x[1] <- ranges$x[1]- px*0.1
      ranges$x[2] <- ranges$x[2]+ px*0.1
  })
  
  observeEvent(input$p,{
    ranges$x <- c(min(c(ring.data.1$m$year,ring.data.2$m$year),na.rm=T), max(c(ring.data.1$m$year,ring.data.2$m$year),na.rm=T))
    ranges$y <- c(min(c(ring.data.1$m$distance,ring.data.2$m$distance),na.rm=T), max(c(ring.data.1$m$distance,ring.data.2$m$distance),na.rm=T))
  })
}

shinyApp(ui, server)

