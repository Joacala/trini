### set measure line
library(imager)
library(shiny)
library(keys)
library(imager)

### cargar datos
imc <- load.image("02-data\\jaime_pino.png")

line.measured <- function(imc,rsize.per){
require(imager)
  
  im <- resize(imc, rsize.per, rsize.per) # x = width; y =heigh
  imc <- flatten.alpha(imc, bg = "white")# solo para png4
  x <- grayscale(imc, method = "Luma", drop = TRUE)
  x <- t(channels(x, drop = T)[[1]])
  slider.size <- (min(x)-max(x))*(dim(imc)[1]/2/max(x))

  
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
                           actionButton("undo", "Undo")),
                  tabPanel("Smoothing",value="smooth",
                           radioButtons(
                             inputId = "line_type",
                             label = "Target lines",
                             choices = list("Interactive" = "int",
                                            "Single" = "sin",
                                            "Multi" = "mul")),
                           numericInput(inputId = "band_x1", 
                                        label = "Initial band", 
                                        value = round(dim(imc)[1]/2),
                                        min = 0, 
                                        max = dim(imc)[1], 
                                        step = 1),
                           numericInput(inputId = "band_xn", 
                                        label = "Final band", 
                                        value = round(dim(imc)[1]/2),
                                        min = 0, 
                                        max = dim(imc)[1],
                                        step = 1),
                           numericInput(inputId = "band_N", 
                                        label = "N bands", 
                                        value = 1,
                                        min = 1, 
                                        max = dim(imc)[1],
                                        step = 1),
                           numericInput(inputId = "hdm", 
                                        label = "smooth area", 
                                        value = 10,
                                        min = 0, 
                                        max = dim(imc)[2], 
                                        step = 1),
                           # numericInput(inputId = "wdm", 
                           #              label = "Width down", 
                           #              value = 10,
                           #              min = 0, 
                           #              max = dim(imc)[1], 
                           #              step = 1),
                           # numericInput(inputId = "hum", 
                           #              label = "Heigh up", 
                           #              value = 10,
                           #              min = 0, 
                           #              max = dim(imc)[2], 
                           #              step = 1),
                           # numericInput(inputId = "wum", 
                           #              label = "Width up", 
                           #              value = 10,
                           #              min = 0, 
                           #              max = dim(imc)[1], 
                           #              step = 1),
                           numericInput(inputId = "alpha", 
                                        label = "D weight", 
                                        value = 0,
                                        min = 0, 
                                        max = 100, 
                                        step = 1),
                           actionButton("run_smooth", "Run")),
                  tabPanel("Ring detection",value="score.p",
                           sliderInput("score","Score", min = round(slider.size,2), max = 0, round=-2,step=0.01,
                                       value = slider.size/2),
                           numericInput(inputId = "join", 
                                        label = "Merge", 
                                        value = 10,
                                        min = 1, 
                                        max = dim(imc)[2], 
                                        step = 1),
                           numericInput(inputId = "join.inter", 
                                        label = "Cluster resolution", 
                                        value = 10,
                                        min = 1, 
                                        max = dim(imc)[2], 
                                        step = 1),
                           numericInput(inputId = "p.threshold", 
                                        label = "P threshold", 
                                        value = 0.5,
                                        min = 0, 
                                        max = 1, 
                                        step = 0.01),
                           actionButton("run_peak_single", "Run single"),
                           actionButton("run_peak_multi", "Run multi")),
                  tabPanel("Correction", value="corr",
                           actionButton("rese", "Reset"),
                           textInput("file_p", "File name", value = "points.csv"),
                           actionButton("save_p", "Save")),
                  tabPanel("Late wood", value="late",
                           actionButton("run_late", "Run"),
                           actionButton("rese_late", "Reset"),
                           textInput("file_late", "File name", value = "late.csv"),
                           actionButton("save_late", "Save")),
                  tabPanel("Measures", value="measure",
                           textInput("file_dis_ring", "Compute and save distances between rings", value = "distance_ring.csv"),
                           actionButton("dis_ring", "Run & save"),
                           textInput("file_dis_late", "Compute and save distances between ring parts", value = "distance_parts.csv"),
                           actionButton("dis_late", "Run & save"))
          )
      )
      )
    )


  server <- function(input, output) {
    
    addKeys("down", "down")
    addKeys("up", "up")
    addKeys("left", "left")
    addKeys("right", "right")
    addKeys("ctrl", "ctrl")
    addKeys("shift", "shift")
   #addKeys("undo_key","ctrl+z")
    

# functions ---------------------------------------------------------------

    app.plot.img <- function(imc){
       if(is.null(imc)){
        return(NULL)
      }
      if(is.null(ranges$x) | is.null(ranges$y)){
        plot(imc ,xlim=c(dim(imc)[2]/2*-1,dim(imc)[2]/2),ylim=c(dim(imc)[2],0),asp="varying")
      }else{
        plot(imc, xlim=ranges$x,  ylim = c(ranges$y[2], ranges$y[1]),asp="varying")
      }
    }
  
    app.plot.sct <- function(rv){
      # OJO: simplificar codigo repetido
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
          if(is.null(smooth$res)){gst=0}else{gst <- (smooth$res[,1]*(dim(imc)[1]/2))/max(smooth$res)}# OJO: explicar cuando se hace multi que solo se plotea la initial band
          if(is.null(r$m)){yp <- xp <- -1}else{yp <- r$m$y; xp <- r$m$x}
               plot(yp~xp,col=4,pch=3,cex=1.5,
               yaxs="i", xaxs="i",
               xlim=c(dim(imc)[2]/2*-1,dim(imc)[2]/2),ylim=c(dim(imc)[2],0),xlab="",ylab="")
              lines(gst, 1:length(gst),lwd=0.1,col="darkblue")
              abline(v=input$score)
        }else{
          par(bg="transparent")
          if(is.null(smooth$res)){gst=0} else{gst <- (smooth$res[,1]*(dim(imc)[1]/2))/max(smooth$res)}
          if(is.null(r$m)){yp <- xp <- -1}else{yp <- r$m$y; xp <- r$m$x}
               plot(yp~xp,col=4,pch=3,cex=1.5,
               yaxs="i", xaxs="i",
               xlim=ranges$x,  ylim = c(ranges$y[2], ranges$y[1]),xlab="",ylab="") # ylim: importante para que no le de la vuelta a la imagen al hacer zoom
               lines(gst, 1:length(gst),lwd=0.1,col="darkblue")
               abline(v=input$score)
        }
      }
      if(input$tabs=="smooth"){
        if(is.null(r)){
          return(NULL)
        }
        if(is.null(ranges$x) | is.null(ranges$y)){
          par(bg="transparent")
          if(is.null(smooth$res)){gst=0}else{gst <- (smooth$res[,1]*(dim(imc)[1]/2))/max(smooth$res[,1])}
          if(is.null(r$m)){yp <- xp <- -1}else{yp <- r$m$y; xp <- r$m$x}
          plot(yp~xp,col=4,pch="",cex=1.5,
               yaxs="i", xaxs="i",
               xlim=c(dim(imc)[2]/2*-1,dim(imc)[2]/2),ylim=c(dim(imc)[2],0),xlab="",ylab="")
          lines(gst, 1:length(gst),lwd=0.1,col="darkblue")
        }else{
          par(bg="transparent")
          if(is.null(smooth$res)){gst=0} else{gst <- (smooth$res[,1]*(dim(imc)[1]/2))/max(smooth$res[,1])}
          if(is.null(r$m)){yp <- xp <- -1}else{yp <- r$m$y; xp <- r$m$x}
          plot(yp~xp,col=4,pch="",cex=1.5,
               yaxs="i", xaxs="i",
               xlim=ranges$x,  ylim = c(ranges$y[2], ranges$y[1]),xlab="",ylab="") # ylim: importante para que no le de la vuelta a la imagen al hacer zoom
          lines(gst, 1:length(gst),lwd=0.1,col="darkblue")
        }
      }
      if(input$tabs=="late"){ # OJO: cuando se quite un punto late y no haya correspondecia poner un punto
        if(is.null(r)){
          return(NULL)
        }
        
        if(!is.null(late$l)){
          pch.v <- c()
          col.v <- c()
          for(i in 1:(nrow(r$m)-1)){
            if(sum(late$l$y>=r$m$y[i] & late$l$y<=r$m$y[i+1])!=1){
              pch.v[i] <- 19
              col.v[i] <- 1
            }else{pch.v[i] <- 3 ; col.v[i] <- 4}
          }
        }else{pch.v <- 3 ; col.v <- 4}
        
        if(is.null(ranges$x) | is.null(ranges$y)){
          par(bg="transparent")
          plot(r$m$y~r$m$x,col=col.v,pch=pch.v,cex=1.5,
               yaxs="i", xaxs="i",
               xlim=c(dim(imc)[2]/2*-1,dim(imc)[2]/2),ylim=c(dim(imc)[2],0),xlab="",ylab="")
          if(!is.null(late$l)){
            points(late$l$y ~ late$l$x, col=2,pch=3,cex=1.5)
          }
        }else{
          par(bg="transparent")
          plot(r$m$y~r$m$x,col=col.v,pch=pch.v,cex=1.5, 
               yaxs="i", xaxs="i",
               xlim=ranges$x,  ylim = c(ranges$y[2], ranges$y[1]),xlab="",ylab="") 
          if(!is.null(late$l)){
            points(late$l$y ~ late$l$x, col=2,pch=3,cex=1.5)
            }
        }
      }
    }
    band.sel <- function(band, band.end=NA, Nband=NA){
      # OJO: si band.end - band < Nband. Dar aviso
      sel <- round(seq(band,band.end,length=Nband))
      
    }
    
    gaus_decay_w <- function(alpha,t){
      #y(t)~yf+(y0-yf)e-alpha*t
      #t : distance
      1/(1 * exp(alpha * t))
    }
    
    decay.gaus.2d <- function(alpha,th, tv){
      #x <- matrix
      w.h <- gaus_decay_w(alpha,th)
      w.v <- gaus_decay_w(alpha,tv)
      matrix(rep(w.h,each=length(tv)),ncol=length(th))*w.v
      #sum(x*mat.w)/sum(mat.w)
    }
    
    clever.smooth <- function(x, sel, ldm, ldms, lum, lums,  alpha){
      
      #x : ## matrix
      #sel : ## selected bands
      #ldm : ## Heigh dwon mean
      #ldms : ## width dwon mean
      #lum : ## Heigh upper mean
      #lums : ## width upper mean
      #alpha : ## gaussian decay exponent (0 = no decay; 0> increase decay); plot(gaus_decay_w(1:100,alpha))
      
      res <- list()
      x[x==0] <- 1e-10
      for(i in sel){
        #horizontal margins # OJO: si nos pasamos con el area de smooth
        #if(i<(ldms+1)){ldms <- ldms+(i-(ldms+1))}
        #if(ncol(x)< i+(ldms+1)){ldms <- ldms+(ncol(x)-(ldms+1))}
        #if(i-(lums+1)<0){lums <- lums+(i-(lums+1))}
        #if(ncol(x)< i+(lums+1)){lums <- lums+(ncol(x)-(lums+1))}
        
        #vertical margins
        y0 <- (lum+1)
        yf <- (nrow(x)-(ldm+1))
        
        #gaussian weighs
        th <- abs(0:(lums*2)-lums)
        tv <- 0:lum
        dis.up <- decay.gaus.2d(alpha, th, tv)
        
        th <- abs(0:(ldms*2)-ldms)
        tv <- 0:ldm
        dis.dwon <- decay.gaus.2d(alpha, th, tv)
        
        diff <- c()
        diff[1:y0] <- 0
        for(j in y0:yf){
          upper.mean <- sum(x[(j-lum):j,(i-lums):(i+lums)]*dis.up)/sum(dis.up)
          down.mean <- sum(x[j:(j+ldm),(i-ldms):(i+ldms)]*dis.dwon)/sum(dis.dwon)
          diff[j] <- ((down.mean-upper.mean)/upper.mean)#+ down.mean*theta
        }
        diff[(j+1):nrow(x)] <- 0
        res [[length(res)+1]] <- diff
      }
      names(res) <- sel
      do.call(cbind,res)
    }
    
    peaks <- function(x, score, join.dis){
      
      #x : vector where peaks are detected
      #score: thershold to detect a peak
      #join.dis: merge consecutive peaks at a <join.dis pixels
      #late: if late=T, measure late wood
      
      res <- which(x<=score)
      v <- c()
      for(i in 1:length(res)){
        x0 <- res[i]-join.dis; if(x0<1){x0=1}
        xf <- res[i]+join.dis; if(xf>length(x)){xf=length(x)}
        if(sum(x[x0:xf]<x[res[i]],na.rm=T)>0){v[length(v)+1] <- i}
      }			
      res <- res[-v]
    }

    late.f <- function(x,res){# OJO: los puntos corregidos los pone a la altura
      res.o <-res[order(res$y),] 
      res.y <- res.o$y
        
      res.end.y <- c()
        for(i in 1:(length(res.y)-1)){
          id.max <- which.max(x[res.y[i]:res.y[i+1]])
          res.end.y[i] <- c(res.y[i]:res.y[i+1])[id.max]

        }
        id.max <- which.max(x[res.y[length(res.y)]:length(x)])
        res.end.y[length(res.y)] <-c(res.y[length(res.y)]:length(x))[id.max]
        res.end <- data.frame(x=res.o$x,y=res.end.y)
        res.end
    }
    
    clus.peak.bands <- function(x, join.dis, sel){
      
      # x : list of peaks of each band 
      # join.dis: cluster peaks from different bands at <=join.dis height (pixels)
      # sel: selected bands
      
      clus.l <- list()
      i=0
      while (sum(sapply(x,length)>0)>0){
        i=i+1
        
        ti <- x[[i]]
        
        for(j in 1:length(ti)){
          dis <- lapply(x, function(x)abs(x-ti[j]))
          sel.dis <- mapply(function(x,y)y[which(x<=join.dis)],x=dis ,y=x)
          sel.dis <- lapply(sel.dis,function(x){if(length(x)>1){sample(x,1,replace=T)}else{x=x}})
          names(sel.dis) <- sel
          sel.dis<-unlist(sel.dis)
          x <- mapply(function(x,y)y[which(x>join.dis)],x=dis ,y=x)
          
          z=0
          while(z<length(sel.dis)){
            z=z+1
            dis <- lapply(x, function(x)abs(x-sel.dis[z]))
            sel.dis.z <- mapply(function(x,y)y[which(x<=join.dis)],x=dis ,y=x)
            sel.dis.z <- lapply(sel.dis.z,function(x){if(length(x)>1){sample(x,1,replace=T)}else{x=x}})
            names(sel.dis.z) <- sel
            sel.dis.z <- unlist(sel.dis.z)
            sel.dis <- c(sel.dis, sel.dis.z)
            x <- mapply(function(x,y)y[which(x>join.dis)],x=dis ,y=x)
          }
          clus.l[[length(clus.l)+1]]<- sel.dis
        }
      }
      
      clus.l
    }

    find.perpendicular <- function(y,sig.alpha=0.05,sel){
      #y :  vector with clustered points value of function clus.peak.bands
      #sig.alpha : p-value threshold classified regression as significant 
      #perpendicular slope = -1/slope
      #new intercept crossing center = y.center = new inter + perpendicular slope* x.center; new inter = y.center - perpendicular slope* x.center
      
      x <- as.numeric(names(y))
      lms <- summary(lm(y ~ x))
      mode(sel)
      if(lms$coefficients[8]<=sig.alpha & !is.na(lms$coefficients[8])){
        x.center <- mean(sel)
        y.center <- lms$coefficients[1] +  lms$coefficients[2]*x.center
        pendendicular.slope <- -1/lms$coefficients[2]
        new.inter <- y.center - (-1/lms$coefficients[2])*x.center
        res <- c(lms$coefficients[1],lms$coefficients[2],new.inter,pendendicular.slope,x.center,y.center)
        names(res) <- c("intercept","slope","p.intercept","p.slope","x","y")
      }else{
        x.center <- mean(sel)
        y.center <- mean(y)
        pendendicular.slope <- 0
        new.inter <- y.center
        res <- c(y.center,0,new.inter,pendendicular.slope,x.center,y.center)
        names(res) <- c("intercept","slope","p.intercept","p.slope","x","y")
        
      }
      res
    }
    
    intersection.point <- function(x){
      #x : output of find.perpendicular
      x <- x[order(sapply(x,function(x)x["y"]))]
      res <- list()
      for(i in 1:(length(x)-1)){
        x0 <- x[[i]]
        xi <- x[[i+1]]
        if(x0["slope"]==0){
          x.inter <- x0["x"]
          y.inter <- xi["y"]
        }else{
          x.inter <- (x0["p.intercept"] - xi["intercept"])/(xi["slope"]-x0["p.slope"])
          y.inter <- xi["slope"]*x.inter + xi["intercept"]
        }
        res.i <- c(x.inter,y.inter)	
        names(res.i) <- c("x1","y1")
        res[[i]]<-c(x0,res.i)
      }
      res
    }
    
    rings.m <- function(x, prob.threshold, sel, sig.alpha){
      
      #x : value of clus.peak.bands
      #prob.threshold : ring probability of occurring across bands
      #sel : selected bands
      #sig.alpha : p-value threshold clasified regression as significant 
      
      p <- sapply(x,function(x)length(x)/length(sel))
      clus.m <- x[p>prob.threshold]
      fp <- lapply(clus.m, find.perpendicular, sig.alpha, sel)
      ip <- intersection.point(fp)
      data.frame(do.call(rbind,ip))
    }

# reactive ----------------------------------------------------------------
 
    rv = reactiveValues(m=data.frame(x1=NA,y1=NA,x2=NA,y2=NA))# OJO: empieza en NA
    r = reactiveValues(m=NULL)
    r.multi = reactiveValues(m=NULL)
    sf = reactiveValues(r=c(0))
    sel = reactiveValues(sel=c(NA))
    ranges <- reactiveValues(x = NULL, y = NULL)
    smooth = reactiveValues(res = NULL)
    peak = reactiveValues(res = NULL)
    late = reactiveValues(l=NULL)
    


    
# plots -------------------------------------------------------------------
    
    output$plot1 <- renderPlot({
      app.plot.sct(rv)
    })
    
    output$plot2 <- renderPlot({
      app.plot.img(imc) 
    })
    
    output$plot3 <- renderPlot({
      if(input$tabs!="measure"){
      plot(im,xlim=c(dim(im)[2]/2*-1,dim(im)[2]/2),ylim=c(dim(im)[2],0),asp="varying")
      rect(ranges$x[1]/abs(rsize.per), ranges$y[2]/abs(rsize.per), ranges$x[2]/abs(rsize.per), ranges$y[1]/abs(rsize.per))
      }
    })
    
    

# observe events ----------------------------------------------------------
    ## plot management ----------------------------------------------------------
    
    observeEvent(input$plot1_brush, {
      ranges$x <- c(input$plot1_brush$xmin * abs(rsize.per), input$plot1_brush$xmax * abs(rsize.per))
      ranges$y <- c(input$plot1_brush$ymin * abs(rsize.per), input$plot1_brush$ymax * abs(rsize.per))
    })
    
    observeEvent(input$up,{
      if(is.null(ranges$y)){
        ranges$x = c(dim(imc)[2]/2*-1, dim(imc)[2]/2)
        ranges$y = c(0,dim(imc)[2])
      }
      ran <- (ranges$y[2]-ranges$y[1])
      ranges$y[1] <- ranges$y[1]-ran +(ran*0.5)
      ranges$y[2] <- ranges$y[2]-ran +(ran*0.5)
    })
    
    observeEvent(input$down,{
      if(is.null(ranges$y)){
        ranges$x = c(dim(imc)[2]/2*-1, dim(imc)[2]/2)
        ranges$y = c(0,dim(imc)[2])
      }else{
        ran <- (ranges$y[2]-ranges$y[1])
        ranges$y[1] <- ranges$y[1]+ran -(ran*0.5)
        ranges$y[2] <- ranges$y[2]+ran -(ran*0.5)
      }
    })
    
    observeEvent(input$right,{
      if(is.null(ranges$x)){
        ranges$x = c(dim(imc)[2]/2*-1, dim(imc)[2]/2)
        ranges$y = c(0,dim(imc)[2])
      }else{
        ran <- (ranges$x[2]-ranges$x[1])
        ranges$x[1] <- ranges$x[1]+ran -(ran*0.5)
        ranges$x[2] <- ranges$x[2]+ran -(ran*0.5)
        
      }
    })
    
    observeEvent(input$left,{
      if(is.null(ranges$x)){
        ranges$x = c(dim(imc)[2]/2*-1, dim(imc)[2]/2)
        ranges$y = c(0,dim(imc)[2])
      }else{
        ran <- (ranges$x[2]-ranges$x[1])
        ranges$x[1] <- ranges$x[1]-ran +(ran*0.5)
        ranges$x[2] <- ranges$x[2]-ran +(ran*0.5)
      }
    })
    
    observeEvent(input$shift,{
      if(is.null(ranges$y)){
        ranges$x = c(dim(imc)[2]/2*-1, dim(imc)[2]/2)
        ranges$y = c(0,dim(imc)[2])
      }else{
        
        py <- abs(ranges$y[2]-ranges$y[1])
        px <- abs(ranges$x[2]-ranges$x[1])
        ranges$y[1] <- ranges$y[1]+ py*0.1 
        ranges$y[2] <- ranges$y[2]- py*0.1
        ranges$x[1] <- ranges$x[1]+ px*0.1
        ranges$x[2] <- ranges$x[2]- px*0.1
        
      }
    })
    
    observeEvent(input$ctrl,{
      if(is.null(ranges$y)){
        ranges$x = c(dim(imc)[2]/2*-1, dim(imc)[2]/2)
        ranges$y = c(0,dim(imc)[2])
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
        r$m <- r$m[order(r$m$y),]
      }
      if(input$tabs=="late"){
        late$l <- rbind(late$l,unlist(input$plot_click))
      }
      
    })
    dim(imc)
    observeEvent(input$plot_click2, {
      if(input$tabs=="corr"){
        np <- nearPoints(r$m, input$plot_click2, xvar = "x", yvar = "y", allRows = TRUE, maxpoints=1)
        r$m <- r$m[!np$selected_,]
      }
      if(input$tabs=="late"){
        np <- nearPoints(late$l, input$plot_click2, xvar = "x", yvar = "y", allRows = TRUE, maxpoints=1)
        late$l <- late$l[!np$selected_,]
      }
    })
    
    ## lines events ----------------------------------------------------------
    
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
    
    # observeEvent(input$undo_key, {
    #   if(input$tabs=="line"){
    #   if(nrow(rv$m)>0){
    #   if(is.na(rv$m[nrow(rv$m),3])){
    #     rv$m <- rv$m[-nrow(rv$m),]
    #     sf$r <- 0
    #   }else{
    #     rv$m[nrow(rv$m),3:4] <- NA
    #     sf$r <- 1
    #   }}}
    #   # if(input$tabs=="corr"){ # OJO: esto quita a ultima fila pero no el ultimo movimiento
    #   #   if(nrow(r$m)>0){
    #   #   r$m <- r$m[-nrow(r$m),]
    #   #   }
    #   #}
    # })
    # 
    
    ## smooth events ----------------------------------------------------------
    
    observeEvent(input$run_smooth,{  
      if(input$line_type == "int"){sel$sel <- input$band_x1}
      if(input$line_type == "sin"){sel$sel <- input$band_x1}
      if(input$line_type == "mul"){
        sel$sel <- band.sel(input$band_x1,input$band_xn,input$band_N)
      }
      smooth$res <- clever.smooth (x, sel$sel, input$hdm, input$hdm, input$hdm, input$hdm,  input$alpha)
    }
    )
    
    ## ring events ----------------------------------------------------------
    
    observeEvent(input$run_peak_single,{  
      smooth_res <- smooth$res*(dim(imc)[1]/2/max(smooth$res))
      peak_res <- peaks(smooth_res[,1],input$score,input$join)
      r$m <- data.frame(x=input$band_x1, y=peak_res)
    }
    )
    
    observeEvent(input$run_peak_multi,{
      smooth_res <- apply(smooth$res, 2, function(x){x*(dim(imc)[1]/2/max(x))})
      peaks.multi <- apply(smooth_res,2,peaks,input$score,input$join)

      c.peak <- clus.peak.bands (peaks.multi, input$join.inter, sel$sel)
      res <- rings.m(c.peak, input$p.threshold, sel$sel, 0.05)
      r$m <- data.frame(x=res$x, y=res$y)
      r.multi <- data.frame(x=res$x1, y=res$y1)
      
    }
    )
    
    ## correction events ----------------------------------------------------------
    
     observeEvent(input$rese, {
       smooth_res <- smooth$res*(dim(imc)[1]/2/max(smooth$res))
       peak_res <- peaks(smooth_res[,1],input$score,input$join)
       r$m <- data.frame(x=input$band_x1, y=peak_res)
     })
     

    observeEvent(input$save_p, {
      write.table(r$m,input$file_p,sep=" ",row.names=F)
    })
    
    ## late events ----------------------------------------------------------
    
    
    observeEvent(input$save_late, {
      write.table(cbind(r$m,late$l),input$file_late,row.names=F)
      
    })
    
    observeEvent(input$run_late,{  
      late$l <- late.f (smooth$res, r$m)
    }
    )
    
    observeEvent(input$rese_late, {
      late$l <- late.f (smooth$res, r$m)
    })
    
    ## measures events ----------------------------------------------------------
    
    observeEvent(input$dis_ring,{  
      res <- r$m
      res <- res[order(res$y),]
      for(i in 1:(nrow(res)-1)){
          res[i,3] <- dist(res[c(i,i+1),1:2])
      }
      colnames(res)[3] <- "distance"
      write.table(res,input$file_dis_ring,row.names=F)
      }
    )
    
    observeEvent(input$dis_late,{  
      res <- cbind(r$m,late$l)
      colnames(res)[c(3,4)] <-  c("xl","yl")
      res <- res[order(res$y),]
      for(i in 1:(nrow(res)-1)){
        res[i,5] <- dist(res[c(i,i+1),1:2])
        late.i <- res[i,3:4]; colnames(late.i)<- c("x","y")
        early.i <- res[i+1,1:2]; colnames(early.i)<- c("x","y")
        res[i,6] <- dist(rbind(late.i,early.i))
      }
      res[,7] <- apply(res,1,function(x)dist(rbind(c(x[1],x[2]),c(x[3],x[4]))))
      
      colnames(res)[c(5,6,7)] <- c("ring","early","late")
      write.table(res,input$file_dis_late,row.names=F)
    }
    )
    
  }

  shinyApp(ui, server)
  
}


rsize.per <- -10
line.measured(imc,rsize.per)




