### set measure line
library(imager)
library(shiny)
library(keys)


line.measured <- function(imc,rsize.per,name){
require(imager)
  
  im <- resize(imc, rsize.per, rsize.per) # x = width; y =heigh
  imc <- flatten.alpha(imc, bg = "white")# solo para png4
  x <- grayscale(imc, method = "Luma", drop = TRUE)
  x <- t(channels(x, drop = T)[[1]])
  #x <- x[c((nrow(x)-50):nrow(x)),]#para borrar
  qs <- 1
  slider.size <- (min(x)-max(x))*(dim(imc)[1]/2/max(x))
  time.c <- Sys.time()
  
  ui <- fluidPage(
    useKeys(),
    titlePanel(paste("tring... measuring:",name)),
    
    
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
                  # tabPanel("Load data",value="load_data",
                  #          actionButton("start_time", "Start Measure time")),
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
                                        value = round(dim(imc)[1]-dim(imc)[1]*0.1),
                                        min = 0, 
                                        max = dim(imc)[1],
                                        step = 1),
                           numericInput(inputId = "band_N", 
                                        label = "N bands", 
                                        value = 10,
                                        min = 2, 
                                        max = dim(imc)[1],
                                        step = 1),
                           numericInput(inputId = "hdm", 
                                        label = "smooth area", 
                                        value = round(dim(imc)[1]*0.1),
                                        min = 0, 
                                        max = dim(imc)[2], 
                                        step = 1),
                         # numericInput(inputId = "alpha", 
                         #                label = "D weight", 
                         #                value = 0,
                         #                min = 0, 
                         #                max = 100, 
                         #                step = 1),
                         numericInput(inputId = "qv", 
                                      label = "smooth visualization", 
                                      value = 0.95,
                                      min = 0.1, 
                                      max = 1, 
                                      step = 0.01),
                         numericInput(inputId = "show.band", 
                                      label = "Show band (if multi)", 
                                      value = 1,
                                      min = 1, 
                                      max = dim(imc)[1], 
                                      step = 1),
                               actionButton("run_smooth", "Run"),
                           actionButton("save_kk", "save")),
                  tabPanel("Ring detection",value="score.p",
                           sliderInput("score","Score", min = round(slider.size+(0.1*slider.size),2), max = 0, round=-2,step=0.01,
                                       value = slider.size/2),
                           numericInput(inputId = "join", 
                                        label = "Merge", 
                                        value = 10,
                                        min = 1, 
                                        max = dim(imc)[2], 
                                        step = 1),
                           # numericInput(inputId = "join.inter", 
                           #              label = "Cluster resolution", 
                           #              value = 10,
                           #              min = 1, 
                           #              max = dim(imc)[2], 
                           #              step = 1),
                           actionButton("run_peak_int", "Detect interactive"),
                           actionButton("run_peak_single", "Detect single"),
                           actionButton("run_peak_multi", "Detect multi"),
                           sliderInput("prob","Probability", min = 0, max = 1, round=-2,step=0.01,
                              value = 0.5),
                           actionButton("select_multi", "Select multi")),
                  
                  tabPanel("Correction", value="corr",
                           actionButton("rese", "Reset"),
                           radioButtons(
                             inputId = "cor_type",
                             label = "Correction type",
                             choices = list("Single" = "single",
                                            "Interactive" = "int",
                                            "Interactive overlap" = "over",
                                            "Multi" = "multi"))),
                  tabPanel("Late wood", value="late",
                           actionButton("run_late", "Run"),
                           actionButton("rese_late", "Reset")),
                  tabPanel("Measures", value="measure",
                           radioButtons(
                             inputId = "save_type",
                             label = "Save type",
                           choices = list("Single" = "single",
                                          "Interactive" = "int",
                                          "Multi" = "multi")),
                           numericInput(inputId = "year", 
                                        label = "Final year", 
                                        value = NA,
                                        step = 1),
                           numericInput(inputId = "ppp", 
                                        label = "Image resolution (ppp)", 
                                        value = NA,
                                        step = 1),
                           textInput("file_dis_ring", "Compute and save distances between rings", value = paste(name,"distance_ring.csv")),
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
    addKeys("o", "o")
    addKeys("i", "i")
   #addKeys("undo_key","ctrl+z")
    

# functions ---------------------------------------------------------------

    app.plot.img <- function(imc){
       if(input$tabs!="load_data" | !is.null(imc)){
        if(is.null(ranges$x) | is.null(ranges$y)){
          plot(imc ,xlim=c(dim(imc)[2]/2*-1,dim(imc)[2]/2),ylim=c(dim(imc)[2],0),asp="varying")
        }else{
        plot(imc, xlim=ranges$x,  ylim = c(ranges$y[2], ranges$y[1]),asp="varying")
        }
       }
    }
  
    app.plot.sct <- function(rv){
      
      if(is.null(ranges$x) | is.null(ranges$y)){
        xli <- c(dim(imc)[2]/2*-1,dim(imc)[2]/2)
        yli <- c(dim(imc)[2],0)
      }else{
        xli <- ranges$x
        yli <- c(ranges$y[2], ranges$y[1])
      }
      
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
        
        par(bg="transparent")

        if(input$cor_type == "single"){        
          plot(r$m$y~r$m$x,pch=3,cex=1.5,col=4,
               yaxs="i", xaxs="i",
               xlim=xli ,ylim=yli,xlab="",ylab="")
        }
        if(input$cor_type == "multi"){
            plot(r.multi$m$y~r.multi$m$x,pch=3,cex=1.5,
               yaxs="i", xaxs="i",
               xlim=xli ,ylim=yli,xlab="",ylab="")
            apply(r.multi$m,1,function(x)segments(x[9],x[1]+x[2]*x[9],x[10],x[1]+x[2]*x[10],lwd=0.05,lty=1,col=1))
            points(r.multi$m$y~r.multi$m$x,pch=3,cex=1.5,col=4)
        }
        if(input$cor_type == "int"){        
          plot(r$m$y~r$m$x,pch=3,cex=1.5,col=r$m$line,
               yaxs="i", xaxs="i",
               xlim=xli ,ylim=yli,xlab="",ylab="")
        }
        if(input$cor_type == "over"){     
              plot(r$m$y~r$m$x,pch=3,cex=1.5,col=r$m$pair+1,
               yaxs="i", xaxs="i",
               xlim=xli ,ylim=yli,xlab="",ylab="")
        }
      }      
      if(input$tabs=="score.p"){

          par(bg="transparent")
          if(is.null(smooth$res)){gst=0} else{gst <- (smooth$res[,show.band$sb]*(dim(imc)[1]/2))/quantile(smooth$res[,show.band$sb],input$qv)}
   
          if(input$line_type=="sin"){
           if(is.null(r$m)){yp <- xp <- -1}else{yp <- r$m$y; xp <- r$m$x}
               plot(yp~xp,col=4,pch=3,cex=1.5,
               yaxs="i", xaxs="i",
               xlim=xli,  ylim = yli,xlab="",ylab="") 
               lines(gst, 1:length(gst),lwd=0.1,col="darkblue")
               abline(v=input$score)
          }
        
        if(input$line_type=="int"){
          if(is.null(smooth.int$res)){gst=0}else{gst <- apply(smooth.int$res,2, function(x)x*(dim(imc)[1]/2)/quantile(x,input$qv,na.rm=T))}
          if(is.null(r$m)){yp <- xp <- -1}else{yp <- r$m$y; xp <- r$m$x}
          plot(yp~xp,col=4,pch=3,cex=1.5,
               yaxs="i", xaxs="i",
               xlim=xli,  ylim = yli,xlab="",ylab="") 
          apply(gst,2,function(x)lines(x, 1:length(x),lwd=0.1,col="darkblue"))
          abline(v=input$score)
          }
        
        if(input$line_type=="mul"){
          if(is.null(r.multi$m)){yp <- xp <- -1}else{yp <- r.multi$m$y; xp <- r.multi$m$x}
          if(is.null(p$pval)){prob=dim(imc)[1];y=0}else{prob=p$pval$prob*dim(imc)[1]+dim(imc)[1];y<-p$pval$y}
               plot(yp~xp,col=4,pch=3,cex=1.5,
               yaxs="i", xaxs="i",
               xlim=xli,  ylim = yli,xlab="",ylab="") 
               lines(gst, 1:length(gst),lwd=0.1,col="darkblue")
               abline(v=input$score)
               segments(dim(imc)[1],y,prob,y,lwd=0.1,col=2)
               axis(side = 3, at = c(dim(imc)[1], dim(imc)[1]*2), labels = c(0,1))
               abline(v=input$prob*dim(imc)[1]+dim(imc)[1])
        }
               
      }
      if(input$tabs=="smooth"){
        if(is.null(r)){
          return(NULL)
        }
       
          par(bg="transparent")
          if(input$line_type!="int"){
            if(is.null(smooth$res)){gst=0}else{gst <- (smooth$res[,show.band$sb]*(dim(imc)[1]/2))/quantile(smooth$res[,show.band$sb],input$qv,na.rm=T)}
            if(is.null(r$m)){yp <- xp <- -1}else{yp <- r$m$y; xp <- r$m$x}
            plot(yp~xp,col=4,pch="",cex=1.5,
               yaxs="i", xaxs="i",
               xlim=xli,  ylim = yli,xlab="",ylab="") 
            lines(gst, 1:length(gst),lwd=0.1,col="darkblue")
            abline(v=input$band_x1,lwd=0.1,lty=2)
            if(input$line_type=="mul"){abline(v=input$band_xn,lwd=0.1,lty=2)}
              
          }
           else{
            if(is.null(smooth.int$res)){gst=0}else{gst <- apply(smooth.int$res,2, function(x)x*(dim(imc)[1]/2)/quantile(x,input$qv,na.rm=T))}
             if(is.null(r$m)){yp <- xp <- -1}else{yp <- r$m$y; xp <- r$m$x}
             plot(yp~xp,col=4,pch="",cex=1.5,
                  yaxs="i", xaxs="i",
                  xlim=xli,  ylim = yli,xlab="",ylab="")
             apply(gst,2,function(x)lines(x, 1:length(x),lwd=0.1,col="darkblue"))
         }
      }
      if(input$tabs=="late"){ 
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
      if(input$tabs=="measure"){
        if(is.null(r)){
          return(NULL)
        }

        par(bg="transparent")
        
        if(input$save_type %in% c("single","int")){        
          plot(r$m$y~r$m$x,pch=3,cex=1.5,col=4,
               yaxs="i", xaxs="i",
               xlim=xli ,ylim=yli,xlab="",ylab="")
        }
        if(input$save_type == "multi"){
          plot(r.multi$m$y~r.multi$m$x,pch="",cex=1.5,
               yaxs="i", xaxs="i",
               xlim=xli ,ylim=yli,xlab="",ylab="")
          apply(r.multi$m,1,function(x)segments(x[9],x[1]+x[2]*x[9],x[10],x[1]+x[2]*x[10],lwd=0.05,lty=1,col=1))
          apply(r.multi$m,1,function(x)segments(x[5],x[6],x[7],x[8],lwd=0.05,lty=1,col=2))
          
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

        #vertical margins
        y0 <- (lum+1)
        yf <- (nrow(x)-(ldm+1))
        
        # #gaussian weighs
        # th <- abs(0:(lums*2)-lums)
        # tv <- 0:lum
        # dis.up <- decay.gaus.2d(alpha, th, tv)
        # 
        # th <- abs(0:(ldms*2)-ldms)
        # tv <- 0:ldm
        # dis.dwon <- decay.gaus.2d(alpha, th, tv)
        

        if( i-lums <1){ul = i-1}else{ul = lums}
        if( i+lums > ncol(x)){ur = ncol(x)-i}else{ur = lums}
        if( i-ldms <1){dl = i-1}else{dl = ldms}
        if( i+ldms > ncol(x)){dr = ncol(x)-i}else{dr = ldms}

        diff <- c()
        diff[1:y0] <- 0
        for(j in y0:yf){
          upper.mean <- mean(x[(j-lum):j,(i-ul):(i+ur)]) #sum(x[(j-lum):j,(i-ul):(i+ur)]*dis.up)/sum(dis.up)
          down.mean <- mean(x[j:(j+ldm),(i-dl):(i+dr)]) #sum(x[j:(j+ldm),(i-dl):(i+dr)]*dis.dwon)/sum(dis.dwon)
          diff[j] <- ((down.mean-upper.mean)/upper.mean)#+ down.mean*theta
        }
        diff[(j+1):nrow(x)] <- 0
        res [[length(res)+1]] <- diff
      }
      names(res) <- sel
      do.call(cbind,res)
      
    }
    
    clever.smooth.interactive <- function(x, line, ldm, ldms, lum, lums,  alpha){
      #x : ## matrix
      #line : ## rv$m
      #ldm : ## Heigh dwon mean
      #ldms : ## width dwon mean
      #lum : ## Heigh upper mean
      #lums : ## width upper mean
      #alpha : ## gaussian decay exponent (0 = no decay; 0> increase decay); plot(gaus_decay_w(1:100,alpha))
      
      line <- as.numeric(line)
      res <- list()
      x[x==0] <- 1e-10
        #vertical margins
        if(line[2]<line[4]){y0=line[2];yf=line[4]}else{y0=line[4];yf=line[2]}
      
        if(yf>nrow(x)){yf = nrow(x)}
        if(y0<1){y0 = 1}
      
        # #gaussian weighs
        # th <- abs(0:(lums*2)-lums)
        # tv <- 0:lum
        # dis.up <- decay.gaus.2d(alpha, th, tv)
        # 
        # th <- abs(0:(ldms*2)-ldms)
        # tv <- 0:ldm
        # dis.dwon <- decay.gaus.2d(alpha, th, tv)
        
        xs <- c()
        diff <- c()
        for(j in y0:yf){
          xj <- xs[j] <- (-line[5] + j)/line[6]
         
          if(xj<1 | xj>ncol(x)){diff[j] <- xs[j] <- NA}else{
            if( j-lum <1){up = j-1}else{up = lum}
            if( xj-lums <1){ul = xj-1}else{ul = lums}
            if( xj+lums > ncol(x)){ur = ncol(x)-xj}else{ur = lums}
            if( j + ldm > nrow(x)){dw = nrow(x)-j}else{dw = ldm}
            if( xj-ldms < 1){dl = xj-1}else{dl = ldms}
            if( xj+ldms > ncol(x)){dr = ncol(x)-xj}else{dr = ldms}

            upper.mean <- sum(x[(j-up):j,(xj-ul):(xj+ur)]) #sum(x[(j-up):j,(xj-ul):(xj+lums)]*dis.up)/sum(dis.up)
            down.mean  <- sum(x[j:(j+dw),(xj-dl):(xj+dr)]) #sum(x[j:(j+dw),(xj-dl):(xj+dr)]*dis.dwon)/sum(dis.dwon)
            diff[j] <- ((down.mean-upper.mean)/upper.mean) #+ down.mean*theta
          }
        }
      diff[(j+1):nrow(x)] <- NA
      xs[(j+1):nrow(x)] <- NA
      list(diff,xs)
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
      
      p <- sapply(x,function(x)length(unique(names(x)))/length(sel))
      clus.m <- x[p>=prob.threshold]
      fp <- lapply(clus.m, find.perpendicular, sig.alpha, sel)
      ip <- intersection.point(fp)
      data.frame(do.call(rbind,ip))
    }

# reactive ----------------------------------------------------------------
 
    rv = reactiveValues(m=data.frame(x1=NA,y1=NA,x2=NA,y2=NA,int=NA,slo=NA))# OJO: empieza en NA
    r = reactiveValues(m=NULL)
    r.multi = reactiveValues(m=NULL)
    sf = reactiveValues(r=c(0))
    sel = reactiveValues(sel=c(NA))
    ranges = reactiveValues(x = NULL, y = NULL)
    smooth = reactiveValues(res = NULL)
    peak = reactiveValues(res = NULL)
    late = reactiveValues(l=NULL)
    show.band = reactiveValues(sb=1)
    c.peak = reactiveValues(cp=NULL)
    p = reactiveValues(pval=NULL)
    num.click.cor = reactiveValues(r = c(0))
    data.cor.multi = reactiveValues(m = NULL)
    smooth.int = reactiveValues(res = NULL)
    smooth.int.x = reactiveValues(res = NULL)
    over = reactiveValues(m = NULL)
    # time = reactiveValues(t = c())
    click.count <- reactiveValues(cc = c(0))
    click.count2 <- reactiveValues(cc2 = c(0))
    
# plots -------------------------------------------------------------------
    
    output$plot1 <- renderPlot({
      app.plot.sct(rv)
    })
    
    output$plot2 <- renderPlot({
      app.plot.img(imc) 
    })
    
    output$plot3 <- renderPlot({
      if(input$tabs!="load_data"){
        plot(im,xlim=c(dim(im)[2]/2*-1,dim(im)[2]/2),ylim=c(dim(im)[2],0),asp="varying")
      }
    })
    
    

# observe events ----------------------------------------------------------
    ## plot management ----------------------------------------------------------
    
    observeEvent(input$plot1_brush, {
      if(input$tabs!="load_data"){
        ranges$x <- c(input$plot1_brush$xmin * abs(rsize.per), input$plot1_brush$xmax * abs(rsize.per))
        ranges$y <- c(input$plot1_brush$ymin * abs(rsize.per), input$plot1_brush$ymax * abs(rsize.per))
      }
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
        ranges$x[1] <- ranges$x[1]+ran -(ran*0.8)
        ranges$x[2] <- ranges$x[2]+ran -(ran*0.8)
        
      }
    })
    
    observeEvent(input$left,{
      if(is.null(ranges$x)){
        ranges$x = c(dim(imc)[2]/2*-1, dim(imc)[2]/2)
        ranges$y = c(0,dim(imc)[2])
      }else{
        ran <- (ranges$x[2]-ranges$x[1])
        ranges$x[1] <- ranges$x[1]-ran +(ran*0.8)
        ranges$x[2] <- ranges$x[2]-ran +(ran*0.8)
      }
    })
    
    observeEvent(input$i,{
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
    
    observeEvent(input$o,{
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
        rv$m[nrow(rv$m)+1,1:2] <- unlist(input$plot_click)
        sf$r <- sf$r + 1
      }else{
        rv$m[nrow(rv$m),3:4] <- unlist(input$plot_click)
        #pendiente
        m <-  (rv$m[nrow(rv$m),2]-rv$m[nrow(rv$m),4])/(rv$m[nrow(rv$m),1]-rv$m[nrow(rv$m),3])
        #intercepto
        rv$m[nrow(rv$m),5] <- rv$m[nrow(rv$m),2] - m * rv$m[nrow(rv$m),1] 
        rv$m[nrow(rv$m),6] <- m
        #OJO: esto es una guarrada
        if(rv$m[nrow(rv$m),1]==rv$m[nrow(rv$m),3]){rv$m[nrow(rv$m),6] <- 1;rv$m[nrow(rv$m),5] <-0 }
        rv$m <- rv$m[!is.na(rowSums(rv$m)),]
        sf$r <- 0
      }}
      if(input$tabs=="corr"){
        if(input$cor_type == "single"){
          click.count$cc <- click.count$cc + 1
          r$m[nrow(r$m)+1,1] <- input$plot_click$x
          r$m[nrow(r$m),2] <- input$plot_click$y
          r$m[nrow(r$m),3] <- 0
          r$m[nrow(r$m),4] <- 0
          #r$m <- rbind(r$m,c(unlist(input$plot_click),0))
          r$m <- r$m[order(r$m$y),]
        }
        if(input$cor_type == "int"){
          if(num.click.cor$r == 0){
            np <- nearPoints(r$m, input$plot_click, xvar = "x", yvar = "y", allRows = TRUE, maxpoints=1)
            if(sum(np$selected_)==0){
              showNotification("Please click closer to an existing point", duration = 10, type="error")
            }else{
            r$m[nrow(r$m)+1,4] <- r$m$line[np$selected_]
            num.click.cor$r <- 1
            }
          }else{
            r$m[nrow(r$m),1] <- input$plot_click$x
            r$m[nrow(r$m),2] <- input$plot_click$y
            r$m[nrow(r$m),3] <- 0
            r$m <- r$m[order(r$m$y),]
            num.click.cor$r <- 0
          }
        }
        if(input$cor_type == "over"){
          if(num.click.cor$r == 0){
            np <- nearPoints(r$m, input$plot_click, xvar = "x", yvar = "y", allRows = TRUE, maxpoints=1)
            r$m$pair[np$selected_] <-  max(r$m$pair)+1
            num.click.cor$r <- 1
          }else{
            np <- nearPoints(r$m, input$plot_click, xvar = "x", yvar = "y", allRows = TRUE, maxpoints=1)
            r$m$pair[np$selected_] <-  max(r$m$pair)
            num.click.cor$r <- 0
        }}
        if(input$cor_type == "multi"){
          if(num.click.cor$r == 0){
            data.cor.multi$m <- unlist(input$plot_click)
            num.click.cor$r <- 1
          }else{
            num.click.cor$r <- 0
            click.count$cc <- click.count$cc + 1
            data.cor.multi$m <- rbind(data.cor.multi$m,unlist(input$plot_click))
            
            #pendiente
            m <- (data.cor.multi$m[1,2]-data.cor.multi$m[2,2])/(data.cor.multi$m[1,1]-data.cor.multi$m[2,1])
       
            #intercepto
            b <- data.cor.multi$m[1,2] - m * data.cor.multi$m[1,1] 
            
            # perdendicular 
            x.center <- mean(c(data.cor.multi$m[1,1],data.cor.multi$m[2,1]))
            y.center <- b +  m * x.center
            pendendicular.slope <- -1/m
            new.inter <- y.center - (-1/m) * x.center
            r.multi$m[nrow(r.multi$m)+1,c(1:6,9:11)] <- c(b,m,new.inter,pendendicular.slope,x.center,y.center,data.cor.multi$m[1,1],data.cor.multi$m[2,1],NA)

            # interseccion
            r.multi$m <- r.multi$m[order(r.multi$m$y),]
            for(i in 2:nrow(r.multi$m)-1){
              x0 <-r.multi$m[i,]
              xi <- r.multi$m[i+1,]
              if(x0["slope"]==0){
                x.inter <- x0["x"]
                y.inter <- xi["y"]
              }else{
                x.inter <- (x0["p.intercept"] - xi["intercept"])/(xi["slope"]-x0["p.slope"])
                y.inter <- xi["slope"]*x.inter + xi["intercept"]
              }
              r.multi$m[i,c(7,8)] <- c(x.inter,y.inter)	
            }
            
          }
        }
      }
        
      if(input$tabs=="late"){
        late$l <- rbind(late$l,unlist(input$plot_click))
      }})

    observeEvent(input$plot_click2, {
      if(input$tabs=="corr"){
        if(input$cor_type %in% c("int","single")){
          click.count2$cc2 <- click.count2$cc2+1
          np <- nearPoints(r$m, input$plot_click2, xvar = "x", yvar = "y", allRows = TRUE, maxpoints=1)
          r$m <- r$m[!np$selected_,] 
         }
        if(input$cor_type == "multi"){
          click.count2$cc2 <- click.count2$cc2+1
          np <- nearPoints(r.multi$m, input$plot_click2, xvar = "x", yvar = "y", allRows = TRUE, maxpoints=1)
          r.multi$m <- r.multi$m[!np$selected_,]
          r.multi$m <- r.multi$m[order(r.multi$m$y),]
          for(i in 2:nrow(r.multi$m)-1){
            x0 <-r.multi$m[i,]
            xi <- r.multi$m[i+1,]
            if(x0["slope"]==0){
              x.inter <- x0["x"]
              y.inter <- xi["y"]
            }else{
              x.inter <- (x0["p.intercept"] - xi["intercept"])/(xi["slope"]-x0["p.slope"])
              y.inter <- xi["slope"]*x.inter + xi["intercept"]
            }
            r.multi$m[i,c(7,8)] <- c(x.inter,y.inter)	
          }
        }
        if(input$cor_type == "over"){
            np <- nearPoints(r$m, input$plot_click2, xvar = "x", yvar = "y", allRows = TRUE, maxpoints=1)
            pair <- r$m$pair[np$selected_]
            r$m$pair[r$m$pair==pair]<-  0
        }
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
    
    observeEvent(input$save_kk, {
     write.csv(r$m,file="beca_toca_pelotas.csv")
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

      if(input$line_type == "int"){
          smooth_int <- apply(rv$m,1, function(y)clever.smooth.interactive(x,y,input$hdm, input$hdm, input$hdm, input$hdm,  input$alpha))
          smooth.int$res <- sapply(smooth_int,function(x)x[[1]])
          smooth.int.x$res <- data.frame(sapply(smooth_int,function(x)x[[2]]))
      }else{
      if(input$line_type == "sin"){ 
        
        if(input$band_x1 > ncol(x) | input$band_x1<=0){showNotification("Initial band out of core",type="error")
        }else{
          sel$sel <- input$band_x1
          smooth$res <- clever.smooth (x, sel$sel, input$hdm, input$hdm, input$hdm, input$hdm,  input$alpha)
          }
    
      }
      if(input$line_type == "mul"){
        
        show.band$sb <- input$show.band
        
        
         if(input$band_x1 > ncol(x) | input$band_x1<=0 | input$band_xn > ncol(x)| input$band_xn<=0){showNotification("Bands out of core",type="error")
         }else{
           sel$sel <- band.sel(input$band_x1,input$band_xn,input$band_N)
           smooth$res <- clever.smooth (x, sel$sel, input$hdm, input$hdm, input$hdm, input$hdm,  input$alpha)
           }
      }
     }}
    )
    
    ## ring events ----------------------------------------------------------
    
    observeEvent(input$run_peak_single,{  
      smooth_res <- smooth$res*(dim(imc)[1]/2/quantile(smooth$res,input$qv,na.rm=T))
      peak_res <- peaks(smooth_res[,1],input$score,input$join)
      r$m <- data.frame(x=input$band_x1, y=peak_res, pair=0, line=0, type=1)
     }
    )
    
    observeEvent(input$run_peak_int,{  
      smooth_res <- apply(smooth.int$res,2, function(x)x*(dim(imc)[1]/2)/quantile(x,input$qv,na.rm=T))
      peak_res <-apply(smooth_res,2,peaks,input$score,input$join)
      if(!is.list(peak_res)) {peak_res <- list(peak_res)}
      xs <- c()
      for(i in 1:length(peak_res)){
        xs <- c(xs,smooth.int.x$res[peak_res[[i]],i])
      }
      r$m <- data.frame(x=xs, y=unlist(peak_res), pair=0,line=rep(1:length(peak_res),sapply(peak_res,length)), type=1)
    }
    )

    observeEvent(input$run_peak_multi,{
      smooth_res <- apply(smooth$res, 2, function(x){x*(dim(imc)[1]/2/quantile(x,input$qv))})
      peaks.multi <- apply(smooth_res,2,peaks,input$score,input$join)
      c.peak$cp <- clus.peak.bands (peaks.multi, input$join, sel$sel)
      prob <- sapply(c.peak$cp,function(x)length(unique(names(x)))/length(sel$sel))
      y <- sapply(c.peak$cp,mean)
      p$pval <- data.frame(prob=prob,y=y)
    })
    
    observeEvent(input$select_multi,{
      res <- rings.m(c.peak$cp, input$prob, sel$sel, 0.05)
      res$band_x1 <- input$band_x1
      res$band_xn <- input$band_xn
      res$type=1
      r.multi$m <- res
    }
    )
    NULL+1
    ## correction events ----------------------------------------------------------
    
     observeEvent(input$rese, {
       smooth_res <- smooth$res*(dim(imc)[1]/2/quantile(smooth$res,input$qv))
       peak_res <- peaks(smooth_res[,1],input$score,input$join)
       r$m <- data.frame(x=input$band_x1, y=peak_res, pair=0)
     })
    
    

     
    ## late events ----------------------------------------------------------
   
    
    observeEvent(input$run_late,{  # sino funciona el por el r$m$pair y line
      late$l <- late.f (smooth$res, r$m)
    }
    )
    
    observeEvent(input$rese_late, {
      late$l <- late.f (smooth$res, r$m)
    })
    
    ## measures events ----------------------------------------------------------
    
    observeEvent(input$dis_ring,{
      capture.output(c(Sys.time()-time.c,click.count$cc,click.count2$cc2),file=paste(name," time_",input$file_dis_ring,sep=""))
      if(input$save_type!="multi"){
        if(sum(r$m$pair)==0){
         res <- r$m
         res <- res[order(res$y),]
         for(i in 1:(nrow(res)-1)){
           res[i,"distance"] <- dist(res[c(i,i+1),1:2])
          }
        }else{
        id.line <- unique(r$m$line)
        res.l <- list()
          for(j in 1:length(id.line)){
            res.i <- r$m[r$m$line==id.line[j],]
            res.i <- res.i[order(res.i$y),]
            for(i in 1:(nrow(res.i)-1)){
              res.i[i,"distance"] <- dist(res.i[c(i,i+1),1:2])
            }
            res.l[[j]] <- res.i
          }
        
        res.l <- res.l[order(tapply(r$m$y,r$m$line,min))]
        id.pair <- unique(r$m$pair); id.pair <- id.pair[id.pair!=0]
        for(i in 1:length(id.pair)){
          sel <- 1:length(res.l)[sapply(res.l,function(x)id.pair%in%x$pair)]
          fl <- res.l[min(sel)][[1]] 
          sl <- res.l[max(sel)][[1]] 
          fl<- fl[-nrow(fl),]
          ul<- rbind(fl,sl)
          res.l <- res.l[-sel]         
          res.l [[length(res.l)+1]]<-ul
        }
        
        res <- do.call(rbind,res.l[order(sapply(res.l,min))])
        }
        
      }else{
        res <- r.multi$m
        res <- data.frame(res[order(res$y),])
        res$distance<- apply(res,1,function(x)sqrt((x[5]-x[7])^2+(x[6]-x[8])^2))
        }
      res$year <- input$year-c(1:nrow(res)-1)
      res[,"distance(cm)"] <- (res$distance/input$ppp)*2.54
      write.table(res,input$file_dis_ring,row.names=F)
      
      })
    
    observeEvent(input$dis_late,{
      if(input$save_type=="single"){
      colnames(late$l) <- c("xl","yl")
      res <- data.frame(cbind(r$m,late$l))
      res <- res[order(res$y),]
      for(i in 1:(nrow(res)-1)){
        res$ring[i] <- dist(res[c(i,i+1),c("x","y")])
        late.i <- res[i,c("xl","yl")]; colnames(late.i)<- c("x","y")
        early.i <- res[i+1,c("x","y")]; colnames(early.i)<- c("x","y")
        res$early[i] <- dist(rbind(late.i,early.i))
      }
      res$late <- apply(res,1,function(x)dist(rbind(x[c("x","y")],x[c("xl","yl")])))
      res$year <- input$year-c(1:nrow(res)-1)
      
      res[,"ring(cm)"] <- (res$ring/input$ppp)*2.54
      res[,"early(cm)"] <- (res$early/input$ppp)*2.54
      res[,"late(cm)"] <- (res$late/input$ppp)*2.54

      write.table(res,input$file_dis_late,row.names=F)
      }else{showNotification("Sorry only implemented for single type", duration = 10, type="error")}
      
    }
    )
    
    # observeEvent(input$start_time,{time$t <- Sys.time()})

    
    
  }

  shinyApp(ui, server)
  
}

### cargar datos
imc <- load.image("02-data\\bec_tune.jpeg")
rsize.per <- -10
name <- "testigo de prueba"
line.measured(imc,rsize.per,name)



