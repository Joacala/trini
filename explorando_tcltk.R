library(tcltk)

  
##########################################################################
##########################################################################
##########################################################################
##########################################################################
###     REMOVE/ADD POINTS

hand.job<-function(res,img){
  require(tcltk)
    message("Click on plot to add or delete points.  To stop function, click stop, then click on a point in the plot.")
  action<-tclVar("add") 
  
  x<-res[,1]
  y<-res[,2]
  zoom <- c(0,dim(img)[2])
  range <- dim(img)[2]
  old.par <- par(no.readonly = TRUE)
  
  dev.new(height=5,width=5*1.4)
  #plot(imc)
  plot(x,y,col=2,pch="+",cex=1)

  add.points<-function(){
    if(action=="delete")m<-delete.points()
    else{
      loc<-locator(1)
      x<-c(x,loc$x)
      y<-c(y,loc$y)
      cbind(x,y) }
  } 
 
  zoomin <- function(){                            
    y <- locator(1)$y
    zoom[1] <- y-range/4
    zoom[2] <- y+range/4
    range <- range/2
    list(zoom, range)
}
  
  delete.points<-function(){
    #ans<-identify(y,x, n=1, plot=FALSE,tolerance = )
    z <- locator(1)
    ans <- which(apply(cbind(x,y),1, function(x)abs(dist(cbind(x,c(z$x,z$y)))))<nrow(img)/15)
    if(sum(ans)>0){
    x<-x[-ans]
    y<-y[-ans]
    }
    cbind(x,y)
  }
  repeat{
    refresh<-function(){
      
      action<-tclvalue(action)
      if(action=="stop") {stop()}	
      else if(action=="add")m<-add.points()
      else if(action=="delete")m<-delete.points()
      else if(action=="zoomin")z <- zoomin()
      tkdestroy(tt)
      if(action=="add" | action=="delete"){x<<-m[,1];y<<-m[,2]}
      if(action=="zoomin"){
        zoom[1]<<-z[[1]][1];zoom[2]<<-z[[1]][2]
        range <<- z[[2]]
        }
      dev.hold()
      par(xpd=FALSE)
      #if(action=="zoomin"){plot(imc,ylim=c(zoom[2],zoom[1]))}
      plot(x,y,col=2,pch="+",cex=1)
      dev.flush()
    }
    
    tclServiceMode(TRUE)   
    tt <- tktoplevel()
    tkwm.geometry(tt, "+50+4")
    tkwm.title(tt, "Add/delete points plus zoom in")
    tkpack(tklabel(tt,text="Add/delete points plus zoom in"))
    tkpack(tklabel(tt,text=""))
    tkpack(tklabel(tt, text = "  Action: "), side = "top")
    for ( i in c("add", "delete","zoomin", "stop")){                           
      tmp <- tkradiobutton(tt, text=i, variable=action, value=i)
      tkpack(tmp, anchor = "w")}
    tkpack(tkbutton(tt, text = "Exit", command = function()tkdestroy(tt))) 
    refresh()
  }
  on.exit(par(old.par))
  }
  
hand.job(res,img)

res<- cbind(sample(1:10),sample(1:10))

















library(loon)
library(rpanel)
install.packages("rpanel")

tt <- tktoplevel()
l2 < - tklabel(tt, text = "Ho")
l1 <- tklabel(tt, text = "Heave")
tkpack(l1, l2)
tkpack.configure(l1, side = "left")


p <- l_plot(x = quakes$long, y = quakes$lat, 
            xlabel = "longitude", ylabel = "latitude",
            title = "Tonga trench earthquakes")

library(tcltk)
tt <- tktoplevel()
zoom.widget <- tklabel(tt, text = "zoom")
zoom <- c(0,dim(img)[2])
dev.new(height=5,width=5*1.4)
plot(imc,ylim=c(0,zoom[2]))

zoom.in <- function(){                            
  dev.off()
  dev.new(height=5,width=5*1.4)
  zoom[2] <- zoom[2]-zoom[2]/2
  plot(imc,ylim=c(zoom[2],zoom[1]))
}

button.zoom.p <- tkbutton(tt, text = "+",
                          command = zoom.in)

button.zoom.l <- tkbutton(tt, text = "-",
                          command = function()cat("Probre Becario infeliz\n"))

tkpack(zoom.widget,button.zoom.p, button.zoom.l) # geometry manager



# see Tk-commands

## Push the button and then...

tkdestroy(tt)

## test for themed widgets
if(as.character(tcl("info", "tclversion")) >= "8.5") {
  # make use of themed widgets
  # list themes
  as.character(tcl("ttk::style", "theme", "names"))
  # select a theme -- here pre-XP windows
  tcl("ttk::style", "theme", "use", "winnative")
} else {
  # use Tk 8.0 widgets
}
# }



gulls.panel <- rp.control("STEPS: The Birds and the Bees",
                          gulls.all = gulls.all,
                          lmk.names = c("Wing tip", "Tail feathers", "Wing joint",
                                        "Bottom of bill", "Tip of beak", "Top of bill",
                                        "Top of head", "Back of head"),
                          lmks.x = c( 25, 40, 218, 417, 449, 436, 362, 330),
                          lmks.y = c(134, 167, 183, 79, 78, 52, 11, 23),
                          lmk1 = NA, lmk2 = NA)

rp.image(gulls.panel, "gulllmks.gif",
         id = "gulls.image", action = click.capture)

rp.button(gulls.panel,
          title = "Collect data", action = collect.data)

rp.gulls(df.name = "kk", panel.plot = TRUE)





