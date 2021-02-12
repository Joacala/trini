###################################################################################################################
###################################################################################################################
###################################################################################################################
EXAMPLE
source("C:\\Users\\F541U\\Desktop\\proyectos\\Julen\\functions_ringmer.R")
library(imager)

### cargar datos
imc <- load.image("C:\\Users\\F541U\\Desktop\\proyectos\\Julen\\jaime_pino.png")
#imc <- resize(imc,round(width(imc)/3),round(height(imc)/3))
img <- imc
img <- flatten.alpha(img, bg = "white")

#img <- imgradient(img,"y")
# por explorar

img <- grayscale(img, method = "Luma", drop = TRUE)
g <- channels(img, drop = T)
g <- t(g[[1]])
dim(g)


###
#
# ALL TOGETHER SINGLE BAND ANALYSIS
#
###

# SELECCI?N  #
plot(img)
band <- round(ncol(g)/2)
abline(v=band)

x <- g
band <- band
ldm <- 5 ## Heigh down mean
lum <- 3 ## Heigh upper mean
lums <- 30 ## width upper mean
ldms <- 30 ## width down mean
alpha <- 0 ; plot(gaus_decay_w(1:ldms,alpha)) ## gaussian decay exponent (0 = no decay; 0> increase decay); 
adaptive <- F
late=F
score <- -0.02
join.dis <- 10
distance <- T

start_time <- Sys.time()
res.s <- rings(x, band, ldm, ldms, lum, lums,  alpha, score, join.dis, adaptive,late,distance)
end_time <- Sys.time()
end_time - start_time
gs <- clever.smooth (x, band, ldm, ldms, lum, lums, alpha)

dim(imc)
247/4

gs <- gs[[1]]
max(gs)

gst <- (gs*(dim(imc)[1]/4))/max(gs)


path <- "C:\\Users\\F541U\\Desktop\\proyectos\\Julen\\data_shiny.csv" 
visual.cor(res.s,imc,-10, path)



#      PLOT	      # 
# :::::::::::::::: #

path <- "C:\\Users\\F541U\\Desktop\\proyectos\\Julen\\resultados\\"
name <- "incana_procesado_1.PDF"
par <- paste("ldm_",ldm,"-lum_",lum,"-lums_",lums,"-ldms_",ldms,"-alpha_",alpha,"-adaptive_",adaptive,"-score_",score,"-join.dis_",join.dis,"_",sep="")

dev.off()
pdf(paste(path,par,name,sep=""))
plot(imc)
points(res.s[,2]~res.s[,1],col=4,pch="_",cex=0.05)
for(i in 1:(nrow(res.s)-1)){
	segments(res.s[i,1], res.s[i,2], res.s[i+1,1], res.s[i+1,2],col=4,lwd=0.1,lty=3)
	text(res.s[i,1]+4,(res.s[i,3])/2+res.s[i,2],round(res.s[i,3],2),cex=0.1)
}
lines(gst, 1:length(gst),lwd=0.1,col=2)
abline(h=res.s[,2],lwd=0.01,lty=2)
abline(v=score*ncol(g),lwd=0.05,lty=1)
dev.off()

class(imc)

plot(gr,ylim=c(1500,1000))
c <- locator(1)
points(c$x,c$y)
imgradient(imc,"xy") %>% enorm %>% plot(main="Gradient magnitude (again)",ylim=c(1000,2000))

identify(c,n=1, plot=FALSE,tolerance = 0.5)

###
#
# ALL TOGETHER MULTI-BAND ANALYSIS
#
###



# SELECCI?N  #
# :::::::::: #

plot(img)
band <- 165#round(ncol(g)/2)
abline(v=band)

x <- gr
band <- band# round(ncol(g)/2)
ldm <- 5 ## Heigh down mean
lum <- 5 ## Heigh upper mean
lums <- 20 ## width upper mean
ldms <- 20 ## width down mean
alpha <- 0 ; plot(gaus_decay_w(1:ldms,alpha)) ## gaussian decay exponent (0 = no decay; 0> increase decay); 
score= -0.015
adaptive <- F
join.dis.intra <- 15
join.dis.inter <- 7
prob.threshold <- 0.6
distance <- T
threshold.cor <- 0.6


res.m <- rings.multi (x, band,threshold.cor, ldm, ldms, lum, lums,  alpha, score, join.dis.intra, adaptive, join.dis.inter, distance)


#      PLOT	      # 
# :::::::::::::::: #

pdf("C:\\Users\\F541U\\Desktop\\proyectos\\Julen\\ringmeR_mult_JI.pdf")
plot(imc,xlim=c(-300,300))
points(res.m[,2]~res.m[,1],col=4,pch="_",cex=0.05)
#for(i in 1:(nrow(res.m)-1)){
#	segments(res.m[i,1], res.s[i,2], res.m[i+1,1], res.m[i+1,2],col=4,lwd=0.1,lty=3)
#	text(res.m[i,1]+4,(res.s[i,3])/2+res.m[i,2],round(res.m[i,3],2),cex=0.1)
#}
abline(h=res.m[,2],lwd=0.05)
abline(v=band,lwd=0.05)
dev.off()


###
#
# BY PARTS
#
###


# :::::::::: #
# SELECCI?N  #
# :::::::::: #

plot(img)
band <- 21# round(ncol(g)/2)
band.end <- ncol(g)-21
Nband <- 15
even =T
abline(v=band)
abline(v=band.end)
cor.threshold <- 0.5
sel <- band.sel (g, band, cor.threshold,even,band.end,Nband) 

# ::::::::::::::: #
# PEAK DETECTION  #
# ::::::::::::::: #

ldm <- 2 ## Heigh down mean
lum <- 3 ## Heigh upper mean
lums <- 20 ## width upper mean
ldms <- 20 ## width side mean
alpha <- 0 ; plot(gaus_decay_w(1:ldms,alpha)) ## gaussian decay exponent (0 = no decay; 0> increase decay); 

gs <- clever.smooth (g, sel, ldm, ldms, lum, lums,  alpha)

join.dis <- 5
adaptive <- F
late <- F
score <- -0.0175
res <- lapply(gs,peaks,score,join.dis,adaptive,late)


#tardia
#path <- "C:\\Users\\F541U\\Desktop\\proyectos\\Julen\\resultados\\"
#name <- "tardia_pino_jaime.PDF"
#par <- paste("ldm_",ldm,"-lum_",lum,"-lums_",lums,"-ldms_",ldms,"-alpha_",alpha,"-adaptive_",adaptive,"-score_",score,"-join.dis_",join.dis,"_",sep="")

#res$x <- band
#pdf(paste(path,par,name,sep=""))
#plot(imc,xlim=c(-300,300))
#points(res$x~res$ini,col=4,pch="_",cex=0.05)
#lines(gs[[1]]*ncol(g) , 1:length(gs[[1]]),lwd=0.1,col=2)
#abline(h=res$ini,lwd=0.01,lty=2,col=3)
#abline(h=res$end,lwd=0.01,lty=2,col=4)
#abline(v=score*ncol(g),lwd=0.05,lty=1)
#abline(v=band,lwd=0.05)
#dev.off()


# ::::::::::::::: #
# CONGRUENCE BANDS#
# ::::::::::::::: #
join.dis <- 6
clu.peaks <- clus.peak.bands(res,join.dis,sel)


#pdf("kk1.pdf")
#plot(img)
#lines((clu.peaks[[1]]*-400)+500,1:nrow(g),lwd=0.1,col=1)
#for(i in 1:length(clus.m)){
#points(clus.m[[i]]~as.numeric(names(clus.m[[i]])),col=i,pch=19,cex=0.05)
#}
#dev.off()

# :::::::::::::::  #
# MEASURE	      #  
# :::::::::::::::: #
band <- 31#round(ncol(g)/2)
band.end <- ncol(g)-31
Nband <- 21
even =T
cor.threshold <- 0.5
distance <- T
prob.threshold <- 0.1
sig.alpha <- 0.05
join.dis.intra <- 10
join.dis.inter <- 10
adaptive <- F
late <- F
score <- -0.01
ldm <- 1 ## Heigh down mean
lum <- 1 ## Heigh upper mean
lums <- 30 ## width upper mean
ldms <- 30 ## width side mean
alpha <- 0 ; plot(gaus_decay_w(1:ldms,alpha)) ## gaussian decay exponent (0 = no decay; 0> increase decay); 


start_time <- Sys.time()
sel <- band.sel (g, band, cor.threshold,even,band.end,Nband) 
gs <- clever.smooth (g, sel, ldm, ldms, lum, lums,  alpha)
res <- lapply(gs,peaks,score,join.dis.intra,adaptive,late)
clu.peaks <- clus.peak.bands(res,join.dis.inter,sel)
res <- rings.m (clu.peaks, prob.threshold, sel, sig.alpha, distance)
end_time <- Sys.time()
end_time - start_time

path <- "C:\\Users\\F541U\\Desktop\\proyectos\\Julen\\resultados\\"
name <- "jaime_incana.PDF"
par <- paste("ldm_",ldm,"-lum_",lum,"-lums_",lums,"-ldms_",ldms,"-alpha_",alpha,"-adaptive_",adaptive,"-score_",score,"-join.dis.inter_",join.dis.inter,"_",sep="")

pdf(paste(path,par,name,sep=""))
plot(imc)
apply(res,1,function(x)segments(x[5],x[6],x[7],x[8],lwd=0.05,lty=1,col=2))
#apply(res,1,function(x)points(x[5],x[6],pch="+",col=3,cex=0.1))
#apply(res,1,function(x)points(x[7],x[8],pch="*",col=4,cex=0.05))
apply(res,1,function(x)segments(sel[1],x[1]+x[2]*sel[1],sel[Nband],x[1]+x[2]*sel[Nband],lwd=0.05,lty=1,col=1))
dev.off()


write.csv(res,"para_jaime.csv")
  

