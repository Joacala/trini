###################################################################################################################
###################################################################################################################
###################################################################################################################

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

###################################################################################################################
###################################################################################################################
###################################################################################################################

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
		#horizontal margins
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

###################################################################################################################
###################################################################################################################
###################################################################################################################
#x : vector where peaks are detected
#width.w : number of pixels to perform the mean
#score: thershold to detect a peak: If adatative = lower quantile; if !adaptive row value 
#join.dis: if adatative: moving window length; if !adaptive: merge consecutive peaks at a <join.dis pixels
#adaptive: search for general peaks if F or local peaks if T.
#late: if late=T, measure late wood

peaks <- function(x, score, join.dis, adaptive, late){

	if(!adaptive){
		res <- which(x<=score)
		v <- c()
		for(i in 1:length(res)){
			x0 <- res[i]-join.dis; if(x0<1){x0=1}
			xf <- res[i]+join.dis; if(xf>length(x)){xf=length(x)}
			if(sum(x[x0:xf]<x[res[i]],na.rm=T)>0){v[length(v)+1] <- i}
		}			
		res <- res[-v]
	}else{
		res <- which(x < 0)
		v <- c()
		for(i in 1:length(res)){
			x0 <- res[i]-join.dis; if(x0<1){x0=1}
			xf <- res[i]+join.dis; if(xf>length(x)){xf=length(x)}
			if(x[res[i]]<=quantile(x[x0:xf],score)){v[length(v)+1] <- i}
		}
		res <- res[v]
	}
	
	if(late){
		res.end <- c()
		for(i in 1:(length(res)-1)){
			res.end[i] <- which.max(x[res[i]:res[i+1]])+res[i]-1
		}
		res.end[length(res)] <- which.max(x[res[length(res)]]:x[length(x)])+res[length(res)]-1
		res <- data.frame(ini = res, end= res.end)
	}
	res
					
}

###################################################################################################################
###################################################################################################################
###################################################################################################################
# x : matrix
# band : targeted band (column number)
# threshold : correlation threshold
# even: if T, select Nband evenly distributed between band and band.N

band.sel <- function(x, band, band.end=NA, Nband=NA){

		sel <- round(seq(band,band.end,length=Nband))

}


###################################################################################################################
###################################################################################################################
###################################################################################################################
# x : list of peaks of each band 
# join.dis: cluster peaks from different at a <=join.dis heigh (pixels)
# sel: selected bands

clus.peak.bands <- function(x, join.dis, sel){
  
  # x : list of peaks of each band 
  # join.dis: cluster peaks from different at a <=join.dis heigh (pixels)
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



###################################################################################################################
###################################################################################################################
###################################################################################################################
#y :  vector with clustered points value of function clus.peak.bands
#sig.alpha : p-value threshold clasified regression as significant 
#pendendicular slope = -1/slope
#new intercept crossing center = y.center = new inter + pendendicular slope* x.center; new inter = y.center - perpendicular slope* x.center

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

###################################################################################################################
###################################################################################################################
###################################################################################################################

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



###################################################################################################################
###################################################################################################################
###################################################################################################################
#x : value of clus.peak.bands
#prob.threshold : ring probability of occurring across bands
#sel : selected bands
#distance:calculate distances between consecutive rings
#sig.alpha : p-value threshold clasified regression as significant 

rings.m <- function(x, prob.threshold, sel, sig.alpha, distance){
  
  #x : value of clus.peak.bands
  #prob.threshold : ring probability of occurring across bands
  #sel : selected bands
  #distance:calculate distances between consecutive rings
  #sig.alpha : p-value threshold clasified regression as significant 
  
	p <- sapply(x,function(x)length(x)/length(sel))
	clus.m <- x[p>prob.threshold]
	fp <- lapply(clus.m, find.perpendicular, sig.alpha, sel)
	ip <- intersection.point(fp)
	res <- data.frame(do.call(rbind,ip))
	if(distance){
		res$dis <- apply(res,1,function(x)dist(rbind(x[c("x","y")],x[c("x1","y1")])))
	}
	res
}

###################################################################################################################
###################################################################################################################
###################################################################################################################
#x : ## matrix
#band : ## targeted band (column number)
#ldm : ## Heigh dwon mean
#ldms : ## width dwon mean
#lum : ## Heigh upper mean
#lums : ## width upper mean
#alpha : ## gaussian decay exponent (0 = no decay; 0> increase decay); plot(gaus_decay_w(1:100,alpha))
#score: thershold to detect a peak: If adatative = lower quantile; if !adaptive row value 
#join.dis: if adatative: moving window length; if !adaptive: merge consecutive peaks at a <join.dis pixels
#adaptive: search for general peaks if F or local peaks if T.
#distance: calculate distances between consecutive rings

rings <- function(x, band, ldm, ldms, lum, lums,  alpha, score, join.dis, adaptive, late=F, distance){

	sx <- clever.smooth (x, band, ldm, ldms, lum, lums, alpha)
	peak <- peaks(sx[[1]], score, join.dis, adaptive, late)
	res <- data.frame(x=band,y=peak)
	res <- res[order(res$y),]
	if(distance){
		for(i in 1:(nrow(res)-1)){
			res[i,3] <- dist(res[c(i,i+1),])
		}
	}
	res
}



###################################################################################################################
###################################################################################################################
###################################################################################################################
#x : ## matrix
#band : targeted band (column number)
#threshold.cor : correlation threshold
# even: if T, select Nband evenly distributed between band and band.N
#band.end
#Nband
#ldm : ## Heigh dwon mean
#ldms : ## width dwon mean
#lum : ## Heigh upper mean
#lums : ## width upper mean
#alpha : ## gaussian decay exponent (0 = no decay; 0> increase decay); plot(gaus_decay_w(1:100,alpha))
#score: thershold to detect a peak. In our case should be a negative number
#join.dis.intar: if adatative: moving window length; if !adaptive: merge consecutive peaks at a <join.dis pixels
#adaptive: search for general peaks if F or local peaks if T.
#late: if late=T, measure late wood
#join.dis.inter: cluster peaks from different bands at a <=join.dis heigh (pixels)
#prob.threshold : ring probability of occurring across bands
#sig.alpha
#distance:calculate distances between consecutive rings


rings.multi <- function(x, band, cor.threshold, even, band.end, Nband, ldm, ldms, lum, lums,  alpha, score, join.dis.intra, adaptive, late=F, join.dis.inter, sig.alpha=0.05, distance){

	sel <- band.sel(x, band, cor.threshold,even,band.end,Nband)
	sx <- clever.smooth (x, sel, ldm, ldms, lum, lums,  alpha)
	peak <- lapply(sx,peaks,score,join.dis.intra,adaptive,late)
	c.peak <- clus.peak.bands (peak, join.dis.inter, sel)
	rings.m(c.peaks, prob.threshold, sel, sig.alpha, distance)

}
