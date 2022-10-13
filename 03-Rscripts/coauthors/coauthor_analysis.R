setwd("C:\\Users\\F541U\\Desktop\\proyectos\\Julen\\shiny_dendro\\data_coau\\tring-EG\\shiny_dendro\\shiny_dendro-main\\02-data\\")

# load order data
fi <- list.files(pattern=".csv",)
da <- data.frame(do.call(rbind,lapply(fi, read.csv)))
da$order <- rep(1:18,length(fi))
da$name <- rep(gsub(".csv","",fi),each=18)
da$method <- sapply(strsplit(da$cores,"_"),function(x)x[2])
da$cores <- sapply(strsplit(da$cores,"_"),function(x)x[1])


# load time and error data


setwd("C:\\Users\\F541U\\Desktop\\proyectos\\Julen\\shiny_dendro\\data_coau\\data_clean\\")
authors <- list.files()

das <- list()
for (i in 1:length(authors)){

		fi.i <- list.files(authors[i],pattern="time")
		da.i <- lapply(fi.i, function(x)read.csv(paste(authors[i],"/",x,sep="")))
		da.i <- data.frame(t(sapply(da.i,function(x){y <- as.numeric(strsplit(x[1,]," ")[[1]]); y[!is.na(y)]})))
		da.i$cores <- sapply(strsplit(fi.i, " time"),function(x)x[1])
		da.i$name <- authors[i]
		
		das [[i]] <- da.i

}
	
das <- do.call(rbind,das)

das$mergeID <- paste(das$cores,das$name)
da$mergeID <- paste(da$cores,da$name)

dat <- merge(da,das[,-c(4,5)], by="mergeID",all=F)[,-c(1,3)]
names(dat)[5:7] <-c ("time","add","delete")


##:::::::::##

# ANALISIS

##:::::::::##

cor(dat[,5:7],method="spearman")

# resultado 1: mejor poner umbrales donde se seleccionen más anillos (más rápido quitar que poner)

library(lme4)
library(emmeans)
library(AICcmodavg)
library(ggplot2)
library(performance)
library(effects)
library(nlme)

install.packages("performance")
## MODELOS TIEMPO
m1 <- lm(time ~ order * name + method, data=dat)
m2 <- lm(time ~ order * method + name, data=dat)
m3 <- lm(time ~ order + method * name, data=dat)
m4 <- lm(time ~ order + method + name, data=dat)
m5 <- lm(time ~ order + method , data=dat)
m6 <- lm(time ~ order + name, data=dat)
m7 <- lm(time ~  method + name, data=dat)
m8 <- lm(time ~ name, data=dat)
m9 <- lm(time ~ order, data=dat)

m1 <- lme(time ~ order + method , random = ~ 1|name  , data=dat)
m2 <- lme(time ~ order + method , random = ~ 1|name , data=dat)
m3 <- lme(time ~ poly(order,2) + method , random = ~ 1|name , data=dat)

m3 <- lme(time ~ order  , random = ~ 1|name , data=dat)
m4 <- lme(time ~ method  , random = ~ 1|name , data=dat)


m1 <- lmer(time ~ order * method + (1|cores) + (1|name)  , data=dat)
m2 <- lmer(time ~ order + method + (1|cores) + (1|name)  , data=dat)
m3 <- lmer(time ~ poly(order,2) + method + (1|cores) + (1|name)  , data=dat)
plot(allEffects(m3))

aictab(list(m1,m2,m3))
summary(m2)

m1 <- lmer(log(ade) ~ cores*order + (1|name)   , data=dat)
m2 <- lmer(log(ade) ~ cores + order + (1|name)   , data=dat)
m3 <- lmer(log(ade) ~ cores + (1|name)   , data=dat)
m4 <- lmer(log(ade) ~ order + (1|name)   , data=dat)

aictab(list(m1,m2,m3,m4))


em <- emmeans(m1, "name", contr="tukey")
plot(em)

ggplot(dat, aes(x=order, y =time, color = name)) +
geom_line() +
geom_smooth(method = "lm") 

## MODELOS ADD/DELETE
dat$ade <- dat$add + dat$delete
m1 <- lm(log(ade) ~ order * name + method, data=dat)
m2 <- lm(log(ade)~ order * method + name, data=dat)
m3 <- lm(log(ade)~ order + method * name, data=dat)
m4 <- lm(log(ade)~ order + method + name, data=dat)

m1 <- lmer(log(ade)~ order * method + (1|cores) + (1|name)  , data=dat)
m2 <- lmer(log(ade)~ order + method + (1|cores) + (1|name)  , data=dat)
m3 <- lmer(log(ade)~ poly(order,2) + method + (1|cores) + (1|name)  , data=dat)

cor.test(dat$ade, dat$time)
aictab(list(m1,m2,m3))

m5 <- lm(log(ade) ~ order * name , data=dat)

plot(allEffects(m1))
