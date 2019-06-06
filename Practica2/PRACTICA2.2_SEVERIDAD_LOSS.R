####################### PRACTICA 2.2  #####################

################  ESTUDIO DE LA SEVERIDAD  ################



#PAquetes que vamos a necesitar
library(moments)
library(MASS)
library(actuar) 
library(fitdistrplus)
library(ggplot2)



####################
#Gr?fico de la Funci?n de distribuci?n
#BD de perdidas
data(danishuni)
head(danishuni)


x <-danishuni$Loss
summary(x)
skewness(x)  #coef. de asimetria - Asimetria positiva o la dcha
kurtosis(x)  #coef. de curtosis  - Leprtoc?rtica o mas apuntada Normal

hist(x, pch=10, breaks=50, prob=TRUE, main="Fraudes",
     xlab =" X", ylab = "Densidad")




###ESTIMACION

dmixgampar <- function(x, prob, nu, lambda, alpha, theta)
  prob*dgamma(x, nu, lambda) + (1-prob)*dpareto(x, alpha, theta)
pmixgampar <- function(q, prob, nu, lambda, alpha, theta) 
  prob*pgamma(q, nu, lambda) + (1-prob)*ppareto(q, alpha, theta)


fgam <- fitdist(x, "gamma", lower=0)
fgam


fpar <- fitdist(x, "pareto", start=list(shape=2, scale=2), lower=0)
fpar


fmixgampar <- fitdist(x, "mixgampar",
                      start=list(prob=1/2, nu=1, lambda=1, alpha=2,
                                 theta=2), lower=0)
fmixgampar

cbind(SINGLE= c(NA, fgam$estimate, fpar$estimate), 
      MIXTURE=fmixgampar$estimate)



fburr <- fitdist(x, "burr", start=list(shape1=2, shape2=2, scale=2),
                 lower=c(0.1,1/2, 0))
fburr$estimate





#Comparando las densidades ajustadas


#Funci?n cdfcomp


FDD=cdfcomp(list(fgam, fpar, fmixgampar, fburr), xlogscale=TRUE,
            ylab = "Probabilidad", datapch=".",
            datacol="red", fitcol="black", fitlty=2:5,
            legendtext=c("gamma","Pareto", "Par-gam", "Burr"),
            main="Ajuste p?rdidas legales", plotstyle = "ggplot")
FDD


#Realizamos la selecci?n de distribuci?n


#Con funciones de densidad


hist(x, pch=10, breaks=100, prob=TRUE, main="PERDIDAS LEGALES EN EUROS",
     xlab =" X", ylab = "Densidad")

curve(dburr(x, fburr$estimate[1],fburr$estimate[2], fburr$estimate[3]),
      col="red", lwd=2, add=T)



#Otros gr?ficos interesantes


#Gr?ficos QQ-Plot y PP-Plot
#Graficos de la FD emp?rica contra la FD ajustada 

## qqplot con normal
qqnorm(x)
qqline(x, col="red")


#funci?n de distribuci?n de la mixtura pareto-gamma

qmixgampar <- function(p, prob, nu, lambda, alpha, theta)
{L2 <- function(q, p) 
  (p - pmixgampar(q, prob, nu, lambda, alpha, theta))^2 
sapply(p, function(p) optimize(L2, c(0, 10^3), p=p)$minimun)
}



ppcomp(list(fgam, fpar, fmixgampar, fburr), xlogscale=TRUE, ylogscale=TRUE, 
       ylab="probabilidades emp?ricas", xlab="probabilidades te?ricas", fitcol="blue",
       main="PP-plot sobre p?rdidas", legendtext=c("gamma", "Pareto", "Par-gam", "Burr"),
       plotstyle = "ggplot", fitpch=1:4)

#La que mejor ajusta es la Burr con lo teórico, en cambio la distribución empírica de Pareto es la que menos con respecto a la teórica

qqcomp(list(fgam, fpar, fmixgampar, fburr), xlogscale=TRUE, ylogscale=TRUE, 
       ylab="cuantiles emp?ricos", xlab="cuantiles te?ricos",
       fitcol="black", main="QQ-plot sobre p?rdidas", addlegend = TRUE,
       legendtext=c("gamma", "Pareto","Par-gam", "Burr"), fitpch=1:4)


#Grafico de asimetria y curtosis   EXPLICAR



p <- c(.9, .95, .975, .99)
rbind(empirica= quantile(danishuni$Loss, prob=p),
  gamma= quantile(fgam, prob=p)$quantiles,
  Pareto= quantile(fpar, prob=p)$quantiles,
  Pareto_gamma= quantile(fmixgampar, prob=p)$quantiles,
  Burr= quantile(fburr, prob=p)$quantiles)

compmom <- function(order)
  c(empirical= sum(danishuni$Loss^order)/length(x),
      gamma=mgamma(order, fgam[[1]][1], fgam[[1]][2]),
      Pareto=mpareto(order, fpar[[1]][1], fpar[[1]][2]),
      Pareto_gamma= as.numeric(fmixgampar[[1]][1]*
                                   mgamma(order, fmixgampar[[1]][2], fmixgampar[[1]][3])+
                                   (1-fmixgampar[[1]][1])*
                                   mpareto(order, fmixgampar[[1]][4], fmixgampar$estimate[5])),
      Burr=mburr(order, fburr[[1]][1], fburr[[1]][2], fburr[[1]][3]))
rbind(Mean=compmom(1), Mom2nd= compmom(2))

descdist(danishuni$Loss,discrete = FALSE, boot =100, method = "unbiased",
         graph = TRUE, obs.col = "blue", obs.pch = 16, boot.col = "red")

