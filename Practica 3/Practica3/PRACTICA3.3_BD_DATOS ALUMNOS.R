#######  VALORES EXTREMOS BD_SEVERIDAD



#Paquetes que vamos a necesitar

library(MASS)
library(actuar) 
library(fitdistrplus)
library(ggplot2)
library(evmix)



xx=read.csv("BD_BUSINESS UNIT.csv",header = FALSE, sep = ";")
str(xx)



#Se fija el umbral???? 
#Por ejemplo, 650000
u<-650000
excess<- xx [xx$V1 >u,] #Para seleccionar los valores sobre un umbral
excess
summary(excess)




#Visualizacion de las colas

n.u <- length(excess) #n? de casos que superan u
n.u
surv.prob <- 1 - rank(excess)/(n.u + 1)  #rank ofrece el n? de orden
surv.prob

plot(excess, surv.prob, log = "xy", xlab = "Excesos", 
     ylab = "Survival probability", ylim=c(0.01, 1))






