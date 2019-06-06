################# DISTRIBUCIONES DE PROBABILIDAD ######################

#Funciones de la distribuciones de probabilidad
#raiz:foo (nombre de la distribuci?n)

#dfoo--funci?n de densidad
#pfoo--funci?n de distribuci?n (porb.acumulada)
#qfoo--Funci?n inversa F^-1(x)
#rfoo--generador aleatorio de valores


#Librerias centradas en las principales distribuciones de la ciencia actuarial
# actuar y ActuDistns

library(actuar)



# DISTRIBUCIONES DE SEVERIDAD

dgamma(1:2, shape=2, rate=3/2)
pgamma(1:2, shape=2, rate=3/2)
qgamma(1/2, shape=2, rate=3/2) #Nos da el valor de la distribución total de probabilidad
set.seed(1)
rgamma(5, shape=2, rate=3/2)
#rate es el parámetor de forma


#Tansformaci?n de gamma

f <- function(x) dgamma(x,2)
f1 <- function(x) f(x-1)
f2 <- function (x) f(x/2)/2
f3 <- function (x) 2*x*f(x^2)
f4 <- function (x) f(1/x)/x^2
f5 <- function (x) f(exp(x))*exp(x)
f6 <- function (x) f(log(x))/x
x=seq(0,10, by=.025)
plot(x,f(x), ylim=c(0, 1.3), xlim=c(0, 10), main= "Densidades te?ricas", 
     lwd=2, type="l", xlab="x", ylab="")

lines(x, f1(x), lty=2, lwd=2)
lines(x, f2(x), lty=3, lwd=2)
lines(x, f3(x), lty=4, lwd=2)
lines(x, f4(x), lty=1, col="blue", lwd=2)
lines(x, f5(x), lty=2, col="green", lwd=2)
lines(x, f6(x), lty=3, col="red", lwd=2)
legend("topright", lty=1:4, col = c(rep("black",4), "blue",
                                    "green", "red"),
       leg=c("X", "X+1", "2X", "sqrt(X)", "1/X", "log(x)", "exp(x)"))



# A trav?s de simulaciones con visualizaci?n de densidades Kernel

set.seed (123)

x <- rgamma(100,2) #Si omitimos el par?metro escala, se asume que es 1
x
x1 <- (x+1)
x2 <- 2*x
x3 <- sqrt(x)
x4 <- 1/x
x5 <- log(x)
x6 <- exp(x)
x=seq(0,10, by=.025)
plot(density(x), ylim=c(0, 1), xlim=c(0, 10), main="Densidades emp?ricas",
     lwd=2, xlab="x", ylab="f_X(x)")
lines(density(x1), lty=2, lwd=2)
lines(density(x2), lty=3, lwd=2)
lines(density(x3), lty=4, lwd=2)
lines(density(x4), lty=1, col="blue", lwd=2)
lines(density(x5), lty=2, col="green", lwd=2)
lines(density(x6), lty=3, col="red", lwd=2)
legend("topright", lty=1:4, col = c(rep("black",4), "blue","green", "red"),
       leg=c("X", "X+1", "2X", "sqrt(X)", "1/X", "log(x)", "exp(x)"))






#DISTRIBUCION DE FRECUENCIA


#DISTRIBUCION POISSON 
#Generar muestra de n=100 Lambda=0.9
x= rpois(100, 0.9)
rpois(100,0.9)
hist(x,probability=T)  #Histograma

mean (x)  #calculo de la media
var(x)  #calculo de la varianza
sd(x)  #calculo de desviaci?n t?pida
sqrt(var(x))
median(x)
quantile(x,probs=c(0.05, 0.95))
quantile(x,seq(0,1, 0.20)) #calculo de quintiles
quantile(x,seq(0.9,1, 0.01))  #Calculo de percentiles del 90-100
summary(x)


#Transformaciones de la Poisson

#Se suelen implementar en paquetes especiales, pero tambien con function:

dpoisZM <- function(x, prob, lambda)
  prob*(x == 0) + (1-prob)*(x >0)*dpois(x-1, lambda)
ppoisZM <- function(q, prob, lambda)
  prob*(q >= 0) + (1-prob)*(q > 0)*ppois(q-1, lambda)
qpoisZM <- function(p, prob, lambda)
  ifelse(p <=prob, 0, a+qpois((p-prob)/(1-prob), lamda))
rpoisZM <- function(n, prob, lambda)
  
x <- rpoisZM(100, 1/2, 3)
plot(ecdf(x), main="F. distribucion Poisson truncada")
lines(z <- sort(c(0:12, 0:12-1e-6)),
      ppoisZM(z, 1/2, 3), col="blue", lty=4, lwd=2)
legend("bottomright", lty=c(1,4), lwd=1:2,
       col=c("black", "blue"), leg=c("empir?rica", "te?rica"))





#### Distribuciones Mixtas

#####Distribucion gamma y Pareto

dmixgampar <- function(x, prob, nu, lambda, alpha, theta)
  prob*dgamma(x, nu, lambda) + (1-prob)*dpareto(x, alpha, theta)
pmixgampar <- function(q, prob, nu, lambda, alpha, theta) 
  prob*pgamma(q, nu, lambda) + (1-prob)*ppareto(q, alpha, theta)


############################################################################################################################

library(moments)
library(MASS)
library(actuar)
library(ggplot2)
library(fitdistrplus)


data("danishuni") #Tenemos 2167 observaciones con dos variables
head(danishuni)

x<- danishuni$Loss
summary(x)
skewness(x)
kurtosis(x)
hist(x)

boxplot(x) #Valores muy concentrados en el priemr cuartil y valores extremos muy dispersos en el último, lo cual es muy ASIMETRÍA
ggplot(danishuni) + geom_density(aes(x))



fgam<-fitdist(x, 'gamma',lower=0)
fgam
plot(fgam)

fpar<- fitdist(x, 'pareto', start= list(shape=2, scale=2), lower= 0)
fpar
plot(fpar)

#Por defecto se estima por máxima verosimilitud








