library(ggplot2)
library(MASS)
library(moments)
library(fitdistrplus)
library(actuar)

datos <- read.csv('BD_BUSINESS UNIT (1).csv', header = FALSE)
datos
str(datos)

ggplot(datos,aes(V1))+geom_histogram()

#Existen 4 outliers
boxplot(datos)

#La media y la mediana difieren bastante en sus valores por lo que vamos que existen outliers, los cuales quedan afectados en la media pero no en la mediana
summary(datos)

#No tenemos valores faltantes
anyNA(datos)


skewness(datos) #Al tener un valor superior a 3 la distribución no se comporta como una Normal, nos encontramos ante una distribución asimétrica
kurtosis(datos) #En la Kurtosis nos encontramos ante una situación de asimetría, teniendo un valor de 24.644, lo cual todo nos indica a que se comporta como una leptocúrtica


hist(datos$V1, pch=10, breaks=50, prob=TRUE, main="Perdidas de la empresa",
     xlab =" X", ylab = "Densidad")

x <- datos$V1

###ESTIMACION

dmixgampar <- function(x, prob, nu, lambda, alpha, theta)
  prob*dgamma(x, nu, lambda) + (1-prob)*dpareto(x, alpha, theta)
pmixgampar <- function(q, prob, nu, lambda, alpha, theta) 
  prob*pgamma(q, nu, lambda) + (1-prob)*ppareto(q, alpha, theta)


fgam <- fitdist(x, "gamma", lower=0)
fgam
plot(fgam)

fpar <- fitdist(x, "pareto",lower=0)
fpar
plot(fpar)

cbind(SINGLE= c(NA, fgam$estimate, fpar$estimate))



fburr <- fitdist(x, "burr", start=list(shape1=2, shape2=2, scale=2),
                 lower=c(0.1,1/2, 0))
fburr$estimate


#Comparando las densidades ajustadas



FDD=cdfcomp(list(fgam, fpar,fburr), xlogscale=TRUE,
            ylab = "Probabilidad", datapch=".",
            datacol="red", fitcol="black", fitlty=2:5,
            legendtext=c("gamma","Pareto", "Burr"),
            main="Ajuste perdidas legales", plotstyle = "ggplot")
FDD

#Realizando la comparación entre los diferentes modelos de Gamma, Pareto y Burr entre los valores teóricos con los observados la que mejor se comporta es la BURR


hist(x, pch=10, breaks=100, prob=TRUE, main="PERDIDAS LEGALES EN EUROS",
     xlab =" X", ylab = "Densidad")

curve(dburr(x, fburr$estimate[1],fburr$estimate[2], fburr$estimate[3]),
      col="red", lwd=2, add=T)



## qqplot con normal
qqnorm(x)
qqline(x, col="red")

#Nuestros valores más centricos se comportan como una Normal, sin embargo, los datos situados en las colas representan una mayor asimetría.


qmixgampar <- function(p, prob, nu, lambda, alpha, theta)
{L2 <- function(q, p) 
  (p - pmixgampar(q, prob, nu, lambda, alpha, theta))^2 
sapply(p, function(p) optimize(L2, c(0, 10^3), p=p)$minimun)
}



ppcomp(list(fgam, fpar, fburr), xlogscale=TRUE, ylogscale=TRUE, 
       ylab="probabilidades emp?ricas", xlab="probabilidades te?ricas", fitcol="blue",
       main="PP-plot sobre p?rdidas", legendtext=c("gamma", "Pareto", "Par-gam", "Burr"),
       plotstyle = "ggplot", fitpch=1:4)

#La linea recta representa el caso teórico y los datos representados en leyenda son los datos empíricos. Podemos observar que el comportamiento seguido
#por la distribución PAR-GAM es el más ajustado, seguido de GAMMA y finalmente el que menos se ajusta es el de PARETO.

qqcomp(list(fgam, fpar,fburr), xlogscale=TRUE, ylogscale=TRUE, 
       ylab="cuantiles emp?ricos", xlab="cuantiles te?ricos",
       fitcol="black", main="QQ-plot sobre p?rdidas", addlegend = TRUE,
       legendtext=c("gamma", "Pareto","Par-gam", "Burr"), fitpch=1:4)

#El gráfico, en relación con el anterior, muestra la semejanza respecto al ajuste de los datos teóricos representados a través de la recta, respecto a nuestros datos empíricos.


