################    Pr?ctica 1_fraude1     ###########################




#Paquetes que vamos a necesitar

library(moments) #Package para calculo de momentos (asimetr?a, curtosis, ...)
library(actuar) #Package para an?lisis actuarial
library(fitdistrplus) #Package para ajuste de distribuciones
library(ggplot2) #Package para visulacion



x<-read.csv("C:/Users/jcasa/OneDrive/Escritorio/2º SEMESTRE/Gestión del riesgo operativo/Practica1/pract1.csv")
names(x)[1]=paste("Fraudes")



################### Informacion sobre los datos


head(x$Fraudes,10) #vemos los 10 primeros elementos
summary(x$Fraudes)
table(x$Fraudes) #tabla de frecuencias

skewness(x$Fraudes)  #coef. de asimetria - Asimetria positiva o la dcha. Si el coeficiente fuese 0 estaríamos ante una Normal
kurtosis(x$Fraudes)  #coef. de curtosis  - Leprtocurtica o mas apuntada Normal. Es máás apuntada. Valores superiores a 3 significa que no sigue una
#distribución Normal, siendo en tal caso leptocurtica


#Tambien podemos hacer:

mean(x$Fraudes)
var(x$Fraudes)  
median(x$Fraudes)
quantile(x$Fraudes,probs=c(0.05, 0.95))
quantile(x$Fraudes,seq(0,1, 0.20)) 
quantile(x$Fraudes,seq(0.9,1, 0.01))

hist(x$Fraudes, pch=20, breaks=25, prob=TRUE, main="HISTOGRAMA",
     xlab =" N? Fraudes", ylab = "Frecuencia")




#MODELOS PARAMETRICOS

#Ajuste m?xima verosimilitud de distribuciones univariables

fpois=fitdist(x$Fraudes, "pois")
fpois

#El lambda es 0.4862 es la media. Hay que tener e cuenta que el lambda es n*p
plot(fpois)
#Estos gráficos son la función de densidad y por otra parte la función de distribución.
#Vemos que tanto lo empírico como lo teórico está bastante bien ajustado.


fnbinom=fitdist(x$Fraudes, "nbinom")
fnbinom
plot(fnbinom)

#Ambas gráficas nos ofrecen los mismos resultados. 

#¿Pero... con cuál de las dos nos quedamos?


#Ajuste por bondad del ajuste

gofstat(list(fpois, fnbinom), chisqbreaks=c(0:4, 9), discrete=TRUE,
        fitnames=c("Poisson", "Binomial Negativa"))


#Ajustar unos valores a un modelo. Unos datos empíricos con unos teóricos y el estadístico de contraste es la Chi-cuadrado.
#Los chisqbreaks quiero que me agrupe por lo que quiero. 


#Estadisticos de la Chi-cuadrado:
#p-valor evidencia que sigue una binomial negativa y una Poisson puesto que para ambas, la hipótesis nula es la normalidad.
# Ejemplo: 
##oBSERVADO: 6131 OBSERVACIONES PARA EL 0
##Si siguiesen una Poisson habría 6150 para el 0 y para la distribución negativa 6.149
##AIC Y EL BIC. El ajuste es mucho mejor en la de Poisson que en la binomial negativa, ya que tanto el AIC como el BIC es inferior

#Por este criterio entonces nos quedaremos con la distribución de Poisson














