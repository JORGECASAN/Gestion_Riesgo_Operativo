######################     CASOS VAR y ES #########################


## NOTA

alpha <- c(0.05)
q=qnorm(alpha, mean=0.05, sd=0.18) 
q


R=q*20000 #Perdida con rentabilidad negativa
R

Var=-R  #VaR,cambia el signo a R, para que resulte valor positivo
Var
#######


## CASO 1

#  VaR
alpha <- c(0.95, 0.99)
qnorm(alpha, mean=-0.05, sd=0.18) * 20000


# Tambien cambiando de signo a la media

(-0.05 + 0.18 * qnorm(alpha)) * 20000


# ES

(-0.05 + 0.18 * dnorm(qnorm(alpha))/(1 - alpha)) * 20000


# Representamos ambos casos

x <- seq(0.9,0.999, length=100)

yVaR <- (-0.05 + 0.18 * qnorm(x)) * 20000
yES <- (-0.05 + 0.18 * dnorm(qnorm(x))/(1 - x)) * 20000

plot(x, yVaR, type="l", ylim=range(yVaR, yES),
     xlab=expression(alpha), ylab="")

lines(x, yES, lty=2, col=2)
legend("topleft", legend=c("VaR","ES"),col=1:2, lty=1:2)



## CASO 2

library("sn")  #Para trabajar con la distribucion t


# a) VaR
alpha <- c(0.95,0.99)
mu <- 0.05
lambda <- 0.116
nu <- 5
q <- qst(alpha, location=-mu, scale=lambda, df=nu)

q * 20000


# ES
f <- dt(qt(alpha, df=nu), df=nu)
(-mu + lambda * f / (1-alpha) * (nu + qt(alpha, df=nu)^2) / (nu-1) ) * 20000





## CASO 3 


## Fijamos las fecha del analisis
sp.end <- as.Date("2019-03-31")
(sp.start <- sp.end - 1000)



## Tomamos datos de SP500

library("tseries") #la libreria permite descargar los datos de cotizacion



x <- get.hist.quote(start=sp.start, end = sp.end, instrument = "^gspc",
                    provider = "yahoo", quote="Close")


nrow(x)   # son 1000? 


#Fijamos la fecha comienzo de descarga

x <- get.hist.quote(start="2000-01-01", end = sp.end,
                    instrument = "^gspc",
                    provider = "yahoo", quote="Close")

x <- tail(x, 1000)  #Tama?o de la cola 

nrow(x) # Ahora son 1000?


plot(x, xlab="S&P 500", ylab="level")
grid() #A?adimos cudricula


## b)

library("zoo")  #libreria para series temporales


head(x)


#diff, calcula la tasa de variacion logaritmica de los precios = rentabilidad


sp500.ret.zoo <- diff(log(x))
head(sp500.ret.zoo)

sp500.ret <- as.numeric(sp500.ret.zoo)

hist(sp500.ret, breaks=50, freq=FALSE)
alpha <- 0.95
q<- quantile(sp500.ret, probs=1-alpha) 
q



abline(v=q, col=2)



## c)
## VaR empirico (Simulacion historica)

q* (-20000)

## ES empirico (Simulacion historica)

mean(sp500.ret[sp500.ret < q]) * (-20000)



## d) IC bootstrap 

set.seed(1234)
B <- 10000

## Utilizando la libreria "boot"

library(boot)
VaRES <- function(x,i){
        q <- quantile(x[i], probs=1-alpha)   
        c(-q * 20000, -mean(x[x < q]) * 20000)
}



res<-boot(sp500.ret,statistic=VaRES, R=B, stype="i")
  # stype="i", para tipo de caracteres indice (pueden ser frecuencias, pesos,..)

# Calulo del Intervalo de Confinza

boot.ci(res, conf=0.95, type="perc", index=1) 
boot.ci(res, conf=0.95, type="perc", index=2)# 2 ES



## e) Representacion grafica

plot(sp500.ret.zoo)
abline(h=q, col="red")
points(sp500.ret.zoo[sp500.ret < q], pch=20, col=2)





