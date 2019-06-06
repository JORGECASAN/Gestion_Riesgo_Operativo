#### MODELIZACION DE VALORES EXTREMOS


#Utilizamos la libreria evmix, para valores extremos
library(evmix)

#Distribucion Generalizada de Pareto
#expecificamos si calculo log densidad o no

dgpd(0.1, u = 0, sigmau = 1, xi = c(0.05, 0.1, 0.15), log = TRUE)



#Especifi si la probabilidad es acumulativa o de excedencia si la opcionlower.tail = FALSE o TRUE

pgpd(0.1,u = 0, sigmau = c(0.99, 1, 1.01), xi = 0.1, lower.tail = FALSE)

x=rgpd(2000,u = 0, sigmau = 1, xi = 0)


hist(x, pch=10, breaks=50, prob=TRUE, main="Fraudes",
     xlab =" X", ylab = "Densidad")
curve(dgpd(x, 0), col="red", lwd=2,
      add=T) #add=T para añadir al histograma anteior



### Mixtura de modelos param?tricos

##Esta libreria permite trabajar con modelos mixtos

dnormgpd(1.1, c(0.02, 0), 1, 1.28, 0.5173, -0.1489, phiu = TRUE)#phiu= pb de superar umbral=0.1

pnormgpd(1.4, 0, 1,c(1.1,1.28), 0.5173, c(-0.1,-0.1489), phiu=0.1)



#### ALGUNAS REPRESENTACION DE FUNCIONES DENSIDAD Y UMBRALES ######



par(mfrow=c(2,2))
x = seq(0,10,0.01)
f = dgammagpd(x, 2.1, 2, 7.7, 1.52, -0.07, phiu = 0.1) #phiu= pb de superar umbral=0.1
          #parám.u=7.7



plot(x, f, type = 'l', lty = 1, lwd = 2, ylab = "f(x)", main = "Func.densidad Gamma GPD")
abline(v = 7.7, lty = 3, lwd = 2) #Fijamos el umbral en 7.7


f = dgammagpd(x, 9, 0.5, 6.5, 0.62, -0.21, phiu = 0.1)

plot(x, f, type = 'l', lty = 1, lwd = 2, ylab = "f(x)", main = "Func.densidad Gamma GPD")
abline(v = 6.5, lty = 3, lwd = 2)


f = pgammagpd(x, 2.1, 2, 7.7, 1.52, -0.07, phiu = 0.1)

plot(x, f, type = 'l', lty = 1, lwd = 2, ylab = "F(x)", main = "Func.densidad Gamma GPD")
abline(v = 7.7, lty = 3, lwd = 2)


f = pgammagpd(x, 9, 0.5, 6.5, 0.62, -0.21, phiu = 0.1)
plot(x, f, type = 'l', lty = 1, lwd = 2, ylab = "F(x)", main = "Func.densidad Gamma GPD")
abline(v = 6.5, lty = 3, lwd = 2)




##### AJUSTE

x = rgamma(1000, shape = 2)
xx = seq(-1, 10, 0.01)
y = dgamma(xx, shape = 2)
#ar(xaxs="i", yaxs="i")

# Modelo de Bulk
fit = fgammagpd(x, phiu = TRUE, std.err = FALSE)

hist(x, breaks = 100, freq = FALSE, xlim = c(0, 10))
lines(xx, y)
lines(xx, dgammagpd(xx, gshape = fit$gshape, gscale = fit$gscale, u = fit$u,
                    sigmau = fit$sigmau, xi = fit$xi, phiu = TRUE), col="red")
abline(v = fit$u)



#Analisis de los valores de la cola
fit2 = fgammagpd(x, phiu = FALSE, std.err = FALSE)
plot(xx, y, type = "l")
lines(xx, dgammagpd(xx, gshape = fit$gshape, gscale = fit$gscale, u = fit$u,
                    sigmau = fit$sigmau, xi = fit$xi, phiu = TRUE), col="red")
lines(xx, dgammagpd(xx, gshape = fit2$gshape, gscale = fit2$gscale, u = fit2$u,
                    sigmau = fit2$sigmau, xi = fit2$xi, phiu = fit2$phiu), col="blue")
abline(v = fit$u, col = "red")
abline(v = fit2$u, col = "blue")
legend("topright", c("True Density","Bulk T. F","Parameterised T. F"),
       col=c("black", "red", "blue"), lty = 1, cex= 0.8)



### Ajuste por Kernel

#Aproximaciones visuales

x = rnorm(1000, 0, 1)

fit = fkdengpd(x, phiu = FALSE, std.err = FALSE)
hist(x, 100, freq = FALSE, xlim = c(-4, 4))
xx = seq(-4, 4, 0.01)
lines(xx, dkdengpd(xx, x, fit$lambda, fit$u, fit$sigmau, fit$xi, fit$phiu), col="blue", lwd = 2)

abline(v = fit$u, col="blue", lwd = 2) #Umbral estimado en el ajuste
legend("topright", "kdengpd", col = "blue",lty = 1, lwd = 2)



