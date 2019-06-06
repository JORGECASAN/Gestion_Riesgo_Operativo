#########################  PRACTICA 4.1  ###############################

################ DISTRIBUCION DE PERDIDAS AGREGADAS ####################



library(actuar)

#El codigo implementa una Gamma, con "n" v.a. Xi distribuidas como Gamma(n*shape, rate)


pgamsum <- function(x, dfreq, argfreq, shape, rate, Nmax=10)
  {
    tol <- 1e-10; maxit <- 10
    nbclaim <- 0:Nmax
    dnbclaim <- do.call(dfreq, c(list(x=nbclaim), argfreq))
    psumfornbclaim <- sapply(nbclaim, function(n)
      pgamma(x, shape=shape*n, rate=rate))
    psumtot <- psumfornbclaim %*% dnbclaim
    dnbclaimtot <- dnbclaim
    iter <- 0
    while( abs(sum(dnbclaimtot)-1) > tol && iter < maxit)
      {
        nbclaim <- nbclaim+Nmax
        dnbclaim <- do.call(dfreq, c(list(x=nbclaim), argfreq))
        psumfornbclaim <- sapply(nbclaim, function(n)
          pgamma(x, shape=shape*n, rate=rate))
        psumtot <- psumtot + psumfornbclaim %*% dnbclaim
        dnbclaimtot <- c(dnbclaimtot, dnbclaim)
        iter <- iter+1
         }
    as.numeric(psumtot)
}



#CASO 1: GAMMA

#N sigue una Poisson(10) y X sigue una Gamma(3,2)

parsev <- c(3, 2); parfreq <- 10 #Fijamos parametros v.a. severidad y frecuencia

meansev <- mgamma(1, parsev[1], parsev[2]) #Momento de orden 1 gamma
varsev <- mgamma(2, parsev[1], parsev[2]) - meansev^2 #Momento de orden 2 gamma
skewsev <- (mgamma(3, parsev[1], parsev[2]) -
                3*meansev*varsev - meansev^3)/varsev^(3/2) #Coef.Asimetria gamma

meanfreq <- varfreq <- parfreq[1]; skewfreq <- 1/sqrt(parfreq[1]) #Momento de orden 1 Poisson

meanagg <- meanfreq * meansev # Momento 1 Varaible agregeda

varagg <- varfreq * (varsev + meansev^2) # Varianza v. agregada

skewagg <- (skewfreq*varfreq^(3/2)*meansev^3 + 3*varfreq*meansev*
         varsev + meanfreq*skewsev*varsev^(3/2))/varagg^(3/2) # Coef.asimetria agre




######### Funci?n de distribucion de la perdidas agregadas F(s)

## funcion "aggregaDist"

#Proyecto de simulaci?n

F.s <- aggregateDist("simulation", model.freq = expression(y =rpois(parfreq)),
                     model.sev = expression(y =rgamma(parsev[1], parsev[2])),
                     nb.simul = 1000)


#Aproximaci?n a trav?s de Normal

F.n <- aggregateDist("normal", moments = c(meanagg, varagg))


#Aproximaci?n a trav?s de normal-power

F.np <- aggregateDist("npower", moments = c(meanagg, varagg, skewagg))


#Funcion exacta
F.exact <- function(x) pgamsum(x, dpois, list(lambda=parfreq),
                                  parsev[1], parsev[2], Nmax=100)


x <- seq(25,40) #Cambiar a 25,40

plot(x, F.exact(x), type="l",
        main="Distribucion Agregada de perdidas", ylab="F(x)")
lines(x, F.s(x), lty=2, col = "red")
lines(x, F.n(x), lty=3,col = "blue" )
lines(x, F.np(x), lty=4, col = "green")
legend("bottomright", leg=c("exacta", "simulacion",
                              "Aprox.normal", "Approx.NP"),
       col = c("black", "red", "blue", "green"),
       lty = 1:4, text.col = "black")






#CASO DE LA DISTRIBUCION PARETO

#N sigue una Pareto (10) y X sigue una gamma (3,2)

parsev <- c(3.1, 2*2.1) ; parfreq <- 10

xmax <- qpareto(1-1e-9, parsev[1], parsev[2]) #Func.cuantialica  


#Calculamos funci?n de distribucion discretizandola (funcion discretize)

#Discretizaci?n por M. recursivo

#Metodo insesgado
fx2 <- discretize(ppareto(x, parsev[1], parsev[2]), from = 0,
    to = xmax, step = 0.5, method = "unbiased",
            lev = levpareto(x, parsev[1], parsev[2])) #unbiased# metodo centrado


F2 <- aggregateDist("recursive", model.freq = "poisson",
     model.sev = fx2, lambda = parfreq, x.scale = 0.5, maxit=2000)

#Metodo d. superior
fx.u2 <- discretize(ppareto(x, parsev[1], parsev[2]), from = 0,
        to = xmax, step = 0.5, method = "upper")

F.u2 <- aggregateDist("recursive", model.freq = "poisson",
        model.sev = fx.u2, lambda = parfreq, x.scale = 0.5, maxit=2000)

#Metodo d. inferior
fx.l2 <- discretize(ppareto(x, parsev[1], parsev[2]), from = 0,
        to = xmax, step = 0.5, method = "lower")

F.l2 <- aggregateDist("recursive", model.freq = "poisson",
        model.sev = fx.l2, lambda = parfreq, x.scale = 0.5, maxit=2000)



x <- seq(50, 120)
plot(x, F2(x), type="l",
     main="Distribucion Agregada de perdidas", ylab="F1(y)")

lines(x, F.u2(x), lty=2, col="red")

lines(x, F.l2(x), lty=3, col="blue")


legend("bottomright", leg=c("Rec.insesgado", "Rec.superior", "Rec.inferior"),
       col = c("black", "red", "blue"),
       lty = 1:3, text.col = "black")



# Comentarios:
#ojo- con los cuantiles m?s altos; significativamente diferentes
#Para el caso gamma, la aproximaci?n normal-power es adecuada para la funci?n de distribuci?n exacta, 

