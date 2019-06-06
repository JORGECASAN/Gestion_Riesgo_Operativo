#####################  VALORES EXTREMOS #######################

#Incluir
library(MASS)
library(CASdatasets)
library(car)
library(actuar) 
library(fitdistrplus)
library(ggplot2)

#Los datos con los que trabajaremos son los de danishuni

data(danishuni)

head(danishuni)
x <-danishuni$Loss
summary(x)
 
var(x)  

quantile(x,probs=c(0.05, 0.95))
quantile(x,seq(0,1, 0.20)) 
quantile(x,seq(0.9,1, 0.01))

#Comentario: La media y la mediana difieren, existe asimetr?a muy apuntada en torno al 4 cuartil.


# Datos de perdidas
danish.claim <- danishuni[,2]
danish.claim


#Block Maxima: Extrac. de los valores max de cada año

#Extrar las cuatro primeros valores de la variable
years <- as.numeric(substr(danishuni[,1], 1, 4))
years
danish.max <- aggregate(danish.claim, by=list(years), max, na.rm=TRUE)[,2]
danish.max


# Exceso sobre umbral. Extrac. de Valores que exceden un umbral
u <- 50
danish.exc <- danishuni[danishuni[,2] > u, 2]
danish.exc



#Visualizacion de las colas

n.u <- length(danish.exc) #n? de casos que superan u
surv.prob <- 1 - rank(danish.exc)/(n.u + 1)  #rank ofrece el n? de orden
surv.prob

plot(danish.exc, surv.prob, log = "xy", xlab = "Excesos", 
     ylab = "Probabilidades", ylim=c(0.01, 1))



#Añadimos las prob teoricas de la D.Pareto

#Determinamos los parámetros necesarios
alpha <- - cov(log(danish.exc), log(surv.prob)) / var(log(danish.exc))
alpha 

x = seq(u, max(danish.exc), length = 100) #divide de u a max() 100 interv.
y = (x / u)^(-alpha)
lines(x, y)

#Funci?n de distribuci?n acumulada
prob <- rank(danish.exc) / (n.u + 1)
plot(danish.exc, prob, log = "x", xlab= "Excesos", ylab = "Probabilidades de no excesos")
y = 1 - (x / u)^(-alpha)
lines(x, y)




 ########  ESTIMACION


#Dist. valores extremos generalizados (GEV)

nllik.gev <- function(par, data){
  mu <- par[1]
  sigma <- par[2]
  xi <- par[3]
  if ((sigma <= 0) | (xi <= -1))
    return(1e6)
  n <- length(data)
  if (xi == 0)
    n * log(sigma) + sum((data - mu) / sigma) +
    sum(exp(-(data - mu) / sigma))
  else {
    if (any((1 + xi * (data - mu) / sigma) <= 0))
      return(1e6)
    n * log(sigma) + (1 + 1 / xi) *
      sum(log(1 + xi * (data - mu) / sigma)) +
      sum((1 + xi * (data - mu) / sigma)^(-1/xi))
    }
  }


sigma.start <- sqrt(6) * sd(danish.max) / pi
mu.start <- mean(danish.max) + digamma(1) * sigma.start
fit.gev <- nlm(nllik.gev, c(mu.start, sigma.start, 0),
                 hessian = TRUE, data = danish.max)
fit.gev
fit.gev$estimate #par.posici?n, escala y forma

sqrt(diag(solve(fit.gev$hessian))) 




#Modelo Poisson-Generalizada de Pareto 

nllik.gp <- function(par, u, data){
  tau <- par[1]
  xi <- par[2]
    if ((tau <= 0) | (xi < -1))
      return(1e6)
    m <- length(data)
      if (xi == 0)
        m * log(tau) + sum(data - u) / tau
    else {
        if (any((1 + xi * (data - u) / tau) <= 0))
          return(1e6)
      m * log(tau) + (1 + 1 / xi) *
         sum(log(1 + xi * (data - u) / tau))
       }
     }


u <- 10
tau.start <- mean(danish.exc) - u
fit.gp <- nlm(nllik.gp, c(tau.start, 0), u = u, hessian = TRUE,
              data = danish.exc)
fit.gp
fit.gp$estimate 
#El parametro eta m/n=0,005



#Intervalo de confianza del valor m?ximo del indice de cola (si=0,50)
# al 95%
prof.nllik.gp <- function(par,xi, u, data)
  nllik.gp(c(par,xi), u, data)
prof.fit.gp <- function(x)
  -nlm(prof.nllik.gp, tau.start, xi = x, u = u, hessian = TRUE,
         data = danish.exc)$minimum
vxi = seq(0,1.8,by=.025)
prof.lik <- Vectorize(prof.fit.gp)(vxi)
plot(vxi, prof.lik, type="l", xlab = expression(xi),
       ylab = "Profile log-likelihood")
opt <- optimize(f = prof.fit.gp, interval=c(0,3), maximum=TRUE)
opt




up <- opt$objective
abline(h = up, lty=2)
abline(h = up-qchisq(p = 0.95, df = 1), col = "grey")
I <- which(prof.lik >= up-qchisq(p = 0.95, df = 1))
lines(vxi[I], rep(up-qchisq(p = 0.95, df = 1), length(I)),
        lwd = 5, col = "grey")
abline(v = range(vxi[I]), col = "grey", lty = 2)
abline(v = opt$maximum, col="grey")




#Point Process, o para valores err?ticos
nllik.pp <- function(par, u, data, n.b){
  mu <- par[1]
  sigma <- par[2]
  xi <- par[3]
  if ((sigma <= 0) | (xi <= -1))
    return(1e6)
  if (xi == 0)
    poiss.meas <- n.b * exp(-(u - mu) / sigma)
    else
      poiss.meas <- n.b * max(0, 1 + xi * (u - mu) / sigma)^(-1/xi)
      exc <- data[data > u]
      m <- length(exc)
      if (xi == 0)
        poiss.meas + m * log(sigma) + sum((exc - mu) / sigma)
      else {
        if (any((1 + xi * (exc - mu) / sigma) <= 0))
          return(1e6)
        poiss.meas + m * log(sigma) + (1 + 1 / xi) *
          sum(log(1 + xi * (exc - mu) / sigma))
        }
}

n.b <- 1991 - 1980
u <- 10
sigma.start <- sqrt(6) * sd(danish.exc) / pi
mu.start <- mean(danish.exc) + (log(n.b) + digamma(1)) *
  sigma.start


fit.pp <- nlm(nllik.pp, c(mu.start, sigma.start, 0), u = u,
              hessian = TRUE, data = danishuni[,2], n.b = n.b)
fit.pp


#Estimacion otros indices de cola

#Intervalo confianza asintotico del ?ndice de cola

logXs <- log(sort(danishuni[,2], decreasing=TRUE))
n <- length(logXs)
xi <- 1/1:n * cumsum(logXs) - logXs
ci.up <- xi + qnorm(0.975) * xi / sqrt(1:n)
ci.low <- xi - qnorm(0.975) * xi / sqrt(1:n)
matplot(1:n, cbind(ci.low, xi, ci.up),lty = 1, type = "l",
          col = c("blue", "black", "blue"), ylab = expression(xi),
          xlab = "Numero valores extremos")


#Intervalo confianza de alfa=1/indice de cola

alpha <- 1 / xi
alpha.std.err <- alpha / sqrt(1:n)
ci.up <- alpha + qnorm(0.975) * alpha / sqrt(1:n)
ci.low <- alpha - qnorm(0.975) * alpha / sqrt(1:n)
matplot(1:n, cbind(ci.low, alpha, ci.up), lty = 1, type = "l",
          col = c("blue", "black", "blue"), ylab = expression(alpha),
          xlab = "Numero valores extremos")


#Exceso sobre la media

meanExcessPlot <- function(data, u.range = range(data),n.u = 100){
  mean.excess <- ci.up <- ci.low <- rep(NA, n.u)
  all.u <- seq(u.range[1], u.range[2], length = n.u)
  for (i in 1:n.u){
  u <- all.u[i]
  excess <- data[data > u] - u
  n.u <- length(excess)
  mean.excess[i] <- mean(excess)
  var.mean.excess <- var(excess)
  ci.up[i] <- mean.excess[i] + qnorm(0.975) *
  sqrt(var.mean.excess / n.u)
  ci.low[i] <- mean.excess[i] - qnorm(0.975) *
  sqrt(var.mean.excess / n.u)
  }
      matplot(all.u, cbind(ci.low, mean.excess, ci.up), col = 1,
      lty = c(2, 1, 2), type = "l", xlab = "u", ylab = "Exc.sobre media")
}
meanExcessPlot(danish.exc)




#### VALIDACION DEL MODELO

#Q-Q Plot para la Dist. Generalizada de Pareto (DGP)

qqgpd <- function(data, u, tau, xi){
  excess <- data[data > u]
  m <- length(excess)
  prob <- 1:m / (m + 1)
  x.hat <- u + tau / xi * ((1 - prob)^-xi - 1)
    ylim <- xlim <- range(x.hat, excess)
    plot(sort(excess), x.hat, xlab = "Quantiles en la muestra",
           ylab = "Quantiles ajustados", xlim = xlim, ylim = ylim)
    abline(0, 1, col = "grey")
    }

qqgpd(danishuni[,2], 10, 7, 0.5)#u=10, tau=7 y indice cola=0,5




#P-P Plot para la Dist. Generalizada de Pareto (DGP)

ppgpd <- function(data, u, tau, xi){
  excess <- data[data > u]
  m <- length(excess)
  emp.prob <- 1:m / (m + 1)
  prob.hat <- 1 - (1 + xi * (sort(excess) - u) / tau)^(-1/xi)
  plot(emp.prob, prob.hat, xlab = "Probabilidades empiricas",
         ylab = "Probabilidades ajustadas", xlim = c(0, 1),
         ylim = c(0, 1))
  abline(0, 1, col = "grey")
}

ppgpd(danishuni[,2], 10, 7, 0.5) #u=10, tau=7 y indice cola=0,5







############## VAR###################################
library(vars)
library(sn)


modelo<- qnorm(0.05,0.05,0.18)
modelo

R<- modelo*20000
R

var<--R
var
dnorm() 












