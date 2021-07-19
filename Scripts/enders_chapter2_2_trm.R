#####################################
#####################################
###### TRM 

library("gdata")
library(readxl)
library(rugarch)
library(zoo)

# carga datos de trm2020
datosbase <- trm2020
View(datosbase)
datosbase$fecha <- as.Date(datosbase$fecha,format="%Y-%m-%d")

datosbase$g <- c(NA,diff(log(datosbase$TRM)))
datosbase$g[datosbase$g==0] <- NA

datos <- subset(datosbase,!is.na(datosbase$g))
View(datos)

### modelos ARMA para g

par(mar=c(2,3,1,1))

par(mfrow=c(2,1))
plot(datos$fecha,datos$TRM,type="l",las=1,xaxs="i",yaxs="i",
     xlab="",ylab="",main="TRM",tck=0.02,col="steelblue4",
     ylim=c(1500,4200))
abline(h=0)
plot(datos$fecha,datos$g,type="l",las=1,xaxs="i",yaxs="i",
     xlab="",ylab="",main="tasa de crecimiento TRM",
     tck=0.02,col="steelblue4",ylim=c(-0.1,0.1))
abline(h=0)

par(mfrow=c(2,1))
acf2=acf(datos$g,lag=12,tck=.02,xlab="",ylab="",main="",las=1)
pacf2=pacf(datos$g,lag=12,tck=.02,xlab="",ylab="",main="",las=1)


spec.ar1 = arfimaspec(mean.model=list(armaOrder=c(1,0),include.mean=TRUE))
fit.ar1 = arfimafit(spec=spec.ar1,data=datos$g)
fit.ar1
res.ar1 = fit.ar1@fit$residuals
Box.test(res.ar1,lag=4,type="Ljung-Box")
Box.test(res.ar1,lag=8,type="Ljung-Box")
Box.test(res.ar1,lag=12,type="Ljung-Box")

acf.ar1=acf(res.ar1,lag=24,tck=.02,xlab="",ylab="",main="",las=1)


spec.ar13 = arfimaspec(mean.model=list(armaOrder=c(3,0),include.mean=TRUE),
                       fixed.pars =list(ar2=0) )
fit.ar13 = arfimafit(spec=spec.ar13,data=datos$g)
fit.ar13
res.ar13 = fit.ar13@fit$residuals
Box.test(res.ar13,lag=4,type="Ljung-Box")
Box.test(res.ar13,lag=8,type="Ljung-Box")
Box.test(res.ar13,lag=12,type="Ljung-Box")


spec.ar1.ma1 = arfimaspec(mean.model=list(armaOrder=c(1,1),
                                           include.mean=TRUE) )

fit.ar1.ma1 = arfimafit(spec=spec.ar1.ma1,data=datos$g)
fit.ar1.ma1
res.ar1.ma1 = fit.ar1.ma1@fit$residuals
Box.test(res.ar1.ma1,lag=4,type="Ljung-Box")
Box.test(res.ar1.ma1,lag=8,type="Ljung-Box")
Box.test(res.ar1.ma1,lag=12,type="Ljung-Box")


tpron <- which(datos$fecha=="2020-12-31")
tpron
actual = datos$g[-(1:tpron)]
total <- length(datos$g)
muestra <- total - tpron


### 1-STEP AHEAD ROLLING WINDOW FORECAST

# OPCIONES para solver = "solnp","gosolnp","nlminb","L-BGFS-U"
# intente solnp <- gosolnp  <-  nlminb

fore.ar1 = NULL
spec.ar1 = arfimaspec(mean.model=list(armaOrder=c(1,0),include.mean=TRUE))
print("hizo spec.ar1")

for (i in 1:muestra) {
   print(i)
   print(tpron+i-1)
   fit.ar1=arfimafit(spec=spec.ar1,data=datos$g[1:(tpron+i-1)],solver="solnp")
   print("hizo fit.ar1")
   pron.ar1 <- arfimaforecast(fit.ar1,n.ahead=1)
   fore.ar1[i]=pron.ar1@forecast$seriesFor
   print("hizo fore.ar1")
}

mean(fore.ar1)
var(fore.ar1)

par(mfrow=c(1,1))
plot(fore.ar1,type="l",col="blue",ylim=c(-0.02,0.02))
lines(actual,type="l",col="darkgreen")

### PAGE 95
lm95 <- summary(lm(actual~fore.ar1))
#resolver la prueba H0: beta1=1   H1: beta1 =/= 1       tep = (beta1^ - 1)/ee(beta1^)
betagorro <-  lm95$coefficients[2,1]
eebetagorro <- lm95$coefficients[2,2]
tep = (betagorro - 1)/eebetagorro
tep
2*pt(abs(tep),df=50-2,lower.tail = F)

plot(actual,fore.ar1,xlim=c(-0.02,0.02),ylim=c(-0.02,0.02))
abline(a=0,b=1,col="red")


### SEASONALITY         OJO líneas nuevas
par(mar=c(2,3,1,1))
par(mfrow=c(2,1))

plot(datos$fecha,datos$TRM,type="l",las=1,xaxs="i",yaxs="i",
     xlab="",ylab="",main="TRM",tck=0.02,col="steelblue4",
     ylim=c(1500,4200))
abline(h=0)
plot(datos$fecha,datos$g,type="l",las=1,xaxs="i",yaxs="i",
     xlab="",ylab="",main="tasa de crecimiento TRM",
     tck=0.02,col="steelblue4",ylim=c(-0.1,0.1))
abline(h=0)



#-------------------------------------------------------
# código nuevo
#  5 días hábiles cada semana
# g  perdió una observación, la primera

t = rep(c(1,2,3,4,5),length.out=length(datos$g))

by(datos$g,t,summary)

par(mfrow=c(2,1))
par(mar=c(3,2,1,1))
plot(t,datos$g)
boxplot(datos$g~t)

#-----------------------------------------------------

### PARAMETER INESTABILITY

par(mfrow=c(2,1))
par(mar=c(3,2,1,1))

plot(datos$fecha,datos$TRM,type="l",las=1,
     xaxs="i",yaxs="i",xlab="",ylab="",
     main="T R M",
     tck=0.02,col="steelblue4",ylim=c(1500,4000))
abline(h=0)
plot(datos$fecha,datos$g,type="l",las=1,
     xaxs="i",yaxs="i",xlab="",ylab="",
     main="Tasa variación TRM",
     tck=0.02,col="steelblue4",ylim=c(-0.02,0.02))
abline(h=0)

tm <- which(datos$fecha=="2014-12-02")
t <-  which(datos$fecha=="2016-11-01")

spec.arma1 = arfimaspec(mean.model=list(armaOrder=c(1,0),
                                        include.mean=TRUE))
fit.arma1.pre = arfimafit(spec=spec.arma1,data=datos$g[1:tm])
fit.arma1.pre
fit.arma1.post = arfimafit(spec=spec.arma1,data=datos$g[(tm+1):t])
fit.arma1.post
fit.arma1 = arfimafit(spec=spec.arma1,data=datos$g)
fit.arma1


length(fit.arma1@model$fixed.pars) #   
length(fit.arma1@fit$coef)      #  11: mu,ar1,..,arp,ma1,...,maq,sigma
n <- length(fit.arma1@fit$coef) - length(fit.arma1@model$fixed.pars)  - 1
gl1 <- n
gl2 <- t - 2*n

SSR1 = sum(fit.arma1.pre@fit$residuals^2)
SSR2 = sum(fit.arma1.post@fit$residuals^2)
SSR  = sum(fit.arma1@fit$residuals^2)
Fcambio = ((SSR-SSR1-SSR2)/gl1)/((SSR1+SSR2)/gl2)
pf(Fcambio,df1=gl1,df2=gl2,lower.tail = F)



### ENDOGENOUS BREAKS

# AR1
# CUMSUM PLOT
library(forecast)

forecasts = NULL
for (i in 1:(length(datos$g)-2)){
   print(i)
   model = arima(datos$g[1:(2+i-1)],c(1,0,0))
   forecasts[i] = forecast(model,h=1)$mean # Gives the one-step forecast
}

res = datos$g[-c(1:2)]-forecasts

#  solver = solnp, gosolnp, nlminb, nloptr

# INTERCEPT PLOT
slope.est=intercept.est=slope.sigma.est=int.sigma.est=estadF=NULL
for (i in 1:(length(datos$g)-3)){
   print(i)
   spec.ar1 = arfimaspec(mean.model=list(armaOrder=c(1,0),
                                           include.mean=TRUE))
   fit.ar1 = arfimafit(spec=spec.ar1,data=datos$g[1:(3+i)],
                       solver="nloptr")

   intercept.est[i] <- fit.ar1@fit$matcoef[1,1]
   slope.est[i] <- fit.ar1@fit$matcoef[2,1]
   int.sigma.est[i] <- fit.ar1@fit$matcoef[1,2]
   slope.sigma.est[i] <- fit.ar1@fit$matcoef[2,2]
   
}

int.upper.band=intercept.est+2*int.sigma.est
int.lower.band=intercept.est-2*int.sigma.est
slope.upper.band=slope.est+2*slope.sigma.est
slope.lower.band=slope.est-2*slope.sigma.est

par(mfrow=c(3,1))
plot(intercept.est, type="l",xlab="",ylab="",ylim=c(-1,1),xaxs="i",las=1,tck=.02,main="Intercept")
lines(int.upper.band, col="steelblue4")
lines(int.lower.band, col="steelblue4")
abline(h=0,lty=2)


plot(slope.est, type="l",xlab="",ylab="",ylim=c(-1,1),xaxs="i",las=1,tck=.02,main="Autoregressive Parameter")
lines(slope.upper.band, col="steelblue4")
lines(slope.lower.band, col="steelblue4")
abline(h=0,lty=2)

# CUMSUM
# corregir: cambie todos los length( )  por  nrow( )


CUMSUM=NULL
for (i in 1:400){
   CUMSUM=c(CUMSUM,sum(res[1:i])/sd(res))
}

lower=upper=NULL
for (i in 1:400){
   upper[i]= 0.948*(400^0.5)+2*(i-1)*(400^-0.5)
   lower[i]=-0.948*(400^0.5)-2*(i-1)*(400^-0.5)
}

plot(CUMSUM, type="l", xlim=c(0,150),ylim=c(-40,50),las=1,xaxs="i",xlab="",ylab="",tck=0.02,main="The CUMSUM Test")
lines(upper, col="steelblue4",lty=2)
lines(lower, col="steelblue4",lty=2)
abline(h=0,lty=2)

par(mfrow=c(1,1))

acf(res)

acfr <- acf(res)
acfr
