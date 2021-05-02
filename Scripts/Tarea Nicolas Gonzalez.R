### Tarea
library(readxl)
library(tidyverse)
library(lubridate)

rgdp <- read_excel("C:/Users/NIGOJ/Desktop/Nico/Cosas de la U/Econometria R/Econometria/Bases de datos/ch4_rgdp.xls")
datos <- as.data.frame(rgdp)

datos$DATE <- ymd(datos$DATE)

View(datos)

### FIGURE 3.1
par(mfcol = c(1,1), oma = c(0,0,1,0) + 0.2, mar = c(0,2,0,0) + 1, mgp = c(0, 0.2, 0))
plot(datos$DATE,datos$RGDP,type="l",las=1,xaxs="i",xlab="",ylab="",tck=.02,yaxs="i",ylim=c(0,15000),col="steelblue4")
lines(datos$DATE,datos$Potential,lty=3,col="steelblue4")
lines(datos$DATE,datos$RCons,lty=2,col="steelblue4")
lines(datos$DATE,datos$Rinv,lty=4,col="steelblue4")

par(mfcol = c(1,1), oma = c(0,0,1,0) + 0.2, mar = c(0,2,0,0) + 1, mgp = c(0, 0.2, 0))
plot(datos$DATE,datos$RGDP,type="l",las=1,xaxs="i",xlab="",ylab="",tck=.02,yaxs="i",ylim=c(0,15000),col="steelblue4")
lines(datos$DATE,datos$Potential,lty=3,col="purple")
lines(datos$DATE,datos$RCons,lty=2,col="red")
lines(datos$DATE,datos$Rinv,lty=4,col="green")

plot(datos$DATE[-c(1)],100*diff(log(datos$RGDP,4)),type="l",las=1,xaxs="i",xlab="",ylab="",tck=.02,yaxs="i",col="steelblue4",ylim=c(-3,4))
abline(h=0)

datos$ret = c(0,100*diff(log(datos$RGDP,4)))

spec.ar1 = arfimaspec(mean.model=list(armaOrder=c(1,0)))
fit.ar1 = arfimafit(spec.ar1,data=datos$ret,solver="gosolnp")
fit.ar1
sres.ar1 = fit.ar1@fit$z

spec.ar4 = arfimaspec(mean.model=list(armaOrder=c(4,0)))
fit.ar4 = arfimafit(spec.ar4,data=sres.ar1^2,solver="gosolnp")
fit.ar4

datos$indicator = 1
datos$indicator[1:which(datos$DATE=="1984-01-01")-1] = 0
spec.ar1arch1 = ugarchspec(mean.model=list(armaOrder=c(1,0)),
                           variance.model=list(garchOrder=c(1,0),external.regressors=matrix(datos$indicator)))
fit.ar1arch1 = ugarchfit(spec.ar1arch1,data=datos$ret,solver="gosolnp")
fit.ar1arch1

#GARCH(1,2)
spec.ar1garch12 = ugarchspec(mean.model=list(armaOrder=c(1,0)),
                           variance.model=list(garchOrder=c(1,2),external.regressors=matrix(datos$indicator)))
fit.ar1garch12 = ugarchfit(spec.ar1garch12,data=datos$ret,solver="gosolnp")
fit.ar1garch12

#GARCH(2,1)
spec.ar1garch21 = ugarchspec(mean.model=list(armaOrder=c(1,0)),
                           variance.model=list(garchOrder=c(1,0),external.regressors=matrix(datos$indicator)))
fit.ar1garch21 = ugarchfit(spec.ar1garch21,data=datos$ret,solver="gosolnp")
fit.ar1garch21

#ARCH(2)
spec.ar1arch2 = ugarchspec(mean.model=list(armaOrder=c(1,0)),
                           variance.model=list(garchOrder=c(1,0),external.regressors=matrix(datos$indicator)))
fit.ar1arch2 = ugarchfit(spec.ar1arch2,data=datos$ret,solver="gosolnp")
fit.ar1arch2

#ARCH(3)
spec.ar1arch3 = ugarchspec(mean.model=list(armaOrder=c(1,0)),
                           variance.model=list(garchOrder=c(1,0),external.regressors=matrix(datos$indicator)))
fit.ar1arch3 = ugarchfit(spec.ar1arch3,data=datos$ret,solver="gosolnp")
fit.ar1arch3