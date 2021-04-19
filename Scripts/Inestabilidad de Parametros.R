### PARAMETER INSTABILITY


library(readxl)
library(rugarch)
library(zoo)

data = read_excel("C:/Users/NIGOJ/Desktop/Nico/Cosas de la U/Econometria R/Econometria/Bases de datos/ch1_quarterly.xls")

data$DATE = as.yearqtr(data$DATE)
data$spread = data$r5-data$Tbill

par(mfrow=c(2,1))
par(mar=c(3,2,1,1))

plot(data$DATE,data$spread,type="l",las=1,xaxs="i",yaxs="i",xlab="",ylab="",main="The Interest Rate Spread",tck=0.02,col="steelblue4",ylim=c(-2,4))
abline(h=0)
plot(data$DATE[-1],diff(data$spread),type="l",las=1,xaxs="i",yaxs="i",xlab="",ylab="",main="First-Difference of The Spread",tck=0.02,col="steelblue4",ylim=c(-3,3))
abline(h=0)

tm <- which(data$DATE=="1981 Q4")
t <-  which(data$DATE=="2008 Q1")

spec.arma27 = arfimaspec(mean.model=list(armaOrder=c(2,7),include.mean=TRUE),
                         fixed.pars=list(ma2=0,ma3=0,ma4=0,ma5=0,ma6=0))
fit.arma27.pre = arfimafit(spec=spec.arma27,data=data$spread[1:tm])
fit.arma27.pre
fit.arma27.post = arfimafit(spec=spec.arma27,data=data$spread[(tm+1):t],solver="gosolnp")
fit.arma27.post
fit.arma27 = arfimafit(spec=spec.arma27,data=data$spread,solver="gosolnp")
fit.arma27


length(fit.arma27@model$fixed.pars)    #   5: ma2,ma3,ma4,ma5,ma6,
length(fit.arma27@fit$coef)            #  11: mu,ar1,ar2,m1,...,ma7,sigma   
n <- length(fit.arma27@fit$coef) - length(fit.arma27@model$fixed.pars)  - 1
gl1 <- n
gl2 <- t - 2*n

SSR1 = sum(fit.arma27.pre@fit$residuals^2)
SSR2 = sum(fit.arma27.post@fit$residuals^2)
SSR  = sum(fit.arma27@fit$residuals^2)
Fcambio = ((SSR-SSR1-SSR2)/gl1)/((SSR1+SSR2)/gl2)
pf(Fcambio,df1=gl1,df2=gl2,lower.tail = F)


#  método 2 
# se sospecha de un cambio en 1981Q4
data$Indicator = 0
data$Indicator[which(data$DATE=="1982 Q1"):nrow(data)]=1
data$Indicator

spec.arma27.ex = arfimaspec(mean.model=list(armaOrder=c(2,7),include.mean=TRUE,
                                            external.regressors=matrix(data$Indicator)),
                            fixed.pars=list(ma2=0,ma3=0,ma4=0,ma5=0,ma6=0))
fit.arma27.ex = arfimafit(spec=spec.arma27.ex,data=data$spread)
fit.arma27.ex


### ENDOGENOUS BREAKS
### PAGE 104
library(tseries)
Breaks <-  read_excel("C:/Users/NIGOJ/Desktop/Nico/Cosas de la U/Econometria R/Econometria/Bases de datos/y_break.xlsx")

br = Breaks[,-1]
par(mfrow=c(1,1))

plot(br$y_break,type="l",xax="i",las=1,xaxs="i",tck=0.02,col="steelblue4",xlab="",ylab="")
spec.ar1 = arfimaspec(mean.model=list(armaOrder=c(1,0),include.mean=TRUE))
fit.ar1 = arfimafit(spec.ar1,data=br$y_break)
fit.ar1
fit.ar1@fit$coef[1]*(1-fit.ar1@fit$coef[2]) 
# arma(br, order=c(1,0)) 
# both are correct however one shows the unconditional mean 
# and the other the intercept

Breaks$Indicator=1
Breaks$Indicator[1:100]=0
break_y = embed(as.matrix(Breaks),2)
break_y = break_y[,-c(1,4,6)]

#  para qué sirve embed
#
x <-  1:10
xemb <- embed(x,4)
xemb
#
# fin embed



### PAGE 108
summary(lm(break_y[,1]~break_y[,3]+break_y[,2]))
summary(lm(break_y[,1]~break_y[,3]+break_y[,2]+I(break_y[,3]*break_y[,2])))

arma(br,c(1,0,0))
length(br)

# corregir error [1:100,1]
arma(br[1:100,1],c(1,0,0))
arma(br[50:150,1],c(1,0,0))




# AR1
# CUMSUM PLOT
library(forecast)

forecasts = NULL
for (i in 1:(nrow(br)-2)){
  model = arima(br[1:(2+i-1),1],c(1,0,0))
  forecasts[i] = forecast(model,h=1)$mean # Gives the one-step forecast
}
res = br[-c(1:2),1]-forecasts

# INTERCEPT PLOT
slope.est=intercept.est=slope.sigma.est=int.sigma.est=estadF=NULL
for (i in 1:(nrow(br)-3)){
  y.br = embed(as.matrix(br[1:(3+i),1]),2)
  print(nrow(y.br))
  model=lm(y.br[,1]~y.br[,-1])
  intercept.est[i]=coef(model)[1]
  slope.est[i]=coef(model)[2]
  int.sigma.est[i]=summary(model)$coefficients[1,2]
  slope.sigma.est[i]=summary(model)$coefficients[2,2]
  
}

int.upper.band=intercept.est+2*int.sigma.est
int.lower.band=intercept.est-2*int.sigma.est
slope.upper.band=slope.est+2*slope.sigma.est
slope.lower.band=slope.est-2*slope.sigma.est

par(mfrow=c(3,1))
plot(intercept.est, type="l",xlab="",ylab="",ylim=c(-2,4),xaxs="i",las=1,tck=.02,main="Intercept")
lines(int.upper.band, col="steelblue4")
lines(int.lower.band, col="steelblue4")
abline(h=0,lty=2)


plot(slope.est, type="l",xlab="",ylab="",ylim=c(-2,2),xaxs="i",las=1,tck=.02,main="Autoregressive Parameter")
lines(slope.upper.band, col="steelblue4")
lines(slope.lower.band, col="steelblue4")
abline(h=0,lty=2)

# CUMSUM
# corregir: cambie todos los length( )  por  nrow( )


CUMSUM=NULL
for (i in 1:nrow(br)){
  CUMSUM=c(CUMSUM,sum(res[1:i,1])/sd(res[,1]))
}

lower=upper=NULL
for (i in 1:nrow(br)){
  upper[i]= 0.948*(nrow(br)^0.5)+2*(i-1)*(nrow(br)^-0.5)
  lower[i]=-0.948*(nrow(br)^0.5)-2*(i-1)*(nrow(br)^-0.5)
}

for (i in 1:nrow(br)){
  upper[i]= 0.948*(nrow(br)^0.5)+2*(i-1)*(nrow(br)^-0.5)
  lower[i]=-0.948*(nrow(br)^0.5)-2*(i-1)*(nrow(br)^-0.5)
}

plot(CUMSUM, type="l", xlim=c(0,150),ylim=c(-40,50),las=1,xaxs="i",xlab="",ylab="",tck=0.02,main="The CUMSUM Test")
lines(upper, col="steelblue4",lty=2)
lines(lower, col="steelblue4",lty=2)
abline(h=0,lty=2)



### COMBINING FORECASTS


library(readxl)
library(rugarch)
library(zoo)

data = read_excel("C:/Users/NIGOJ/Desktop/Nico/Cosas de la U/Econometria R/Econometria/Bases de datos/ch1_quarterly.xls")

data$DATE = as.yearqtr(data$DATE)
data$spread = data$r5-data$Tbill

fore = NULL
space=51

### ONE STEP AHEAD OUT-OF-SAMPLE FORECAST (last on is out of analysed sample)
for (i in 1:space) {
  print(i)
  sample = data$spread[1:(length(data$spread)-space+i)]
  
  spec.ar7 = arfimaspec(mean.model=list(armaOrder=c(7,0),include.mean=TRUE))
  fit.ar7 = arfimafit(spec=spec.ar7,data=sample,solver="gosolnp")
  fore.ar7 = arfimaforecast(fit.ar7,n.ahead=1)@forecast$seriesFor
  
  spec.ar6 = arfimaspec(mean.model=list(armaOrder=c(6,0),include.mean=TRUE))
  fit.ar6 = arfimafit(spec=spec.ar6,data=sample,solver="gosolnp")
  fore.ar6 = arfimaforecast(fit.ar6,n.ahead=1)@forecast$seriesFor
  
  spec.ar2 = arfimaspec(mean.model=list(armaOrder=c(2,0),include.mean=TRUE))
  fit.ar2 = arfimafit(spec=spec.ar2,data=sample,solver="gosolnp")
  fore.ar2 = arfimaforecast(fit.ar2,n.ahead=1)@forecast$seriesFor
  
  spec.ar127 = arfimaspec(mean.model=list(armaOrder=c(7,0),include.mean=TRUE),
                          fixed.pars=list(ar3=0,ar4=0,ar5=0,ar6=0))
  fit.ar127 = arfimafit(spec=spec.ar127,data=sample,solver="gosolnp")
  fore.ar127 = arfimaforecast(fit.ar127,n.ahead=1)@forecast$seriesFor
  
  spec.arma11 = arfimaspec(mean.model=list(armaOrder=c(1,1),include.mean=TRUE))
  fit.arma11 = arfimafit(spec=spec.arma11,data=sample,solver="gosolnp")
  fore.arma11 = arfimaforecast(fit.arma11,n.ahead=1)@forecast$seriesFor
  
  spec.arma21 = arfimaspec(mean.model=list(armaOrder=c(2,1),include.mean=TRUE))
  fit.arma21 = arfimafit(spec=spec.arma21,data=sample,solver="gosolnp")
  fore.arma21 = arfimaforecast(fit.arma21,n.ahead=1)@forecast$seriesFor
  
  spec.arma217 = arfimaspec(mean.model=list(armaOrder=c(2,7),include.mean=TRUE),
                            fixed.pars=list(ma2=0,ma3=0,ma4=0,ma5=0,ma6=0))
  fit.arma217 = arfimafit(spec=spec.arma217,data=sample,solver="gosolnp")
  fore.arma217 = arfimaforecast(fit.arma217,n.ahead=1)@forecast$seriesFor
  
  fore = cbind(fore,c(fore.ar7,fore.ar6,fore.ar2,fore.ar127,fore.arma11,fore.arma21,fore.arma217))
  print(nrow(fore))
  print(ncol(fore))
  
}

### EQUALLY WEIGHTED FORECAST
sum((1/nrow(fore))*fore[,51])

foreprom <- colMeans(fore)
actual = data$spread[(length(data$spread)-49):length(data$spread)]

par(mar=c(3,2,1,1))
plot(c(NA,actual),foreprom)
abline(a=0,b=1)

plot(c(NA,actual),type="l",xlab="",ylab="",main="",las=1,xaxs="i",tck=0.02,ylim=c(min(fore),max(fore)),lty=2)
lines(foreprom,col="steelblue4",lty=1)

m1 <- lm(actual~foreprom)
m1res <- summary(m1)
coef(m1)

fore.error = t(fore[,-ncol(fore)])-actual

### OPTIMAL WEIGHTED FORECAST

plot(c(NA,actual),type="l",xlab="",ylab="",main="",las=1,xaxs="i",tck=0.02,ylim=c(min(fore),max(fore)),lty=2)
length(actual)
colores <- c("purple","red","orange","yellow","green","blue","violet")
for (i in 1:nrow(fore)) {
  lines(fore[i,],col=colores[i])
}


lines(foreprom,col="black",lty=1)

weight <- rep(1/nrow(fore),nrow(fore))
lines(t(fore)%*%weight,col="steelblue4",lwd=2)


#  apply(X, MARGIN, FUN)
#  apply tiene tres argumentos:
#  X:       Una matriz, generalmente un data frame
#  MARGIN:  La dimensión (margen) que agrupará los elementos de la matriz X, 
#           para aplicarles una función. 
#           1 son renglones y 2 son colummnas
#  FUN:     La función que aplicaremos a la matriz X en su dimensión MARGIN.

# pesos por varianzas

forecast.error.variance.opt = apply(fore.error,2,var)

weight.opt = (1/forecast.error.variance.opt)/sum(1/forecast.error.variance.opt)
weight.opt # optimal weights

opt.fore = weight.opt%*%fore

plot(c(NA,actual),type="l",xlab="",ylab="",main="",las=1,xaxs="i",tck=0.02,ylim=c(min(fore),max(fore)),lty=2)
lines(c(opt.fore),col="steelblue1",lwd=2,lty=2)


# pesos por criterio de información Schwarz-Bayesian SBC
# infocriteria(fit.ar2)[2]

SBC = NULL
SBC = c(SBC,infocriteria(fit.ar7)[2])
SBC = c(SBC,infocriteria(fit.ar6)[2])
SBC = c(SBC,infocriteria(fit.ar2)[2])
SBC = c(SBC,infocriteria(fit.ar127)[2])
SBC = c(SBC,infocriteria(fit.arma11)[2])
SBC = c(SBC,infocriteria(fit.arma21)[2])
SBC = c(SBC,infocriteria(fit.arma217)[2])

SBC
sbc0 <- min(SBC)
alfa <- exp((sbc0-SBC)/2)
weight.sbc <- alfa/sum(alfa)

sbc.fore = weight.sbc%*%fore

plot(c(NA,actual),type="l",xlab="",ylab="",main="",las=1,xaxs="i",tck=0.02,ylim=c(min(fore),max(fore)),lty=2)
lines(c(sbc.fore),col="steelblue1",lwd=2,lty=2)


