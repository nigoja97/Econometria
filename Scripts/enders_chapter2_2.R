install.packages("rugarch")
library("Ecdat") # for a guide for modern econometrics

#####################################
#####################################
###### 2. STATIONARY PROCESSES ######
#####################################
#####################################

### PAGE 72
library("gdata")
library(readxl)
data = read_excel("C:/Users/arias/Documents/Jorge trabajo/UExternado/Economia/ux ECO econometria R/Enders 4 data/ch2_sim2.xlsx")
data = ch2_sim2
par(mar=c(3,2,1,1))
par(mfrow=c(1,1))
plot(data$Y1,type="l")
Y1 = data[,"Y1"]
Y2=data[,"Y2"]
par(mfcol = c(2,1), oma = c(0,0,1,0) + 0.2, mar = c(0,1,0,0) + 1, mgp = c(0, 0.2, 0))
# par(mfcol = c(2,1), oma = c(2,0,1,0), mar = c(0,1,0,0) , mgp = c(0, 0.2, 0))
acf1 = acf(Y1,las=1,lag.max=20,tck=.02,xlab="",ylab="",main="ACF")
pacf1 = pacf(Y1,las=1,lag.max=20,tck=.02,xlab="",ylab="",main="PACF")
acf1
pacf1
acf2 = acf(Y2,las=1,lag.max=20,tck=.02,xlab="",ylab="",main="",las=1)
pacf2 = pacf(Y2,las=1,lag.max=20,tck=.02,xlab="",ylab="",main="",las=1)
acf2; pacf2

# TABLE 2.2: MODEL 1
library("rugarch")
spec.ar1 = arfimaspec(mean.model=list(armaOrder=c(1,0),include.mean=FALSE))
fit.ar1 = arfimafit(spec=spec.ar1,data=as.numeric(unlist(data$Y1)))
fit.ar1
res.ar1 = fit.ar1@fit$residuals

### FIGURE 2.4
par(mfrow=c(1,1))
res.acf = acf(res.ar1)
res.acf

Box.test(res.ar1,lag=8,type="Ljung-Box")
Box.test(res.ar1,lag=16,type="Ljung-Box")
Box.test(res.ar1,lag=24,type="Ljung-Box")

# TABLE 2.2: MODEL 2
spec.arma112 = arfimaspec(mean.model=list(armaOrder=c(1,12),include.mean=FALSE),
                      fixed.pars=list(ma1=0,ma2=0,ma3=0,ma4=0,ma5=0,ma6=0,ma7=0,ma8=0,ma9=0,ma10=0,ma11=0))
fit.arma112 = arfimafit(spec=spec.arma112,data=as.numeric(unlist(data$Y1)))
fit.arma112
res.arma112 = fit.arma112@fit$residuals

Box.test(res.arma112,lag=8,type="Ljung-Box")
Box.test(res.arma112,lag=16,type="Ljung-Box")
Box.test(res.arma112,lag=24,type="Ljung-Box")


### PAGE 74
Y2 = data[,"Y2"]
spec.ar1 = arfimaspec(mean.model=list(armaOrder=c(1,0),include.mean=FALSE))
fit.ar1 = arfimafit(spec=spec.ar1,data=data$Y2)
fit.ar1
res.ar1 = fit.ar1@fit$residuals
Box.test(res.ar1,lag=8,type="Ljung-Box")
Box.test(res.ar1,lag=24,type="Ljung-Box")

spec.arma11 = arfimaspec(mean.model=list(armaOrder=c(1,1),include.mean=FALSE))
fit.arma11 = arfimafit(spec=spec.arma11,data=data$Y2)
fit.arma11
res.arma11 = fit.arma11@fit$residuals
Box.test(res.arma11,lag=8,type="Ljung-Box")
Box.test(res.arma11,lag=24,type="Ljung-Box")

spec.ar2 = arfimaspec(mean.model=list(armaOrder=c(2,0),include.mean=FALSE))
fit.ar2 = arfimafit(spec=spec.ar2,data=data$Y2)
fit.ar2

### PAGE 74
Y3 = data[,"Y3"]
acf.Y3 = acf(Y3)
acf.Y3
pacf.Y3 = pacf(Y3)
pacf.Y3

spec.arma216 = arfimaspec(mean.model=list(armaOrder=c(2,16),include.mean=FALSE),
                          fixed.pars=list(ma1=0,ma2=0,ma3=0,ma4=0,ma5=0,ma6=0,ma7=0,ma8=0,
                                          ma9=0,ma10=0,ma11=0,ma12=0,ma13=0,ma14=0,ma15=0))
fit.arma216 = arfimafit(spec=spec.arma216,data=data$Y3)
fit.arma216
res.arma216 = fit.arma216@fit$residuals

acf.arma216 = acf(data$Y3[50:100],lag=20)
pacf.arma216 = pacf(data$Y3[50:100],lag=20)
acf.arma216
pacf.arma216

spec.ar2 = arfimaspec(mean.model=list(armaOrder=c(2,0),include.mean=FALSE))
fit.ar2 = arfimafit(spec=spec.ar2,data=data$Y3[50:100])
fit.ar2
res.ar2 = fit.ar2@fit$residuals
Box.test(res.ar2,lag=8,type="Ljung-Box")
Box.test(res.ar2,lag=16,type="Ljung-Box")
Box.test(res.ar2,lag=24,type="Ljung-Box")


library(readxl)
library(rugarch)
library(zoo)
### PAGE 88
data = read.xls("/Users/user/Google Drive/Website/Book/Enders/QUARTERLY.xls")
data = ch1_quarterly
data$DATE = as.yearqtr(data$DATE)
data$spread = data$r5-data$Tbill

par(mar=c(2,3,1,1))

par(mfrow=c(2,1))
plot(data$DATE,data$spread,type="l",las=1,xaxs="i",yaxs="i",xlab="",ylab="",main="The Interest Rate Spread",tck=0.02,col="steelblue4",ylim=c(-2,4))
abline(h=0)
plot(data$DATE[-1],diff(data$spread),type="l",las=1,xaxs="i",yaxs="i",xlab="",ylab="",main="First-Difference of The Spread",tck=0.02,col="steelblue4",ylim=c(-3,3))
abline(h=0)

par(mfrow=c(2,1))
acf2=acf(data$spread,lag=12,tck=.02,xlab="",ylab="",main="",las=1)
pacf2=pacf(data$spread,lag=12,tck=.02,xlab="",ylab="",main="",las=1)
acf2      ## Buscar más de dos desviaciones estandar raiz(1/T)
pacf2


### PAGE 91
### TABLE 2.4
spec.ar9 = arfimaspec(mean.model=list(armaOrder=c(9,0),include.mean=TRUE))
fit.ar9 = arfimafit(spec=spec.ar9,data=data$spread)
fit.ar9
res.ar9 = fit.ar9@fit$residuals
Box.test(res.ar9,lag=4,type="Ljung-Box")
Box.test(res.ar9,lag=8,type="Ljung-Box")
Box.test(res.ar9,lag=12,type="Ljung-Box")


spec.ar6 = arfimaspec(mean.model=list(armaOrder=c(6,0),include.mean=TRUE))
fit.ar6 = arfimafit(spec=spec.ar6,data=data$spread)
fit.ar6
res.ar6 = fit.ar6@fit$residuals
Box.test(res.ar6,lag=4,type="Ljung-Box")
Box.test(res.ar6,lag=8,type="Ljung-Box")
Box.test(res.ar6,lag=12,type="Ljung-Box")


spec.ar2 = arfimaspec(mean.model=list(armaOrder=c(2,0),include.mean=TRUE))
fit.ar2 = arfimafit(spec=spec.ar2,data=data$spread)
fit.ar2
res.ar2 = fit.ar2@fit$residuals
Box.test(res.ar2,lag=4,type="Ljung-Box")
Box.test(res.ar2,lag=8,type="Ljung-Box")
Box.test(res.ar2,lag=12,type="Ljung-Box")


spec.ar29 = arfimaspec(mean.model=list(armaOrder=c(9,0),include.mean=TRUE),
                       fixed.pars=list(ar3=0,ar4=0,ar5=0,ar6=0,ar7=0,ar8=0))
fit.ar29 = arfimafit(spec=spec.ar29,data=data$spread)
fit.ar29
res.ar29 = fit.ar29@fit$residuals
Box.test(res.ar29,lag=4,type="Ljung-Box")
Box.test(res.ar29,lag=8,type="Ljung-Box")
Box.test(res.ar29,lag=12,type="Ljung-Box")


spec.arma11 = arfimaspec(mean.model=list(armaOrder=c(1,1),include.mean=TRUE))
fit.arma11 = arfimafit(spec=spec.arma11,data=data$spread)
fit.arma11
res.arma11 = fit.arma11@fit$residuals
Box.test(res.arma11,lag=4,type="Ljung-Box")
Box.test(res.arma11,lag=8,type="Ljung-Box")
Box.test(res.arma11,lag=12,type="Ljung-Box")


spec.arma21 = arfimaspec(mean.model=list(armaOrder=c(2,1),include.mean=TRUE))
fit.arma21 = arfimafit(spec=spec.arma21,data=data$spread)
fit.arma21
res.arma21 = fit.arma21@fit$residuals
Box.test(res.arma21,lag=4,type="Ljung-Box")
Box.test(res.arma21,lag=8,type="Ljung-Box")
Box.test(res.arma21,lag=12,type="Ljung-Box")


spec.arma27 = arfimaspec(mean.model=list(armaOrder=c(2,7),include.mean=TRUE),
                         fixed.pars=list(ma2=0,ma3=0,ma4=0,ma5=0,ma6=0))
fit.arma27 = arfimafit(spec=spec.arma27,data=data$spread,solver="nlminb")
fit.arma27
res.arma27 = fit.arma27@fit$residuals
Box.test(res.arma27,lag=4,type="Ljung-Box")
Box.test(res.arma27,lag=8,type="Ljung-Box")
Box.test(res.arma27,lag=12,type="Ljung-Box")


spec.ar7 = arfimaspec(mean.model=list(armaOrder=c(7,0),include.mean=TRUE))
fit.ar7 = arfimafit(spec=spec.ar7,data=data$spread,solver="nlminb")
fit.ar7
res.ar7 = fit.ar7@fit$residuals
Box.test(res.ar7,lag=4,type="Ljung-Box")
Box.test(res.ar7,lag=8,type="Ljung-Box")
Box.test(res.ar7,lag=12,type="Ljung-Box")




### PAGE 94
library(readxl)
library(rugarch)
library(zoo)
data = read.xls("QUARTERLY.xls")
data= ch1_quarterly
data$DATE = as.yearqtr(data$DATE)
data$spread = data$r5-data$Tbill
length(data$spread)

which(data$DATE=="2000 Q2")
data$DATE[162]
actual = data$spread[-c(1:162)]
length(data$spread)
### 1-STEP AHEAD ROLLING WINDOW FORECAST

# OPCIONES para solver = "solnp","gosolnp","nlminb","L-BGFS-U"
# intente solnp
# si no converge gosolnp puede mejorar
# además intente nlminb

fore.arma27 = fore.ar7 = NULL
for (i in 1:50) {
   fit.ar7=arfimafit(spec=spec.ar7,data=data$spread[1:(162+i-1)],solver="nlminb")
   fore.ar7[i]=arfimaforecast(fit.ar7,n.ahead=1)@forecast$seriesFor
   fit.arma27=arfimafit(spec=spec.arma27,data=data$spread[1:(162+i-1)],solver="gosolnp")
   fore.arma27[i]=arfimaforecast(fit.arma27,n.ahead=1)@forecast$seriesFor
}
mean(fore.ar7)
mean(fore.arma27)
var(fore.ar7)
var(fore.arma27)

par(mfrow=c(1,1))
plot(fore.ar7, type="l",col="blue")
lines(fore.arma27, type="l",col="red")
lines(actual,type="l",col="green")


### FORECAST ERROR and FORECAST ERROR VARIANCE
fore.error.ar7 = fore.ar7-actual
fore.error.arma27 = fore.arma27-actual
mean(fore.error.ar7)
mean(fore.error.arma27)
var(fore.error.ar7)
var(fore.error.arma27)

### PAGE 95
lm95 <- summary(lm(actual~fore.ar7))
#resolver la prueba beta1=1
betagorro <-  lm95$coefficients[2,1]
eebetagorro <- lm95$coefficients[2,2]
tep = (betagorro - 1)/eebetagorro
tep
2*pt(abs(tep),df=50-2,lower.tail = F)

lm95b <- summary(lm(actual~fore.arma27))
betagorro <-  lm95b$coefficients[2,1]
eebetagorro <- lm95b$coefficients[2,2]
tep = (betagorro - 1)/eebetagorro
tep
2*pt(abs(tep),df=50-2,lower.tail = F)

### GRANGER-NEWBOLD TEST     VER PAG 85 para descripción de la prueba
x = fore.error.ar7+fore.error.arma27
z = fore.error.ar7-fore.error.arma27
corxz = cor(z,x)
GN = corxz/( sqrt( (1-corxz^2)/(length(fore.error.ar7)-1)))
2*pt(abs(GN),df=49,lower.tail = F)


### DIEBOLD-MARIANO TEST
library(forecast)
dm.test(fore.error.ar7,fore.error.arma27, h=1, power=4)

d = (fore.error.ar7)^4-(fore.error.arma27)^4
DM = mean(d)/(var(d)/(length(d)-1))^0.5
DM
2*pt(abs(DM),df=49,lower.tail = F)

acf.d = acf(d)
acf.d


### SEASONALITY
### PAGE 98
par(mfrow=c(2,1))
par(mar=c(3,2,1,1))
plot(data$DATE,data$M1NSA,type="l",las=1,xaxs="i",yaxs="i",xlab="",ylab="",main="",tck=0.02,col="steelblue4",ylim=c(0,2500))

mg = 100*diff(log(data$M1NSA))
plot(data$DATE[-1],mg,type="l",las=1,xaxs="i",yaxs="i",xlab="",ylab="",main="",tck=0.02,col="steelblue4",ylim=c(-4,8))
abline(h=0)

### PANEL A
par(mfrow=c(2,1))
acf.mg = acf(mg,lag=25,tck=.02,xlab="",ylab="",main="")
acf.mg
pacf.mg = pacf(mg,lag=25,tck=.02,xlab="",ylab="",main="")
pacf.mg
#                   ¿      QUÉ ES diff(   )  ?

diff(x)     
diff(x,1,1)    #  diff(variable, rezagos, orden de diferenciacicón)
diff(x,4)
diff(x,4,2)

par(mfrow=c(1,1))
smg = diff(diff(log(data$M1NSA),4))
plot(smg,type="l",las=1,xaxs="i",yaxs="i",xlab="",ylab="",main="",tck=0.02,col="steelblue4")
abline(h=0)

### PANEL B
par(mfrow=c(2,1))
acf.mg = acf(smg,lag=25,tck=.02,xlab="",ylab="",main="")
acf.mg
pacf.mg = pacf(smg,lag=25,tck=.02,xlab="",ylab="",main="")
pacf.mg

# eff
spec.arma14 = arfimaspec(mean.model=list(armaOrder=c(1,4),include.mean=TRUE),
                         fixed.pars=list(ma1=0,ma2=0,ma3=0))
fit.arma14 = arfimafit(spec=spec.arma14,data=smg,solver="nlminb")
fit.arma14
res.arma14 = fit.arma14@fit$residuals
Box.test(res.arma14,lag=4,type="Ljung-Box")
Box.test(res.arma14,lag=8,type="Ljung-Box")
Box.test(res.arma14,lag=12,type="Ljung-Box")




### PARAMETER INSTABILITY
data = read.xls("/Users/user/Google Drive/Website/Book/Enders/QUARTERLY.xls")
library("zoo")
data$DATE = as.yearqtr(data$DATE)
data$spread = data$r5-data$Tbill

par(mfrow=c(2,1))
plot(data$DATE,data$spread,type="l",las=1,xaxs="i",yaxs="i",xlab="",ylab="",main="The Interest Rate Spread",tck=0.02,col="steelblue4",ylim=c(-2,4))
abline(h=0)
plot(data$DATE[-1],diff(data$spread),type="l",las=1,xaxs="i",yaxs="i",xlab="",ylab="",main="First-Difference of The Spread",tck=0.02,col="steelblue4",ylim=c(-3,3))
abline(h=0)

spec.arma27 = arfimaspec(mean.model=list(armaOrder=c(2,7),include.mean=TRUE),
                         fixed.pars=list(ma2=0,ma3=0,ma4=0,ma5=0,ma6=0))
fit.arma27.pre = arfimafit(spec=spec.arma27,data=data$spread[1:88])
fit.arma27.pre
fit.arma27.post = arfimafit(spec=spec.arma27,data=data$spread[89:193],solver="gosolnp")
fit.arma27.post
fit.arma27 = arfimafit(spec=spec.arma27,data=data$spread,solver="gosolnp")
fit.arma27

SSR1 = sum(fit.arma27.pre@fit$residuals^2)
SSR2 = sum(fit.arma27.post@fit$residuals^2)
SSR  = sum(fit.arma27@fit$residuals^2)
Fcambio = ((SSR-SSR1-SSR2)/5)/((SSR1+SSR2)/(212-2*5))
pf(Fcambio,df1=5,df2=212-2*5,lower.tail = F)


#  método 2 
data$Indicator = 0
data$Indicator[which(data$DATE=="1982 Q1"):nrow(data)]=1
data$Indicator

spec.arma27.ex = arfimaspec(mean.model=list(armaOrder=c(2,7),include.mean=TRUE,external.regressors=matrix(data$Indicator)),
                         fixed.pars=list(ma2=0,ma3=0,ma4=0,ma5=0,ma6=0))
fit.arma27.ex = arfimafit(spec=spec.arma27.ex,data=data$spread)
fit.arma27.ex


### ENDOGENOUS BREAKS
### PAGE 104
library(tseries)
Break = read.xls("/Users/user/Google Drive/Website/Book/Enders/y_break.xls")
br = Breaks[,-1]
par(mfrow=c(1,1))

plot(br$y_break,type="l",xax="i",las=1,xaxs="i",tck=0.02,col="steelblue4",xlab="",ylab="")
spec.ar1 = arfimaspec(mean.model=list(armaOrder=c(1,0),include.mean=TRUE))
fit.ar1 = arfimafit(spec.ar1,data=br$y_break)
fit.ar1
fit.ar1@fit$coef[1]*(1-fit.ar1@fit$coef[2]) # arma(br, order=c(1,0)) # both are correct however one shows the unconditional mean and the other the intercept

Breaks$Indicator=1
Breaks$Indicator[1:100]=0
break_y = embed(as.matrix(Breaks),2)
break_y = break_y[,-c(1,4,6)]

### PAGE 108
summary(lm(break_y[,1]~break_y[,3]+break_y[,2]))
summary(lm(break_y[,1]~break_y[,3]+break_y[,2]+I(break_y[,3]*break_y[,2])))

arma(br,c(1,0,0))
arma(br[1:100],c(1,0,0))
arma(br[50:150],c(1,0,0))


# AR1
# CUMSUM PLOT
forecasts = NULL
for (i in 1:(length(br)-2)){
   model = arima(br[1:(2+i-1)],c(1,0,0))
   forecasts[i] = forecast(model,h=1)$mean # Gives the one-step forecast
}
res = br[-c(1:2)]-forecasts

# INTERCEPT PLOT
slope.est=intercept.est=slope.sigma.est=int.sigma.est=NULL
for (i in 1:(length(br)-3)){
   y.br = embed(br[1:(3+i)],2)
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


CUMSUM=NULL
for (i in 1:length(br)){
   CUMSUM=c(CUMSUM,sum(res[1:i])/sd(res))
}

lower=upper=NULL
for (i in 1:length(br)){
   upper[i]= 0.948*(length(br)^0.5)+2*(i-1)*(length(br)^-0.5)
   lower[i]=-0.948*(length(br)^0.5)-2*(i-1)*(length(br)^-0.5)
}

plot(CUMSUM, type="l", xlim=c(0,150),ylim=c(-40,50),las=1,xaxs="i",xlab="",ylab="",tck=0.02,main="The CUMSUM Test")
lines(upper, col="steelblue4",lty=2)
lines(lower, col="steelblue4",lty=2)
abline(h=0,lty=2)



### COMBINING FORECASTS
data$spread
fore = NULL
space=51
### ONE STEP AHEAD OUT-OF-SAMPLE FORECAST (last on is out of analysed sample)
for (i in 1:space) {
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
}

### EQUALLY WEIGHTED FORECAST
sum((1/nrow(fore))*fore[,51])

actual = data$spread[(length(data$spread)-49):length(data$spread)]
fore.error = t(fore[,-ncol(fore)])-actual


### OPTIMAL WEIGHTED FORECAST
weight%*%fore[,51]

plot(c(actual,NA),type="l",xlab="",ylab="",main="",las=1,xaxs="i",tck=0.02,ylim=c(min(fore),max(fore)),lty=2)
for (i in 1:nrow(fore)) {
   lines(fore[i,],col=paste0("grey",round(100/10*i)))
}
lines(t(fore)%*%weight,col="steelblue4",lwd=2)

lm1 = summary(lm(actual~t(fore[,-ncol(fore)])+0))
lm1
ind1 = which(lm1$coefficients[,1]<0)
lm2 = summary(lm(actual~t(fore[-ind1,-ncol(fore)])+0))
lm2
sum(lm2$coefficients[,1]*t(fore[-ind,ncol(fore)]))

forecast.error.variance.opt = apply(fore.error[,-ind],2,var)
weight.opt = forecast.error.variance.opt/sum(forecast.error.variance.opt)
weight.opt # optimal weights

opt.fore = weight.opt%*%fore[-ind,]
lines(c(opt.fore),col="steelblue1",lwd=2,lty=2)

### END
