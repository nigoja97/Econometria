#################################################
#################################################
###### 5. MULTIEQUATION TIME-SERIES MODELS ######
#################################################
#################################################
pacman::p_load(readxl,gdata,stargazer,MTS,rugarch,forecast,vars,urca,aTSA)
### PAGE 260
library(readxl)
library(gdata)
library(stargazer)
library(MTS)
library(rugarch)
library(forecast)
library(vars)
library(urca)
library(aTSA)



datos = read_excel("C:/Users/NIGOJ/Desktop/Nico/Cosas de la U/Econometria R/Econometria/Bases de datos/ch5_Terrorism.xls")

### FIGURE 5.1
# Totales trimestrasles de incidentes terroristas con al menos una víctima desde 1970Q1
datos$date = seq(1970,by=0.25,length.out=nrow(datos))
par(mfcol = c(2,1), oma = c(0,0,1,0) + 0.2, mar = c(0,1,0,0) + 1, mgp = c(0, 0.2, 0))
plot(datos$date,datos$Domestic,type="l",las=1,xaxs="i",yaxs="i",xlab="",ylab="",tck=.02,main=colnames(datos)[2],col="steelblue4",ylim=c(0,400))
plot(datos$date,datos$Transnational,type="l",las=1,xaxs="i",yaxs="i",xlab="",ylab="",tck=.02,main=colnames(datos)[3],col="steelblue4",ylim=c(0,80))

# enero 1973 US empezó la instalacioón de detectores de metales en aeropuertos
ind = which(datos$date == 1973.00)
datos$pure.jump = 1
datos$pure.jump[1:ind-1]=0
datos$pulse = 0
datos$pulse[ind]=1
datos$grad.change = 0
datos$grad.change[ind:(ind+3)]=seq(0.25,1,by=0.25)
datos$prol.pulse = 0
datos$prol.pulse[ind:(ind+3)]=seq(1,0.25,by=-0.25)


space=20
par(mfrow=c(2,2))
plot(datos$date[1:space],datos$pure.jump[1:space],type="h",las=1,xaxs="i",xlab="",ylab="",tck=.02,main="Salto",col="steelblue4",ylim=c(0,1))
plot(datos$date[1:space],datos$pulse[1:space],type="h",las=1,xaxs="i",xlab="",ylab="",tck=.02,main="Pulso",col="steelblue4",ylim=c(0,1))
plot(datos$date[1:space],datos$grad.change[1:space],type="h",las=1,xaxs="i",xlab="",ylab="",tck=.02,main="Cambio gradual",col="steelblue4",ylim=c(0,1))
plot(datos$date[1:space],datos$prol.pulse[1:space],type="h",las=1,xaxs="i",xlab="",ylab="",tck=.02,main="Pulso prolongado",col="steelblue4",ylim=c(0,1))








#  Estimación de efecto de salto en 1994Q4

ind = which(datos$date ==1994.75)
datos$salto = 1
datos$salto[1:ind-1]=0  
trans = datos$Transnational[1:ind-1]
auto.arima(trans,ic="bic")
arima(datos$Transnational,order=c(1,1,0),xreg=datos$salto)
datos$pulso.pro = 0
datos$pulso.pro[ind:(ind+3)]=seq(1,0.25,by=-0.25)
arima(datos$Transnational,order=c(1,1,0),xreg=datos$pulso.pro)




### INTERPRETATION
library("forecast")

auto.arima(datos$Transnational,xreg=datos$pure.jump,ic="bic")
auto.arima(datos$Transnational,xreg=datos$pulse,ic="bic")
auto.arima(datos$Transnational,xreg=datos$grad.change,ic="bic")
auto.arima(datos$Transnational,xreg=datos$prol.pulse,ic="bic")

auto.arima(datos$Domestic,xreg=datos$pure.jump,ic="bic")
auto.arima(datos$Domestic,xreg=datos$pulse,ic="bic")
auto.arima(datos$Domestic,xreg=datos$grad.change,ic="bic")
auto.arima(datos$Domestic,xreg=datos$prol.pulse,ic="bic")



### PAGE 278

datos = read_excel("C:/Users/NIGOJ/Desktop/Nico/Cosas de la U/Econometria R/Econometria/Bases de datos/ch5_italy.xls")
datos = ch5_italy
datos$ENTRY = seq(1971,by=0.25,length.out=nrow(datos))

ind1 = which(datos$ENTRY==1971)
ind2 = which(datos$ENTRY==1988.75)

par(mfrow=c(2,1))
plot(datos$ENTRY,datos$Attkit,type="l",las=1,xaxs="i",xlab="",ylab="",tck=.02,main="",col="steelblue4",yaxs="i")
abline(h=0)
acf1 = acf(datos$Attkit[ind1:ind2])
acf1

library(MTS)
par(mar=c(2,1,1,1))
ccor = ccm(datos[,-1],level=TRUE)

ccor$ccm[3,]

#####   función embed
#  ejemplo para dummies
x = 1:10
x
matrizx = embed(x,3)
matrizx
#####   fin del ejemplo


plag = 3
X = embed(datos$Attkit,plag+1)
summary(lm(datos$Slitaly[-c(1:plag)]~X))
summary(lm(datos$Slitaly[-c(1:plag)]~X[,-ncol(X)]))
summary(lm(datos$Slitaly[-c(1:plag)]~X[,-c(1,ncol(X))]))
summary(lm(datos$Slitaly[-c(1:plag)]~X[,3]))
summary(lm(datos$Slitaly[-c(1:plag)]~X[,2]))

m1 = lm(datos$Slitaly[-c(1:plag)]~X)
m2 = lm(datos$Slitaly[-c(1:plag)]~X[,-ncol(X)])
m3 = lm(datos$Slitaly[-c(1:plag)]~X[,2:3])
m4 = lm(datos$Slitaly[-c(1:plag)]~X[,3])
m5 = lm(datos$Slitaly[-c(1:plag)]~X[,2])

stargazer(m3,m4,m5,type="text",out="m345.txt")


lm1 = summary(lm(datos$Slitaly[-c(1:plag)]~0+X[,2:3]))
adl.res=lm1$residuals
plot(datos$ENTRY[-c(1:3)],adl.res,type="l",las=1,xaxs="i",xlab="",ylab="",tck=.02,main="",col="steelblue4",yaxs="i",ylim=c(-.3,.3))
abline(h=0,lty=2)
acf.res = acf(adl.res)
acf.res
pacf.res = pacf(adl.res)
pacf.res

library(rugarch)
spec.ar15 = arfimaspec(mean.model=list(armaOrder=c(5,0),include.mean=F),
                       fixed.pars=list(ar2=0,ar3=0))
fit.ar15 = arfimafit(spec=spec.ar15,data=adl.res)
fit.ar15
res.ar15 = fit.ar15@fit$residuals
acf(res.ar15)

spec.adl15 = arfimaspec(mean.model=list(armaOrder=c(5,0),include.mean=F,
                                        external.regressors =X[-1,2:3]),
                        fixed.pars=list(ar2=0,ar3=0))
fit.adl15 = arfimafit(spec=spec.adl15,data=Y[,1])
fit.adl15
res.adl15 = fit.adl15@fit$residuals
acf(res.ar15)





auto=auto.arima(adl.res,start.p = 1,start.q=0,max.q = 0,stationary=T,seasonal=F)
auto
acf(auto$residuals)

yplag = 4
Y = embed(datos$Slitaly,yplag+1)
lm2 = summary(lm(datos$Slitaly[-c(1:yplag)]~0+Y[,2]+X[-1,2]))
lm2
lmdum = summary(lm(datos$Slitaly[-c(1:yplag)]~Y[,2]))
lmdum
adl2.res=lm2$residuals
plot(datos$ENTRY[-c(1:4)],adl2.res,type="l",las=1,xaxs="i",xlab="",ylab="",tck=.02,main="",col="steelblue4",yaxs="i",ylim=c(-.3,.3))
abline(h=0,lty=2)
acf2.res = acf(adl2.res)
acf2.res
pacf2.res = pacf(adl2.res)
pacf2.res


plot(datos$Slitaly,type="l")


### PAGE 310
library(vars)
datos = read_excel("C:/Users/NIGOJ/Desktop/Nico/Cosas de la U/Econometria R/Econometria/Bases de datos/ch5_Terrorism.xls")
datos$ENTRY = seq(1970,by=0.25,length.out=nrow(datos))

library(urca)
library(aTSA)
colnames(datos)


adf.dom = adf.test(datos$ENTRY,nlag=2)
adf.tra = adf.test(datos$ENTRY,nlag=1)
adf.dom
adf.tra


VARselect(datos[-c(1:which(datos$ENTRY=="1979.25")),-1],type="const") # AIC suggests 3 lags
var.terror = VAR(datos[-c(1:which(datos$ENTRY=="1979.25")),-1],p=3)
summary(var.terror)

fevd.terror = fevd(var.terror,n.ahead=12)
fevd.terror
plot(fevd.terror)

par(mfcol = c(2,2), oma = c(0,0,1,0) + 0.2, mar = c(0,1,0,0) + 1, mgp = c(0, 0.2, 0))
plot(irf(var.terror),col=1,las=1,xaxs="i",xlab="",ylab="")

k=ncol(datos[,-1])
amat = diag(k)
diag(amat) = NA
amat[1,2] = NA
amat # the coefficient b[2,1] is set equal to zero
svar.terror = SVAR(var.terror, estmethod="direct", Amat = amat) 
svar.terror
summary(svar.terror)
plot(irf(svar.terror),col=1,las=1,xaxs="i",xlab="",ylab="")


### PAGE 325
datos = read_excel("C:/Users/NIGOJ/Desktop/Nico/Cosas de la U/Econometria R/Econometria/Bases de datos/Enders_Holt.xls")
datos$ENTRY = seq(1974.25,by=0.25,length.out=nrow(datos))
k = ncol(datos[,-1])

par(mfcol = c(2,2), oma = c(0,0,1,0) + 0.2, mar = c(0,1,0,0) + 1, mgp = c(0, 0.2, 0))
for (i in 1:k) {
  plot(datos$ENTRY,datos[,i+1],type="l",las=1,xaxs="i",xlab="",ylab="",tck=.02,col="steelblue4",yaxs="i",main=colnames(datos)[i+1])
  abline(h=0,lty=2)
}
var.enders = VAR(na.omit(datos[,-1]),p=11)
summary(var.enders)

amat = diag(k)
diag(amat) = NA
amat[2,1] = NA
amat[4, ] = NA
amat
svar.enders = SVAR(var.enders, estmethod="direct", Amat = amat) 
svar.enders
plot(irf(svar.enders,boot=FALSE),col=1,las=1,xaxs="i",xlab="",ylab="")


### PAGE 331
datos = read_excel("C:/Users/NIGOJ/Desktop/Nico/Cosas de la U/Econometria R/Econometria/Bases de datos/Exrates.xls")
head(datos)

datos$loge_ca = log(datos$e_uk)
datos$logr_ca = datos$loge_ca-log(datos$p_uk)+log(datos$p_us)
dloge_ca = c(diff(datos$loge_ca))
dlogr_ca = c(diff(datos$logr_ca))

datos$date = as.yearqtr(datos$DESCRIPTOR)

X1 = embed(dloge_ca,2)
X2 = embed(datos$loge_ca,2)
summary(lm(X1[,1]~X2[-1,2]+X1[,-1]))

df1 = ur.df(dloge_ca,type="drift")
df1@testreg

df = data.frame(datos$logr_ca,datos$loge_ca)
VAR.PPP = VAR(df,p=3)
summary(VAR.PPP)
fevd(VAR.PPP)

k = ncol(df)
amat = diag(k)
diag(amat) = NA
amat[2,1] = NA
amat
SVAR.PPP = SVAR(VAR.PPP, estmethod = "scoring", Amat = amat, Bmat = NULL,
                max.iter = 100, maxls = 1000, conv.crit = 1.0e-8)
fevd(SVAR.PPP)

plot(irf(SVAR.PPP,boot=FALSE),col=1,las=1,xaxs="i",xlab="",ylab="")

BQ.PPP = BQ(VAR.PPP)
fevd(BQ.PPP)
plot(irf(BQ.PPP,boot=FALSE),col=1,las=1,xaxs="i",xlab="",ylab="")



### PAGE 340
datos = read_excel("C:/Users/NIGOJ/Desktop/Nico/Cosas de la U/Econometria R/Econometria/Bases de datos/ch1_quarterly")
dinfl = diff(log(datos$CPI))
dlip = diff(log(datos$IndProd))
Y = cbind(dlip,dinfl)
VAR.PPP = VAR(Y,p=3)
summary(VAR.PPP)
fevd(VAR.PPP)

SBQ.PPP = BQ(VAR.PPP)
IRF.PPP = irf(SBQ.PPP,boot=TRUE,cumulative=T,n.ahead=25,ortho=T,las=1)

par(mfcol = c(2,1), oma = c(0,0,1,0) + 1, mar = c(0,1,0,0) + 1, mgp = c(0, 0.2, 0))
plot(IRF.PPP$irf$dinfl[,1]/0.01,type="l",las=1,xaxs="i",xlab="",ylab="",tck=0.02,col="steelblue4",ylim=c(-1,6),yaxs="i")
lines(IRF.PPP$Lower$dinfl[,1]/0.01,col="steelblue4",lty=2)
lines(IRF.PPP$Upper$dinfl[,1]/0.01,col="steelblue4",lty=2)
lines(IRF.PPP$irf$dinfl[,2]/0.005,type="l",las=1,xaxs="i",xlab="",ylab="",tck=0.02,col="steelblue1")
lines(IRF.PPP$Lower$dinfl[,2]/0.005,col="steelblue1",lty=2)
lines(IRF.PPP$Upper$dinfl[,2]/0.005,col="steelblue1",lty=2)
abline(h=0)

plot(IRF.PPP$irf$dlip[,1]/0.01,type="l",las=1,xaxs="i",xlab="",ylab="",tck=0.02,col="steelblue4",ylim=c(-6,6),yaxs="i")
lines(IRF.PPP$Lower$dlip[,1]/0.01,col="steelblue4",lty=2)
lines(IRF.PPP$Upper$dlip[,1]/0.01,col="steelblue4",lty=2)
lines(IRF.PPP$irf$dlip[,2]/0.005,type="l",las=1,xaxs="i",xlab="",ylab="",tck=0.02,col="steelblue1")
lines(IRF.PPP$Lower$dlip[,2]/0.005,col="steelblue1",lty=2)
lines(IRF.PPP$Upper$dlip[,2]/0.005,col="steelblue1",lty=2)
abline(h=0)


k = ncol(Y)
amat = diag(k)
diag(amat) = NA
amat[1,2] = NA
amat # inflation is not influencing GDP
SVAR.PPP = SVAR(VAR.PPP, estmethod = "direct", Amat = amat, Bmat = NULL,
                max.iter = 100, maxls = 1000, conv.crit = 1.0e-8)
IRF.PPP = irf(SVAR.PPP,boot=TRUE,cumulative=T,n.ahead=25,ortho=T,las=1)
plot(IRF.PPP$irf$dinfl[,1]/0.01,type="l",las=1,xaxs="i",xlab="",ylab="",tck=0.02,col="steelblue4",ylim=c(-4,8),yaxs="i")
lines(IRF.PPP$Lower$dinfl[,1]/0.01,col="steelblue4",lty=2)
lines(IRF.PPP$Upper$dinfl[,1]/0.01,col="steelblue4",lty=2)
lines(IRF.PPP$irf$dinfl[,2]/0.005,type="l",las=1,xaxs="i",xlab="",ylab="",tck=0.02,col="steelblue1")
lines(IRF.PPP$Lower$dinfl[,2]/0.005,col="steelblue1",lty=2)
lines(IRF.PPP$Upper$dinfl[,2]/0.005,col="steelblue1",lty=2)
abline(h=0)

plot(IRF.PPP$irf$dlip[,1]/0.01,type="l",las=1,xaxs="i",xlab="",ylab="",tck=0.02,col="steelblue4",ylim=c(0,4),yaxs="i")
lines(IRF.PPP$Lower$dlip[,1]/0.01,col="steelblue4",lty=2)
lines(IRF.PPP$Upper$dlip[,1]/0.01,col="steelblue4",lty=2)
lines(IRF.PPP$irf$dlip[,2]/0.005,type="l",las=1,xaxs="i",xlab="",ylab="",tck=0.02,col="steelblue1")
lines(IRF.PPP$Lower$dlip[,2]/0.005,col="steelblue1",lty=2)
lines(IRF.PPP$Upper$dlip[,2]/0.005,col="steelblue1",lty=2)
abline(h=0)

### END