# ENDERS Applied econometric time series, 4ed
#####################################
#####################################
###### 3. MODELLING VOLATILITY ######
#####################################
#####################################


# simulación de heterocedasticidad
#
v = rnorm(100)
plot(v,type="l")

e = rep(0,100)
alfa0 = 1
alfa1 = 0.8
for (i in 2:100) e[i]=v[i]*sqrt(alfa0+alfa1*(e[i-1])^2)
plot(e,type="l")

ya = rep(0,100)
alfaya1 = 0.2
for (i in 2:100) ya[i]=alfaya1*ya[i-1]+e[i]
plot(ya,type="l")

yb = rep(0,100)
alfayb1 = 0.9
for (i in 2:100) yb[i]=alfayb1*yb[i-1]+e[i]
plot(yb,type="l")


### PAGE 134
library(readxl)
library(gdata)
library(rugarch)


oil <- read_excel("C:/Users/NIGOJ/Desktop/Nico/Cosas de la U/Econometria R/Econometria/Bases de datos/ch3_oil.xls")
datos <- as.data.frame(oil)
View(datos)
datos$ret = c(0,diff(log(datos$Spot),1))

par(mfcol = c(2,1), oma = c(0,0,1,0) + 0.2, mar = c(0,1,0,0) + 1, mgp = c(0, 0.2, 0))
plot(datos$Date,datos$Spot,type="l",las=1,xaxs="i",xlab="",ylab="",tck=0.02,col="steelblue4")
plot(datos$Date,datos$ret,type="l",las=1,xaxs="i",xlab="",ylab="",tck=0.02,col="steelblue4")

#  estimar un modelo para datos$ret
acf(datos$ret)
pacf(datos$ret)


spec.ma13 = arfimaspec(mean.model=list(armaOrder=c(0,3),include.mean=TRUE),
                      fixed.pars=list(ma2=0))
fit.ma13 = arfimafit(spec=spec.ma13,data=datos$ret)
fit.ma13
res.ma13 = fit.ma13@fit$residuals
sq.res.ma13 = res.ma13^2


### Ljung Box test para residuos al cuadrado
Box.test(sq.res.ma13,type="Ljung-Box",lag=4)


### MCLEOD-LI TEST
spec.ar4 = arfimaspec(mean.model=list(armaOrder=c(4,0),include.mean=TRUE))
fit.ar4 = arfimafit(spec=spec.ar4,data=sq.res.ma13)
fit.ar4

##############################
#  univariate GARCH specification
spec.ma1garch11 = ugarchspec(mean.model=list(armaOrder=c(0,1),include.mean=TRUE),
                       variance.model=list(garchOrder=c(1,1),model="sGARCH"))
fit.ma1garch11 = ugarchfit(spec=spec.ma1garch11,data=datos$ret)
fit.ma1garch11
res.ma1garch11 = fit.ma1garch11@fit$residuals
z.ma1garch11 = fit.ma1garch11@fit$z

acf1 = acf(res.ma1garch11/fit.ma1garch11@fit$sigma)
acf1

acf2 = acf(z.ma1garch11) # two ways to get the same result
acf2

acf3 = acf(z.ma1garch11^2)
acf3

Box.test(z.ma1garch11,lag=4,type="Ljung-Box")
Box.test(z.ma1garch11,lag=8,type="Ljung-Box")
Box.test(z.ma1garch11^2,lag=4,type="Ljung-Box")
Box.test(z.ma1garch11^2,lag=8,type="Ljung-Box")


spec.ar4 = arfimaspec(mean.model=list(armaOrder=c(4,0),include.mean=TRUE))
fit.ar4 = arfimafit(spec=spec.ar4,data=z.ma1garch11)
fit.ar4


### PAGE 136
rgdp <- read_excel("C:/Users/NIGOJ/Desktop/Nico/Cosas de la U/Econometria R/Econometria/Bases de datos/ch4_rgdp.xls")

datos = as.data.frame(rgdp)
## datos$DATE = as.Date(datos$DATE,"%Y-%m-%d")
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

# tarea estimar modelo garch para tasa crecimiento anual de rgpd


### PAGE 134
datos = read.xls("/Users/user/Google Drive/Website/Book/Enders/OIL.xls")
datos = as.data.frame(oil)

dim(datos)
datos$ret = c(0,100*diff(log(datos$Spot),1))
spec.ma3 = arfimaspec(mean.model=list(armaOrder=c(0,3)),
                      fixed.pars=list(ma2=0))
fit.ma3 = arfimafit(spec.ma3,data=datos$ret)
res.ma3 = fit.ma3@fit$residuals

spec.ar4 = arfimaspec(mean.model=list(armaOrder=c(4,0)))
fit.mcleodli.ar4 = arfimafit(spec.ar4,data=res.ma3^2)
fit.mcleodli.ar4

### PAGE 135
spec.ma1garch11 = ugarchspec(mean.model=list(armaOrder=c(0,1)),
                             variance.model=list(garchOrder=c(1,1)))
fit.ma1garch11 = ugarchfit(spec.ma1garch11,data=datos$ret)
fit.ma1garch11
sres.ma1garch11 = fit.ma1garch11@fit$z
acf1 = acf(sres.ma1garch11)
acf1
acf2 = acf(sres.ma1garch11^2)
acf2

Box.test(sres.ma1garch11,lag=4,type="Ljung-Box")
Box.test(sres.ma1garch11,lag=8,type="Ljung-Box")
Box.test(sres.ma1garch11^2,lag=4,type="Ljung-Box")
Box.test(sres.ma1garch11^2,lag=8,type="Ljung-Box")

### PAGE 136
###
datos = read.xls("RGDP.xls")
datos = as.data.frame(rgdp)
datos$DATE = as.Date(as.character(datos$DATE),"%Y-%m-%d")
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


### PAGE 137

library(readxl)
datos = read.xls("QUARTERLY.xls")

datos=quarterly
library("zoo")
datos$DATE = as.yearqtr(datos$DATE)
datos$spread = datos$r5-datos$Tbill
View(datos)

### FIGURE 3.4
plot(datos$DATE,datos$Tbill,type="l",las=1,xlab="",ylab="",main="",xaxs="i",yaxs="i",tck=.02,col="steelblue4",ylim=c(-2,16))
lines(datos$DATE,datos$r5,col="steelblue4",lty=4)
lines(datos$DATE,datos$spread,col="steelblue4",lty=3)
abline(h=0,lty=2)

acf(datos$spread)
pacf(datos$spread)


spec.arma27 = arfimaspec(mean.model=list(armaOrder=c(2,7)),
                         fixed.pars=list(ma2=0,ma3=0,ma4=0,ma5=0,ma6=0))
fit.arma27 = arfimafit(spec.arma27,data=datos$spread,solver="gosolnp")
fit.arma27

res.arma27 = fit.arma27@fit$residuals
spec.ar7 = arfimaspec(mean.model=list(armaOrder=c(7,0)))
fit.mcleodli.ar7 = arfimafit(spec.ar7,res.arma27)
fit.mcleodli.ar7

res.arma27sq = res.arma27^2
fit.mcleodli.ar7sq = arfimafit(spec.ar7,res.arma27sq)
fit.mcleodli.ar7sq


### PAGE 138
spec.arma27garch11 = ugarchspec(mean.model=list(armaOrder=c(2,7)),
                                variance.model=list(garchOrder=c(1,1),model="sGARCH"),
                                fixed.pars=list(ma2=0,ma3=0,ma4=0,ma5=0,ma6=0))
fit.arma27garch11 = ugarchfit(spec.arma27garch11,data=datos$spread,solver="gosolnp")
fit.arma27garch11

sres.arma27garch11 = fit.arma27garch11@fit$z
acf1 = acf(sres.arma27garch11)
acf1

spec.ar2 = arfimaspec(mean.model=list(armaOrder=c(2,0)))
fit.ar2 = arfimafit(spec.ar2,data=sres.arma27garch11^2)
fit.ar2

##################################
#
#                    TAREA
#
#  ¿qué pasaría con modelos 
#                       GARCH(1,2)
#                       GARCH(2,1)
#                       ARCH(2)
#                       ARCH(3)
#  ?
#
##################################
#######   

### PAGE 139 y 140
spec.arma27arch2 = ugarchspec(mean.model=list(armaOrder=c(2,7)),
                              variance.model=list(garchOrder=c(2,0),model="sGARCH"),
                              fixed.pars=list(ma2=0,ma3=0,ma4=0,ma5=0,ma6=0))
fit.arma27arch2 = ugarchfit(spec.arma27arch2,data=datos$spread,solver="gosolnp")
fit.arma27arch2

sres.fit.arma27arch2 = fit.arma27arch2@fit$z
acf1 = acf(sres.fit.arma27arch2)
acf1
acf2 = acf(sres.fit.arma27arch2^2)
acf2

spec.ar4 = arfimaspec(mean.model=list(armaOrder=c(4,0)))
fit.ar4 = arfimafit(spec.ar4,data=sres.fit.arma27arch2^2)
fit.ar4


######################    
###
###
###      ARCH(3)   PAG 140

library(readxl)
library(rugarch)
library("zoo")

datos = read.xls("QUARTERLY.xls")
datos$DATE = as.yearqtr(datos$DATE)
datos$spread = datos$r5-datos$Tbill
View(datos)

plot(datos$DATE,datos$spread,type="l",las=1,xlab="",ylab="",main="",xaxs="i",yaxs="i",tck=.02,col="steelblue4",ylim=c(-2,5))

spec.arma27arch3 = ugarchspec(mean.model=list(armaOrder=c(2,7)),
                              variance.model=list(garchOrder=c(3,0),model="sGARCH"),
                              fixed.pars=list(ma2=0,ma3=0,ma4=0,ma5=0,ma6=0))
fit.arma27arch3 = ugarchfit(spec.arma27arch3,data=datos$spread,solver="gosolnp")
fit.arma27arch3
sres.fit.arma27arch3 = fit.arma27arch3@fit$z
acf1 = acf(sres.fit.arma27arch3)
acf1
acf2 = acf(sres.fit.arma27arch3^2)
acf2

start = 50
space = nrow(datos)-start
space
datos$DATE[start]
fore = matrix(NA,ncol=3,nrow=space)
for (i in 1:space) {
   sample = datos$spread[1:(length(datos$spread)-space+i)]
   spec.arma217arch3 = ugarchspec(mean.model=list(armaOrder=c(2,7),include.mean=TRUE),
                                  variance.model=list(garchOrder=c(3,0)),
                                  fixed.pars=list(ma2=0,ma3=0,ma4=0,ma5=0,ma6=0))
   fit.arma217arch3 = ugarchfit(spec=spec.arma217arch3,data=sample,solver="hybrid")
   fore.arma217arch3 = ugarchforecast(fit.arma217arch3,n.ahead=1)
   c.mean = fore.arma217arch3@forecast$seriesFor
   lower = c.mean - 2*fore.arma217arch3@forecast$sigmaFor   
   upper = c.mean + 2*fore.arma217arch3@forecast$sigmaFor
   fore[i,] = c(c.mean,lower,upper)
   print(i)
}
fore.date=datos$DATE[-c(1:start)]

par(mfrow=c(1,1))
plot(fore.date,fore[,1],type="l",xaxs="i",yaxs="i",las=1,xlab="",ylab="",main="",ylim=c(-6,6),tck=.02)
lines(fore.date,fore[,2],col="steelblue4",lty=1)
lines(fore.date,fore[,3],col="steelblue4",lty=1)
abline(h=0,lty=2)

# incluir datos originales en la gráfica
#
observados = matrix(NA,nrow=162,ncol=1)
for (i in 1:162) observados[i] = datos$spread[start+i]
lines(fore.date,observados,col="red",lty=1)
# 

#####################

#   TALLER NY

#########################


### PAGE 159
datos = read.xlsx("NYSEReturns.xlsx",1)
datos = as.data.frame(NYSEReturns)
return = as.numeric(as.character(datos$RETURN))
rate = na.omit(RATE)
return = datos$RETURN
rate = datos$RATE

plot(datos$ENTRY,datos$RETURN,type="l",las=1,xaxs="i",xlab="",ylab="",tck=.02,col="steelblue4")
plot(datos$ENTRY,datos$RATE,type="l",las=1,xaxs="i",xlab="",ylab="",tck=.02,col="steelblue4")


### FIGURE 3.13
x=seq(-5,5,0.01)
y1=dnorm(x)
y2=dt(x,df=3)

plot(x,y1,type="l",las=1,xaxs="i",xlab="",ylab="",tck=.02,ylim=c(0,0.6),yaxs="i")
lines(x,y2,lty=2)
lines(density(datos$RATE),col="red")
abline(v=0)

spec.ar2 = arfimaspec(mean.model=list(armaOrder=c(2,0)))
fit.ar2 = arfimafit(spec.ar2,data=datos$RATE)
fit.ar2
res.ar2 = fit.ar2@fit$residuals

spec.ar5 = arfimaspec(mean.model=list(armaOrder=c(5,0)))
fit.ar5 = arfimafit(spec.ar5,data=res.ar2^2)
fit.ar5

### PAGE 161
spec.ar2garch11.norm = ugarchspec(mean.model=list(armaOrder=c(2,0)),
                             variance.model=list(garchOrder=c(1,1),model="sGARCH"),
                             distribution.model="norm")
fit.ar2garch11.norm = ugarchfit(spec.ar2garch11.norm,data=datos$RATE)
fit.ar2garch11.norm

spec.ar2garch11.t = ugarchspec(mean.model=list(armaOrder=c(2,0)),
                                  variance.model=list(garchOrder=c(1,1),model="sGARCH"),
                                  distribution.model="std")
fit.ar2garch11.t = ugarchfit(spec.ar2garch11.t,data=datos$RATE)
fit.ar2garch11.t

### PAGE 162
spec.ma2igarchm11.norm = ugarchspec(mean.model=list(armaOrder=c(0,2),archm=TRUE,archpow=2),
                                    variance.model=list(garchOrder=c(1,1),model="iGARCH"),
                                    distribution.model="norm")
#
# archpow Indicates whether to use st.deviation (1) or variance (2) in the ARCH in mean regression.
fit.ma2igarchm11.norm = ugarchfit(spec.ma2igarchm11.norm,data=datos$RATE)
fit.ma2igarchm11.norm

# Diagnostic checking page 162
sres.ma2igarchm11.norm = fit.ma2igarchm11.norm@fit$z
Box.test(sres.ma2igarchm11.norm,lag=5,type="Ljung-Box")
Box.test(sres.ma2igarchm11.norm,lag=10,type="Ljung-Box")
Box.test(sres.ma2igarchm11.norm,lag=15,type="Ljung-Box")

acf1 = acf(sres.ma2igarchm11.norm^2)
acf1

spec.ar2 = arfimaspec(mean.model=list(armaOrder=c(2,0)))
fit.ar2 = arfimafit(spec.ar2,data=sres.ma2igarchm11.norm^2)
fit.ar2



### PAGE 163
### LEVERAGE EFFECT
plag = 2
st = embed(sres.ma2igarchm11.norm,plag+1)
summary(lm(st[,1]^2~st[,-1]))

d = ifelse(sres.ma2igarchm11.norm<0,1,0)
plag = 3
d3 = embed(d,plag+1)[,-1]

### ENGLE-NG SIGN TEST
summary(lm(sres.ma2igarchm11.norm[-c(1:plag)]^2~d3))

plag = 1
st = embed(sres.ma2igarchm11.norm,plag+1)
d = ifelse(st[,-1]<0,1,0)
summary(lm(st[,1]^2~d+I(d*st[,-1])+I((1-d)*st[,-1])))


spec.ar2tgarch11 = ugarchspec(mean.model=list(armaOrder=c(2,0)),
                              variance.model=list(garchOrder=c(1,1),model="apARCH"),
                              fixed.pars=list(delta=1))
fit.ar2tgarch11 = ugarchfit(spec.ar2tgarch11,data=datos$RATE,solver="gosolnp")
fit.ar2tgarch11


### VOLATILITY SMILE
###
plot(fit.ar2tgarch11,which=12)


spec.ar2egarch11 = ugarchspec(mean.model=list(armaOrder=c(2,0)),
                              variance.model=list(garchOrder=c(1,1),model="eGARCH"),
                              distribution.model="std")
fit.ar2egarch11 = ugarchfit(spec.ar2egarch11,data=datos$RATE,solver="gosolnp")
fit.ar2egarch11

###
### VOLATILITY SMILE
###
plot(fit.ar2egarch11,which=12)
res.ar2egarch11 = fit.ar2egarch11@fit$z


###
### PAGE 164
###
spec.ar1 = arfimaspec(mean.model=list(armaOrder=c(1,0)))
fit.ar1 = arfimafit(spec.ar1,data=res.ar2egarch11^2)
fit.ar1

### FIGURE 3.14
plot(fit.ar2egarch11@fit$sigma^2,type="l",las=1,xaxs="i",xlab="",ylab="",tck=.02,yaxs="i",col="steelblue4")



