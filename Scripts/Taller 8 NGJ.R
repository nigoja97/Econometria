#############Taller 8##############----
#PAQUETES
library(readxl)
library(rugarch)
library(lubridate)
library(tidyverse)
library(tseries)
library(forecast)
library(ggplot2)
library(TSA)

data <- read_excel("C:/Users/NIGOJ/Desktop/Nico/Cosas de la U/Econometria R/Econometria/Bases de datos/trm2020.xlsx")
data$fecha <- ymd(data$fecha)
data$TRM <- ts(data = data$TRM, frequency=365)

#GRÁFICO DE LA TRM----
#GRÁFICAS

#TRM
ggplot(data = data, mapping = aes(x = fecha)) + 
  geom_line(aes(y = TRM), color="darkblue")+
  scale_x_date(breaks = function(x) seq.Date(from = min(x),to = max(x),by = "24 months")) 


#VARIACIÓN ABSOLUTA
data[,"dTRM"] <- c(NA,diff(data$TRM))
ggplot(data = data, mapping = aes(x = fecha)) + 
  geom_line(aes(y = dTRM), color="darkblue")+
  scale_x_date(breaks = function(x) seq.Date(from = min(x),to = max(x),by = "24 months"))


#VARIACIÓN RELATIVA
data[,"lndTRM"] <- c(NA,diff(log(data$TRM)))
ggplot(data = data, mapping = aes(x = fecha)) + 
  geom_line(aes(y = lndTRM), color="darkblue")+
  scale_x_date(breaks = function(x) seq.Date(from = min(x),to = max(x),by = "24 months"))

#ESTACIONARIEDAD----
#PRUEBAS ADF
adf.test(data$TRM, alternative = "stationary")
adf.test(diff(data$TRM), alternative = "stationary")
adf.test(diff(log(data$TRM)), alternative = "stationary")
#SE PUEDE CONLCUIR QUE LA SERIE TRM ES INTEGRADA DE ORDEN 1 (d=1)

lndTRM <- diff(log(data$TRM))
#EXTRAEMOS Y SEPARAMOS LA VARIACIÓN RELATIVA DE LA TRM

#DETERMINACIÓN DE ÓRDEN ARMA(p,q)----
#AUTOCORRELACION Y AUTOCORRELACION PARCIAL
par(mfrow=c(2,1))
par(mar=c(2,3,1,1))
acf(data$TRM)
pacf(data$TRM)
#LOS GRAFICOS NO SON CONCLUYENTES AL NO TRATARSE DE UNA SERIE ESTACIONARIA

par(mfrow=c(2,1))
par(mar=c(2,4,1,1))
acf(lndTRM,lag=30,tck=.02,xlab="",ylab="",main="")
pacf(lndTRM,lag=30,tck=.02,xlab="",ylab="",main="")
#AL TRABAJAR CON LA SERIE ESTACIONARIA PODEMOS DETERMINAR EL ORDEN AR(p) Y MA(q) DE MANERA ÓPTIMA

#CREACIÓN DEL MODELO
#POSIBLES ORDENES SEGUN AUTOCORRELACIONES
#AR(p): 1, 8, 10, 13, 17, 18, 24, 26, 28
#MA(q): 1, 8, 10, 13, 17, 19, 24, 28

#MODELO 1: AR(1)
spec.ar1 <- arfimaspec(mean.model=list(armaOrder=c(1,0),include.mean=TRUE))
fit.ar1 <- arfimafit(spec=spec.ar1,data=lndTRM)
fit.ar1
res.ar1 = fit.ar1@fit$residuals
Box.test(res.ar1,lag=8,type="Ljung-Box")
Box.test(res.ar1,lag=9,type="Ljung-Box")
Box.test(res.ar1,lag=13,type="Ljung-Box")

#MODELO 2: AR(2)
spec.ar2 <- arfimaspec(mean.model=list(armaOrder=c(2,0),include.mean=TRUE))
fit.ar2 <- arfimafit(spec=spec.ar2,data=lndTRM)
fit.ar2
res.ar2 = fit.ar2@fit$residuals
Box.test(res.ar2,lag=8,type="Ljung-Box")
Box.test(res.ar2,lag=9,type="Ljung-Box")
Box.test(res.ar2,lag=13,type="Ljung-Box")
#COEFICIENTE AR2 NO SIGNIFICATIVO. MODELO 2 DESCARTADO

#MODELO 3: ARMA(1,13(1,8,10,13))
spec.arma113 <- arfimaspec(mean.model=list(armaOrder=c(1,13),include.mean=TRUE), 
                         fixed.pars = list(ma2=0,ma3=0,ma4=0,ma5=0,ma6=0,ma7=0,
                                           ma9=0,ma11=0,ma12=0))
fit.arma113 <- arfimafit(spec=spec.arma113,data=lndTRM)
fit.arma113
res.arma113 = fit.arma113@fit$residuals
Box.test(res.arma113,lag=8,type="Ljung-Box")
Box.test(res.arma113,lag=16,type="Ljung-Box")
Box.test(res.arma113,lag=28,type="Ljung-Box")
#DIFERENTES COEFICIENTES NO SIGNIFICATIVOS PERO AR10 Y AR13 SIGNIFICATIVOS. DE TODOS MODOS, MODELO 3 DESCARTADO

#MODELO 4: ARMA(1,28(10,13,17,24,28))
spec.arma128 <- arfimaspec(mean.model=list(armaOrder=c(1,28),include.mean=FALSE), 
                       fixed.pars = list(ma1=0,ma2=0,ma3=0,ma4=0,ma5=0,ma6=0,ma7=0,
                                         ma8=0,ma9=0,ma11=0,ma12=0,ma14=0,ma15=0,ma16=0,
                                         ma18=0,ma19=0,ma20=0,ma21=0,ma22=0,ma23=0,ma25=0,
                                         ma26=0,ma27=0))

fit.arma128 <- arfimafit(spec=spec.arma128,data=lndTRM)
fit.arma128
res.arma128 = fit.arma128@fit$residuals
Box.test(res.arma128,lag=8,type="Ljung-Box")
Box.test(res.arma128,lag=16,type="Ljung-Box")
Box.test(res.arma128,lag=28,type="Ljung-Box")
#TODOS LOS COEFICIENTES SON SIGNIFICATIVOS. MODELO 4 NO DESCARTADO

infocriteria(fit.ar1)
infocriteria(fit.ar2)
infocriteria(fit.arma113)
infocriteria(fit.arma128)
#EL MODELO 4 ES EL QUE TIENE MEJOR COMPORTAMIENTO SEGÚN LOS CRITERIOS DE INFORMACIÓN. SE TOMARÁ ESTE MODELO PARA HACER EL PRONÓSTICO


#INESTABILIDAD DE PARAMETROS----
tm <- which(data$fecha=="2021-01-01")-1
t <-  which(data$fecha=="2021-04-21")

spec.arma128 <- arfimaspec(mean.model=list(armaOrder=c(1,28),include.mean=FALSE), 
                           fixed.pars = list(ma1=0,ma2=0,ma3=0,ma4=0,ma5=0,ma6=0,ma7=0,
                                             ma8=0,ma9=0,ma11=0,ma12=0,ma14=0,ma15=0,ma16=0,
                                             ma18=0,ma19=0,ma20=0,ma21=0,ma22=0,ma23=0,ma25=0,
                                             ma26=0,ma27=0))

fit.arma128.pre <- arfimafit(spec=spec.arma128,data=lndTRM[1:tm], solver = "gosolnp")
fit.arma128.pre
fit.arma128.post <- arfimafit(spec=spec.arma128,data=lndTRM[(tm+1):length(lndTRM)], solver = "gosolnp")
fit.arma128.post
fit.arma128 <- arfimafit(spec=spec.arma128,data=lndTRM, solver = "gosolnp")
fit.arma128

length(fit.arma128@model$fixed.pars)    #   28: 10,13,17,24,28
length(fit.arma128@fit$coef)            #  11: mu,ar1,ma10,...,ma28,sigma   
n <- length(fit.arma128@fit$coef) - length(fit.arma128@model$fixed.pars)  - 1
gl1 <- n
gl2 <- t - (2*n)

SSR1 = sum(fit.arma128.pre@fit$residuals^2)
SSR2 = sum(fit.arma128.post@fit$residuals^2)
SSR  = sum(fit.arma128@fit$residuals^2)
Fcambio = ((SSR-SSR1-SSR2)/gl1)/((SSR1+SSR2)/gl2)
pf(Fcambio,df1=5,df2=212-2*5,lower.tail = F)
#NO HAY EVIDENCIA DE CAMBIO ESTRUCTURAL. P-VALOR 0.6727>0.05 ENTONCES NO SE RECHAZA H0


#PRONÓSTICOS----
which(data$fecha=="2021-01-01")
fecha2021 = lndTRM[5844:length(lndTRM)]
space = length(fecha2021)

fore.arma128 = NULL
for (i in 1:space) {
  print(i)
  fit.arma128 <- arfimafit(spec=spec.arma128,data=lndTRM[1:(5843+i-1)],solver="solnp")
  fore.arma128[i] <- arfimaforecast(fit.arma128,n.ahead=1)@forecast$seriesFor
}

mean(fore.arma128)
var(fore.arma128)
par(mfrow=c(1,1))
plot(fecha2021, type="l",col="red")
lines(fore.arma128, type="l",col="blue")


fore.error.arma128 = fore.arma128-fecha2021
mean(fore.error.arma128)
var(fore.error.arma128)

mod4 <- summary(lm(fecha2021~fore.arma128))
mod4

#CONCLUSION: Para hallar el error entre las dos regresiones se realiza una resta entre las observaciones pronosticadas por el modelo y las  observaciones reales. 
#Al calcular la media de esta operacion podemos observar que corresponde al valor de 0.000500114, este valor indica el promedio de todos los datos, en otras palabras, 
#es una medida estándar de la distribución de los datos y al ser tan pequeña quiere decir que el error es demasiado pequeño y podemos concluir que el modelo se ajusta 
#bastante bien a los datos actuales de la regresion, ademas tambien se puede calcular la varianza que corresponde al valor de 0.00000337063 lo que quiere decir que la 
#dispercion de los datos es demasiado pequeña, lo que refuerza la conclusion anterior. Sin embargo, los resultados negativos al regresar las observaciones originales sobre
#las pronósticadas no son los más ajustadas.

