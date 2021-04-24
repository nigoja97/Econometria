#############Taller 8##############----
library(gdata)
library(readxl)
library(Ecdat)
library(rugarch)
library(lubridate)
library(tidyverse)
library(tseries)
library(forecast)
library(ggplot2)
library(TSA)

data <- read_excel("C:/Users/NIGOJ/Desktop/Nico/Cosas de la U/Econometria R/Econometria/Bases de datos/trm2020.xlsx")
data$fecha <- ymd(data$fecha)

#GRÁFICO DE LA TRM----
ggplot(data = data, mapping = aes(x = fecha)) + 
  geom_line(aes(y = TRM), color="darkblue")+
  scale_x_date(breaks = function(x) seq.Date(from = min(x),to = max(x),by = "24 months")) 

data[,"dTRM"] <- c(NA,diff(data$TRM))
ggplot(data = data, mapping = aes(x = fecha)) + 
  geom_line(aes(y = dTRM), color="darkblue")+
  scale_x_date(breaks = function(x) seq.Date(from = min(x),to = max(x),by = "24 months"))

data[,"lndTRM"] <- c(NA,diff(log(data$TRM)))
ggplot(data = data, mapping = aes(x = fecha)) + 
  geom_line(aes(y = lndTRM), color="darkblue")+
  scale_x_date(breaks = function(x) seq.Date(from = min(x),to = max(x),by = "24 months"))


#ESTACIONARIEDAD----
#Promedio movil de 7 y 30 dias para determinar componentes estacionarios y cíclicos de la serie
data$Trmma7 = ma(data$TRM, order=7)
data$Trmma30 = ma(data$TRM, order=30)

ggplot() +
  geom_line(data = data, aes(x = fecha, y = TRM, color = "TRM")) +
  geom_line(data = data, aes(x = fecha, y = Trmma7,   color = "Semanal"))  +
  geom_line(data = data, aes(x = fecha, y = Trmma30, color = "Mensual"))  +
  ylab('TRM')+
  scale_x_date(breaks = function(x) seq.Date(from = min(x),to = max(x),by = "24 months")) 


#PRUEBAS ADF
adf.test(data$TRM, alternative = "stationary")
adf.test(diff(data$TRM), alternative = "stationary")
adf.test(diff(log(data$TRM)), alternative = "stationary")
#SE PUEDE CONLCUIR QUE LA SERIE ES INTEGRADA DE ORDEN 1 (d=1)


#DETERMINACIÓN DE ÓRDEN ARMA(p,q)----
#AUTOCORRELACION Y AUTOCORRELACION PARCIAL
par(mfrow=c(2,1))
par(mar=c(2,3,1,1))
acf(data$TRM)
pacf(data$TRM)
#LOS GRAFICOS NO SON CONCLUYENTES AL NO TRATARSE DE UNA SERIE ESTACIONARIA

par(mfrow=c(2,1))
par(mar=c(2,4,1,1))

acf(diff(data$TRM))
pacf(diff(data$TRM))
acf(diff(log(data$TRM)))
pacf(diff(log(data$TRM)))
#AL TRABAJAR CON LA SERIE ESTACIONARIA PODEMOS DETERMINAR EL ORDEN AR(p) Y MA(q) DE MANERA ÓPTIMA

#CREACIÓN DEL MODELO
