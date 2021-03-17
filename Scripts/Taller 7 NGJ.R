#**Taller 7 Nicolas Gonzalez J & Sofia Galeano F**----

#Paquetes----
library(readxl)
library(lmtest)
library(tseries)

#Base de datos----
#Se importa la base de datos al ambiente
exrates<- read_excel("Bases de datos/ch5_ExratesXLS.119153453.xlsx")

#Primera serie: e_can----
#Crea una serie de tiempo que tome valores a lo largo de la base de datos con la variable que desea analizar en este caso e_can
e_can = ts(exrates$e_can, start=c(1973,1), frequency=4)
plot(e_can)

#Indentifica el orden de integracion de la serie por medio de pruebas de Dickey-Fuller Aumentadas a traves de la funcion adf.test del paquete tseries
adf.test(e_can)
adf.test(diff(e_can))
adf.test(diff(e_can,1,2))


e_can21 <-  c(NA,NA,diff(e_can,1,2))
plot(e_can21, type = "l")

#Grafica las autocorrelaciones simples y parciales para determinar el orden AR(p) y MA(q) de la serie e_can
par(mfrow=c(2,1))
par(mar=c(2,3,1,2))
acf(e_can)
pacf(e_can)

acf(diff(e_can,1,2))
pacf(diff(e_can,1,2))

