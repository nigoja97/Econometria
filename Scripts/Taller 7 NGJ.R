#**Taller 7 Nicolas Gonzalez J & Sofia Galeano F**----

#Paquetes----
library(readxl)
library(lmtest)
library(tseries)
library(stargazer)

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

#A partir de las graficas de la correlacion simple y parcial de la serie estacionaria podemos determinar que rezagos fijos iran en la estimacion del arima
arfijos1 <- c(1,3,4)
mafijos1 <- c(1,2,3)
arorden1 <- max(arfijos1)
maorden1 <- max(mafijos1)

fijos1 <- rep(0, arorden1+maorden1)
fijos1

for (i in arfijos1) {
  fijos1[i] <- NA
}

for (i in mafijos1) {
  fijos1[i+arorden1] <- NA
}

#Estimamos 3 modelos 
mod11 <- arima(e_can, order = c(4,2,3), fixed = fijos1)
mod12 <- arima(e_can, order = c(4,2,3))
mod13 <- arima(e_can, order = c(2,2,3))
stargazer(mod11,mod12,mod13, type = "text")

#Segunda serie: e_ja----
#Crea una serie de tiempo que tome valores a lo largo de la base de datos con la variable que desea analizar en este caso e_can
e_ja = ts(exrates$e_ja, start=c(1973,1), frequency=4)
par(mfrow = c(1,1))
plot(e_ja)

#Indentifica el orden de integracion de la serie por medio de pruebas de Dickey-Fuller Aumentadas a traves de la funcion adf.test del paquete tseries
adf.test(e_ja)
adf.test(diff(e_ja))

par(mfrow=c(1,1))
e_ja11 <-  c(NA,diff(e_ja,1,1))
plot(e_ja11, type = "l")

#Grafica las autocorrelaciones simples y parciales para determinar el orden AR(p) y MA(q) de la serie e_can
par(mfrow=c(2,1))
par(mar=c(2,3,1,2))
acf(e_ja)
pacf(e_ja)


#A partir de las graficas de la correlacion simple y parcial de la serie estacionaria podemos determinar que rezagos fijos iran en la estimacion del arima
acf(diff(e_ja,1,1))
pacf(diff(e_ja,1,1))

arfijos2 <- c(15)
mafijos2 <- c(1,15)
arorden2 <- max(arfijos2)
maorden2 <- max(mafijos2)

fijos2 <- rep(0, arorden2+maorden2)
fijos2

for (i in arfijos2) {
  fijos2[i] <- NA
}

for (i in mafijos2) {
  fijos2[i+arorden2] <- NA
}

mod21 <- arima(e_ja, order = c(15,1,16))
mod22 <- arima(e_ja, order = c(15,1,15), fixed = fijos2)
mod23 <- arima(e_ja, order = c(1,1,1))
stargazer(mod21,mod22,mod23, type = "text", no.space = TRUE)


#Tercera serie: e_uk----
#Crea una serie de tiempo que tome valores a lo largo de la base de datos con la variable que desea analizar en este caso e_can
e_uk = ts(exrates$e_uk, start=c(1973,1), frequency=4)
par(mfrow = c(1,1))
plot(e_uk)

#Indentifica el orden de integracion de la serie por medio de pruebas de Dickey-Fuller Aumentadas a traves de la funcion adf.test del paquete tseries
adf.test(e_uk)
adf.test(diff(e_uk))

e_uk11 <-  c(NA,diff(e_uk,1,1))
plot(e_uk11, type = "l")

#Grafica las autocorrelaciones simples y parciales para determinar el orden AR(p) y MA(q) de la serie e_can
par(mfrow=c(2,1))
par(mar=c(2,3,1,2))
acf(e_uk)
pacf(e_uk)


#A partir de las graficas de la correlacion simple y parcial de la serie estacionaria podemos determinar que rezagos fijos iran en la estimacion del arima
acf(diff(e_uk,1,1))
pacf(diff(e_uk,1,1))

arfijos3 <- c(1,16)
mafijos3 <- c(1,13,16)
arorden3 <- max(arfijos3)
maorden3 <- max(mafijos3)

fijos3 <- rep(0, arorden3+maorden3)
fijos3

for (i in arfijos3) {
  fijos3[i] <- NA
}

for (i in mafijos3) {
  fijos3[i+arorden3] <- NA
}

mod31 <- arima(e_uk, order = c(16,1,16), fixed = fijos3)
mod32 <- arima(e_uk, order = c(1,1,1))
stargazer(mod31,mod32, type = "text", no.space = TRUE)


