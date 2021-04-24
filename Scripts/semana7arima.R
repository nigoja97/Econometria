## Estrategia de Construcción del Modelo

# La estrategia se divide en tres partes:
  
# Especificación del modelo
# Ajuste del modelo
# Diagnóstico del modelo
# Pronóstico

install.packages("TSA")

## Especificación del modelo

library(readxl)
library(ggplot2)
library(forecast)
library(tseries)
library(TSA)     

trm2020 <- read_excel("trm2020.xlsx")
TRM <- trm2020
View(TRM)

# Se ajusta la base para que R pueda leer la columna de f 
# como una variable de tiempo.

TRM$f=as.Date(TRM$f, format = "%d/%m/%Y")

TRM$f <- TRM$fecha
TRM <- TRM[,-1]

data$TRM <- TRM$TRM
TRM <- TRM[,-1]
TRM$f <- as.Date(TRM$f)

#### Grafico de la TRM 
par(mar=c(3,2,1,1))
plot(TRM$f,data$TRM,type = 'l')
ggplot(data = TRM, mapping = aes(x = f)) + 
  geom_line(aes(y = Precio), color="darkblue")+
  scale_x_date(breaks = function(x) seq.Date(from = min(x),to = max(x),by = "24 months")) 

plot(TRM$f[-1],diff(log(data$TRM)),ylab='Cambio en Log(Precio)',type='l')

# Transformación de la serie en un objeto ts

# Se calculan dos columnas adicionales, 
# la primera es un promedio móvil de siete días, 
# y la segunda es un promedio móvil del precio por treinta días. 
# El promedio movil ayuda a determinar 
# patrones cíclicos, estacionarios y de tendencia en la serie. 

TRM$Trmma = ma(data$TRM, order=7)
TRM$Trmma30 = ma(data$TRM, order=30)

ggplot() +
  geom_line(data = TRM, aes(x = f, y = Precio, color = "TRM")) +
  geom_line(data = TRM, aes(x = f, y = Trmma,   color = "Semanal"))  +
  geom_line(data = TRM, aes(x = f, y = Trmma30, color = "Mensual"))  +
  ylab('TRM')+
  scale_x_date(breaks = function(x) seq.Date(from = min(x),to = max(x),by = "24 months")) 


### Componente estacional y de tendencia 

# El método  que se puede implementar para desestacionalizar 
# la serie y quitar la tendencia, 
# el primero usa la función stl que tiene una ventana de mínimo siete dias 

TRmma = ts(na.omit(data$TRM), frequency=365)
decomp = stl(TRmma, s.window="periodic")

# decomp contiene una matriz
# decomp$time.series de 3 columnas 
#     seasonal   trend   remainder

deseasonal_trm <- seasadj(decomp)

plot(decomp)
plot(deseasonal_trm)
plot(data$TRM,deseasonal_trm)

deseasonal_trm[1:10]
data$TRM[1:10]

length(decomp$time.series)
length(data$TRM)
compon <- decomp$time.series

### Componente Estacionario----

# Se usa el test de raiz unitaria para determinar si se rechaza 
# la hipotesis nula de no estacionariedad. 

adf.test(data$TRM, alternative = "stationary")
adf.test(na.omit(TRM$Trmma), alternative = "stationary")
adf.test(na.omit(TRM$Trmma30), alternative = "stationary")

# El p value, muestra que la hipótesis nula no se debe rechazar, 
# por lo tanto la serie no es estacionaria y 
# es preciso generar un nuevo test diferenciando la serie original 
# y trabajando con los retornos.


adf.test(diff(data$TRM), alternative = "stationary")
adf.test(diff(log(data$TRM)), alternative = "stationary")
# Con un p value muy por debajo a 0.01 
# se rechaza la hipótesis nula de Raiz unitaria 
# y por lo tanto se puede empezar a trabajar con esta serie 
# con el fin de ajustar un modelo que permita pronosticar la TRM

# diff log es tasa de crecimiento
# diff es variación absoluta

## Ajuste del modelo

### Función de Autocorrelación y de Autocorrelación parcial

# La función de autocorrelación muestra la dependencia de las observaciones, 
# entre mayor sea el grado de autocorrelación significa que 
# las observaciones toman datos pasados para generar precios futuros. 

# El primer paso conocido para definir los parámetros del modelo es con 
# la función de autocorrelación ACF
# Permite definir el grado de dependencia entre los errores 
# por lo tanto el orden para el componente MA se obtendrá
# con ayuda del correlograma ACF. 
# El correlograma del PACF permite saber el nivel de autocorrelación 
# con las observaciones pasadas.
# por lo tanto el orden para el componente AR se obtendrá
# con ayuda del correlograma PACF. 

par(mfrow=c(2,1))
par(mar=c(2,3,1,1))
acf(data$TRM)
pacf(data$TRM)

# En las figuras se generan los correlogramas para obtener 
# los rezagos del modelo con los precios de la TRM, 
# como se puede ver el ACF tiene una caida lineal, por lo tanto, 
# trabajar con las diferencias del precio puede traer 
# mejores resultados puesto que la serie se puede volver estacionaria, 
# por otra parte el PCAF muestra un rezago con la primera observación.

# diferencias = variación absoluta
par(mfrow=c(2,1))
acf(diff(data$TRM))
pacf(diff(data$TRM))

# dif log, variaciones relativas
par(mfrow=c(2,1))
par(mar=c(2,4,1,1))
acf(diff(log(data$TRM)))
pacf(diff(log(data$TRM)))

# La figura muestra las funciones de autocorrelación de la primera diferencia

### Creación del modelo


fit2 = arima(data$TRM, order=c(2,1,1))
fit2

AIC(fit2)
BIC(fit2)
dev.off()
par(mfrow=c(1,1))
par(mar=c(2,4,1,1))
plot(residuals(fit2))
par(mar=c(2,4,1,1))
tsdisplay(residuals(fit2), lag.max=15, main=' Model Residuals')
#  tsdisplay es de {forecast}


## Diagnóstico del modelo

#
fit2sig <- 2*(1-pnorm(abs(fit2$coef)/sqrt(abs(diag(fit2$var.coef)))))
fit2sig

# De acuerdo con el gráfico de autocorrelación
# se genera primero un modelo saturado 
# usando el coeficiente del AR y del MA más altos 
# y diferenciando una sola vez la serie, 
# pues el gráfico es el correlograma de la serie diferenciada.

# * Primer Modelo


primero = arima(data$TRM, order=c(2,1,3))
primero
signif(acf(residuals(primero),plot=F)$acf[1:6],2)
Box.test(resid(primero),type="Ljung",lag=30,fitdf=1)
AIC(primero)
BIC(primero)
win.graph(width=4.875,height=4.5)
tsdiag(primero,gof=15,omit.initial=F)

# Residuos estandarizados


qqnorm(residuals(primero))
qqline(residuals(primero))
hist(residuals(primero))


# * Segundo Modelo

segundo = arima(data$TRM, order=c(2,1,2))
segundo
signif(acf(residuals(segundo),plot=FALSE)$acf[1:6],2)
Box.test(resid(segundo),type="Ljung",lag=20,fitdf=1)
AIC(segundo)
BIC(segundo)
win.graph(width=4.875,height=4.5)
tsdiag(segundo,gof=14,omit.initial=F)


# Residuos estandarizados


qqnorm(residuals(segundo))
qqline(residuals(segundo))




# * Tercer Modelo


tercero = arima(data$TRM, order=c(2,1,1))
tercero
signif(acf(residuals(tercero),plot=FALSE)$acf[1:6],2)
Box.test(resid(tercero),type="Ljung",lag=8,fitdf=1)
Box.test(resid(tercero),type="Ljung",lag=15,fitdf=1)
Box.test(resid(tercero),type="Ljung",lag=20,fitdf=1)
AIC(tercero)
BIC(tercero)
win.graph(width=4.875,height=4.5)
tsdiag(tercero,gof=15,omit.initial=F)

# Residuos estandarizados


qqnorm(residuals(segundo))
qqline(residuals(segundo))


## Pronóstico


pronostico=predict(fit2)
dev.off()
plot(fit2, n.ahead=100,type="l",ylab= "TRM", xlab="Tiempo",main="Pronóstico TRM",col="blue")
abline(h=coef(fit2)[names(coef(fit2))=='intercepto'],col="red")




# Se puede usar la función de auto.arima para que con ayuda del 
# rmse R obtenga el mejor modelo, sin embargo, 
# es mejor hacer un primer modelo saturado e ir 
# revisando cada uno de los componentes que arroja la función 
# ACF y PCAF y encontrar el mejor candidato 
# con los test AIC y BIC. 
# Es importante entender la serie puesto que,
# dependiendo de los supuestos con los que se está trabajando,
# algunas veces puede ser mejor transformar la serie.

auto.arima(data$TRM, seasonal=FALSE)

fit <- auto.arima(data$TRM)
fit
par(mar=c(2,2,1,1))
tsdisplay(residuals(fit), lag.max=45, main='Model Residuals')


####  =============================
#     como time series de R

TRMT=ts(data$TRM,start=c(2005,05,01),freq=365)
auto.arima(TRMT, seasonal=FALSE)

fitts <- auto.arima(TRMT)
fitts
par(mar=c(2,2,1,1))
tsdisplay(residuals(fitts), lag.max=30, main='Model Residuals')

arfijos <- c(1,2,8,15)
mafijos <- c(1,3)
arorden <- max(arfijos)
maorden <- max(mafijos)

fijos <- rep(0, arorden+maorden)
fijos

for (i in arfijos) {
  fijos[i] <- NA
}

for (i in mafijos) {
  fijos[i+arorden] <- NA
}


