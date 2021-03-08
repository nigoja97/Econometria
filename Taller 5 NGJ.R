##**Taller 5 Nicolás González J & Sofía Galeano F**

#Limpiar 
rm(list = ls())

#Base de datos y paquetes.
install.packages("stargazer")
install.packages("lmtest")
install.packages("sandwich")
install.packages("readxl")
library(stargazer)
library(lmtest)
library(sandwich)
library(readxl)

base_de_datos <- read_excel("growth.xlsx") #Suba la base de datos de la cual desea conocer la regresión lineal, en este caso en particular la base de datos en cuestión se llama growth.xlsx.
View(base_de_datos)

#1. Estimar la regresión lineal de la variable dependiente sobre un grupo de k variables independientes.
#Se supone, como regla general, que la primera columna de la base de datos es la variable dependiente y el resto son independientes.

var.dependiente <- as.matrix(base_de_datos[1])
var.independientes <- as.matrix(base_de_datos[2:5]) #La regresion planteada pide que se tome como independiente hasta la variable 5 ("rev"). De lo contrario, var.independientes se definiría como  as.matrix(base_de_datos[2:dim(base_de_datos)[2]]).                 

mod1 <- lm(var.dependiente~var.independientes, data = base_de_datos)
mod1
mod1res <- stargazer(mod1, type="text")

#2. Generar los valores ajustados de la variable dependiente.
coeficientes <- as.matrix(mod1$coefficients)

intercepto <- matrix(1, nrow = dim(base_de_datos)[1], ncol = 1)
observaciones <- cbind(intercepto, var.independientes)
observaciones

valores.ajustados <- observaciones%*%coeficientes

#3. Generar los residuales de la regresión.
u <- mod1$residuals

#4. Generar los cuadrados de los residuales.
u2 <- u^2

#5. Estimar la regresión auxiliar de los residuales al cuadrado sobre las variables independientes.
#H0: Homocedasticidad.
fitbp <- lm(u2 ~ var.independientes, data=base_de_datos) 
R2bp <- summary(fitbp)$r.squared
k <- dim(var.independientes)[2]
n <- dim(base_de_datos)[1]
estadisticobp <- n * R2bp
valorPbp <- pchisq(q=estadisticobp, df=k, lower.tail=FALSE)
Breusch_Pagan <- cbind(estadisticobp, valorPbp)
Breusch_Pagan
#No Rechaza H0.

#6. Estimar la regresión auxiliar de los residuales al cuadrado sobre los valores ajustados de la variable dependiente y los cuadrados de los valores ajustados de la variable dependiente.
#H0: Homocedasticidad.
fitw <- lm(u2 ~ valores.ajustados+valores.ajustados^2, data=base_de_datos) 
R2w <- summary(fitw)$r.squared
estadisticow <- n * R2w
valorPw <- pchisq(q=estadisticow, df=k, lower.tail=FALSE)
White <- cbind(estadisticow, valorPw)
White
#No Rechaza H0

#7. Para cada una de las regresiones estimadas en 5 y 6 calcular los estadísticos F y L.
#Aunque no es necesario, volvemos a estimar k, n, R2bp y R2w. 
k <- dim(var.independientes)[2]
n <- dim(base_de_datos)[1]
R2bp <- summary(fitbp)$r.squared
R2w <- summary(fitw)$r.squared

#Estimamos el estadistico F haciendo uso de la formula general.
Fbp <- (R2bp/k)/((1-R2bp)/(n-(k+1)))
Fw <- (R2w/k)/((1-R2w)/(n-(k+1)))
Fbp
Fw

#Calculamos los valores P asociados al estadistico F de ambas pruebas.
valorPbpF <- pf(q=Fbp, df1 = k, df2 = n-k-1, lower.tail = FALSE)
valorPwF <- pf(q=Fw, df1 = k, df2 = n-k-1, lower.tail = FALSE)
valorPbpF
valorPwF
#Bajo estos resultados podemos concluir que, NO se rechaza H0, por lo tanto, la prueba de Breusch-Pagan y de White determina que el modelo es homocedastico y no hay problemas de heterocedasticidad.

#El estadistico L ya habia sido calculado en 5 y 6. Acá solo lo renombramos por comodidad.
Lbp <- estadisticobp
Lbw <- estadisticow

#Del estadistico L ya conocemos los valores P que son los siguientes y los renombamos por comodidad.
valorPbpL <- valorPbp
valorPwL <- valorPw
valorPbpL
valorPwL
#Bajo estos resultados podemos concluir que, NO se rechaza H0, por lo tanto, la prueba de Breusch-Pagan y de White determina que el modelo es homocedastico y no hay problemas de heterocedasticidad.

#8. Generar errores estándar robustos a heterocedasticidad para cada uno de los estimadores de los parámetros del modelo.
#Para estimar los errores estandar robustos hacemos uso de la siguiente formula.
lmtest::coeftest(mod1, vcov. = sandwich::vcovHC(mod1, type = 'HC1'))

#coeftest(mod1, vcov = vcovHC(mod1)) Esta linea de codigo y las siguientes son algunos ejemplos de las diferentes formas que encontramos para estimar los errores estandar robustos
#coeftest(mod1, vcov = vcovHC(mod1, "HC1"))
#coeftest(mod1, vcov = vcovHC(mod1, "HC2"))
#coeftest(mod1, vcov = vcovHC(mod1, "HC3"))

#Profe, no pudimos estimar los errores estandar robustos para los coeficientes como lo sugería el taller. Sin embargo, econtramos una solución en internet la cual se muestra anteriormente, el único inconveniente que hayamos fue la presencia de numeros negativos en los errores estandar.

#9. Regresión auxiliar para variación de la prueba White interacciones
#Se estimó un modelo de white con interacciones para determinar la presencia de heterocedasticidad en el modelo.fitwint <- lm(u2 ~ var.independientes+(var.independientes%*%(t(var.independientes))), data=base_de_datos) 
#H0: Homocedasticidad
fitwint <- lm(u2 ~ var.independientes+(var.independientes%*%(t(var.independientes))), data=base_de_datos)
R2wint <- summary(fitwint)$r.squared
estadisticowint <- n * R2wint
valorPwint <- pchisq(q=estadisticowint, df=k, lower.tail=FALSE)
Whiteint <- cbind(estadisticowint, valorPwint)
Whiteint

#A partir del resultado obtenido de la prueba de White con interacciones (Mismo resultado que en la prueba de Breusch-Pagan), se puede concluir que:
#No se Rechaza H0



