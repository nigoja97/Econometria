library(readxl)
library(stargazer)
base_de_datos <- read_excel("growth.xlsx") #Suba la base de datos de la cual desea conocer la regresión lineal, en este caso en particular la base de datos en cuestión se llama growth.xlsx.
var.independientes <- as.matrix(base_de_datos[2:5])

var.dependiente <- as.matrix(base_de_datos[1])
var.independientes <- as.matrix(base_de_datos[2:5]) #La regresion planteada pide que se tome como independiente hasta la variable 5 ("rev"). De lo contrario, var.independientes se definiría como  as.matrix(base_de_datos[2:dim(base_de_datos)[2]]).                 

mod1 <- lm(var.dependiente~var.independientes, data = base_de_datos)
mod1
mod1res <- stargazer(mod1, type="text")

u <- mod1$residuals
u2 <- u^2
n <- dim(base_de_datos)[1]
k <- dim(var.independientes)[2]

#Bono. Regresión auxiliar para variación de la prueba White interacciones
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

#En el script del taller 5 ya estaba este punto. Sin embargo, revisandolo me di cuenta que la linea que contenia el codigo, que en este script es la 4, se habia borrado.