##**Taller 5 Nicolás González J & Sofía Galeano F**

#Limpiar 
rm(list = ls())

#Base de datos
library(readxl)
base_de_datos <- read_excel("growth.xlsx") #Suba la base de datos de la cual desea conocer la regresión lineal, en este caso en particular la base de datos en cuestión se llama growth.xlsx
View(base_de_datos)

#1. Estimar la regresión lineal de la variable dependiente sobre un grupo de k variables independientes
#Se supone, como regla general, que la primera columna de la base de datos es la variable dependiente y el resto son independientes.

var.dependiente <- as.matrix(base_de_datos[1])
var.independientes <- as.matrix(base_de_datos[2:5]) #La regresion planteada pide que se tome como independiente hasta la variable 5 ("rev"). De lo contrario, var.independientes se definiría como  as.matrix(base_de_datos[2:dim(base_de_datos)[2]])                 

mod1 <- lm(var.dependiente~var.independientes, data = base_de_datos)
mod1
mod1res <- stargazer(mod1, type="text")

#2. Generar los valores ajustados de la variable dependiente
coeficientes <- as.matrix(mod1$coefficients)

intercepto <- matrix(1, nrow = dim(base_de_datos)[1], ncol = 1)
observaciones <- cbind(intercepto, var.independientes)
observaciones

valores.ajustados <- valores%*%coeficientes

#3. Generar los residuales de la regresión
u <- mod1$residuals

#4. Generar los cuadrados de los residuales
u2 <- u^2




