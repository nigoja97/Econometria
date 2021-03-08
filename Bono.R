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
