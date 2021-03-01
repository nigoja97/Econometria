#Taller 4 Nicolás González y Sofía Galeano
#Base de Datos y Paquetes

library(readxl)
mrw_base <- read_excel("mrw_base.xlsx")
install.packages("stargazer")
library(stargazer)

#Desarrollo 
#Discriminamos algunas variables de la base de datos y las transformamos. Tambien se define la suma de la tasa de depreciacion del capital (d) y la tasa de crecimiento de la tecnologia (g) (gd =g+d) como 0.05

y <- mrw_base$YL85
s <- mrw_base$IY/100
n <- mrw_base$N6085/100
gd <- 0.05

#Transformamos las variables para estabilizar varianza

lny <- log(y)
lns <- log(s)
lngdn <- log(gd+n)
lnschool <- log(mrw_base$SCHOOL)
lnsgdn <- lns-lngdn

#Estimamos los modelos 

mod1 <- lm(lny~lns+lngdn,data=mrw_base)
mod1res <- summary(mod1)

mod2 <- lm(lny~lnsgdn,data=mrw_base)
mod2res <- summary(mod2)

mod3 <- lm(lny~lns+lngdn+lnschool,data=mrw_base)
mod3res <- summary(mod3)

resumen <- stargazer(mod1,mod2, mod3, type="text", dep.var.labels = "Logaritmo del Ingreso per cápita")

#Conclusiones

#Se estimaron 3 modelos sobre el logaritmo natural del ingreso per capita. 
#El primero de todos tiendo en cuenta el logaritmo natural de la proporcion del ingreso destinado al ahorro y la tasa de depreciacion del capital sumada a la tasa de crecimiento de la poblacion laboral.
#El segundo modelo se diferencia del primero pues se estima el logaritmo del ingreso per capita bajo una nueva variable definida como la resta entre el logaritmo natural de la proporcion del ingreso destinada a ahorrar y la suma entre el logaritmo natrual de la tasa de depreciacion del capital y la tasa de crecimiento de la poblacion laboral.
#El tercer modelo estima el logaritmo del ingreso per capita teniendo en cuenta el logaritmo de la proporcion de ingreso destinada al ahorro, la tasade depreciacion del capital sumada a la tasa de crecimiento de la poblacion laboral y, adicionalmente, el logaritmo del porcentaje de población trabajadora en escuela secundaria.

#Luego de estimar los 3 modelos y analizar los coeficientes que acompañan cada variable podemos determinar que:

#En el primer modelo las 2 variables poseen significancia estadistica del 1%. Entonces, en promedio y manteniendo todo lo demás constante: 
#Ante un aumento del 1% en el logaritmo natural de la proporcion del ingreso destinada al ahorro,el logaritmo natural del ingreso per capita aumentará en 1.325%. 
#Ante un aumento del 1% en el logaritmo natural de la suma entre el la tasa de depreciacion del capital y la tasa de crecimiento de la poblacion laboral, el logaritmo natural del ingreso per capita disminuirá en 2.013%. 
#Se espera que si el resto de las variables valen cero, el logaritmo natural de la ingreso per capita tenga un valor de 5.368%
#La fraccion de la variacion del logaritmo del ignreso per capita explicada por el modelo es de 0.602


#En el segundo modelo la variable posee significancia estadistica del 1%. Entonces, en promedio y manteniendo todo lo demás constante: 
#Ante un aumento del 1% en la resta entre el logaritmo natural de la proporcion del ingreso destinada a ahorrar y la suma entre el logaritmo natrual de la tasa de depreciacion del capital y la tasa de crecimiento de la poblacion laboral, el logaritmo natural del ingreso per capita aumentará en 1.437%.
#Se espera que si el resto de las variables valen cero, el logaritmo natural de la ingreso per capita tenga un valor de 7.086%
#La fraccion de la variacion del logaritmo del ignreso per capita explicada por el modelo es de 0.595


#En el tercer modelo las 3 variables poseen significancia estadistica del 1%. Entonces, en promedio y manteniendo todo lo demás constante: 
#Ante un aumento del 1% en el logaritmo natural de la proporcion del ingreso destinada al ahorro, el logaritmo natural del ingreso per capita aumentará en 0.710%. 
#Ante un aumento del 1% en el logaritmo natural de la suma entre el la tasa de depreciacion del capital y la tasa de crecimiento de la poblacion laboral, el logaritmo natural del ingreso per capita disminuirá en 1.497%. 
#Ante un aumento del 1% en el logaritmo del porcentaje de población trabajadora en escuela secundaria, el logaritmo natural del ingreso per capita aumentará en 0.729%
#Se espera que si el resto de las variables valen cero, el logaritmo natural de la ingreso per capita tenga un valor de 4.451%
#La fraccion de la variacion del logaritmo del ignreso per capita explicada por el modelo es de 0.782

#De manera individual, los 3 modelos poseen una significancia estadistica conjunta significativa al 1%. Sin embargo, las diferencias entre los R^2 sugieren que el modelo que más se ajusta y que mejor predice la realidad es el tercero, sin tener en cuenta los criterios de informacion de AIC, BIC y HQ.








