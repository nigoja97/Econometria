#**TAller 6 Nicolas Gonzalez J & Sofia Galeano F***----
#Punto 1----

#Ruido Blanco----
et = rnorm(200, mean = 0, sd = 1)
t <- (1:200)

#Series, Graficas, Facs y Facp----
rt <- ts(start=1,end=200,frequency = 1)
for (j in 1:200) rt[j]=10+0.8*t[j]+et[j]
plot(rt)
facsrt <- acf(rt)
facprt <- pacf(rt)
#No es estacionaria en la media

vt <- ts(start=1,end=200,frequency = 1)
for (j in 1:200) vt[j]=10+5*t[j]-0.05*((t[j])^2)+et[j]
plot(vt)
facsvt <- acf(vt)
facpvt <- pacf(vt)
#No es estacionaria en la media

wt <- ts(start=1,end=200,frequency = 1)
wt[1]=et[1]
for (j in 2:200) wt[j]=1.05*wt[j-1]+et[j]
plot(wt)
facswt <- acf(wt)
facpwt <- pacf(wt)
#No es estacionaria en la media

xt <- ts(start=1,end=200,frequency = 1)
xt[1]=et[1]
for (j in 2:200) xt[j]=xt[j-1]+et[j]
plot(xt)
facsxt <- acf(xt)
facpxt <- pacf(xt)
#No es estacionaria ni en media ni en varianza

yt <- ts(start=1,end=200,frequency = 1)
yt[1]=et[1]
yt[2]=2*yt[1]+et[2]
for (j in 3:200) yt[j]=2*yt[j-1]-yt[j-2]+et[j]
plot(yt)
facsyt <- acf(yt)
facpyt <- pacf(yt)
#No es estacionaria en la media

at <- ts(start=1,end=200,frequency = 1)
at[1]=et[1]
for (j in 2:200) at[j]=0.7*at[j-1]+et[j]
plot(at) 
facsat <- acf(at)
facpat <- pacf(at)
#Es estacionaria en media pero no en varianza

bt <- ts(start=1,end=200,frequency = 1)
bt[1]=et[1]
bt[2]=0.5*bt[1]+et[2]
for (j in 3:200) bt[j]=0.5*bt[j-1]-0.3*bt[j-2]+et[j]
plot(bt)
facsbt <- acf(bt)
facpbt <- pacf(bt)
#Es estacionaria en media y varianza

ct <- ts(start=1,end=200,frequency = 1)
ct[1]=et[1]
for (j in 2:200) ct[j]=et[j]+0.7*et[j-1]
plot(ct)
facsct <- acf(ct)
facpct <- pacf(ct)
#Es estacionara en media y en varianza

dt <- ts(start=1,end=200,frequency = 1)
dt[1]=et[1]
dt[2]=et[2]+0.7*et[1]
for (j in 3:200) dt[j]=et[j]+0.7*et[j-1]-0.3*et[j-2]
plot(dt)
facsdt <- acf(dt)
facpdt <- pacf(dt)
#Es estacionara en media y en varianza

ft <- ts(start=1,end=200,frequency = 1)
ft[1]=et[1]
for (j in 2:200) ft[j]=0.7*ft[j-1]+et[j]+0.5*et[j-1]
plot(ft)
facsft <- acf(ft)
facpft <- pacf(ft)
#Es estacionaria en media pero no en varianza

#Punto 2---- 
#Series Estacionarias----
library(stargazer)
plot(ct)
plot(facsct)
plot(facpct)

mod1ct = arima(ct, order = c(2,0,2))#
mod2ct = arima(ct, order = c(6,0,2))#
mod3ct = arima(ct, order = c(5,0,2))#
stargazer(mod1ct, mod2ct, mod3ct, type="text")
#Luego de estimar 3 modelos diferentes variando la parte AR(p) entre 2, 6 y 5. Se decidio escoger el modelo 2 ARIMA(6,0,2) debido a que todos los coeficientes son significativos y con el menor valor de AIC de los 3.


plot(dt)
plot(facsdt) 
plot(facpdt) 

mod1dt = arima(dt, order = c(1,0,3))#
mod2dt = arima(dt, order = c(2,0,3))#
mod3dt = arima(dt, order = c(3,0,3))#
stargazer(mod1dt, mod2dt, mod3dt, type="text")
#Luego de estimar 3 modelos diferentes variando la parte AR(p) entre 1, 2 y 3. Se decidio escoger el modelo 1 ARIMA(1,0,3) debido a que todos los coeficientes son significativos y con el menor valor de AIC de los 3.


#Serie no estacionaria----

plot(yt)
plot(facsyt)
plot(facpyt)

yt12 = c(NA, NA, diff(yt,1,2))
plot(yt11, type = "l")

acf(diff(yt,1,2))
pacf(diff(yt,1,2))

mod1yt = arima(yt, order = c(0,2,1))
mod2yt = arima(yt, order = c(1,2,0))
mod3yt = arima(yt, order = c(1,2,0))

stargazer(mod1yt, mod2yt, mod3yt, type="text")

#Tras diferenciar 2 veces determinamos que la serie no estacionaria (yt) tiene un comportamiento de ruido blanco. Debido a que en las pruebas de autocorrelacion no se encuentran rezagos significativos.

Box.test(yt12, lag = 20,type = "Ljung-Box") 

#Tras correr la prueba de Ljung-Box podemos determinar que la serie yt diferenciada 2 veces es, efecttivamente, un ruido blanco al NO rechazarse la hipotesis nula.
