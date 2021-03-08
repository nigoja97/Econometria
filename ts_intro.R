# establecer márgenes
# abajo, izquierda, arriba, derecha

par(mar=c(5,1,1,1))
set.seed(20210308)

num = 1:10
num

numcum = cumsum(num)
numcum

ts2010q = ts(1:20, frequency = 4, start = c(2010, 2)) # 2nd Quarter of 2010
ts2010q

ts2010cum = cumsum(ts2010q)
ts2010cum

ts2010cum = ts(cumsum(ts2010q),start=c(2010,2), frequency=4)
ts2010cum


## Using July 1954 as start date:
gnp <- ts(cumsum(1 + round(rnorm(100), 2)),start=c(1954,7),frequency=12)
plot(gnp) # using 'plot.ts' for time-series plot
#  acf(x, lag.max = NULL, 
#      type = c("correlation", "covariance", "partial"),
#      plot = TRUE)

facs <- acf(gnp)
facp <- pacf(gnp)

facs <- acf(gnp,type="correlation")
facp <- acf(gnp,type="partial")


rw <- ts(cumsum(round(rnorm(100), 2)),start=c(1954,7),frequency=12)
rw
plot(rw)
facsrw <- acf(rw)
facprw <- pacf(rw)
facsrw <- acf(rw, lag.max=50)

# generación de series rezagadas
# diff(x, lag=1, differences=1)

m = sample(1:9,size=50,replace=T)
m
m11 = c(NA,diff(m,1,1))
m12 = c(NA,NA,diff(m,1,2))

L.11.12 = cbind(m,m11,m12)
L.11.12

m21=c(NA,NA,diff(m,2,1))
L21 = cbind(m,m21)
L21


y = ts(start=1,end=200,frequency = 1)
# AR(1)  t>1  y(t) = 0.6*y(t-1)+e(t)    y(1) = e(1)
#   
e = rnorm(200)      # rnorm(n, mean=0, sd=1)
y[1]=e[1]
for (j in 2:200) y[j]=0.6*y[j-1]+e[j]
plot(y)

acf(y)
pacf(y)

mod1 = arima(y, order = c(1,0,3))
mod2 = arima(y, order = c(1,0,1))

mod1$coef
mod1$sigma2
mod1$var.coef
mod1$aic
mod1$arma
mod1$residuals

library(stargazer)
stargazer(mod1,mod2,type="text")

m1v = mod1$var.coef
m1v
eear1 = sqrt(m1v[1,1])
eear1

m1res = mod1$residuals
Box.test(m1res, lag = 20,type = "Ljung-Box") 


err = rnorm(200)
yt = ts(start=1,end=200,frequency = 1)
yt[1] = err[1]
yt[2] = 2*yt[1]+err[2]
for (t in 3:80) yt[t]=2*yt[t-1]-yt[t-2]+err[t] 
plot(yt)
plot(diff(yt,1,1))
plot(diff(yt,1,2))



acf(yt)
acf(diff(yt,1,1))
acf(diff(yt,1,2))
acf(diff(yt,1,3))

modyt = arima(yt,order=c(1,2,0))
modyt$coef


