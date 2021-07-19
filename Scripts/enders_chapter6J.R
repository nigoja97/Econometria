
#########################################################
#########################################################
###### 6. COINTEGRATION AND ERRORCORRECTION MODELS ######
#########################################################
#########################################################

### PAGE 366
library("gdata")
library("readxl")
library("vars")
library(tseries)


#--------------------------------------------------------------------
# Ejemplo sencillo de cointegración entre 2 variables

set.seed(20210519)
# m  random walk
# m(t) = m(t-1) + em
# em ey ez white noise
# y = m + ey
# z = m + ez
n = 25
em = rnorm(n)
ey = rnorm(n)
ez = rnorm(n)
m = em
for (i in 2:n) {
  m[i]=m[i-1]+em[i]
}
y = m + ey
z = m + ez

par(mfcol = c(2,1), oma = c(0,0,1,0) + 0.2, mar = c(0,1,0,0) + 1, mgp = c(0, 0.2, 0))

w = y-z

par(mfcol = c(2,1))
plot(y,type="l",xaxs="i",las=1,xlab="",ylab="",tck=.02)
lines(z,lty=2, col="red")
plot(w,type="l",xaxs="i",las=1,xlab="",ylab="",tck=.02)

#--------------------------------------------------------------------


datos = read_excel("C:/Users/NIGOJ/Desktop/Nico/Cosas de la U/Econometria R/Econometria/Bases de Datos/Coint6.xls")


par(mfcol = c(1,1), oma = c(0,0,1,0) + 0.2, mar = c(0,1,0,0) + 1, mgp = c(0, 0.2, 0))
plot(datos$y,type="s",xaxs="i",las=1,xlab="",ylab="",ylim=c(-12,2),tck=.02)
lines(datos$z,lty=2, col="red")
lines(datos$w,lty=3, col="blue")
abline(h=0)

par(mfcol = c(1,1), oma = c(0,0,1,0) + 0.2, mar = c(1,1,0,0) + 1, mgp = c(0, 0.2, 0))
plot(datos$y+datos$z-datos$w,type="l",xaxs="i",las=1,xlab="y+z-w",ylab="",ylim=c(-1,1),tck=.02)


adf.test(datos$y,k=0)
adf.test(datos$z,k=0)
adf.test(datos$w,k=0)

adf.test(diff(datos$y),k=0)
adf.test(diff(datos$z),k=0)
adf.test(diff(datos$w),k=0)




lm1 = summary(lm(datos$y~datos$z+datos$w))
lm1
lm2 = summary(lm(datos$z~datos$y+datos$w))
lm2
lm3 = summary(lm(datos$w~datos$y+datos$z))
lm3


reslm1 = lm1$residuals
reslm2 = lm2$residuals
reslm3 = lm3$residuals

par(mfcol = c(1,1), oma = c(0,0,1,0) + 0.2, mar = c(0,1,0,0) + 1, mgp = c(0, 0.2, 0))
plot(reslm1,type="l",xaxs="i",las=1,xlab="",ylab="",ylim=c(-1,1),tck=.02)
lines(reslm2,lty=1, col="red")
lines(reslm3,lty=1, col="blue")
abline(h=0)

adf.test(reslm1)
adf.test(reslm2)
adf.test(reslm3)



library("urca")
### TABLE 6.2

# Usage
# Augmented-Dickey-Fuller Unit Root Test
#
# ur.df(y, 
#       type = c("none", "drift", "trend"), lags = 1,
#       selectlags = c("Fixed", "AIC", "BIC")) 


adf1 = ur.df(datos$y,lag=0)
adf1@testreg
adf1 = ur.df(datos$y,lag=4)
adf1@testreg
adf2 = ur.df(datos$z,lag=0)
adf2@testreg
adf2 = ur.df(datos$z,lag=4)
adf2@testreg
adf3 = ur.df(datos$w,lag=0)
adf3@testreg
adf3 = ur.df(datos$w,lag=4)
adf3@testreg

### TABLE 6.3
adf1 = ur.df(lm1$residuals,lag=0)
adf1@testreg
adf1 = ur.df(lm1$residuals,lag=4)
adf1@testreg
adf2 = ur.df(lm2$residuals,lag=0)
adf2@testreg
adf2 = ur.df(lm2$residuals,lag=4)
adf2@testreg
adf3 = ur.df(lm3$residuals,lag=0)
adf3@testreg
adf3 = ur.df(lm3$residuals,lag=4)
adf3@testreg

DATO = matrix(nrow=nrow(datos)-1,ncol=ncol(datos))
DATO[,1] = diff(datos$y)
DATO[,2] = diff(datos$z)
DATO[,3] = diff(datos$w)
colnames(DATO) = c("dy","dz","dw")

e.w = lm3$residuals

VAR(DATO,p=1,exogen=e.w[-length(e.w)])

#  ¿cómo podemos determinar el orden del VAR ?


### PAGE 390
library("urca")

# ca.jo : Johansen Procedure for VAR
# ca.jo(x, 
#       type = c("eigen", "trace"), 
#       ecdet = c("none", "const", "trend"), 
#       K = 2,
#       spec=c("longrun", "transitory"), 
#       season = NULL, 
#       dumvar = NULL)
#

jo.ci = ca.jo(datos,type="trace",ecdet="none")
summary(jo.ci)
jo.ci@lambda

jo.ci = ca.jo(datos,type="trace",ecdet="const")
summary(jo.ci)
jo.ci@lambda


jo.ci = ca.jo(datos,type="eigen",ecdet="none")
summary(jo.ci)
jo.ci@lambda


jo.ci = ca.jo(datos,type="eigen",ecdet="const")
summary(jo.ci)
jo.ci@lambda


jo.ci = ca.jo(datos,type="trace",ecdet="none")
summary(jo.ci)
jo.ci@lambda

var.ci = vec2var(jo.ci,r=1)
var.ci
plot(irf(var.ci))


##                                          ###########

#  jo.ci@V    es la matriz de eigenvectors
#             normalizados con respecto a la primera variable
#
#   en este ejercicio r=1  hay una ecuación de cointegración
#   interesa la primera columna de V


ecm = jo.ci@V[,1]%*%t(datos)
plot(c(ecm),type="l",xaxs="i",las=1,xlab="",ylab="",ylim=c(-1,1),tck=.02)

ecm = ecm[1,]
plot(ecm,type="l",xaxs="i",las=1,xlab="",ylab="",ylim=c(-1,1),tck=.02)


# cajorls : OLS regression of VECM

# sistema represenatdo como VECM

vecm1 <- cajorls(jo.ci,r=1)
vecm1


#---------------------------------------------

datos = read.xls("QUARTERLY.xls")
datos = quarterly

datos$DATE=as.yearqtr(datos$DATE)

lm1 = summary(lm(datos$r10~datos$Tbill))
res.lm1 = lm1$residuals
adf1 = ur.df(res.lm1,type="drift",lag=1)
adf1@testreg

lm2 = summary(lm(datos$Tbill~datos$r10))
res.lm2 = lm2$residuals
adf2 = ur.df(res.lm2,type="drift",lag=1)
adf2@testreg

df = data.frame(datos$r10,datos$Tbill)
jo1 = ca.jo(df)
summary(jo1)


#  datos = read.xls("quarterly.xls")
lm1 = summary(lm(datos$IndProd~datos$M1NSA))
plot(lm1$residuals,type="l")
ur.df(lm1$residuals,type="drift",lag=1)


datos = read.xls("real.xls")
lm1 = summary(lm(log(datos$RGDP)~log(datos$RCons)))
lm1

### END
