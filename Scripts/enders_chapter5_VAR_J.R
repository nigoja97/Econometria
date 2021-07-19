#################################################
#################################################
###### 5. MULTIEQUATION TIME-SERIES MODELS ######
#################################################
#################################################
##  pacman::p_load(readxl,gdata,stargazer,MTS,rugarch,forecast,vars,urca,aTSA)
##
library(readxl)
library(gdata)
library(stargazer)
library(MTS)
library(rugarch)
library(forecast)
library(vars)
library(urca)
library(aTSA)

### PAGE 310
datos = read_excel("C:/Users/NIGOJ/Desktop/Nico/Cosas de la U/Econometria R/Econometria/Bases de datos/ch5_Terrorism.xls")
datos = Terrorism
datos$ENTRY = seq(1970,by=0.25,length.out=nrow(datos))

par(mfcol = c(2,1), oma = c(0,0,1,0) + 0.2, mar = c(0,1,0,0) + 1, mgp = c(0, 0.2, 0))
plot(datos$ENTRY,datos$Domestic,type="l",las=1,xaxs="i",yaxs="i",xlab="",ylab="",tck=.02,main=colnames(datos)[2],col="steelblue4",ylim=c(0,400))
plot(datos$ENTRY,datos$Transnational,type="l",las=1,xaxs="i",yaxs="i",xlab="",ylab="",tck=.02,main=colnames(datos)[3],col="steelblue4",ylim=c(0,80))

adf.dom = adf.test(datos$Domestic[-c(1:which(datos$ENTRY==1979.25))],nlag=2)
adf.dom

adf.dom = adf.test(datos$Domestic[which(datos$ENTRY==1979.50):nrow(datos)],nlag=2)
adf.dom

# En las opciones con deriva se rechaza H0 -> estacionarias
# sugiere incluir constantes en el modelo
#  VARselect(y, lag.max = 10, type = c("const", "trend", "both", "none"),
#          season = NULL, exogen = NULL)

VARselect(datos[-c(1:which(datos$ENTRY=="1979.25")),-1],type="const") 
# AIC suggests 3 lags

var.terror = VAR(datos[-c(1:which(datos$ENTRY=="1979.25")),-1],p=3)
summary(var.terror)


# fevd: forecast error variance decomposition of a VAR(p) for n.ahead steps
fevd.terror = fevd(var.terror,n.ahead=12)
fevd.terror

par(mfcol = c(1,1), mar = c(0,0,1,0) )
plot(fevd.terror)


# IRF from Domestic
# IRF from Transnational

windows(16,12)
par(mfcol = c(2,2), oma = c(0,0,1,0) + 0.2, mar = c(0,1,0,0) + 1, mgp = c(0, 0.2, 0))
plot(irf(var.terror),col=1,las=1,xaxs="i",xlab="",ylab="")






#########################################

####                                                SVAR

k=ncol(datos[,-1])
amat = matrix(NA,ncol=k,nrow=k)
amat
# the coefficient b[2,1] is set equal to zero
amat[2,1]=0
amat

svar.terror = SVAR(var.terror, estmethod="direct", Amat = amat) 
svar.terror
summary(svar.terror)


windows(16,12)
par(mfcol = c(2,2), oma = c(0,0,1,0) + 0.2, mar = c(0,1,0,0) + 1, mgp = c(0, 0.2, 0))

plot(irf(svar.terror),col=1,las=1,xaxs="i",xlab="",ylab="")
plot(irf(svar.terror),col=2,las=1,xaxs="i",xlab="",ylab="")


### PAGE 325
datos = read.xls("/Users/user/Google Drive/Website/Book/Enders/Enders_Holt.xls")
datos$ENTRY = seq(1974.25,by=0.25,length.out=nrow(datos))
k = ncol(datos[,-1])

par(mfcol = c(2,2), oma = c(0,0,1,0) + 0.2, mar = c(0,1,0,0) + 1, mgp = c(0, 0.2, 0))
for (i in 1:k) {
   plot(datos$ENTRY,datos[,i+1],type="l",las=1,xaxs="i",xlab="",ylab="",tck=.02,col="steelblue4",yaxs="i",main=colnames(datos)[i+1])
   abline(h=0,lty=2)
}
var.enders = VAR(na.omit(datos[,-1]),p=11)
summary(var.enders)

amat = diag(k)
diag(amat) = NA
amat[2,1] = NA
amat[4, ] = NA
amat
svar.enders = SVAR(var.enders, estmethod="direct", Amat = amat) 
svar.enders
plot(irf(svar.enders,boot=FALSE),col=1,las=1,xaxs="i",xlab="",ylab="")


#########################################

####                                                SVAR

### PAGE 331
datos = read.xls("/Users/user/Google Drive/Website/Book/Enders/Exrates.xls")
head(datos)

datos$loge_ca = log(datos$e_uk)
datos$logr_ca = datos$loge_ca-log(datos$p_uk)+log(datos$p_us)
dloge_ca = c(diff(datos$loge_ca))
dlogr_ca = c(diff(datos$logr_ca))

datos$date = as.yearqtr(datos$DESCRIPTOR)

X1 = embed(dloge_ca,2)
X2 = embed(datos$loge_ca,2)
summary(lm(X1[,1]~X2[-1,2]+X1[,-1]))

df1 = ur.df(dloge_ca,type="drift")
df1@testreg

df = data.frame(datos$logr_ca,datos$loge_ca)
VAR.PPP = VAR(df,p=3)
summary(VAR.PPP)
fevd(VAR.PPP)


windows(16,12)
par(mfcol = c(2,2), oma = c(0,0,1,0) + 0.2, mar = c(0,1,0,0) + 1, mgp = c(0, 0.2, 0))


plot(irf(VAR.PPP,boot=FALSE),col=1,las=1,xaxs="i",xlab="",ylab="")


