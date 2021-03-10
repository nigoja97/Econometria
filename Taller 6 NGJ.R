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
#

vt <- ts(start=1,end=200,frequency = 1)
for (j in 1:200) vt[j]=10+5*t[j]-0.05*((t[j])^2)+et[j]
plot(vt)
facsvt <- acf(vt)
facpvt <- pacf(vt)
#

wt <- ts(start=1,end=200,frequency = 1)
wt[1]=et[1]
for (j in 2:200) wt[j]=1.05*wt[j-1]+et[j]
plot(wt)
facswt <- acf(wt)
facpwt <- pacf(wt)
#

xt <- ts(start=1,end=200,frequency = 1)
xt[1]=et[1]
for (j in 2:200) xt[j]=xt[j-1]+et[j]
plot(xt)
facsxt <- acf(xt)
facpxt <- pacf(xt)
#

yt <- ts(start=1,end=200,frequency = 1)
yt[1]=et[1]
yt[2]=2*yt[1]+et[2]
for (j in 3:200) yt[j]=2*yt[j-1]-yt[j-2]+et[j]
plot(yt)
facsyt <- acf(yt)
facpyt <- pacf(yt)
#

at <- ts(start=1,end=200,frequency = 1)
at[1]=et[1]
for (j in 2:200) at[j]=0.7*at[j-1]+et[j]
plot(at)
facsat <- acf(at)
facpat <- pacf(at)
#

bt <- ts(start=1,end=200,frequency = 1)
bt[1]=et[1]
bt[2]=0.5*bt[1]+et[2]
for (j in 3:200) bt[j]=0.5*bt[j-1]-0.3*bt[j-2]+et[j]
plot(bt)
facsbt <- acf(bt)
facpbt <- pacf(bt)
#

ct <- ts(start=1,end=200,frequency = 1)
ct[1]=et[1]
for (j in 2:200) ct[j]=et[j]+0.7*et[j-1]
plot(ct)
facsct <- acf(ct)
facpct <- pacf(ct)
#

dt <- ts(start=1,end=200,frequency = 1)
dt[1]=et[1]
dt[2]=et[2]+0.7*et[1]
for (j in 3:200) dt[j]=et[j]+0.7*et[j-1]-0.3*et[j-2]
plot(dt)
facsdt <- acf(dt)
facpdt <- pacf(dt)
#

ft <- ts(start=1,end=200,frequency = 1)
ft[1]=et[1]
for (j in 2:200) ft[j]=0.7*ft[j-1]+et[j]+0.5*et[j-1]
plot(ft)
facsft <- acf(ft)
facpft <- pacf(ft)
#




