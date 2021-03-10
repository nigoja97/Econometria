#**TAller 6 Nicolas Gonzalez J & Sofia Galeano F***
#Punto 1

#Ruido Blanco 
e_t = rnorm(200, mean = 0, sd = 1)
t <- (1:200)

#Series
r_t <- ts(start=1,end=200,frequency = 1)
