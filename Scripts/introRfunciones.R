# Instruciones para controlar el flujo del programa

set.seed(20210127)
Z = matrix(rnorm(16), nrow=4, byrow=FALSE)
Z

x <- runif(100, 0, 10)
y <- 2*x+3+rnorm(100)
cor(x,y)

#Suma y Multiplicacion de Matrices

I3 <- diag(3) #Crea matriz identidad 3x3
I3
A <- matrix(1:9, nrow = 3, byrow = TRUE)
A

I3+A
I3*A #Multiplicación término a término 
I3%*%A #Producto matricial que hacemos en algebra lineal

B = A
B[,2]=5*B[,2]
B


#Si quiero tener la posicion de los datos de una matriz A[ , ]
#A[1,2] me muestra la posicion a12
#A[1,] me muestra toda la fila 1
#A[,2] me muestra toda la columna 2

colSums() #Suma de la culumna
colMeans() #Media de la columna
rowSums() #Suma de la fila
rowMeans() #Media de la fila

for (i in 1:5) {
  print(i)
  #
  #
}
#Todas las lineas que estan entre las llaves se van a ejecutar cada vez que i tome un valor dentro del intervalo

i <- 4
while (i>=1) {
  print(i)
  i = i-1
}
#Mientras esta condicion sea cierta

i <- k+1
repeat {
  print(i)
  i = i-1
  if (i<(1)){
    break
  }
}

i <- n
repeat {
  print(i)
  i = i-1
  if (i<(n-k)){
    break
  }
}
#Repite una operacion hasta que la condicion se incumpla

#
#Crear una funcion
#Vamos a hacer una funcion que sume dos cosas

suma <- function(a,b){
  s=a+b
  return(s)
}

c <- suma(4,18)
c

m <- 40
p <- 25

suma(m,p)

#
#  Construye matriz cuadrada nxn con 1 en diagonal 
#  0.2 en el resto
#
cova1 <- function(ro,n) {
  m <- matrix(nrow=n,ncol=n)
  for (i in 1:n) {
    for (j in 1:n) {
      if (i==j) m[i,j]=1 else m[i,j]=ro
      # m[i,j] = ifelse(i==j,1,0.2)
    }
  }
  return(m)
}
B = cova1(0.2,3)
B


cova2 <- function(ro,n) {
  m <- matrix(nrow=n,ncol=n)
  m <- ifelse(row(m)==col(m),1,ro)
  return(m)
}
A <- cova2(0.2,3)
A

#
# posición del mayor en un vector
posvec <- function(v) {
  n=length(v)
  mayor <- v[1] #Arrancamos suponiendo que el mayor es el valor en la posicion 1
  posi <- 1 #Y que la posicion es la 1
  for (i in 2:n) { #Aca empezamos a comparar desde la posicion 2
    if (v[i] > mayor) { #Si el valor de la posicion i es mayor al que yo suponia que era el mayor, hay un nuevo valor mayor
      mayor <- v[i] #Me dice cual es el valor mayor
      posi <- i #Me dice en que posicion del vector está el valor mayor 
    }
  }
  return(c(posi,mayor)) #Me devuelve la posicion del mayor dentro del vector y el valor del mayor
}
w = c(1,5,7,8,2)
pw = posvec(w)
pw

#
#  suma de 2 números menores que 10
suma2 <- function(a,b) {
  if (a>9 & b>9) {
    return("No se puede, a>9  y  b>9")  
    break
  }
  
  if (a>9) {
    return("No se puede, a>9")
    break } 
  
  if (b>9) {
    return("No se puede b>9")
    break }
  
  s=a+b
  return(s)
}

suma2(3,4)
suma2(3,14)
suma2(13,4)
suma2(13,14)

