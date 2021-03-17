#**TALLER 1 SOFÍA GALEANO FORERO & NICOLÁS GONZÁLEZ JARAMILLO**


#**PRIMER PUNTO**
#La función devolverá la suma de dos periodos de tiempo
#Debe especificar los periodos de tiempo en minutos y segundos como variable independiente

tiempo <- function(t_1, t_2){
  if ((t_1[2]+t_2[2])>=60){
    t=t_1+t_2+c(1,-60)
  } 
  
  if ((t_1[2]+t_2[2])<60){
    t=t_1+t_2
  }
  
  return(t)
  
}


t_1=c(30,10)
t_2=c(15,25)

x <- tiempo(t_1, t_2)
x

#**SEGUNDO PUNTO**
#La función devolverá una matriz B igual a la matriz A, excepto por la columna j que resulta de multiplicar la columna j de A por el valor k
#Debe especificar 3 argumentos, la matriz A, el numero entero j, y el real k

colk <- function (A, j, k){
  B = A
  B[,j]=k*B[,j]
  return(B)
}

A <- matrix(1:4, nrow = 2, ncol = 2,  byrow = FALSE)
j <- 2
k <- 10

C <- colk(A,j, k)
C

#**TERCER PUNTO**
#La función devolverá una nueva matriz B igual a la matriz A, excepto por la columna j que resulta de sumar la columna j de A con k veces la columna 1 de A
#Debe especificar 3 argumentos, la matriz A, el numero entero j, y el real k

sumk <- function (A, j, k){
  B = A
  B[,j]=(B[,j]+k*B[,1])
  return(B)
}

A <- matrix(1:4, nrow = 2, ncol = 2,  byrow = FALSE)
j <- 2
k <- 10

D <- sumk(A, j, k)
D

#**CUARTO PUNTO**
#La función devuelve el elemento en la posición [ i , j ] del producto matricial entre A yB.
#Debe especificar 4 argumentos, la matriz A, la matriz B, y la poscion [i, j] que desea conocer

multij <- function(A, B, i, j){
  m <- matrix(NA, dim(A)[1], dim(B)[2])
  for(x in seq_along(B[1, ])){
    for(y in seq_along(A[, 1])){ 
      m [y, x] <- sum(A[y, ] * B[, x])
    }
  }
  t <- m[i, j]
  return(t)
}

E <-  matrix(1:9, nrow = 3, ncol = 3, byrow = TRUE)
F <-  matrix(10:18, nrow = 3, ncol = 3, byrow = TRUE)
E
F

G <- multij(A, B, 1, 2)
G

#**QUINTO PUNTO**
# la Función devuelve en una matriz H el resultado de multiplicar A*B
#Debe especificar las 2 matrices que desea multiplicar

multiplica <- function(A, B){
  m <- matrix(NA, dim(A)[1], dim(B)[2])
  for(x in seq_along(B[1, ])){
    for(y in seq_along(A[, 1])){
      m [y, x] <- sum(A[y, ] * B[, x])
    }
  }
  return(m)
}

E <-  matrix(1:9, nrow = 3, ncol = 3, byrow = TRUE)
F <-  matrix(10:18, nrow = 3, ncol = 3, byrow = TRUE)
E
F

H <- multiplica(A, B)
H

#**SEXTO PUNTO**
#La función devuelve una lista de 3 números (fila, columna, mayor) con la posición (fila y columna) y el valor más grande en la matriz 
#Debe especificar la matriz en cuestión

sofia <- function(A) {
posi <- which(A == max(A), arr.ind = TRUE) 
max <- max(A)
return(c(posi, max))
}

E = matrix(1:9, nrow = 3, ncol = 3, byrow = TRUE)
H = sofia(E)
H




