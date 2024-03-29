#**TALLER 2 NICOLÁS GONZÁLEZ JARAMILLO**

#**PUNTO 1**
#A partir de dos funciones A y B disesñamos una funcón que construye una matriz C que tiene a la matriz A en la esquina superior izquierda, a la matriz B en la esquna inferior derecha y el resto de posiciones son 0
#PASO 1: Crear una función con cualquier nombre que dependa de dos argumentos
#PASO 2: Definir n como la suma de las filas de ambas matrices y m como la suma de las columnas
#PASO 3: crear una matriz C de tamaño nxm rellena de 0
#PASO 4: Utilizar dos funciones "for" para delimitar las posiciones de C que serán ocupadas por A. De esta manera, se define que i toma valores desde 1 hasta la cantidad de filas de A y j desde 1 hasta la cantidad de columnas de A. Se remplaza C[i,j] como A[i,j]
#PASO 5: Así como en el PASO 3, se debe definir el espacio que ocupará B dentro de C, por lo que ahora i tomará valores desde la posicion 1+dim(A)[1] hasta n y j tomará valores desde 1+dim(A)[2] hasta m para evitar que se sobrepongan las matrices. Se reemplaza C[i,j] como B[(i-dim(A)[1]),(j-dim(A)[2])]  para que tome los valores de B dentro de las dimensiones de C que son diferentes.
#PASO 6: Finalmente solo pedimos que la funcion devuelva C
#PASO 7: Para probar la función incluya como argumentos llas dos matrices que desea ubicar dentro de la matriz C

matrix1 <- function(A,B) {
  n <- nrow(A)+nrow(B)
  m <- ncol(A)+ncol(B)
  
  C <- matrix(0, nrow = n, ncol = m)
       for (i in 1:dim(A)[1]) {
         for (j in 1:dim(A)[2]){
           C[i,j]=A[i,j]  
         }
       }
        
  
       for (i in (dim(A)[1]+1):(dim(A)[1]+dim(B)[1])) {
         for(j in (dim(A)[2]+1):(dim(A)[2]+dim(B)[2])) {
           C[i,j]=B[(i-dim(A)[1]),(j-dim(A)[2])] 
         }
       }
  return(C)
}
  


A <- matrix(1:9, nrow = 3, ncol = 3, byrow = TRUE)
B <- matrix(10:18, nrow = 3, ncol = 3, byrow = TRUE)



C <- matrix1(A,B)
C

#**PUNTO 2**
#Diseñar una funcion que a partir de un vector que contiene los valores observados de una serie temporal, cree una matriz de 4 columnas que corresponden a las observaciones originales y los 3 primeros rezagos de la variable
#PASO 1: Crear una funcion que dependa de un único argumento
#PASO 2: Definir n como el tamaño del vector en cuestión
#PASO 3: Crear una matriz m rellena de 0 de tamaño (n-3)x4, porque las primeras 3 observaciones no tienen 3 rezagos
#PASO 4: Reemplazar por columnas los valores del vector desplazandolo una unidad hacia el inicio de esta para poder observar el rezago inmediatamente anterior
#PASO 5: La funcion debe devolver el valor de m
#PASO 6: Para proobarlo debe escribir la funcioón cuyo argumento será cualquier vector de cualquier tamaño que tenga previamente definido

rez <- function(V){
  
  n <- length(V)
  
  m <- matrix(NA, nrow = n-3, ncol = 4)
   
  m[,1]=V[4:n]
  m[,2]=V[3:(n-1)]
  m[,3]=V[2:(n-2)]
  m[,4]=V[1:(n-3)] 
 return(m) 
}

V <- c(1:8)
H <- rez(V)
H


#**PUNTO 3**
#Diseñar una función que dadas las matrices A y B construya la matriz K definida como el producto Kronecker de A y B
#PASO 1: Crear una función con el nombre que quiera que dependa de dos argumentos
#PASO 2: Definir una matriz K de tamaño nxm donde n es la multiplicación de las filas de A y las filas de B y m es la multiplicacion de las columnas de A y las columnas de B
#PASO 3: Utilizar la función "for" para definir que los valores que va a tomar i serán iguales a la secuencia de numeros de la cantidad de filas que tiene la matriz A
#PASO 4: Crear la variable cur_rows que será igual a la secuencia de numeros igua al tamaño de las filas de la matriz B sumada a los valores que tome i menos 1 multiplicado las filas de B
#PASO 5: Utilizar una función "for" para determinar que los valores que tomará j será la secuencia de numeros igual al tamaño de las columnas de A
#PASO 6: Reemplazar en K los valores de la multiplicación de A[i,j]\*B
#PASO 7: Para probar la función remplace como argumentos las dos matrices de las cuales quiere conocer el producto Kronecker.

#Si desea verificar el resultado puede compararlo con la funcion predeterminada de base r llamada kronecker
  
kron <-  function(A,B){
  
  m <- matrix(0, nrow = nrow(A)*nrow(B), ncol = ncol(A)*ncol(B))
  
  for (i in seq_len(nrow(A))){
    cur_rows <- seq_len(nrow(B)) + (i-1)*nrow(B)
    for (j in seq_len(ncol(A)))
      m[cur_rows, seq_len(ncol(B)) + (j-1)*ncol(B)] <- A[i,j]*B
  }
  
  return(m)
}


A <-  matrix(c(1, 2, 3, 1), nrow = 2, ncol =  2, byrow = TRUE)
B <-  matrix(c(0, 3, 2, 1), nrow = 2, ncol =  2, byrow = TRUE)
A
B

C <- kron(A,B)
C  


