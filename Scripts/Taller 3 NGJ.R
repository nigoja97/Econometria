library(readxl)
wage <- read_excel("C:/Users/NIGOJ/Downloads/wage.xlsx")
View(wage)

n <- dim(wage)[1]
n
k <- dim(wage) [2]-1
k

x <- as.matrix(wage)
x[,1]=1
View(x)

y <- as.matrix(wage [1])
y
View(y)

B <- (solve(t(x)%*%x))%*%(t(x)%*%y)
B

SRC <-  (t(y)%*%y)-(t(B)%*%(t(x)%*%y))
SRC

s2 <- (SRC/(n-(k+1)))
s2


class(s2)

v <- as.numeric(s2)*solve(t(x)%*%x)
v

ee <- sqrt(diag(v))
ee

t_ep <- B/ee
t_ep

pt <- 2*pt(abs(t_ep), df = n-1, lower.tail = FALSE)
pt

s2_y <- var(y)
s2_y

STC <- (n-1)*s2_y
STC

r2 <- 1-(SRC/STC)
r2

gl1 <- k
gl1
gl2 <- n-(k+1)
gl2
ftest <- (r2/gl1)/((1-r2)/gl2)
ftest

pf <- pf(ftest, df1 = gl1, df2 = gl2, lower.tail = FALSE)
pf #Rechaza H0








