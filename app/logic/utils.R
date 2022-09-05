library('matlib')
library(meshgrid)
library(purrr)



exponent_matrix <- function(N, L){
  n <- -N:N
  tl <- seq(0, 1, length=L)
  V <- outer(tl, n, function(a,b) exp(2*pi*1i*a*b))
  V
}


transform_matrix <- function(V){
  V%*%solve(t(V)%*%V)
}

coefficients <- function(z, T){
  t(z)%*%T
}
'
points_to_complex(x,y){
  x+1i*y
}

approximate <- function(a, V){
  V%*%a
}

N <- 10
x <- 1
y <- 1

# Calc
L <- length(x)
z <- points_to_complex(x,y)
V <- exponent_matrix(N, L)
T <- transform_matrix(V)
a <- coefficients(z, T)
zhat <- approximate(a, V)

err <- Mod(zhat -z)
