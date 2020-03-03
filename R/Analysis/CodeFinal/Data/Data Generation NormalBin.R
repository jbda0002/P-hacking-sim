### Data Generation Normal H_1 Bin ###

dataGen1 <- function(N,cor){
  
  M = matrix(c(1, cor,0,
               cor, 1,0,
               0,0,1
               
  ), nrow=3, ncol=3)
  L = chol(M)
  nvars = dim(L)[1]
  
  r = t(L) %*% matrix(rnorm(nvars*N), nrow=nvars, ncol=N)
  r = t(r)
  u <- pnorm(r)
  h1=qbern(u[,3],0.5)
  data = as.data.frame(cbind(r[,1:2],h1))
  names(data) = c('y1', 'x1','h1')
  
  return(data)
}
dataGen2 <- function(N,cor){
  
  M = matrix(c(1, cor, cor,0,
               cor, 1, 0,0,
               cor, 0, 1,0,
               0,0,0,1
  ), nrow=4, ncol=4)
  L = chol(M)
  nvars = dim(L)[1]
  
  r = t(L) %*% matrix(rnorm(nvars*N), nrow=nvars, ncol=N)
  r = t(r)
  u <- pnorm(r)
  h1=qbern(u[,4],0.5)
  data = as.data.frame(cbind(r[,1:3],h1))
  names(data) = c('y1', 'x1', 'x2','h1')
  
  return(data)
}
dataGen3 <- function(N,cor){
  
  M = matrix(c(1, cor, cor,cor,0,
               cor, 1, 0,0,0,
               cor, 0, 1,0,0,
               cor,0,0,1,0,
               0,0,0,0,1
  ), nrow=5, ncol=5)
  L = chol(M)
  nvars = dim(L)[1]
  
  r = t(L) %*% matrix(rnorm(nvars*N), nrow=nvars, ncol=N)
  r = t(r)
  u <- pnorm(r)
  h1=qbern(u[,5],0.5)
  data = as.data.frame(cbind(r[,1:4],h1))
  names(data) = c('y1', 'x1', 'x2','x3','h1')
  return(data)
}
dataGen4 <- function(N,cor){
  
  M = matrix(c(1, cor, cor, cor,cor,0,
               cor, 1, 0, 0,0,0,
               cor, 0, 1, 0,0,0,
               cor, 0, 0, 1,0,0,
               cor, 0, 0, 0, 1,0,
               0,0,0,0,0,1
  ), nrow=6, ncol=6)
  L = chol(M)
  nvars = dim(L)[1]
  r = t(L) %*% matrix(rnorm(nvars*N), nrow=nvars, ncol=N)
  r = t(r)
  u <- pnorm(r)
  h1=qbern(u[,6],0.5)
  data = as.data.frame(cbind(r[,1:5],h1))
  names(data) = c('y1', 'x1', 'x2','x3','x4','h1')
  
  return(data)
}
dataGen5 <- function(N,cor){
  
  M = matrix(c(1, cor, cor, cor,cor,cor,0,
               cor, 1, 0, 0, 0, 0,0,
               cor, 0, 1, 0, 0, 0,0,
               cor, 0, 0, 1, 0, 0,0,
               cor, 0, 0, 0, 1, 0,0,
               cor, 0, 0, 0, 0, 1,0,
               0,0,0,0,0,0,1
  ), nrow=7, ncol=7)
  L = chol(M)
  nvars = dim(L)[1]
  r = t(L) %*% matrix(rnorm(nvars*N), nrow=nvars, ncol=N)
  r = t(r)
  u <- pnorm(r)
  h1=qbern(u[,7],0.5)
  data = as.data.frame(cbind(r[,1:6],h1))
  names(data) = c('y1', 'x1', 'x2','x3','x4','x5','h1')
  
  return(data)
}
dataGen6 <- function(N,cor){
  
  M = matrix(c(1, cor, cor, cor,cor,cor,cor,0,
               cor, 1, 0, 0, 0, 0,0,0,
               cor, 0, 1, 0, 0, 0,0,0,
               cor, 0, 0, 1, 0, 0,0,0,
               cor, 0, 0, 0, 1, 0,0,0,
               cor, 0, 0, 0, 0, 1,0,0,
               cor, 0, 0, 0, 0, 0,1,0,
               0,0,0,0,0,0,0,1
  ), nrow=8, ncol=8)
  L = chol(M)
  nvars = dim(L)[1]
  r = t(L) %*% matrix(rnorm(nvars*N), nrow=nvars, ncol=N)
  r = t(r)
  u <- pnorm(r)
  h1=qbern(u[,8],0.5)
  data = as.data.frame(cbind(r[,1:7],h1))
  names(data) = c('y1', 'x1', 'x2','x3','x4','x5','x6','h1')
  return(data)
}
dataGen7 <- function(N,cor){
  
  
  M = matrix(c(1, cor, cor, cor,cor,cor,cor,cor,0,
               cor, 1, 0, 0, 0, 0,0,0,0,
               cor, 0, 1, 0, 0, 0,0,0,0,
               cor, 0, 0, 1, 0, 0,0,0,0,
               cor, 0, 0, 0, 1, 0,0,0,0,
               cor, 0, 0, 0, 0, 1,0,0,0,
               cor, 0, 0, 0, 0, 0,1,0,0,
               cor, 0, 0, 0, 0, 0, 0,1,0,
               0,0,0,0,0,0,0,0,1
  ), nrow=9, ncol=9)
  L = chol(M)
  nvars = dim(L)[1]
  r = t(L) %*% matrix(rnorm(nvars*N), nrow=nvars, ncol=N)
  r = t(r)
  u <- pnorm(r)
  h1=qbern(u[,9],0.5)
  data = as.data.frame(cbind(r[,1:8],h1))
  names(data) = c('y1', 'x1', 'x2','x3','x4','x5','x6','x7','h1')
  return(data)
}

