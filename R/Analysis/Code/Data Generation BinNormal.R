### Data Generation Bin H_1 is Normal ###
dataGen1 <- function(N,cor){
  
  M = matrix(c(1, cor,
               cor, 1
               
  ), nrow=2, ncol=2)
  L = chol(M)
  nvars = dim(L)[1]
  
  r = t(L) %*% matrix(rnorm(nvars*N), nrow=nvars, ncol=N)
  r = t(r)
  u <- pnorm(r)
  y1=r[,1]
  x2=qbern(u[,2],0.5)
  x1<-rnorm(N)
  data = as.data.frame(cbind(y1,x1,x2))
  
  
  return(data)
}
dataGen2 <- function(N){
  
  M = matrix(c(1, cor, cor,
               cor, 1, 0,
               cor, 0, 1
  ), nrow=3, ncol=3)
  L = chol(M)
  nvars = dim(L)[1]
  
  r = t(L) %*% matrix(rnorm(nvars*N), nrow=nvars, ncol=N)
  r = t(r)
  u <- pnorm(r)
  y1=r[,1]
  x2=qbern(u[,2],0.5)
  x3=qbern(u[,3],0.5)
  x1<-rnorm(N)
  data = as.data.frame(cbind(y1,x1,x2,x3))
  
  
  return(data)
}
dataGen3 <- function(N){
  
  M = matrix(c(1, cor, cor,cor,
               cor, 1, 0,0,
               cor, 0, 1,0,
               cor,0,0,1
  ), nrow=4, ncol=4)
  L = chol(M)
  nvars = dim(L)[1]
  
  r = t(L) %*% matrix(rnorm(nvars*N), nrow=nvars, ncol=N)
  r = t(r)
  u <- pnorm(r)
  y1=r[,1]
  x2=qbern(u[,2],0.5)
  x3=qbern(u[,3],0.5)
  x4=qbern(u[,4],0.5)
  x1<-rnorm(N)
  data = as.data.frame(cbind(y1,x1,x2,x3,x4))
  
  return(data)
}
dataGen4 <- function(N){
  
  M = matrix(c(1, cor, cor, cor,cor,
               cor, 1, 0, 0,0,
               cor, 0, 1, 0,0,
               cor, 0, 0, 1,0,
               cor, 0, 0, 0, 1), nrow=5, ncol=5)
  L = chol(M)
  nvars = dim(L)[1]
  r = t(L) %*% matrix(rnorm(nvars*N), nrow=nvars, ncol=N)
  r = t(r)
  u <- pnorm(r)
  y1=r[,1]
  x2=qbern(u[,2],0.5)
  x3=qbern(u[,3],0.5)
  x4=qbern(u[,4],0.5)
  x5=qbern(u[,5],0.5)
  x1<-rnorm(N)
  data = as.data.frame(cbind(y1,x1,x2,x3,x4,x5))
  
  return(data)
}
dataGen5 <- function(N){
  
  M = matrix(c(1, cor, cor, cor,cor,cor,
               cor, 1, 0, 0, 0, 0,
               cor, 0, 1, 0, 0, 0,
               cor, 0, 0, 1, 0, 0,
               cor, 0, 0, 0, 1, 0,
               cor, 0, 0, 0, 0, 1), nrow=6, ncol=6)
  L = chol(M)
  nvars = dim(L)[1]
  r = t(L) %*% matrix(rnorm(nvars*N), nrow=nvars, ncol=N)
  r = t(r)
  u <- pnorm(r)
  y1=r[,1]
  x2=qbern(u[,2],0.5)
  x3=qbern(u[,3],0.5)
  x4=qbern(u[,4],0.5)
  x5=qbern(u[,5],0.5)
  x6=qbern(u[,6],0.5)
  x1<-rnorm(N)
  data = as.data.frame(cbind(y1,x1,x2,x3,x4,x5,x6))
  
  return(data)
}
dataGen6 <- function(N){
  
  M = matrix(c(1, cor, cor, cor,cor,cor,cor,
               cor, 1, 0, 0, 0, 0,0,
               cor, 0, 1, 0, 0, 0,0,
               cor, 0, 0, 1, 0, 0,0,
               cor, 0, 0, 0, 1, 0,0,
               cor, 0, 0, 0, 0, 1,0,
               cor, 0, 0, 0, 0, 0,1), nrow=7, ncol=7)
  L = chol(M)
  nvars = dim(L)[1]
  r = t(L) %*% matrix(rnorm(nvars*N), nrow=nvars, ncol=N)
  r = t(r)
  u <- pnorm(r)
  y1=r[,1]
  x2=qbern(u[,2],0.5)
  x3=qbern(u[,3],0.5)
  x4=qbern(u[,4],0.5)
  x5=qbern(u[,5],0.5)
  x6=qbern(u[,6],0.5)
  x7=qbern(u[,7],0.5)
  x1<-rnorm(N)
  data = as.data.frame(cbind(y1,x1,x2,x3,x4,x5,x6,x7))
  return(data)
}
dataGen7 <- function(N){
  
  
  M = matrix(c(1, cor, cor, cor,cor,cor,cor,cor,
               cor, 1, 0, 0, 0, 0,0,0,
               cor, 0, 1, 0, 0, 0,0,0,
               cor, 0, 0, 1, 0, 0,0,0,
               cor, 0, 0, 0, 1, 0,0,0,
               cor, 0, 0, 0, 0, 1,0,0,
               cor, 0, 0, 0, 0, 0,1,0,
               cor, 0, 0, 0, 0, 0, 0,1
  ), nrow=8, ncol=8)
  L = chol(M)
  nvars = dim(L)[1]
  r = t(L) %*% matrix(rnorm(nvars*N), nrow=nvars, ncol=N)
  r = t(r)
  u <- pnorm(r)
  y1=r[,1]
  x2=qbern(u[,2],0.5)
  x3=qbern(u[,3],0.5)
  x4=qbern(u[,4],0.5)
  x5=qbern(u[,5],0.5)
  x6=qbern(u[,6],0.5)
  x7=qbern(u[,7],0.5)
  x8=qbern(u[,8],0.5)
  x1<-rnorm(N)
  data = as.data.frame(cbind(y1,x1,x2,x3,x4,x5,x6,x7,x8))
  return(data)
}
