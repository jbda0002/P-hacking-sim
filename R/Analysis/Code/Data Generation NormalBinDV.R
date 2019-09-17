### Data Generation is Normal Multiple DV  Bin H_1 ###
dataGen1 <- function(N,cor,corDV){
  M = matrix(c(1, corDV, cor, 
               corDV ,1, cor, 
               cor,  cor, 1
               
  ), nrow=3, ncol=3)
  L = chol(M)
  nvars = dim(L)[1]
  
  r = t(L) %*% matrix(rnorm(nvars*N), nrow=nvars, ncol=N)
  r = t(r)
  x1<-rbinom(N,1,0.5)
  data = as.data.frame(cbind(r,x1))
  names(data) = c('y1', "y2",'x2',"x1")
  
  return(data)
}
dataGen2 <- function(N,cor,corDV){
  
  M = matrix(c(1, corDV, cor, cor,
               corDV ,1, cor, cor,
               cor,  cor, 1,   0,
               cor, cor,  0, 1
  ), nrow=4, ncol=4)
  L = chol(M)
  nvars = dim(L)[1]
  
  r = t(L) %*% matrix(rnorm(nvars*N), nrow=nvars, ncol=N)
  r = t(r)
  x1<-rbinom(N,1,0.5)
  data = as.data.frame(cbind(r,x1))
  names(data) = c('y1', "y2", 'x2', 'x3',"x1")
  
  return(data)
}
dataGen3 <- function(N,cor,corDV){
  M = matrix(c(1, corDV, cor, cor,cor,
               corDV ,1, cor, cor,cor,
               cor,  cor, 1,   0,0,
               cor, cor,  0, 1,0,
               cor,cor,0,0,1
  ), nrow=5, ncol=5)
  L = chol(M)
  nvars = dim(L)[1]
  
  r = t(L) %*% matrix(rnorm(nvars*N), nrow=nvars, ncol=N)
  r = t(r)
  x1<-rbinom(N,1,0.5)
  data = as.data.frame(cbind(r,x1))
  names(data) = c('y1',"y2", 'x2', 'x3',"x4","x1")
  return(data)
}
dataGen4 <- function(N,cor,corDV){
  
  M = matrix(c(1, corDV, cor, cor,cor,cor,
               corDV ,1, cor, cor,cor,cor,
               cor,  cor, 1,   0,0,0,
               cor, cor,  0, 1,0,0,
               cor,cor,0,0,1,0,
               cor,cor,0,0,0,1
  ), nrow=6, ncol=6)
  L = chol(M)
  nvars = dim(L)[1]
  r = t(L) %*% matrix(rnorm(nvars*N), nrow=nvars, ncol=N)
  r = t(r)
  x1<-rbinom(N,1,0.5)
  data = as.data.frame(cbind(r,x1))
  names(data) = c('y1',"y2", 'x2', 'x3',"x4","x5","x1")
  
  return(data)
}
dataGen5 <- function(N,cor,corDV){
  
  M = matrix(c(1, corDV, cor, cor,cor,cor,cor,
               corDV ,1, cor, cor,cor,cor,cor,
               cor,  cor, 1,   0,0,0,0,
               cor, cor,  0, 1,0,0,0,
               cor,cor,0,0,1,0,0,
               cor,cor,0,0,0,1,0,
               cor,cor,0,0,0,0,1
  ), nrow=7, ncol=7)
  L = chol(M)
  nvars = dim(L)[1]
  r = t(L) %*% matrix(rnorm(nvars*N), nrow=nvars, ncol=N)
  r = t(r)
  x1<-rbinom(N,1,0.5)
  data = as.data.frame(cbind(r,x1))
  names(data) = c('y1',"y2", 'x2', 'x3',"x4","x5","x6","x1")
  
  return(data)
}
dataGen6 <- function(N,cor,corDV){
  
  M = matrix(c(1, corDV, cor, cor,cor,cor,cor,cor,
               corDV ,1, cor, cor,cor,cor,cor,cor,
               cor,  cor, 1,   0,0,0,0,0,
               cor, cor,  0, 1,0,0,0,0,
               cor,cor,0,0,1,0,0,0,
               cor,cor,0,0,0,1,0,0,
               cor,cor,0,0,0,0,1,0,
               cor,cor,0,0,0,0,0,1
  ), nrow=8, ncol=8)
  L = chol(M)
  nvars = dim(L)[1]
  r = t(L) %*% matrix(rnorm(nvars*N), nrow=nvars, ncol=N)
  r = t(r)
  x1<-rbinom(N,1,0.5)
  data = as.data.frame(cbind(r,x1))
  names(data) = c('y1',"y2", 'x2', 'x3',"x4","x5","x6","x7","x1")
  return(data)
}
dataGen7 <- function(N,cor,corDV){
  
  
  M = matrix(c(1, corDV, cor, cor,cor,cor,cor,cor,cor,
               corDV ,1, cor, cor,cor,cor,cor,cor,cor,
               cor,  cor, 1,   0,0,0,0,0,0,
               cor, cor,  0, 1,0,0,0,0,0,
               cor,cor,0,0,1,0,0,0,0,
               cor,cor,0,0,0,1,0,0,0,
               cor,cor,0,0,0,0,1,0,0,
               cor,cor,0,0,0,0,0,1,0,
               cor,cor,0,0,0,0,0,0,1
  ), nrow=9, ncol=9)
  L = chol(M)
  nvars = dim(L)[1]
  r = t(L) %*% matrix(rnorm(nvars*N), nrow=nvars, ncol=N)
  r = t(r)
  x1<-rbinom(N,1,0.5)
  data = as.data.frame(cbind(r,x1))
  names(data) = c('y1',"y2", 'x2', 'x3',"x4","x5","x6","x7","x8","x1")
  return(data)
}

DataGenListNormBinDV <- list(dataGen1,dataGen2,dataGen3,dataGen4,dataGen5,dataGen6,dataGen7)

