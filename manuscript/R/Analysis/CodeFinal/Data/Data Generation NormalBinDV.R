### Data Generation is Normal Multiple DV  Bin H_1 ###
dataGen1 <- function(N,cor,corDV){
  M = matrix(c(1, corDV,corDV, cor,
               corDV ,1,corDV, cor,
               corDV,corDV, 1,cor,
               cor,cor,cor,1
  ), nrow=4, ncol=4)
  L = chol(M)
  nvars = dim(L)[1]
  
  r = t(L) %*% matrix(rnorm(nvars*N), nrow=nvars, ncol=N)
  r = t(r)
  data = as.data.frame(r)
  h1<-rbinom(N,1,0.5)
  data = as.data.frame(cbind(r,h1))
  names(data) = c('y1',"y2","y3", 'x1',"h1")
  
  return(data)
}
dataGen2 <- function(N,cor,corDV){
  
  M = matrix(c(1, corDV,corDV, cor,cor,
               corDV ,1,corDV, cor,cor,
               corDV,corDV, 1,cor,cor,
               cor,cor,cor,1,0,
               cor,cor,cor,0,1
  ), nrow=5, ncol=5)
  L = chol(M)
  nvars = dim(L)[1]
  
  r = t(L) %*% matrix(rnorm(nvars*N), nrow=nvars, ncol=N)
  r = t(r)
  h1<-rbinom(N,1,0.5)
  data = as.data.frame(cbind(r,h1))
  names(data) = c('y1',"y2","y3", 'x1',"x2","h1")
  
  return(data)
}
dataGen3 <- function(N,cor,corDV){
  M = matrix(c(1, corDV,corDV, cor,cor,cor,
               corDV ,1,corDV, cor,cor,cor,
               corDV,corDV, 1,cor,cor,cor,
               cor,cor,cor,1,0,0,
               cor,cor,cor,0,1,0,
               cor,cor,cor,0,0,1
  ), nrow=6, ncol=6)
  L = chol(M)
  nvars = dim(L)[1]
  
  r = t(L) %*% matrix(rnorm(nvars*N), nrow=nvars, ncol=N)
  r = t(r)
  h1<-rbinom(N,1,0.5)
  data = as.data.frame(cbind(r,h1))
  names(data) = c('y1',"y2","y3", 'x1',"x2","x3","h1")
  return(data)
}
dataGen4 <- function(N,cor,corDV){
  
  M = matrix(c(1, corDV,corDV, cor,cor,cor,cor,
               corDV ,1,corDV, cor,cor,cor,cor,
               corDV,corDV, 1,cor,cor,cor,cor,
               cor,cor,cor,1,0,0,0,
               cor,cor,cor,0,1,0,0,
               cor,cor,cor,0,0,1,0,
               cor,cor,cor,0,0,0,1
  ), nrow=7, ncol=7)
  L = chol(M)
  nvars = dim(L)[1]
  
  r = t(L) %*% matrix(rnorm(nvars*N), nrow=nvars, ncol=N)
  r = t(r)
  h1<-rbinom(N,1,0.5)
  data = as.data.frame(cbind(r,h1))
  names(data) = c('y1',"y2","y3", 'x1',"x2","x3","x4","h1")
  return(data)
}
