### Data Generation Bin ###
library(BinNor)
dataGen1 <- function(N,cor){
  
  no.bin=2; no.nor=1
  mean.vec.nor=c(0); var.nor=c(1)
  prop.vec.bin=c(0.5,0.5)
  
  M = matrix(c(1, 0,0,
               0, 1,cor,
               0,cor,1
               
  ), nrow=3, ncol=3)
  sigma.star=compute.sigma.star( no.nor=1,no.bin=2, prop.vec.bin=prop.vec.bin,
                                 corr.mat=M)
  mydata=as.data.frame(jointly.generate.binary.normal(N,no.bin,no.nor,prop.vec.bin,
                                                      mean.vec.nor,var.nor, sigma_star=sigma.star$sigma_star))
  
  names(mydata)<-c("h1","x1","y1")
  
  
  return(mydata)
}
dataGen2 <- function(N,cor){
  
  no.bin=3; no.nor=1
  mean.vec.nor=c(0); var.nor=c(1)
  prop.vec.bin=c(0.5,0.5,0.5)
  
  M = matrix(c(1, 0,0,0,
               0, 1,0,cor,
               0,0,1,cor,
               0,cor,cor,1
               
  ), nrow=4, ncol=4)
  sigma.star=compute.sigma.star( no.nor=1,no.bin=3, prop.vec.bin=prop.vec.bin,
                                 corr.mat=M)
  mydata=as.data.frame(jointly.generate.binary.normal(N,no.bin,no.nor,prop.vec.bin,
                                                      mean.vec.nor,var.nor, sigma_star=sigma.star$sigma_star))
  
  names(mydata)<-c("h1","x1","x2","y1")
  
  
  return(mydata)
}
dataGen3 <- function(N,cor){
  no.bin=4; no.nor=1
  mean.vec.nor=c(0); var.nor=c(1)
  prop.vec.bin=c(0.5,0.5,0.5,0.5)
  
  M = matrix(c(1, 0,0,0,0,
               0, 1,0,0,cor,
               0,0,1,0,cor,
               0,0,0,1,cor,
               0,cor,cor,cor,1
               
  ), nrow=5, ncol=5)
  sigma.star=compute.sigma.star( no.nor=1,no.bin=4, prop.vec.bin=prop.vec.bin,
                                 corr.mat=M)
  mydata=as.data.frame(jointly.generate.binary.normal(N,no.bin,no.nor,prop.vec.bin,
                                                      mean.vec.nor,var.nor, sigma_star=sigma.star$sigma_star))
  
  names(mydata)<-c("h1","x1","x2","x3","y1")
  return(mydata)
}
dataGen4 <- function(N,cor){
  no.bin=5; no.nor=1
  mean.vec.nor=c(0); var.nor=c(1)
  prop.vec.bin=c(0.5,0.5,0.5,0.5,0.5)
  
  M = matrix(c(1, 0,0,0,0,0,
               0, 1,0,0,0,cor,
               0,0,1,0,0,cor,
               0,0,0,1,0,cor,
               0,0,0,0,1,cor,
               0,cor,cor,cor,cor,1
               
  ), nrow=6, ncol=6)
  sigma.star=compute.sigma.star( no.nor=1,no.bin=5, prop.vec.bin=prop.vec.bin,
                                 corr.mat=M)
  mydata=as.data.frame(jointly.generate.binary.normal(N,no.bin,no.nor,prop.vec.bin,
                                                      mean.vec.nor,var.nor, sigma_star=sigma.star$sigma_star))
  
  names(mydata)<-c("h1","x1","x2","x3","x4","y1")
  return(mydata)
}
