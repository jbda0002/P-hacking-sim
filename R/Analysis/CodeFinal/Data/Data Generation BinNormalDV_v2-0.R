### Data Generation Bin H_1 is Normal ###

library(BinNor)

dataGen2 <- function(N,cor,corDV){
  
  no.bin=2; no.nor=4
  mean.vec.nor=c(0,0,0,0); var.nor=c(1,1,1,1)
  prop.vec.bin=c(0.5,0.5)
  
  M = matrix(c(1, 0,0,cor,cor,cor,
               0, 1,0,cor,cor,cor,
               0,0,1,0,0,0,
               cor,cor,0,1,corDV,corDV,
               cor,cor,0,corDV,1,corDV,
               cor,cor,0,corDV,corDV,1
               
  ), nrow=6, ncol=6)
  sigma.star=compute.sigma.star( no.nor=4,no.bin=2, prop.vec.bin=prop.vec.bin,
                                 corr.mat=M)
  mydata=as.data.frame(jointly.generate.binary.normal(N,no.bin,no.nor,prop.vec.bin,
                                                      mean.vec.nor,var.nor, sigma_star=sigma.star$sigma_star))
  
  names(mydata)<-c("x1","x2","h1","y1","y2","y3")
  
  
  return(mydata)
}
dataGen3 <- function(N,cor,corDV){
  no.bin=3; no.nor=4
  mean.vec.nor=c(0,0,0,0); var.nor=c(1,1,1,1)
  prop.vec.bin=c(0.5,0.5,0.5)
  
  M = matrix(c(1, 0,0,0,cor,cor,cor,
               0, 1,0,0,cor,cor,cor,
               0,0,1,0,cor,cor,cor,
               0,0,0,1,0,0,0,
               cor,cor,cor,0,1,corDV,corDV,
               cor,cor,cor,0,corDV,1,corDV,
               cor,cor,cor,0,corDV,corDV,1
               
  ), nrow=7, ncol=7)
  sigma.star=compute.sigma.star( no.nor=4,no.bin=3, prop.vec.bin=prop.vec.bin,
                                 corr.mat=M)
  mydata=as.data.frame(jointly.generate.binary.normal(N,no.bin,no.nor,prop.vec.bin,
                                                      mean.vec.nor,var.nor, sigma_star=sigma.star$sigma_star))
  
  names(mydata)<-c("x1","x2","x3","h1","y1","y2","y3")
  return(mydata)
}
dataGen4 <- function(N,cor,corDV){
  no.bin=4; no.nor=4
  mean.vec.nor=c(0,0,0,0); var.nor=c(1,1,1,1)
  prop.vec.bin=c(0.5,0.5,0.5,0.5)
  
  M = matrix(c(1, 0,0,0,0,cor,cor,cor,
               0, 1,0,0,0,cor,cor,cor,
               0,0,1,0,0,cor,cor,cor,
               0,0,0,1,0,cor,cor,cor,
               0,0,0,0,1,0,0,0,
               cor,cor,cor,cor,0,1,corDV,corDV,
               cor,cor,cor,cor,0,corDV,1,corDV,
               cor,cor,cor,cor,0,corDV,corDV,1
               
  ), nrow=8, ncol=8)
  sigma.star=compute.sigma.star( no.nor=4,no.bin=4, prop.vec.bin=prop.vec.bin,
                                 corr.mat=M)
  mydata=as.data.frame(jointly.generate.binary.normal(N,no.bin,no.nor,prop.vec.bin,
                                                      mean.vec.nor,var.nor, sigma_star=sigma.star$sigma_star))
  
  names(mydata)<-c("x1","x2","x3","x4","h1","y1","y2","y3")
  return(mydata)
}
