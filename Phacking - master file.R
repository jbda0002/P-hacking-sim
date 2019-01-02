
## Code for p-hacking and simulation
## This code has both a function that only looks at the fixed effects, but also a function that takes all the interavtions with H_1 
## into account. These interavtions are only with one other variable, but can be generalized such that it takes all interactions
## but the modelset will explode exponentially

#### Setting up the simulation ####
## Setting working directory
#setwd("D:/Dropbox")
setwd("C:/Users/jbda0002/Documents/Projects/P-hacking/trunk")
set.seed(1234)

## Loading library
library(ggplot2)
library(data.table)
library(plyr)
library(reshape2)
library(Rlab)

#This one is with a single interaction term, always with H_1
phackingInteraction<-function(data,y,H_1,interaction = TRUE,SD=FALSE){
  ## Loading liberaries
  library(data.table)
  library(ggplot2)
  ## Creating datasets with outliers deleted if sd=TRUE 
  # This here is only to test, the different oulier analysis will be added here 
  # This part of the code can also be optimized
  # Use the MAD (Median Absolute Deviation) method as well
  if(SD==TRUE){
    #Calculate means and sd for all variables
    all.means <- sapply(data,mean)
    all.sd<-sapply(data, sd)
    
    #sd*2
    #Miller, 1991
    outlier = ifelse(data > all.means+all.sd*2, 1, 0)
    outlier <-ifelse(rowSums(outlier)>=1,1,0)
    dataoutlier2<-data
    dataoutlier2$outlier<-outlier
    dataoutlier2<-dataoutlier2[!dataoutlier2$outlier==1,]
    
    #sd*2.5
    #Miller, 1991
    outlier = ifelse(data > all.means+all.sd*2.5, 1, 0)
    outlier <-ifelse(rowSums(outlier)>=1,1,0)
    dataoutlier25<-data
    dataoutlier25$outlier<-outlier
    dataoutlier25<-dataoutlier25[!dataoutlier25$outlier==1,]
    
    #sd*3
    #Howell, 1998 - Statistical methods in human sciences
    outlier = ifelse(data > all.means+all.sd*3, 1, 0)
    outlier <-ifelse(rowSums(outlier)>=1,1,0)
    dataoutlier3<-data
    dataoutlier3$outlier<-outlier
    dataoutlier3<-dataoutlier3[!dataoutlier3$outlier==1,]
    
    dataoutlier<-list(dataoutlier2,dataoutlier25,dataoutlier3)
  }
  
  #Making object ready to pature model and p - value
  RModelF = NULL
  RModelI = NULL
  ModelName =NULL
  
  #Collecting and counting all the different variables, except the DV and H_1
  Cols <- names(data)
  Cols <- Cols[! Cols %in% c(y,H_1)] 
  n <- length(Cols)
  
  #Making different combinations of the variables
  Combin <- unlist(
    lapply(1:n, function(i)combn(1:n,i,simplify=FALSE)), recursive=FALSE)
  
  #Starting of regression
  start<- paste(c(y,H_1),collapse = " ~ ")
  start<- paste(c(start," + "),collapse = "")
  
  #Paste all the combinations of the different regressions into a form that can be read by lm()
  Form <- sapply(Combin,function(i)
    paste(start,paste(Cols[i],collapse=" + ")))
  
  
  if(interaction==TRUE){
    
    ## Clean this up! There must be a better way 
    #Starting of interaction term
    CombinInter <- unlist(
      lapply(1, function(i)combn(1:n,i,simplify=FALSE)), recursive=FALSE)
    
    CombH1<-paste(c(H_1,":"),collapse = "")
    
    #All interactions terms
    Interactions <- sapply(CombinInter,function(i) 
      paste(CombH1,paste(Cols[i],collapse=":")))
    
    # Make all the combinations of the interaction terms and formulas from before
    FormulasInteraction<-apply(expand.grid(Form, Interactions), 1, paste, collapse="+")
    #Set them together in one string
    Formulas<-c(Form,FormulasInteraction)
    
    
  }
  
  #Running all the models
  models<-lapply(Formulas,function(i)
    lm(as.formula(i),data=data))
  
  #Collecting the p-values
  
  for(i in 1:length(models)){
    
    
    #Finding the name of the model
    mod<-Formulas[[i]]
    ModelName<-rbind(mod,ModelName)
    
    #Collecting the p-value
    coef <-summary(models[[i]])$coefficients
    p_h_1<-coef[2,4] 
    RModelF<-rbind(p_h_1,RModelF)
    
    #See if ther interaction with H_1 becomes significant
    if(grepl(":",mod)==TRUE){
      p_h_2<-coef[nrow(coef),4] 
    }
    else{
      p_h_2<-1 
    }
    RModelI<-rbind(p_h_2,RModelI)
  }
  
  
  #Combine all the p-values
  TheModels<-cbind(ModelName,RModelF,RModelI)
  TheModels<-as.data.table(TheModels)
  
  #making them from factor to numeric
  TheModels$V2<-as.numeric(as.character(TheModels$V2))
  TheModels$V3<-as.numeric(as.character(TheModels$V3))
  TheModels$Outlier<-"Non removed"
  
  RModelF = NULL
  RModelI = NULL
  ModelName =NULL
  Outliers= NULL
  
  
  
  if(SD==TRUE){
    for(j in 1:length(dataoutlier)){
      models_out<-lapply(Formulas,function(i)
        lm(as.formula(i),data=dataoutlier[[j]]))
      for(i in 1:length(models_out)){
        
        
        #Finding the name of the model
        mod<-Formulas[[i]]
        ModelName<-rbind(mod,ModelName)
        Outliers<-rbind(j,Outliers)
        
        #Collecting the p-value
        coef <-summary(models_out[[i]])$coefficients
        p_h_1<-coef[2,4] 
        RModelF<-rbind(p_h_1,RModelF)
        
        #See if ther interaction with H_1 becomes significant
        if(grepl(":",mod)==TRUE){
          p_h_2<-coef[nrow(coef),4] 
        }
        else{
          p_h_2<-1 
        }
        RModelI<-rbind(p_h_2,RModelI)
      }
    }
    TheModels_outlier<-cbind(ModelName,RModelF,RModelI,Outliers)
    TheModels_outlier<-as.data.table(TheModels_outlier)
    
    #making them from factor to numeric
    TheModels_outlier$V2<-as.numeric(as.character(TheModels_outlier$V2))
    TheModels_outlier$V3<-as.numeric(as.character(TheModels_outlier$V3))
    colnames(TheModels_outlier)[4]<-"Outlier"
    
    #Make the names such that they mach up to the outlier criteria. 
    
    
    #Combine the 2 datastets
    TheModels<-rbind(TheModels_outlier,TheModels)
  }
  
  
  #Order the models if one wants all the models out in the end
  TheModels<- TheModels[with(TheModels, order(V2)), ]
  
  
  
  Models <- TheModels[(TheModels$V2<=0.05|TheModels$V3<=0.05  ),]
  if(SD==TRUE){
    res1<-'Non removed' %in% Models$Outlier
    res2<-'1' %in% Models$Outlier
    res3<-'2' %in% Models$Outlier
    res4<-'3' %in% Models$Outlier
    
    res<-as.integer(as.logical(c(res1,res2,res3,res4))) 
  }
  else{
    res <- ifelse(nrow(Models)>=1,1,0)
    #res <- nrow(Models)/nrow(TheModels)
  }
  
  
  return((res))
}

### The different types of data:
## For all these different kind of datatypes it always hold that the correlation is 0.2

#### Everything is normal dist.: ####

dataGen1 <- function(N){
  
  M = matrix(c(1, 0, 0.2,
               0, 1, 0,
               0.2, 0, 1
  ), nrow=3, ncol=3)
  L = chol(M)
  nvars = dim(L)[1]
  
  r = t(L) %*% matrix(rnorm(nvars*N), nrow=nvars, ncol=N)
  r = t(r)
  data = as.data.frame(r)
  names(data) = c('y1', 'x1', 'x2')
  
  return(data)
}
dataGen2 <- function(N){
  
  M = matrix(c(1, 0, 0.2,0.2,
               0, 1, 0,0,
               0.2, 0, 1,0,
               0.2,0,0,1
  ), nrow=4, ncol=4)
  L = chol(M)
  nvars = dim(L)[1]
  
  r = t(L) %*% matrix(rnorm(nvars*N), nrow=nvars, ncol=N)
  r = t(r)
  data = as.data.frame(r)
  names(data) = c('y1', 'x1', 'x2','x3')
  return(data)
}
dataGen3 <- function(N){
  
  M = matrix(c(1, 0.0, 0.2, 0.2,0.2,
               0.0, 1, 0, 0,0,
               0.2, 0, 1, 0,0,
               0.2, 0, 0, 1,0,
               0.2, 0, 0, 0, 1), nrow=5, ncol=5)
  L = chol(M)
  nvars = dim(L)[1]
  r = t(L) %*% matrix(rnorm(nvars*N), nrow=nvars, ncol=N)
  r = t(r)
  data = as.data.frame(r)
  names(data) = c('y1', 'x1', 'x2', 'x3','x4')
  
  return(data)
}
dataGen4 <- function(N){
  
  M = matrix(c(1, 0.0, 0.2, 0.2,0.2,0.2,
               0.0, 1, 0, 0, 0, 0,
               0.2, 0, 1, 0, 0, 0,
               0.2, 0, 0, 1, 0, 0,
               0.2, 0, 0, 0, 1, 0,
               0.2, 0, 0, 0, 0, 1), nrow=6, ncol=6)
  L = chol(M)
  nvars = dim(L)[1]
  r = t(L) %*% matrix(rnorm(nvars*N), nrow=nvars, ncol=N)
  r = t(r)
  data = as.data.frame(r)
  names(data) = c('y1', 'x1', 'x2', 'x3','x4','x5')
  return(data)
}
dataGen5 <- function(N){
  
  M = matrix(c(1, 0.0, 0.2, 0.2,0.2,0.2,0.2,
               0.0, 1, 0, 0, 0, 0,0,
               0.2, 0, 1, 0, 0, 0,0,
               0.2, 0, 0, 1, 0, 0,0,
               0.2, 0, 0, 0, 1, 0,0,
               0.2, 0, 0, 0, 0, 1,0,
               0.2, 0, 0, 0, 0, 0,1), nrow=7, ncol=7)
  L = chol(M)
  nvars = dim(L)[1]
  r = t(L) %*% matrix(rnorm(nvars*N), nrow=nvars, ncol=N)
  r = t(r)
  data = as.data.frame(r)
  names(data) = c('y1', 'x1', 'x2', 'x3','x4','x5','x6')
  return(data)
}
dataGen6 <- function(N){
  
  
  M = matrix(c(1, 0.0, 0.2, 0.2,0.2,0.2,0.2,0.2,
               0.0, 1, 0, 0, 0, 0,0,0,
               0.2, 0, 1, 0, 0, 0,0,0,
               0.2, 0, 0, 1, 0, 0,0,0,
               0.2, 0, 0, 0, 1, 0,0,0,
               0.2, 0, 0, 0, 0, 1,0,0,
               0.2, 0, 0, 0, 0, 0,1,0,
               0.2, 0, 0, 0, 0, 0, 0,1
  ), nrow=8, ncol=8)
  L = chol(M)
  nvars = dim(L)[1]
  r = t(L) %*% matrix(rnorm(nvars*N), nrow=nvars, ncol=N)
  r = t(r)
  data = as.data.frame(r)
  names(data) = c('y1', 'x1', 'x2', 'x3','x4','x5','x6','x7')
  return(data)
}
dataGen7 <- function(N){
  
  
  
  M = matrix(c(1, 0.0, 0.2, 0.2,0.2,0.2,0.2,0.2,0.2,
               0.0, 1, 0, 0, 0, 0,0,0,0,
               0.2, 0, 1, 0, 0, 0,0,0,0,
               0.2, 0, 0, 1, 0, 0,0,0,0,
               0.2, 0, 0, 0, 1, 0,0,0,0,
               0.2, 0, 0, 0, 0, 1,0,0,0,
               0.2, 0, 0, 0, 0, 0,1,0,0,
               0.2, 0, 0, 0, 0, 0, 0,1,0,
               0.2, 0, 0, 0, 0, 0, 0, 0, 1
  ), nrow=9, ncol=9)
  L = chol(M)
  nvars = dim(L)[1]
  r = t(L) %*% matrix(rnorm(nvars*N), nrow=nvars, ncol=N)
  r = t(r)
  data = as.data.frame(r)
  names(data) = c('y1', 'x1', 'x2', 'x3','x4','x5','x6','x7','x8')
  return(data)
}

DataGenListNorm <- list(dataGen1,dataGen2,dataGen3,dataGen4,dataGen5,dataGen6,dataGen7)

#### Everything is normal dist. but with binary H_1 ####
dataGen1 <- function(N){
  
  M = matrix(c(1, 0.2,
               0.2, 1
               
  ), nrow=2, ncol=2)
  L = chol(M)
  nvars = dim(L)[1]
  
  r = t(L) %*% matrix(rnorm(nvars*N), nrow=nvars, ncol=N)
  r = t(r)
  x1<-rbinom(N,1,0.5)
  data = as.data.frame(cbind(r,x1))
  names(data) = c('y1', 'x2',"x1")
  
  return(data)
}
dataGen2 <- function(N){
  
  M = matrix(c(1, 0.2, 0.2,
               0.2, 1, 0,
               0.2, 0, 1
  ), nrow=3, ncol=3)
  L = chol(M)
  nvars = dim(L)[1]
  
  r = t(L) %*% matrix(rnorm(nvars*N), nrow=nvars, ncol=N)
  r = t(r)
  x1<-rbinom(N,1,0.5)
  data = as.data.frame(cbind(r,x1))
  names(data) = c('y1', 'x2', 'x3',"x1")
  
  return(data)
}
dataGen3 <- function(N){
  
  M = matrix(c(1, 0.2, 0.2,0.2,
               0.2, 1, 0,0,
               0.2, 0, 1,0,
               0.2,0,0,1
  ), nrow=4, ncol=4)
  L = chol(M)
  nvars = dim(L)[1]
  
  r = t(L) %*% matrix(rnorm(nvars*N), nrow=nvars, ncol=N)
  r = t(r)
  x1<-rbinom(N,1,0.5)
  data = as.data.frame(cbind(r,x1))
  names(data) = c('y1', 'x2', 'x3',"x4","x1")
  return(data)
}
dataGen4 <- function(N){
  
  M = matrix(c(1, 0.2, 0.2, 0.2,0.2,
               0.2, 1, 0, 0,0,
               0.2, 0, 1, 0,0,
               0.2, 0, 0, 1,0,
               0.2, 0, 0, 0, 1), nrow=5, ncol=5)
  L = chol(M)
  nvars = dim(L)[1]
  r = t(L) %*% matrix(rnorm(nvars*N), nrow=nvars, ncol=N)
  r = t(r)
  x1<-rbinom(N,1,0.5)
  data = as.data.frame(cbind(r,x1))
  names(data) = c('y1', 'x2', 'x3',"x4","x5","x1")
  
  return(data)
}
dataGen5 <- function(N){
  
  M = matrix(c(1, 0.2, 0.2, 0.2,0.2,0.2,
               0.2, 1, 0, 0, 0, 0,
               0.2, 0, 1, 0, 0, 0,
               0.2, 0, 0, 1, 0, 0,
               0.2, 0, 0, 0, 1, 0,
               0.2, 0, 0, 0, 0, 1), nrow=6, ncol=6)
  L = chol(M)
  nvars = dim(L)[1]
  r = t(L) %*% matrix(rnorm(nvars*N), nrow=nvars, ncol=N)
  r = t(r)
  x1<-rbinom(N,1,0.5)
  data = as.data.frame(cbind(r,x1))
  names(data) = c('y1', 'x2', 'x3',"x4","x5","x6","x1")
  
  return(data)
}
dataGen6 <- function(N){
  
  M = matrix(c(1, 0.2, 0.2, 0.2,0.2,0.2,0.2,
               0.2, 1, 0, 0, 0, 0,0,
               0.2, 0, 1, 0, 0, 0,0,
               0.2, 0, 0, 1, 0, 0,0,
               0.2, 0, 0, 0, 1, 0,0,
               0.2, 0, 0, 0, 0, 1,0,
               0.2, 0, 0, 0, 0, 0,1), nrow=7, ncol=7)
  L = chol(M)
  nvars = dim(L)[1]
  r = t(L) %*% matrix(rnorm(nvars*N), nrow=nvars, ncol=N)
  r = t(r)
  x1<-rbinom(N,1,0.5)
  data = as.data.frame(cbind(r,x1))
  names(data) = c('y1', 'x2', 'x3',"x4","x5","x6","x7","x1")
  return(data)
}
dataGen7 <- function(N){
  
  
  M = matrix(c(1, 0.2, 0.2, 0.2,0.2,0.2,0.2,0.2,
               0.2, 1, 0, 0, 0, 0,0,0,
               0.2, 0, 1, 0, 0, 0,0,0,
               0.2, 0, 0, 1, 0, 0,0,0,
               0.2, 0, 0, 0, 1, 0,0,0,
               0.2, 0, 0, 0, 0, 1,0,0,
               0.2, 0, 0, 0, 0, 0,1,0,
               0.2, 0, 0, 0, 0, 0, 0,1
  ), nrow=8, ncol=8)
  L = chol(M)
  nvars = dim(L)[1]
  r = t(L) %*% matrix(rnorm(nvars*N), nrow=nvars, ncol=N)
  r = t(r)
  x1<-rbinom(N,1,0.5)
  data = as.data.frame(cbind(r,x1))
  names(data) = c('y1', 'x2', 'x3',"x4","x5","x6","x7","x8","x1")
  return(data)
}

DataGenListNormBin <- list(dataGen1,dataGen2,dataGen3,dataGen4,dataGen5,dataGen6,dataGen7)


#### Everything is bin dist. ####
dataGen1 <- function(N){
  
  M = matrix(c(1, 0.2,
               0.2, 1
               
  ), nrow=2, ncol=2)
  L = chol(M)
  nvars = dim(L)[1]
  
  r = t(L) %*% matrix(rnorm(nvars*N), nrow=nvars, ncol=N)
  r = t(r)
  u <- pnorm(r)
  y1=r[,1]
  x2=qbern(u[,2],0.5)
  x1<-rbinom(N,1,0.5)
  data = as.data.frame(cbind(y1,x1,x2))
  
  
  return(data)
}
dataGen2 <- function(N){
  
  M = matrix(c(1, 0.2, 0.2,
               0.2, 1, 0,
               0.2, 0, 1
  ), nrow=3, ncol=3)
  L = chol(M)
  nvars = dim(L)[1]
  
  r = t(L) %*% matrix(rnorm(nvars*N), nrow=nvars, ncol=N)
  r = t(r)
  u <- pnorm(r)
  y1=r[,1]
  x2=qbern(u[,2],0.5)
  x3=qbern(u[,3],0.5)
  x1<-rbinom(N,1,0.5)
  data = as.data.frame(cbind(y1,x1,x2,x3))
  
  
  return(data)
}
dataGen3 <- function(N){
  
  M = matrix(c(1, 0.2, 0.2,0.2,
               0.2, 1, 0,0,
               0.2, 0, 1,0,
               0.2,0,0,1
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
  x1<-rbinom(N,1,0.5)
  data = as.data.frame(cbind(y1,x1,x2,x3,x4))
  
  return(data)
}
dataGen4 <- function(N){
  
  M = matrix(c(1, 0.2, 0.2, 0.2,0.2,
               0.2, 1, 0, 0,0,
               0.2, 0, 1, 0,0,
               0.2, 0, 0, 1,0,
               0.2, 0, 0, 0, 1), nrow=5, ncol=5)
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
  x1<-rbinom(N,1,0.5)
  data = as.data.frame(cbind(y1,x1,x2,x3,x4,x5))
  
  return(data)
}
dataGen5 <- function(N){
  
  M = matrix(c(1, 0.2, 0.2, 0.2,0.2,0.2,
               0.2, 1, 0, 0, 0, 0,
               0.2, 0, 1, 0, 0, 0,
               0.2, 0, 0, 1, 0, 0,
               0.2, 0, 0, 0, 1, 0,
               0.2, 0, 0, 0, 0, 1), nrow=6, ncol=6)
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
  x1<-rbinom(N,1,0.5)
  data = as.data.frame(cbind(y1,x1,x2,x3,x4,x5,x6))
  
  return(data)
}
dataGen6 <- function(N){
  
  M = matrix(c(1, 0.2, 0.2, 0.2,0.2,0.2,0.2,
               0.2, 1, 0, 0, 0, 0,0,
               0.2, 0, 1, 0, 0, 0,0,
               0.2, 0, 0, 1, 0, 0,0,
               0.2, 0, 0, 0, 1, 0,0,
               0.2, 0, 0, 0, 0, 1,0,
               0.2, 0, 0, 0, 0, 0,1), nrow=7, ncol=7)
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
  x1<-rbinom(N,1,0.5)
  data = as.data.frame(cbind(y1,x1,x2,x3,x4,x5,x6,x7))
  return(data)
}
dataGen7 <- function(N){
  
  
  M = matrix(c(1, 0.2, 0.2, 0.2,0.2,0.2,0.2,0.2,
               0.2, 1, 0, 0, 0, 0,0,0,
               0.2, 0, 1, 0, 0, 0,0,0,
               0.2, 0, 0, 1, 0, 0,0,0,
               0.2, 0, 0, 0, 1, 0,0,0,
               0.2, 0, 0, 0, 0, 1,0,0,
               0.2, 0, 0, 0, 0, 0,1,0,
               0.2, 0, 0, 0, 0, 0, 0,1
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
  x1<-rbinom(N,1,0.5)
  data = as.data.frame(cbind(y1,x1,x2,x3,x4,x5,x6,x7,x8))
  return(data)
}

DataGenListBin <- list(dataGen1,dataGen2,dataGen3,dataGen4,dataGen5,dataGen6,dataGen7)

#### Everything is bin dist. but with normal H_1 ####
dataGen1 <- function(N){
  
  M = matrix(c(1, 0.2,
               0.2, 1
               
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
  
  M = matrix(c(1, 0.2, 0.2,
               0.2, 1, 0,
               0.2, 0, 1
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
  
  M = matrix(c(1, 0.2, 0.2,0.2,
               0.2, 1, 0,0,
               0.2, 0, 1,0,
               0.2,0,0,1
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
  
  M = matrix(c(1, 0.2, 0.2, 0.2,0.2,
               0.2, 1, 0, 0,0,
               0.2, 0, 1, 0,0,
               0.2, 0, 0, 1,0,
               0.2, 0, 0, 0, 1), nrow=5, ncol=5)
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
  
  M = matrix(c(1, 0.2, 0.2, 0.2,0.2,0.2,
               0.2, 1, 0, 0, 0, 0,
               0.2, 0, 1, 0, 0, 0,
               0.2, 0, 0, 1, 0, 0,
               0.2, 0, 0, 0, 1, 0,
               0.2, 0, 0, 0, 0, 1), nrow=6, ncol=6)
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
  
  M = matrix(c(1, 0.2, 0.2, 0.2,0.2,0.2,0.2,
               0.2, 1, 0, 0, 0, 0,0,
               0.2, 0, 1, 0, 0, 0,0,
               0.2, 0, 0, 1, 0, 0,0,
               0.2, 0, 0, 0, 1, 0,0,
               0.2, 0, 0, 0, 0, 1,0,
               0.2, 0, 0, 0, 0, 0,1), nrow=7, ncol=7)
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
  
  
  M = matrix(c(1, 0.2, 0.2, 0.2,0.2,0.2,0.2,0.2,
               0.2, 1, 0, 0, 0, 0,0,0,
               0.2, 0, 1, 0, 0, 0,0,0,
               0.2, 0, 0, 1, 0, 0,0,0,
               0.2, 0, 0, 0, 1, 0,0,0,
               0.2, 0, 0, 0, 0, 1,0,0,
               0.2, 0, 0, 0, 0, 0,1,0,
               0.2, 0, 0, 0, 0, 0, 0,1
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

DataGenListBinNorm <- list(dataGen1,dataGen2,dataGen3,dataGen4,dataGen5,dataGen6,dataGen7)



#### Simulation part ####
## Here is SD=FALSE
## General for all
sample = c(50,100,150,200,250,300,350,400,450,500,600,700,800)
rep=200
finalresultNorm<-c()
finalresultNormBin<-c()
finalresultBin<-c()
finalresultBinNorm<-c()

## Normal simulation ##
for(j in 1:length(DataGenListNorm)){
  res = mapply(function(x) mean(replicate(rep, phackingInteraction(DataGenListNorm[[j]](x),"y1","x1",SD=FALSE))), x=sample)
  result = data.frame(sample,res,j)
  names(result)<-c("SampleSize","Pr","IndependentVariables")
  finalresultNorm=rbind(result,finalresultNorm)
}

## Bin simulation ##
for(j in 1:length(DataGenListBin)){
  res = mapply(function(x) mean(replicate(rep, phackingInteraction(DataGenListBin[[j]](x),"y1","x1",SD=FALSE))), x=sample)
  result = data.frame(sample,res,j)
  names(result)<-c("SampleSize","Pr","IndependentVariables")
  finalresultBin=rbind(result,finalresultBin)
}

## Normal and Bin. H_1 simulation ##
for(j in 1:length(DataGenListNormBin)){
  res = mapply(function(x) mean(replicate(rep, phackingInteraction(DataGenListNormBin[[j]](x),"y1","x1",SD=FALSE))), x=sample)
  result = data.frame(sample,res,j)
  names(result)<-c("SampleSize","Pr","IndependentVariables")
  finalresultNormBin=rbind(result,finalresultNormBin)
}

## Bin and Normal H_1 simulation ##
for(j in 1:length(DataGenListBinNorm)){
  res = mapply(function(x) mean(replicate(rep, phackingInteraction(DataGenListBinNorm[[j]](x),"y1","x1",SD=FALSE))), x=sample)
  result = data.frame(sample,res,j)
  names(result)<-c("SampleSize","Pr","IndependentVariables")
  finalresultBinNorm=rbind(result,finalresultBinNorm)
}

#### Figures ####
## Normal ##
figureNormal <-ggplot(aes(x=SampleSize, y=Pr, group=IndependentVariables, colour=IndependentVariables), data=finalresultNorm)+
  geom_line(aes(colour=as.factor(IndependentVariables))) +
  geom_point(aes(colour=as.factor(IndependentVariables)))+
  scale_color_grey()+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab("Percent of simulations with at least one model with significant random variable")+ 
  xlab("Sample size")+
  labs(colour = "Number of Predictors") +
  ggtitle("Normal")+
  theme_classic()

figureNormal

## Bin ##
figureBin <-ggplot(aes(x=SampleSize, y=Pr, group=IndependentVariables, colour=IndependentVariables), data=finalresultBin)+
  geom_line(aes(colour=as.factor(IndependentVariables))) +
  geom_point(aes(colour=as.factor(IndependentVariables)))+
  scale_color_grey()+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab("Percent of simulations with at least one model with significant random variable")+ 
  xlab("Sample size")+
  labs(colour = "Number of Predictors") +
  ggtitle("All 1/0")+
  theme_classic()

figureBin

## Normal IV and H_1 bin ##
figureNormBin <-ggplot(aes(x=SampleSize, y=Pr, group=IndependentVariables, colour=IndependentVariables), data=finalresultNormBin)+
  geom_line(aes(colour=as.factor(IndependentVariables))) +
  geom_point(aes(colour=as.factor(IndependentVariables)))+
  scale_color_grey()+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab("Percent of simulations with at least one model with significant random variable")+ 
  xlab("Sample size")+
  labs(colour = "Number of Predictors") +
  ggtitle("All normal, H_1 1/0")+
  theme_classic()

figureNormBin

## Normal H_1 and IV bin ##
figureBinNorm <-ggplot(aes(x=SampleSize, y=Pr, group=IndependentVariables, colour=IndependentVariables), data=finalresultBinNorm)+
  geom_line(aes(colour=as.factor(IndependentVariables))) +
  geom_point(aes(colour=as.factor(IndependentVariables)))+
  scale_color_grey()+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab("Percent of simulations with at least one model with significant random variable")+ 
  xlab("Sample size")+
  labs(colour = "Number of Predictors") +
  ggtitle("All 1/0, H_1 normal")+
  theme_classic()

figureBinNorm

#### Part of simulation with how many of the simulations that are significant ####

#This one is with a single interaction term, always with H_1
phackingInteraction<-function(data,y,H_1,interaction = TRUE,SD=FALSE){
  ## Loading liberaries
  library(data.table)
  library(ggplot2)
  ## Creating datasets with outliers deleted if sd=TRUE 
  # This here is only to test, the different oulier analysis will be added here 
  # This part of the code can also be optimized
  # Use the MAD (Median Absolute Deviation) method as well
  if(SD==TRUE){
    #Calculate means and sd for all variables
    all.means <- sapply(data,mean)
    all.sd<-sapply(data, sd)
    
    #sd*2
    #Miller, 1991
    outlier = ifelse(data > all.means+all.sd*2, 1, 0)
    outlier <-ifelse(rowSums(outlier)>=1,1,0)
    dataoutlier2<-data
    dataoutlier2$outlier<-outlier
    dataoutlier2<-dataoutlier2[!dataoutlier2$outlier==1,]
    
    #sd*2.5
    #Miller, 1991
    outlier = ifelse(data > all.means+all.sd*2.5, 1, 0)
    outlier <-ifelse(rowSums(outlier)>=1,1,0)
    dataoutlier25<-data
    dataoutlier25$outlier<-outlier
    dataoutlier25<-dataoutlier25[!dataoutlier25$outlier==1,]
    
    #sd*3
    #Howell, 1998 - Statistical methods in human sciences
    outlier = ifelse(data > all.means+all.sd*3, 1, 0)
    outlier <-ifelse(rowSums(outlier)>=1,1,0)
    dataoutlier3<-data
    dataoutlier3$outlier<-outlier
    dataoutlier3<-dataoutlier3[!dataoutlier3$outlier==1,]
    
    dataoutlier<-list(dataoutlier2,dataoutlier25,dataoutlier3)
  }
  
  #Making object ready to pature model and p - value
  RModelF = NULL
  RModelI = NULL
  ModelName =NULL
  
  #Collecting and counting all the different variables, except the DV and H_1
  Cols <- names(data)
  Cols <- Cols[! Cols %in% c(y,H_1)] 
  n <- length(Cols)
  
  #Making different combinations of the variables
  Combin <- unlist(
    lapply(1:n, function(i)combn(1:n,i,simplify=FALSE)), recursive=FALSE)
  
  #Starting of regression
  start<- paste(c(y,H_1),collapse = " ~ ")
  start<- paste(c(start," + "),collapse = "")
  
  #Paste all the combinations of the different regressions into a form that can be read by lm()
  Form <- sapply(Combin,function(i)
    paste(start,paste(Cols[i],collapse=" + ")))
  
  
  if(interaction==TRUE){
    
    ## Clean this up! There must be a better way   
    #Starting of interaction term
    CombinInter <- unlist(
      lapply(1, function(i)combn(1:n,i,simplify=FALSE)), recursive=FALSE)
    
    CombH1<-paste(c(H_1,":"),collapse = "")
    
    #All interactions terms
    Interactions <- sapply(CombinInter,function(i) 
      paste(CombH1,paste(Cols[i],collapse=":")))
    
    # Make all the combinations of the interaction terms and formulas from before
    FormulasInteraction<-apply(expand.grid(Form, Interactions), 1, paste, collapse="+")
    #Set them together in one string
    Formulas<-c(Form,FormulasInteraction)
    
    
  }
  
  #Running all the models
  models<-lapply(Formulas,function(i)
    lm(as.formula(i),data=data))
  
  #Collecting the p-values
  
  for(i in 1:length(models)){
    
    
    #Finding the name of the model
    mod<-Formulas[[i]]
    ModelName<-rbind(mod,ModelName)
    
    #Collecting the p-value
    coef <-summary(models[[i]])$coefficients
    p_h_1<-coef[2,4] 
    RModelF<-rbind(p_h_1,RModelF)
    
    #See if ther interaction with H_1 becomes significant
    if(grepl(":",mod)==TRUE){
      p_h_2<-coef[nrow(coef),4] 
    }
    else{
      p_h_2<-1 
    }
    RModelI<-rbind(p_h_2,RModelI)
  }
  
  
  #Combine all the p-values
  TheModels<-cbind(ModelName,RModelF,RModelI)
  TheModels<-as.data.table(TheModels)
  
  #making them from factor to numeric
  TheModels$V2<-as.numeric(as.character(TheModels$V2))
  TheModels$V3<-as.numeric(as.character(TheModels$V3))
  TheModels$Outlier<-"Non removed"
  
  RModelF = NULL
  RModelI = NULL
  ModelName =NULL
  Outliers= NULL
  
  
  
  if(SD==TRUE){
    for(j in 1:length(dataoutlier)){
      models_out<-lapply(Formulas,function(i)
        lm(as.formula(i),data=dataoutlier[[j]]))
      for(i in 1:length(models_out)){
        
        
        #Finding the name of the model
        mod<-Formulas[[i]]
        ModelName<-rbind(mod,ModelName)
        Outliers<-rbind(j,Outliers)
        
        #Collecting the p-value
        coef <-summary(models_out[[i]])$coefficients
        p_h_1<-coef[2,4] 
        RModelF<-rbind(p_h_1,RModelF)
        
        #See if ther interaction with H_1 becomes significant
        if(grepl(":",mod)==TRUE){
          p_h_2<-coef[nrow(coef),4] 
        }
        else{
          p_h_2<-1 
        }
        RModelI<-rbind(p_h_2,RModelI)
      }
    }
    TheModels_outlier<-cbind(ModelName,RModelF,RModelI,Outliers)
    TheModels_outlier<-as.data.table(TheModels_outlier)
    
    #making them from factor to numeric
    TheModels_outlier$V2<-as.numeric(as.character(TheModels_outlier$V2))
    TheModels_outlier$V3<-as.numeric(as.character(TheModels_outlier$V3))
    colnames(TheModels_outlier)[4]<-"Outlier"
    
    #Make the names such that they mach up to the outlier criteria. 
    
    
    #Combine the 2 datastets
    TheModels<-rbind(TheModels_outlier,TheModels)
  }
  
  
  #Order the models if one wants all the models out in the end
  TheModels<- TheModels[with(TheModels, order(V2)), ]
  
  
  
  Models <- TheModels[(TheModels$V2<=0.05|TheModels$V3<=0.05  ),]
  if(SD==TRUE){
    res1<-'Non removed' %in% Models$Outlier
    res2<-'1' %in% Models$Outlier
    res3<-'2' %in% Models$Outlier
    res4<-'3' %in% Models$Outlier
    
    res<-as.integer(as.logical(c(res1,res2,res3,res4))) 
  }
  else{
    #res <- ifelse(nrow(Models)>=1,1,0)
    res <- nrow(Models)/nrow(TheModels)
  }
  
  
  return((res))
}

### The different types of data:
## For all these different kind of datatypes it always hold that the correlation is 0.2

#### Everything is normal dist.: ####

dataGen1 <- function(N){
  
  M = matrix(c(1, 0, 0.2,
               0, 1, 0,
               0.2, 0, 1
  ), nrow=3, ncol=3)
  L = chol(M)
  nvars = dim(L)[1]
  
  r = t(L) %*% matrix(rnorm(nvars*N), nrow=nvars, ncol=N)
  r = t(r)
  data = as.data.frame(r)
  names(data) = c('y1', 'x1', 'x2')
  
  return(data)
}
dataGen2 <- function(N){
  
  M = matrix(c(1, 0, 0.2,0.2,
               0, 1, 0,0,
               0.2, 0, 1,0,
               0.2,0,0,1
  ), nrow=4, ncol=4)
  L = chol(M)
  nvars = dim(L)[1]
  
  r = t(L) %*% matrix(rnorm(nvars*N), nrow=nvars, ncol=N)
  r = t(r)
  data = as.data.frame(r)
  names(data) = c('y1', 'x1', 'x2','x3')
  return(data)
}
dataGen3 <- function(N){
  
  M = matrix(c(1, 0.0, 0.2, 0.2,0.2,
               0.0, 1, 0, 0,0,
               0.2, 0, 1, 0,0,
               0.2, 0, 0, 1,0,
               0.2, 0, 0, 0, 1), nrow=5, ncol=5)
  L = chol(M)
  nvars = dim(L)[1]
  r = t(L) %*% matrix(rnorm(nvars*N), nrow=nvars, ncol=N)
  r = t(r)
  data = as.data.frame(r)
  names(data) = c('y1', 'x1', 'x2', 'x3','x4')
  
  return(data)
}
dataGen4 <- function(N){
  
  M = matrix(c(1, 0.0, 0.2, 0.2,0.2,0.2,
               0.0, 1, 0, 0, 0, 0,
               0.2, 0, 1, 0, 0, 0,
               0.2, 0, 0, 1, 0, 0,
               0.2, 0, 0, 0, 1, 0,
               0.2, 0, 0, 0, 0, 1), nrow=6, ncol=6)
  L = chol(M)
  nvars = dim(L)[1]
  r = t(L) %*% matrix(rnorm(nvars*N), nrow=nvars, ncol=N)
  r = t(r)
  data = as.data.frame(r)
  names(data) = c('y1', 'x1', 'x2', 'x3','x4','x5')
  return(data)
}
dataGen5 <- function(N){
  
  M = matrix(c(1, 0.0, 0.2, 0.2,0.2,0.2,0.2,
               0.0, 1, 0, 0, 0, 0,0,
               0.2, 0, 1, 0, 0, 0,0,
               0.2, 0, 0, 1, 0, 0,0,
               0.2, 0, 0, 0, 1, 0,0,
               0.2, 0, 0, 0, 0, 1,0,
               0.2, 0, 0, 0, 0, 0,1), nrow=7, ncol=7)
  L = chol(M)
  nvars = dim(L)[1]
  r = t(L) %*% matrix(rnorm(nvars*N), nrow=nvars, ncol=N)
  r = t(r)
  data = as.data.frame(r)
  names(data) = c('y1', 'x1', 'x2', 'x3','x4','x5','x6')
  return(data)
}
dataGen6 <- function(N){
  
  
  M = matrix(c(1, 0.0, 0.2, 0.2,0.2,0.2,0.2,0.2,
               0.0, 1, 0, 0, 0, 0,0,0,
               0.2, 0, 1, 0, 0, 0,0,0,
               0.2, 0, 0, 1, 0, 0,0,0,
               0.2, 0, 0, 0, 1, 0,0,0,
               0.2, 0, 0, 0, 0, 1,0,0,
               0.2, 0, 0, 0, 0, 0,1,0,
               0.2, 0, 0, 0, 0, 0, 0,1
  ), nrow=8, ncol=8)
  L = chol(M)
  nvars = dim(L)[1]
  r = t(L) %*% matrix(rnorm(nvars*N), nrow=nvars, ncol=N)
  r = t(r)
  data = as.data.frame(r)
  names(data) = c('y1', 'x1', 'x2', 'x3','x4','x5','x6','x7')
  return(data)
}
dataGen7 <- function(N){
  
  
  
  M = matrix(c(1, 0.0, 0.2, 0.2,0.2,0.2,0.2,0.2,0.2,
               0.0, 1, 0, 0, 0, 0,0,0,0,
               0.2, 0, 1, 0, 0, 0,0,0,0,
               0.2, 0, 0, 1, 0, 0,0,0,0,
               0.2, 0, 0, 0, 1, 0,0,0,0,
               0.2, 0, 0, 0, 0, 1,0,0,0,
               0.2, 0, 0, 0, 0, 0,1,0,0,
               0.2, 0, 0, 0, 0, 0, 0,1,0,
               0.2, 0, 0, 0, 0, 0, 0, 0, 1
  ), nrow=9, ncol=9)
  L = chol(M)
  nvars = dim(L)[1]
  r = t(L) %*% matrix(rnorm(nvars*N), nrow=nvars, ncol=N)
  r = t(r)
  data = as.data.frame(r)
  names(data) = c('y1', 'x1', 'x2', 'x3','x4','x5','x6','x7','x8')
  return(data)
}

DataGenListNorm <- list(dataGen1,dataGen2,dataGen3,dataGen4,dataGen5,dataGen6,dataGen7)

#### Everything is normal dist. but with binary H_1 ####
dataGen1 <- function(N){
  
  M = matrix(c(1, 0.2,
               0.2, 1
               
  ), nrow=2, ncol=2)
  L = chol(M)
  nvars = dim(L)[1]
  
  r = t(L) %*% matrix(rnorm(nvars*N), nrow=nvars, ncol=N)
  r = t(r)
  x1<-rbinom(N,1,0.5)
  data = as.data.frame(cbind(r,x1))
  names(data) = c('y1', 'x2',"x1")
  
  return(data)
}
dataGen2 <- function(N){
  
  M = matrix(c(1, 0.2, 0.2,
               0.2, 1, 0,
               0.2, 0, 1
  ), nrow=3, ncol=3)
  L = chol(M)
  nvars = dim(L)[1]
  
  r = t(L) %*% matrix(rnorm(nvars*N), nrow=nvars, ncol=N)
  r = t(r)
  x1<-rbinom(N,1,0.5)
  data = as.data.frame(cbind(r,x1))
  names(data) = c('y1', 'x2', 'x3',"x1")
  
  return(data)
}
dataGen3 <- function(N){
  
  M = matrix(c(1, 0.2, 0.2,0.2,
               0.2, 1, 0,0,
               0.2, 0, 1,0,
               0.2,0,0,1
  ), nrow=4, ncol=4)
  L = chol(M)
  nvars = dim(L)[1]
  
  r = t(L) %*% matrix(rnorm(nvars*N), nrow=nvars, ncol=N)
  r = t(r)
  x1<-rbinom(N,1,0.5)
  data = as.data.frame(cbind(r,x1))
  names(data) = c('y1', 'x2', 'x3',"x4","x1")
  return(data)
}
dataGen4 <- function(N){
  
  M = matrix(c(1, 0.2, 0.2, 0.2,0.2,
               0.2, 1, 0, 0,0,
               0.2, 0, 1, 0,0,
               0.2, 0, 0, 1,0,
               0.2, 0, 0, 0, 1), nrow=5, ncol=5)
  L = chol(M)
  nvars = dim(L)[1]
  r = t(L) %*% matrix(rnorm(nvars*N), nrow=nvars, ncol=N)
  r = t(r)
  x1<-rbinom(N,1,0.5)
  data = as.data.frame(cbind(r,x1))
  names(data) = c('y1', 'x2', 'x3',"x4","x5","x1")
  
  return(data)
}
dataGen5 <- function(N){
  
  M = matrix(c(1, 0.2, 0.2, 0.2,0.2,0.2,
               0.2, 1, 0, 0, 0, 0,
               0.2, 0, 1, 0, 0, 0,
               0.2, 0, 0, 1, 0, 0,
               0.2, 0, 0, 0, 1, 0,
               0.2, 0, 0, 0, 0, 1), nrow=6, ncol=6)
  L = chol(M)
  nvars = dim(L)[1]
  r = t(L) %*% matrix(rnorm(nvars*N), nrow=nvars, ncol=N)
  r = t(r)
  x1<-rbinom(N,1,0.5)
  data = as.data.frame(cbind(r,x1))
  names(data) = c('y1', 'x2', 'x3',"x4","x5","x6","x1")
  
  return(data)
}
dataGen6 <- function(N){
  
  M = matrix(c(1, 0.2, 0.2, 0.2,0.2,0.2,0.2,
               0.2, 1, 0, 0, 0, 0,0,
               0.2, 0, 1, 0, 0, 0,0,
               0.2, 0, 0, 1, 0, 0,0,
               0.2, 0, 0, 0, 1, 0,0,
               0.2, 0, 0, 0, 0, 1,0,
               0.2, 0, 0, 0, 0, 0,1), nrow=7, ncol=7)
  L = chol(M)
  nvars = dim(L)[1]
  r = t(L) %*% matrix(rnorm(nvars*N), nrow=nvars, ncol=N)
  r = t(r)
  x1<-rbinom(N,1,0.5)
  data = as.data.frame(cbind(r,x1))
  names(data) = c('y1', 'x2', 'x3',"x4","x5","x6","x7","x1")
  return(data)
}
dataGen7 <- function(N){
  
  
  M = matrix(c(1, 0.2, 0.2, 0.2,0.2,0.2,0.2,0.2,
               0.2, 1, 0, 0, 0, 0,0,0,
               0.2, 0, 1, 0, 0, 0,0,0,
               0.2, 0, 0, 1, 0, 0,0,0,
               0.2, 0, 0, 0, 1, 0,0,0,
               0.2, 0, 0, 0, 0, 1,0,0,
               0.2, 0, 0, 0, 0, 0,1,0,
               0.2, 0, 0, 0, 0, 0, 0,1
  ), nrow=8, ncol=8)
  L = chol(M)
  nvars = dim(L)[1]
  r = t(L) %*% matrix(rnorm(nvars*N), nrow=nvars, ncol=N)
  r = t(r)
  x1<-rbinom(N,1,0.5)
  data = as.data.frame(cbind(r,x1))
  names(data) = c('y1', 'x2', 'x3',"x4","x5","x6","x7","x8","x1")
  return(data)
}

DataGenListNormBin <- list(dataGen1,dataGen2,dataGen3,dataGen4,dataGen5,dataGen6,dataGen7)


#### Everything is bin dist. ####
dataGen1 <- function(N){
  
  M = matrix(c(1, 0.2,
               0.2, 1
               
  ), nrow=2, ncol=2)
  L = chol(M)
  nvars = dim(L)[1]
  
  r = t(L) %*% matrix(rnorm(nvars*N), nrow=nvars, ncol=N)
  r = t(r)
  u <- pnorm(r)
  y1=r[,1]
  x2=qbern(u[,2],0.5)
  x1<-rbinom(N,1,0.5)
  data = as.data.frame(cbind(y1,x1,x2))
  
  
  return(data)
}
dataGen2 <- function(N){
  
  M = matrix(c(1, 0.2, 0.2,
               0.2, 1, 0,
               0.2, 0, 1
  ), nrow=3, ncol=3)
  L = chol(M)
  nvars = dim(L)[1]
  
  r = t(L) %*% matrix(rnorm(nvars*N), nrow=nvars, ncol=N)
  r = t(r)
  u <- pnorm(r)
  y1=r[,1]
  x2=qbern(u[,2],0.5)
  x3=qbern(u[,3],0.5)
  x1<-rbinom(N,1,0.5)
  data = as.data.frame(cbind(y1,x1,x2,x3))
  
  
  return(data)
}
dataGen3 <- function(N){
  
  M = matrix(c(1, 0.2, 0.2,0.2,
               0.2, 1, 0,0,
               0.2, 0, 1,0,
               0.2,0,0,1
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
  x1<-rbinom(N,1,0.5)
  data = as.data.frame(cbind(y1,x1,x2,x3,x4))
  
  return(data)
}
dataGen4 <- function(N){
  
  M = matrix(c(1, 0.2, 0.2, 0.2,0.2,
               0.2, 1, 0, 0,0,
               0.2, 0, 1, 0,0,
               0.2, 0, 0, 1,0,
               0.2, 0, 0, 0, 1), nrow=5, ncol=5)
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
  x1<-rbinom(N,1,0.5)
  data = as.data.frame(cbind(y1,x1,x2,x3,x4,x5))
  
  return(data)
}
dataGen5 <- function(N){
  
  M = matrix(c(1, 0.2, 0.2, 0.2,0.2,0.2,
               0.2, 1, 0, 0, 0, 0,
               0.2, 0, 1, 0, 0, 0,
               0.2, 0, 0, 1, 0, 0,
               0.2, 0, 0, 0, 1, 0,
               0.2, 0, 0, 0, 0, 1), nrow=6, ncol=6)
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
  x1<-rbinom(N,1,0.5)
  data = as.data.frame(cbind(y1,x1,x2,x3,x4,x5,x6))
  
  return(data)
}
dataGen6 <- function(N){
  
  M = matrix(c(1, 0.2, 0.2, 0.2,0.2,0.2,0.2,
               0.2, 1, 0, 0, 0, 0,0,
               0.2, 0, 1, 0, 0, 0,0,
               0.2, 0, 0, 1, 0, 0,0,
               0.2, 0, 0, 0, 1, 0,0,
               0.2, 0, 0, 0, 0, 1,0,
               0.2, 0, 0, 0, 0, 0,1), nrow=7, ncol=7)
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
  x1<-rbinom(N,1,0.5)
  data = as.data.frame(cbind(y1,x1,x2,x3,x4,x5,x6,x7))
  return(data)
}
dataGen7 <- function(N){
  
  
  M = matrix(c(1, 0.2, 0.2, 0.2,0.2,0.2,0.2,0.2,
               0.2, 1, 0, 0, 0, 0,0,0,
               0.2, 0, 1, 0, 0, 0,0,0,
               0.2, 0, 0, 1, 0, 0,0,0,
               0.2, 0, 0, 0, 1, 0,0,0,
               0.2, 0, 0, 0, 0, 1,0,0,
               0.2, 0, 0, 0, 0, 0,1,0,
               0.2, 0, 0, 0, 0, 0, 0,1
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
  x1<-rbinom(N,1,0.5)
  data = as.data.frame(cbind(y1,x1,x2,x3,x4,x5,x6,x7,x8))
  return(data)
}

DataGenListBin <- list(dataGen1,dataGen2,dataGen3,dataGen4,dataGen5,dataGen6,dataGen7)

#### Everything is bin dist. but with normal H_1 ####
dataGen1 <- function(N){
  
  M = matrix(c(1, 0.2,
               0.2, 1
               
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
  
  M = matrix(c(1, 0.2, 0.2,
               0.2, 1, 0,
               0.2, 0, 1
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
  
  M = matrix(c(1, 0.2, 0.2,0.2,
               0.2, 1, 0,0,
               0.2, 0, 1,0,
               0.2,0,0,1
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
  
  M = matrix(c(1, 0.2, 0.2, 0.2,0.2,
               0.2, 1, 0, 0,0,
               0.2, 0, 1, 0,0,
               0.2, 0, 0, 1,0,
               0.2, 0, 0, 0, 1), nrow=5, ncol=5)
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
  
  M = matrix(c(1, 0.2, 0.2, 0.2,0.2,0.2,
               0.2, 1, 0, 0, 0, 0,
               0.2, 0, 1, 0, 0, 0,
               0.2, 0, 0, 1, 0, 0,
               0.2, 0, 0, 0, 1, 0,
               0.2, 0, 0, 0, 0, 1), nrow=6, ncol=6)
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
  
  M = matrix(c(1, 0.2, 0.2, 0.2,0.2,0.2,0.2,
               0.2, 1, 0, 0, 0, 0,0,
               0.2, 0, 1, 0, 0, 0,0,
               0.2, 0, 0, 1, 0, 0,0,
               0.2, 0, 0, 0, 1, 0,0,
               0.2, 0, 0, 0, 0, 1,0,
               0.2, 0, 0, 0, 0, 0,1), nrow=7, ncol=7)
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
  
  
  M = matrix(c(1, 0.2, 0.2, 0.2,0.2,0.2,0.2,0.2,
               0.2, 1, 0, 0, 0, 0,0,0,
               0.2, 0, 1, 0, 0, 0,0,0,
               0.2, 0, 0, 1, 0, 0,0,0,
               0.2, 0, 0, 0, 1, 0,0,0,
               0.2, 0, 0, 0, 0, 1,0,0,
               0.2, 0, 0, 0, 0, 0,1,0,
               0.2, 0, 0, 0, 0, 0, 0,1
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

DataGenListBinNorm <- list(dataGen1,dataGen2,dataGen3,dataGen4,dataGen5,dataGen6,dataGen7)



#### Simulation part ####
## Here is SD=FALSE
## General for all
finalresultNorm<-c()
finalresultNormBin<-c()
finalresultBin<-c()
finalresultBinNorm<-c()

## Normal simulation ##
for(j in 1:length(DataGenListNorm)){
  res = mapply(function(x) mean(replicate(rep, phackingInteraction(DataGenListNorm[[j]](x),"y1","x1",SD=FALSE))), x=sample)
  result = data.frame(sample,res,j)
  names(result)<-c("SampleSize","Pr","IndependentVariables")
  finalresultNorm=rbind(result,finalresultNorm)
}

## Bin simulation ##
for(j in 1:length(DataGenListBin)){
  res = mapply(function(x) mean(replicate(rep, phackingInteraction(DataGenListBin[[j]](x),"y1","x1",SD=FALSE))), x=sample)
  result = data.frame(sample,res,j)
  names(result)<-c("SampleSize","Pr","IndependentVariables")
  finalresultBin=rbind(result,finalresultBin)
}

## Normal and Bin. H_1 simulation ##
for(j in 1:length(DataGenListNormBin)){
  res = mapply(function(x) mean(replicate(rep, phackingInteraction(DataGenListNormBin[[j]](x),"y1","x1",SD=FALSE))), x=sample)
  result = data.frame(sample,res,j)
  names(result)<-c("SampleSize","Pr","IndependentVariables")
  finalresultNormBin=rbind(result,finalresultNormBin)
}

## Bin and Normal H_1 simulation ##
for(j in 1:length(DataGenListBinNorm)){
  res = mapply(function(x) mean(replicate(rep, phackingInteraction(DataGenListBinNorm[[j]](x),"y1","x1",SD=FALSE))), x=sample)
  result = data.frame(sample,res,j)
  names(result)<-c("SampleSize","Pr","IndependentVariables")
  finalresultBinNorm=rbind(result,finalresultBinNorm)
}

#### Figures ####
## Normal ##
figureNormal <-ggplot(aes(x=SampleSize, y=Pr, group=IndependentVariables, colour=IndependentVariables), data=finalresultNorm)+
  geom_line(aes(colour=as.factor(IndependentVariables))) +
  geom_point(aes(colour=as.factor(IndependentVariables)))+
  scale_color_grey()+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab("Percent of simulations with significant random variable")+ 
  xlab("Sample size")+
  labs(colour = "Number of Predictors") +
  ggtitle("Normal")+
  theme_classic()

figureNormal

## Bin ##
figureBin <-ggplot(aes(x=SampleSize, y=Pr, group=IndependentVariables, colour=IndependentVariables), data=finalresultBin)+
  geom_line(aes(colour=as.factor(IndependentVariables))) +
  geom_point(aes(colour=as.factor(IndependentVariables)))+
  scale_color_grey()+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab("Percent of simulations with significant random variable")+ 
  xlab("Sample size")+
  labs(colour = "Number of Predictors") +
  ggtitle("All 1/0")+
  theme_classic()

figureBin

## Normal IV and H_1 bin ##
figureNormBin <-ggplot(aes(x=SampleSize, y=Pr, group=IndependentVariables, colour=IndependentVariables), data=finalresultNormBin)+
  geom_line(aes(colour=as.factor(IndependentVariables))) +
  geom_point(aes(colour=as.factor(IndependentVariables)))+
  scale_color_grey()+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab("Percent of simulations with significant random variable")+ 
  xlab("Sample size")+
  labs(colour = "Number of Predictors") +
  ggtitle("All normal, H_1 1/0")+
  theme_classic()

figureNormBin

## Normal H_1 and IV bin ##
figureBinNorm <-ggplot(aes(x=SampleSize, y=Pr, group=IndependentVariables, colour=IndependentVariables), data=finalresultBinNorm)+
  geom_line(aes(colour=as.factor(IndependentVariables))) +
  geom_point(aes(colour=as.factor(IndependentVariables)))+
  scale_color_grey()+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab("Percent of simulations with significant random variable")+ 
  xlab("Sample size")+
  labs(colour = "Number of Predictors") +
  ggtitle("All 1/0, H_1 normal")+
  theme_classic()

figureBinNorm