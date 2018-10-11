## Code for p-hacking and simulation
## This code has both a function that only looks at the fixed effects, but also a function that takes all the interavtions with H_1 
## into account. These interavtions are only with one other variable, but can be generalized such that it takes all interactions
## but the modelset will explode exponentially
## Setting working directory

#setwd("D:/Dropbox")
set.seed(1234)

## Loading library
library(ggplot2)
library(data.table)
library(plyr)
library(reshape2)

## Generating data different functions
dataGen1 <- function(N){
  
  M = matrix(c(1, 0.2,
               0.2, 1
               
  ), nrow=2, ncol=2)
  L = chol(M)
  nvars = dim(L)[1]
  
  r = t(L) %*% matrix(rnorm(nvars*N), nrow=nvars, ncol=N)
  r = t(r)
  x1<-rep(c(0,1),N/2)
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
  x1<-rep(c(0,1),N/2)
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
  x1<-rep(c(0,1),N/2)
  data = as.data.frame(cbind(r,x1))
  names(data) = c('y1', 'x2', 'x3',"x4","x1")
  return(data)
}
dataGen4 <- function(N){
  
  M = matrix(c(1, 0.0, 0.2, 0.2,0.2,
               0.0, 1, 0, 0,0,
               0.2, 0, 1, 0,0,
               0.2, 0, 0, 1,0,
               0.2, 0, 0, 0, 1), nrow=5, ncol=5)
  L = chol(M)
  nvars = dim(L)[1]
  r = t(L) %*% matrix(rnorm(nvars*N), nrow=nvars, ncol=N)
  r = t(r)
  x1<-rep(c(0,1),N/2)
  data = as.data.frame(cbind(r,x1))
  names(data) = c('y1', 'x2', 'x3',"x4","x5","x1")
  
  return(data)
}
dataGen5 <- function(N){
  
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
  x1<-rep(c(0,1),N/2)
  data = as.data.frame(cbind(r,x1))
  names(data) = c('y1', 'x2', 'x3',"x4","x5","x6","x1")
  
  return(data)
}
dataGen6 <- function(N){
  
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
  x1<-rep(c(0,1),N/2)
  data = as.data.frame(cbind(r,x1))
  names(data) = c('y1', 'x2', 'x3',"x4","x5","x6","x7","x1")
  return(data)
}
dataGen7 <- function(N){
  
  
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
  x1<-rep(c(0,1),N/2)
  data = as.data.frame(cbind(r,x1))
  names(data) = c('y1', 'x2', 'x3',"x4","x5","x6","x7","x8","x1")
  return(data)
}

DataGenList <- list(dataGen1,dataGen2,dataGen3,dataGen4,dataGen5,dataGen6,dataGen7)


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
  
  
  
  Models <- TheModels[(TheModels$V2<=0.05  ),]
   if(SD==TRUE){
    res1<-'Non removed' %in% Models$Outlier
    res2<-'1' %in% Models$Outlier
    res3<-'2' %in% Models$Outlier
    res4<-'3' %in% Models$Outlier
  
    res<-as.integer(as.logical(c(res1,res2,res3,res4))) 
  }
  else{
    res <- ifelse(nrow(Models)>=1,1,0)
  }
  #Look at what names of outlier that there is a significant model. 
  
  
  
  #res<- nrow(Models)/nrow(TheModels)
  
  #res <- ifelse(nrow(Models)>=1,1,0)
  
  
  return((res))
}

#For SD = FALSE, or if you just want one value when having SD=TRUE
sample = c(50,100,150,200,250,300)
finalresult<-c()
for(j in 1:length(DataGenList)){
  res = mapply(function(x) mean(replicate(200, phackingInteraction(DataGenList[[j]](x),"y1","x1",SD=FALSE))), x=sample)
  result = data.frame(sample,res,j)
  names(result)<-c("SampleSize","Pr","IndependentVariables")
  finalresult=rbind(result,finalresult)
}


f1 <-ggplot(aes(x=SampleSize, y=Pr, group=IndependentVariables, colour=IndependentVariables), data=finalresult)+
  geom_line(aes(colour=as.factor(IndependentVariables))) +
  geom_point(aes(colour=as.factor(IndependentVariables)))+
  scale_color_grey()+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab("Percent of simulations with at least one model with significant random variable")+ 
  xlab("Sample size")+
  labs(colour = "Number of Predictors") +
  theme_classic()

f1


#For SD = TRUE and having one point for each od the outlier methods
sample = c(50,100,200)
finalresult<-c()
for(j in 1:length(DataGenList)){
  res = mapply(function(x) rowMeans(replicate(20, phackingInteraction(DataGenList[[j]](x),"y1","x1",SD=TRUE))), x=sample)
  res=t(res)
  result = data.frame(sample,res,j)
  names(result)<-c("SampleSize","PrNon","PrSD2","PrSD2.5","PrSD3","IndependentVariables")
  finalresult=rbind(result,finalresult)
}

finalresult2 <- melt(finalresult, id.vars=c(1,ncol(finalresult)))
finalresult2$IndependentVariables<-as.factor(finalresult2$IndependentVariables)

## Plot for the second simulation where SD=TRUE
f2<-ggplot(aes(x=SampleSize, y=value, group=IndependentVariables, colour=IndependentVariables), data=finalresult2)+
  geom_line(aes(colour=as.factor(IndependentVariables))) +
  facet_grid(.~variable)+
  scale_color_grey()+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab("Percent of simulations with at least one model with significant random variable")+ 
  xlab("Sample size")+
  labs(colour = "Number of Predictors") +
  theme_classic()
f2


#ggsave("P-hack2.jpeg",width = 10,height = 8)


