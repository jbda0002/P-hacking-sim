### Master file Dist 2.0###

#### Setting up the simulation ####
## Setting working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
set.seed(1230)

## Loading library
library(ggplot2)
library(jtools)
library(ggpubr)
library(data.table)
library(plyr)
library(reshape2)
library(Rlab)
library(tidyr)
library(doSNOW)
library(tcltk)
library(parallel)
library(doParallel)
library(foreach)
library(here)



##### Things that can be changed in the simulation

## Selecting the sample sizes that should be used
sample = 200

## Setting the number of repretetion
repdist=10000
## Setting the correlation between dependent and independent 
corr=0.2


## Loading the P-Hacking function
source(here::here("CodeFinal","phackingFunction_v2-0.R"))


### Here it is added how many covariates there should be in the simulation
## Making the list for the Normal Data
source(here::here("CodeFinal","Data","Data Generation Normal.R"))
DataGenListNorm <- list(dataGen2,dataGen3)

## Making the list for the Bin Data
source(here::here("CodeFinal","Data","Data Generation Bin_v2-0.R"))
DataGenListBin <- list(dataGen2,dataGen3)

## Making the list for the Bin Normal Data
source(here::here("CodeFinal","Data","Data Generation BinNormal_v2-0.R"))
DataGenListBinNorm <- list(dataGen2,dataGen3)

## Making the list for the Normal Bin Data
source(here::here("CodeFinal","Data","Data Generation NormalBin.R"))
DataGenListNormBin <- list(dataGen2,dataGen3)

## Making the list for the Bin Data with effect coding
source(here::here("CodeFinal","Data","Data Generation Bin effect coding_v2-0.R"))
DataGenListBinEffect <- list(dataGen2,dataGen3)

## Making the list for the Bin Data with effect coding
source(here::here("CodeFinal","Data","Data Generation BinNormal effect coding_v2-0.R"))
DataGenListBinNormEffect <- list(dataGen2,dataGen3)


#### Run the simulation ####

## General for all


## The different condetions
P2<-c("TRUE","FALSE")
P3<-c("TRUE","FALSE")
condSD<-c("FALSE")
condMain<-c("TRUE","FALSE")

### Since there is a difference between the sets when Main is False and True two different simulations are made

#### Simulation for Main =T ####

## Collecting the different datatypes in one list
DataGen<-list(m1=DataGenListNorm,m2=DataGenListBin,m3=DataGenListNormBin,m4=DataGenListBinNorm,m5=DataGenListBinEffect,m6=DataGenListBinNormEffect)

#DataGen<-list(m1=DataGenListNorm,m2=DataGenListBin)

## Choosing how many workers there should be used
cl <- makeSOCKcluster(40, autoStop = TRUE)

## Using the SNOW packed as this gives the ability to make a process bar
registerDoSNOW(cl)

## Making the process bar. The process bar will stand still towards the end, as it cannot take into account the time mapply will take
pb <- txtProgressBar(max=length(DataGenListBin)*length(DataGen)*length(condSD)*length(corr)*length(P2)*length(P3)*length(condMain)*length(sample)*repdist, style=3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress=progress)



resultsMTDist<-
  foreach(k=1:length(condSD),.combine=rbind) %:%
  foreach(h=1:length(P2),.combine=rbind, .inorder=FALSE) %:%
  foreach(c=1:length(corr),.combine=rbind, .inorder=FALSE) %:%
  foreach(n=1:length(condMain),.combine=rbind, .inorder=FALSE) %:%
  foreach(l=1:length(P3),.combine=rbind, .inorder=FALSE) %:%
  foreach(g=1:length(sample),.combine=rbind, .inorder=FALSE) %:%
  foreach(i=1:length(DataGen),.combine=rbind, .inorder=F) %:%
  foreach(j=1:length(DataGen[[i]]),.combine=rbind, .inorder=FALSE) %:%
  foreach(t=1:repdist,.combine=rbind,.packages=c("plyr","statip","data.table","BinNor","dplyr"),.options.snow = opts, .inorder=FALSE) %dopar% {
    f = function() {
      
      phackingFunction(DataGen[[i]][[j]](sample[[g]],corr[[c]]),"y1","h1",Ma_HCI =P2[[h]],outlierexclusion=condSD[[k]],Ma_CCI = P3[[l]],Main = T,Per = TRUE)
      
      
    }
    d1=data.frame(f(),Power12=h,Power13=l,Power123=2,OutlierExclusion=k,IndependentVariables=j,Type=i,SampleSize=sample[[g]],Main=1,Correlation=corr[[c]],it=t)
    
    if(h==2 & l==2){
      f = function() {
        
        phackingFunction(DataGen[[i]][[j]](sample[[g]],corr[[c]]),"y1","h1",outlierexclusion=condSD[[k]],Ma_HCI_CCI = T,Main = T,Per = TRUE)
        
        
      }
    }
    else{
      f = function() {
        nothing=NA
        
        nothing
      }
    }
    
    d2=data.frame(f(),Power12=h,Power13=l,Power123=1,OutlierExclusion=k,IndependentVariables=j,Type=i,SampleSize=sample[[g]],Main=1,Correlation=corr[[c]],it=t)
    return(rbind(d1,d2))
    
    
  }

## Close process bar
close(pb)
## Stop the workers 
stopCluster(cl)
registerDoSEQ()

## Choosing how many workers there should be used
cl <- makeSOCKcluster(40, autoStop = TRUE)

## Using the SNOW packed as this gives the ability to make a process bar
registerDoSNOW(cl)

## Making the process bar. The process bar will stand still towards the end, as it cannot take into account the time mapply will take
pb <- txtProgressBar(max=length(DataGenListBin)*length(DataGen)*length(condSD)*length(corr)*length(P2)*length(P3)*length(condMain)*length(sample)*repdist, style=3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress=progress)




resultsMFDist<-
  foreach(k=1:length(condSD),.combine=rbind, .inorder=FALSE) %:%
  foreach(h=1:length(P2),.combine=rbind, .inorder=FALSE) %:%
  foreach(c=1:length(corr),.combine=rbind, .inorder=FALSE) %:%
  foreach(n=1:length(condMain),.combine=rbind, .inorder=FALSE) %:%
  foreach(l=1:length(P3),.combine=rbind, .inorder=FALSE) %:%
  foreach(g=1:length(sample),.combine=rbind, .inorder=FALSE) %:%
  foreach(i=1:length(DataGen),.combine=rbind, .inorder=F) %:%
  foreach(j=1:length(DataGen[[i]]),.combine=rbind, .inorder=FALSE) %:%
  foreach(t=1:repdist,.combine=rbind,.packages=c("plyr","statip","data.table","BinNor"),.options.snow = opts, .inorder=FALSE) %dopar% {
    f = function() {
      
      phackingFunction(DataGen[[i]][[j]](sample[[g]],corr[[c]]),"y1","h1",HCI =P2[[h]],outlierexclusion=condSD[[k]],CCI = P3[[l]],Main = F,Per = TRUE)
      
      
    }
    data1=data.frame(f(),Power2=h,Power3=l,Power12=2,Power13=2,Power23=2,Power123=2,OutlierExclusion=k,IndependentVariables=j,Type=i,SampleSize=sample[[g]],Main=2,Correlation=corr[[c]],it=t)
    if(h!=2 | l!=2){
      f = function() {
        
        phackingFunction(DataGen[[i]][[j]](sample[[g]],corr[[c]]),"y1","h1",Ma_HCI =P2[[h]],outlierexclusion=condSD[[k]],Ma_CCI = P3[[l]],Main = F,Per = TRUE)
        
        
      }
    }
    else{
      f = function() {
        nothing=NA
        
        nothing
      }
    }
    data2=data.frame(f(),Power2=2,Power3=2,Power12=h,Power13=l,Power23=2,Power123=2,OutlierExclusion=k,IndependentVariables=j,Type=i,SampleSize=sample[[g]],Main=2,Correlation=corr[[c]],it=t)
    
    if(h==2 & l==2){
      f = function() {
        
        phackingFunction(DataGen[[i]][[j]](sample[[g]],corr[[c]]),"y1","h1",outlierexclusion=condSD[[k]],Ma_HCI_CCI = T,Main = F,Per = TRUE)
        
        
      }
      data3=data.frame(f(),Power2=h,Power3=l,Power12=2,Power13=2,Power23=2,Power123=1,OutlierExclusion=k,IndependentVariables=j,Type=i,SampleSize=sample[[g]],Main=2,Correlation=corr[[c]],it=t)
      
      f=function(){
        phackingFunction(DataGen[[i]][[j]](sample[[g]],corr[[c]]),"y1","h1",outlierexclusion=condSD[[k]],HCI_CCI = T,Main = F,Per = TRUE)
        
      }
      data4=data.frame(f(),Power2=h,Power3=l,Power12=2,Power13=2,Power23=1,Power123=2,OutlierExclusion=k,IndependentVariables=j,Type=i,SampleSize=sample[[g]],Main=2,Correlation=corr[[c]],it=t)
      
    }
    else{
      f = function() {
        nothing=NA
        
        nothing
      }
      
      data3=data.frame(f(),Power2=h,Power3=l,Power12=2,Power13=2,Power23=2,Power123=1,OutlierExclusion=k,IndependentVariables=j,Type=i,SampleSize=sample[[g]],Main=2,Correlation=corr[[c]],it=t)
      data4=data.frame(f(),Power2=h,Power3=l,Power12=2,Power13=2,Power23=1,Power123=2,OutlierExclusion=k,IndependentVariables=j,Type=i,SampleSize=sample[[g]],Main=2,Correlation=corr[[c]],it=t)
      
    }
    
    
    
    return(rbind(data1,data2,data3,data4))
    
  }


## Close process bar
close(pb)

## Stop the workers 
stopCluster(cl)
registerDoSEQ()

resultsMFDist<-resultsMFDist[!(is.na(resultsMFDist$f..)),]
resultsMTDist<-resultsMTDist[!(is.na(resultsMTDist$f..)),]


finalresults<-dplyr::bind_rows(resultsMTDist, resultsMFDist)
finalresults[is.na(finalresults)] <- 2

finalresults$Set<-ifelse(finalresults$Power2==1 & finalresults$Power3==2 & finalresults$Power12==2 & finalresults$Power13==2 & finalresults$Power123==2 & finalresults$Power23==2, "HCI",
                         ifelse(finalresults$Power2==2 & finalresults$Power3==2 & finalresults$Power12==2 & finalresults$Power13==2& finalresults$Power123==2 & finalresults$Power23==2,"Ma",
                                ifelse(finalresults$Power2==2 & finalresults$Power3==1 & finalresults$Power12==2 & finalresults$Power13==2& finalresults$Power123==2 & finalresults$Power23==2, "CCI",
                                       ifelse(finalresults$Power2==2 & finalresults$Power3==2 & finalresults$Power12==1 & finalresults$Power13==2& finalresults$Power123==2 & finalresults$Power23==2,"Ma + HCI",
                                              ifelse(finalresults$Power2==2 & finalresults$Power3==2 & finalresults$Power12==2 & finalresults$Power13==1& finalresults$Power123==2 & finalresults$Power23==2,"Ma + CCI",
                                                     ifelse(finalresults$Power2==2 & finalresults$Power3==2 & finalresults$Power12==2 & finalresults$Power13==2 & finalresults$Power123==1 & finalresults$Power23==2,"Ma + HCI + CCI",
                                                            ifelse(finalresults$Power2==2 & finalresults$Power3==2 & finalresults$Power12==2 & finalresults$Power13==2 & finalresults$Power23==1,"HCI + CCI",0)))))))



## Since the simulation produce the effect where two sets are true at the same time, we delete them for these figures
finalresultsMain=finalresults[!finalresults$Set==0,]
finalresultsMain$DV=1
finalresultsMain$OutlierExclusion=2

##### Effect of using outlier criteria ####


##### Things that can be changed in the simulation

## Selecting the sample sizes that should be used
sample = c(200)

## Setting the correlation between dependent and independent 
corr=c(0.2)

### Here it is added how many covariates there should be in the simulation
## Making the list for the Normal Data
source(here::here("CodeFinal","Data","Data Generation Normal.R"))
DataGenListNorm <- list(dataGen2)

## Making the list for the Bin Data
source(here::here("CodeFinal","Data","Data Generation Bin_v2-0.R"))
DataGenListBin <- list(dataGen2)

## Making the list for the Bin Normal Data
source(here::here("CodeFinal","Data","Data Generation BinNormal_v2-0.R"))
DataGenListBinNorm <- list(dataGen2)

## Making the list for the Normal Bin Data
source(here::here("CodeFinal","Data","Data Generation NormalBin.R"))
DataGenListNormBin <- list(dataGen2)

## Making the list for the Bin Data with effect coding
source(here::here("CodeFinal","Data","Data Generation Bin effect coding_v2-0.R"))
DataGenListBinEffect <- list(dataGen2)

## Making the list for the Bin Data with effect coding
source(here::here("CodeFinal","Data","Data Generation BinNormal effect coding_v2-0.R"))
DataGenListBinNormEffect <- list(dataGen2)


#### Run the simulation ####

## General for all


## The different condetions
P2<-c("TRUE","FALSE")
P3<-c("TRUE","FALSE")
condSD<-c("TRUE")
condMain<-c("TRUE","FALSE")

### Since there is a difference between the sets when Main is False and True two different simulations are made

#### Simulation for Main =T ####

## Collecting the different datatypes in one list
DataGen<-list(m1=DataGenListNorm,m2=DataGenListBin,m3=DataGenListNormBin,m4=DataGenListBinNorm,m5=DataGenListBinEffect,m6=DataGenListBinNormEffect)

#DataGen<-list(m1=DataGenListNorm,m2=DataGenListBin)

## Choosing how many workers there should be used
cl <- makeSOCKcluster(40, autoStop = TRUE)

## Using the SNOW packed as this gives the ability to make a process bar
registerDoSNOW(cl)

## Making the process bar. The process bar will stand still towards the end, as it cannot take into account the time mapply will take
pb <- txtProgressBar(max=length(DataGenListBin)*length(DataGen)*length(condSD)*length(corr)*length(P2)*length(P3)*length(condMain)*length(sample)*repdist, style=3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress=progress)



resultsMTDistOut<-
  foreach(k=1:length(condSD),.combine=rbind) %:%
  foreach(h=1:length(P2),.combine=rbind, .inorder=FALSE) %:%
  foreach(c=1:length(corr),.combine=rbind, .inorder=FALSE) %:%
  foreach(n=1:length(condMain),.combine=rbind, .inorder=FALSE) %:%
  foreach(l=1:length(P3),.combine=rbind, .inorder=FALSE) %:%
  foreach(g=1:length(sample),.combine=rbind, .inorder=FALSE) %:%
  foreach(i=1:length(DataGen),.combine=rbind, .inorder=F) %:%
  foreach(j=1:length(DataGen[[i]]),.combine=rbind, .inorder=FALSE) %:%
  foreach(t=1:repdist,.combine=rbind,.packages=c("plyr","statip","data.table","BinNor","dplyr"),.options.snow = opts, .inorder=FALSE) %dopar% {
    f = function() {
      
      phackingFunction(DataGen[[i]][[j]](sample[[g]],corr[[c]]),"y1","h1",Ma_HCI =P2[[h]],outlierexclusion=condSD[[k]],Ma_CCI = P3[[l]],Main = T,Per = TRUE)
      
      
    }
    d1=data.frame(f(),Power12=h,Power13=l,Power123=2,OutlierExclusion=1,IndependentVariables=j,Type=i,SampleSize=sample[[g]],Main=1,Correlation=corr[[c]],it=t)
    
    if(h==2 & l==2){
      f = function() {
        
        phackingFunction(DataGen[[i]][[j]](sample[[g]],corr[[c]]),"y1","h1",outlierexclusion=condSD[[k]],Ma_HCI_CCI = T,Main = T,Per = TRUE)
        
        
      }
    }
    else{
      f = function() {
        nothing=NA
        
        nothing
      }
    }
    
    d2=data.frame(f(),Power12=h,Power13=l,Power123=1,OutlierExclusion=1,IndependentVariables=j,Type=i,SampleSize=sample[[g]],Main=1,Correlation=corr[[c]],it=t)
    return(rbind(d1,d2))
    
    
  }

## Close process bar
close(pb)
## Stop the workers 
stopCluster(cl)
registerDoSEQ()

## Choosing how many workers there should be used
cl <- makeSOCKcluster(40, autoStop = TRUE)

## Using the SNOW packed as this gives the ability to make a process bar
registerDoSNOW(cl)

## Making the process bar. The process bar will stand still towards the end, as it cannot take into account the time mapply will take
pb <- txtProgressBar(max=length(DataGenListBin)*length(DataGen)*length(condSD)*length(corr)*length(P2)*length(P3)*length(condMain)*length(sample)*repdist, style=3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress=progress)




resultsMFDistOut<-
  foreach(k=1:length(condSD),.combine=rbind, .inorder=FALSE) %:%
  foreach(h=1:length(P2),.combine=rbind, .inorder=FALSE) %:%
  foreach(c=1:length(corr),.combine=rbind, .inorder=FALSE) %:%
  foreach(n=1:length(condMain),.combine=rbind, .inorder=FALSE) %:%
  foreach(l=1:length(P3),.combine=rbind, .inorder=FALSE) %:%
  foreach(g=1:length(sample),.combine=rbind, .inorder=FALSE) %:%
  foreach(i=1:length(DataGen),.combine=rbind, .inorder=F) %:%
  foreach(j=1:length(DataGen[[i]]),.combine=rbind, .inorder=FALSE) %:%
  foreach(t=1:repdist,.combine=rbind,.packages=c("plyr","statip","data.table","BinNor"),.options.snow = opts, .inorder=FALSE) %dopar% {
    f = function() {
      
      phackingFunction(DataGen[[i]][[j]](sample[[g]],corr[[c]]),"y1","h1",HCI =P2[[h]],outlierexclusion=condSD[[k]],CCI = P3[[l]],Main = F,Per = TRUE)
      
      
    }
    data1=data.frame(f(),Power2=h,Power3=l,Power12=2,Power13=2,Power23=2,Power123=2,OutlierExclusion=1,IndependentVariables=j,Type=i,SampleSize=sample[[g]],Main=2,Correlation=corr[[c]],it=t)
    if(h!=2 | l!=2){
      f = function() {
        
        phackingFunction(DataGen[[i]][[j]](sample[[g]],corr[[c]]),"y1","h1",Ma_HCI =P2[[h]],outlierexclusion=condSD[[k]],Ma_CCI = P3[[l]],Main = F,Per = TRUE)
        
        
      }
    }
    else{
      f = function() {
        nothing=NA
        
        nothing
      }
    }
    data2=data.frame(f(),Power2=2,Power3=2,Power12=h,Power13=l,Power23=2,Power123=2,OutlierExclusion=1,IndependentVariables=j,Type=i,SampleSize=sample[[g]],Main=2,Correlation=corr[[c]],it=t)
    
    if(h==2 & l==2){
      f = function() {
        
        phackingFunction(DataGen[[i]][[j]](sample[[g]],corr[[c]]),"y1","h1",outlierexclusion=condSD[[k]],Ma_HCI_CCI = T,Main = F,Per = TRUE)
        
        
      }
      data3=data.frame(f(),Power2=h,Power3=l,Power12=2,Power13=2,Power23=2,Power123=1,OutlierExclusion=1,IndependentVariables=j,Type=i,SampleSize=sample[[g]],Main=2,Correlation=corr[[c]],it=t)
      
      f=function(){
        phackingFunction(DataGen[[i]][[j]](sample[[g]],corr[[c]]),"y1","h1",outlierexclusion=condSD[[k]],HCI_CCI = T,Main = F,Per = TRUE)
        
      }
      data4=data.frame(f(),Power2=h,Power3=l,Power12=2,Power13=2,Power23=1,Power123=2,OutlierExclusion=1,IndependentVariables=j,Type=i,SampleSize=sample[[g]],Main=2,Correlation=corr[[c]],it=t)
      
    }
    else{
      f = function() {
        nothing=NA
        
        nothing
      }
      
      data3=data.frame(f(),Power2=h,Power3=l,Power12=2,Power13=2,Power23=2,Power123=1,OutlierExclusion=1,IndependentVariables=j,Type=i,SampleSize=sample[[g]],Main=2,Correlation=corr[[c]],it=t)
      data4=data.frame(f(),Power2=h,Power3=l,Power12=2,Power13=2,Power23=1,Power123=2,OutlierExclusion=1,IndependentVariables=j,Type=i,SampleSize=sample[[g]],Main=2,Correlation=corr[[c]],it=t)
      
    }
    
    
    
    return(rbind(data1,data2,data3,data4))
    
  }


## Close process bar
close(pb)

## Stop the workers 
stopCluster(cl)
registerDoSEQ()

resultsMFDistOut<-resultsMFDistOut[!(is.na(resultsMFDistOut$f..)),]
resultsMTDistOut<-resultsMTDistOut[!(is.na(resultsMTDistOut$f..)),]


finalresultsOut<-dplyr::bind_rows(resultsMFDistOut, resultsMTDistOut)
finalresultsOut[is.na(finalresultsOut)] <- 2

finalresultsOut$Set<-ifelse(finalresultsOut$Power2==1 & finalresultsOut$Power3==2 & finalresultsOut$Power12==2 & finalresultsOut$Power13==2 & finalresultsOut$Power123==2 & finalresultsOut$Power23==2, "HCI",
                            ifelse(finalresultsOut$Power2==2 & finalresultsOut$Power3==2 & finalresultsOut$Power12==2 & finalresultsOut$Power13==2& finalresultsOut$Power123==2 & finalresultsOut$Power23==2,"Ma",
                                   ifelse(finalresultsOut$Power2==2 & finalresultsOut$Power3==1 & finalresultsOut$Power12==2 & finalresultsOut$Power13==2& finalresultsOut$Power123==2 & finalresultsOut$Power23==2, "CCI",
                                          ifelse(finalresultsOut$Power2==2 & finalresultsOut$Power3==2 & finalresultsOut$Power12==1 & finalresultsOut$Power13==2& finalresultsOut$Power123==2 & finalresultsOut$Power23==2,"Ma + HCI",
                                                 ifelse(finalresultsOut$Power2==2 & finalresultsOut$Power3==2 & finalresultsOut$Power12==2 & finalresultsOut$Power13==1& finalresultsOut$Power123==2 & finalresultsOut$Power23==2,"Ma + CCI",
                                                        ifelse(finalresultsOut$Power2==2 & finalresultsOut$Power3==2 & finalresultsOut$Power12==2 & finalresultsOut$Power13==2 & finalresultsOut$Power123==1 & finalresultsOut$Power23==2,"Ma + HCI + CCI",
                                                               ifelse(finalresultsOut$Power2==2 & finalresultsOut$Power3==2 & finalresultsOut$Power12==2 & finalresultsOut$Power13==2 & finalresultsOut$Power23==1,"HCI + CCI",0)))))))



## Since the simulation produce the effect where two sets are true at the same time, we delete them for these figures
finalresultsOut=finalresultsOut[!finalresultsOut$Set==0,]
finalresultsOut$DV=1


##### Using several dependent variables ####


##### Things that can be changed in the simulation

sample = c(200)

## Setting the correlation between dependent and independent 
corr=c(0.2)
corrDV=0.5


### Here it is added how many covariates there should be in the simulation
## Making the list for the Normal Data
source(here::here("CodeFinal","Data","Data Generation NormalDV.R"))
DataGenListNorm <- list(dataGen2)

## Making the list for the Bin Data
source(here::here("CodeFinal","Data","Data Generation BinDV_v2-0.R"))
DataGenListBin <- list(dataGen2)

## Making the list for the Bin Data
source(here::here("CodeFinal","Data","Data Generation BinNormalDV_v2-0.R"))
DataGenListBinNorm <- list(dataGen2)

## Making the list for the Bin Data
source(here::here("CodeFinal","Data","Data Generation NormalBinDV.R"))
DataGenListNormBin <- list(dataGen2)



#### Run the simulation ####

## General for all


## The different condetions
P2<-c("TRUE","FALSE")
P3<-c("TRUE","FALSE")
condSD<-c("FALSE")
condMain<-c("TRUE","FALSE")

### Since there is a difference between the sets when Main is False and True two different simulations are made

#### Simulation for Main =T ####

## Collecting the different datatypes in one list
DataGen<-list(m1=DataGenListNorm,m2=DataGenListBin,m3=DataGenListNormBin,m4=DataGenListBinNorm)

#DataGen<-list(m1=DataGenListNorm,m2=DataGenListBin)

## Choosing how many workers there should be used
cl <- makeSOCKcluster(40, autoStop = TRUE)

## Using the SNOW packed as this gives the ability to make a process bar
registerDoSNOW(cl)

## Making the process bar. The process bar will stand still towards the end, as it cannot take into account the time mapply will take
pb <- txtProgressBar(max=length(DataGenListBin)*length(DataGen)*length(condSD)*length(corr)*length(P2)*length(P3)*length(condMain)*length(sample)*repdist, style=3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress=progress)



resultsMTDist<-
  foreach(k=1:length(condSD),.combine=rbind) %:%
  foreach(h=1:length(P2),.combine=rbind, .inorder=FALSE) %:%
  foreach(c=1:length(corr),.combine=rbind, .inorder=FALSE) %:%
  foreach(n=1:length(condMain),.combine=rbind, .inorder=FALSE) %:%
  foreach(l=1:length(P3),.combine=rbind, .inorder=FALSE) %:%
  foreach(g=1:length(sample),.combine=rbind, .inorder=FALSE) %:%
  foreach(i=1:length(DataGen),.combine=rbind, .inorder=F) %:%
  foreach(j=1:length(DataGen[[i]]),.combine=rbind, .inorder=FALSE) %:%
  foreach(t=1:repdist,.combine=rbind,.packages=c("plyr","statip","data.table","BinNor","dplyr"),.options.snow = opts, .inorder=FALSE) %dopar% {
    f = function() {
      
      phackingFunction(DataGen[[i]][[j]](sample[[g]],cor=corr[[c]],corDV=corrDV[[1]]),c("y1","y2","y3"),"h1",Ma_HCI =P2[[h]],outlierexclusion=condSD[[k]],Ma_CCI = P3[[l]],Main = T,Per = TRUE)
      
      
    }
    d1=data.frame(f(),Power12=h,Power13=l,Power123=2,OutlierExclusion=k,IndependentVariables=j,Type=i,SampleSize=sample[[g]],Main=1,Correlation=corr[[c]],it=t)
    
    if(h==2 & l==2){
      f = function() {
        
        phackingFunction(DataGen[[i]][[j]](sample[[g]],cor=corr[[c]],corDV=corrDV[[1]]),c("y1","y2","y3"),"h1",outlierexclusion=condSD[[k]],Ma_HCI_CCI = T,Main = T,Per = TRUE)
        
        
      }
    }
    else{
      f = function() {
        nothing=NA
        
        nothing
      }
    }
    
    d2=data.frame(f(),Power12=h,Power13=l,Power123=1,OutlierExclusion=k,IndependentVariables=j,Type=i,SampleSize=sample[[g]],Main=1,Correlation=corr[[c]],it=t)
    return(rbind(d1,d2))
    
    
  }

## Close process bar
close(pb)

## Stop the workers 
stopCluster(cl)
registerDoSEQ()
#### Simulation for Main = F ####

## Choosing how many workers there should be used
cl <- makeSOCKcluster(40, autoStop = TRUE)
## Using the SNOW packed as this gives the ability to make a process bar
registerDoSNOW(cl)

## Making the process bar. The process bar will stand still towards the end, as it cannot take into account the time mapply will take
pb <- txtProgressBar(max=length(DataGenListBin)*length(DataGen)*length(condSD)*length(corr)*length(P2)*length(P3)*length(condMain)*length(sample)*repdist, style=3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress=progress)


resultsMFDist<-
  foreach(k=1:length(condSD),.combine=rbind) %:%
  foreach(h=1:length(P2),.combine=rbind, .inorder=FALSE) %:%
  foreach(c=1:length(corr),.combine=rbind, .inorder=FALSE) %:%
  foreach(n=1:length(condMain),.combine=rbind, .inorder=FALSE) %:%
  foreach(l=1:length(P3),.combine=rbind, .inorder=FALSE) %:%
  foreach(g=1:length(sample),.combine=rbind, .inorder=FALSE) %:%
  foreach(i=1:length(DataGen),.combine=rbind, .inorder=F) %:%
  foreach(j=1:length(DataGen[[i]]),.combine=rbind, .inorder=FALSE) %:%
  foreach(t=1:repdist,.combine=rbind,.packages=c("plyr","statip","data.table","BinNor"),.options.snow = opts, .inorder=FALSE) %dopar% {
    f = function() {
      
      phackingFunction(DataGen[[i]][[j]](sample[[g]],corr[[c]],corrDV),c("y1","y2","y3"),"h1",HCI =P2[[h]],outlierexclusion=condSD[[k]],CCI = P3[[l]],Main = F,Per = TRUE)
      
      
    }
    data1=data.frame(f(),Power2=h,Power3=l,Power12=2,Power13=2,Power23=2,Power123=2,OutlierExclusion=k,IndependentVariables=j,Type=i,SampleSize=sample[[g]],Main=2,Correlation=corr[[c]],it=t)
    if(h!=2 | l!=2){
      f = function() {
        
        phackingFunction(DataGen[[i]][[j]](sample[[g]],corr[[c]],corrDV),c("y1","y2","y3"),"h1",Ma_HCI =P2[[h]],outlierexclusion=condSD[[k]],Ma_CCI = P3[[l]],Main = F,Per = TRUE)
        
        
      }
    }
    else{
      f = function() {
        nothing=NA
        
        nothing
      }
    }
    data2=data.frame(f(),Power2=2,Power3=2,Power12=h,Power13=l,Power23=2,Power123=2,OutlierExclusion=k,IndependentVariables=j,Type=i,SampleSize=sample[[g]],Main=2,Correlation=corr[[c]],it=t)
    
    if(h==2 & l==2){
      f = function() {
        
        phackingFunction(DataGen[[i]][[j]](sample[[g]],corr[[c]],corrDV),c("y1","y2","y3"),"h1",outlierexclusion=condSD[[k]],Ma_HCI_CCI = T,Main = F,Per = TRUE)
        
        
      }
      data3=data.frame(f(),Power2=h,Power3=l,Power12=2,Power13=2,Power23=2,Power123=1,OutlierExclusion=k,IndependentVariables=j,Type=i,SampleSize=sample[[g]],Main=2,Correlation=corr[[c]],it=t)
      
      f=function(){
        phackingFunction(DataGen[[i]][[j]](sample[[g]],corr[[c]],corrDV),c("y1","y2","y3"),"h1",outlierexclusion=condSD[[k]],HCI_CCI = T,Main = F,Per = TRUE)
        
      }
      data4=data.frame(f(),Power2=h,Power3=l,Power12=2,Power13=2,Power23=1,Power123=2,OutlierExclusion=k,IndependentVariables=j,Type=i,SampleSize=sample[[g]],Main=2,Correlation=corr[[c]],it=t)
      
    }
    else{
      f = function() {
        nothing=NA
        
        nothing
      }
      
      data3=data.frame(f(),Power2=h,Power3=l,Power12=2,Power13=2,Power23=2,Power123=1,OutlierExclusion=k,IndependentVariables=j,Type=i,SampleSize=sample[[g]],Main=2,Correlation=corr[[c]],it=t)
      data4=data.frame(f(),Power2=h,Power3=l,Power12=2,Power13=2,Power23=1,Power123=2,OutlierExclusion=k,IndependentVariables=j,Type=i,SampleSize=sample[[g]],Main=2,Correlation=corr[[c]],it=t)
      
    }
    
    
    
    return(rbind(data1,data2,data3,data4))
    
  }


## Close process bar
close(pb)

## Stop the workers 
stopCluster(cl)
registerDoSEQ()

resultsMFDist<-resultsMFDist[!(is.na(resultsMFDist$f..)),]
resultsMTDist<-resultsMTDist[!(is.na(resultsMTDist$f..)),]


finalresults<-dplyr::bind_rows(resultsMTDist, resultsMFDist)
finalresults[is.na(finalresults)] <- 2

finalresults$Set<-ifelse(finalresults$Power2==1 & finalresults$Power3==2 & finalresults$Power12==2 & finalresults$Power13==2 & finalresults$Power123==2 & finalresults$Power23==2, "HCI",
                         ifelse(finalresults$Power2==2 & finalresults$Power3==2 & finalresults$Power12==2 & finalresults$Power13==2& finalresults$Power123==2 & finalresults$Power23==2,"Ma",
                                ifelse(finalresults$Power2==2 & finalresults$Power3==1 & finalresults$Power12==2 & finalresults$Power13==2& finalresults$Power123==2 & finalresults$Power23==2, "CCI",
                                       ifelse(finalresults$Power2==2 & finalresults$Power3==2 & finalresults$Power12==1 & finalresults$Power13==2& finalresults$Power123==2 & finalresults$Power23==2,"Ma + HCI",
                                              ifelse(finalresults$Power2==2 & finalresults$Power3==2 & finalresults$Power12==2 & finalresults$Power13==1& finalresults$Power123==2 & finalresults$Power23==2,"Ma + CCI",
                                                     ifelse(finalresults$Power2==2 & finalresults$Power3==2 & finalresults$Power12==2 & finalresults$Power13==2 & finalresults$Power123==1 & finalresults$Power23==2,"Ma + HCI + CCI",
                                                            ifelse(finalresults$Power2==2 & finalresults$Power3==2 & finalresults$Power12==2 & finalresults$Power13==2 & finalresults$Power23==1,"HCI + CCI",0)))))))



## Since the simulation produce the effect where two sets are true at the same time, we delete them for these figures
finalresultsDV=finalresults[!finalresults$Set==0,]
finalresultsDV$DV=2
finalresultsDV$OutlierExclusion=2

##### Increasing the correlation ####



##### Things that can be changed in the simulation

## Selecting the sample sizes that should be used
sample = c(200)


## Setting the correlation between dependent and independent 
corr=c(0.3,0.4)


## Due to memory problems only run dataGen4 by itself! 


### Here it is added how many covariates there should be in the simulation
## Making the list for the Normal Data
source(here::here("CodeFinal","Data","Data Generation Normal.R"))
DataGenListNorm <- list(dataGen2)

## Making the list for the Bin Data
source(here::here("CodeFinal","Data","Data Generation Bin_v2-0.R"))
DataGenListBin <- list(dataGen2)

## Making the list for the Bin Normal Data
source(here::here("CodeFinal","Data","Data Generation BinNormal_v2-0.R"))
DataGenListBinNorm <- list(dataGen2)

## Making the list for the Normal Bin Data
source(here::here("CodeFinal","Data","Data Generation NormalBin.R"))
DataGenListNormBin <- list(dataGen2)



#### Run the simulation ####

## General for all


## The different condetions
P2<-c("TRUE","FALSE")
P3<-c("TRUE","FALSE")
condSD<-c("FALSE")
condMain<-c("TRUE","FALSE")

### Since there is a difference between the sets when Main is False and True two different simulations are made

#### Simulation for Main =T ####

## Collecting the different datatypes in one list
DataGen<-list(m1=DataGenListNorm,m2=DataGenListBin,m3=DataGenListNormBin,m4=DataGenListBinNorm)

#DataGen<-list(m1=DataGenListNorm,m2=DataGenListBin)

## Choosing how many workers there should be used
cl <- makeSOCKcluster(40, autoStop = TRUE)

## Using the SNOW packed as this gives the ability to make a process bar
registerDoSNOW(cl)

## Making the process bar. The process bar will stand still towards the end, as it cannot take into account the time mapply will take
pb <- txtProgressBar(max=length(DataGenListBin)*length(DataGen)*length(condSD)*length(corr)*length(P2)*length(P3)*length(condMain)*length(sample)*repdist, style=3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress=progress)



resultsMTDist<-
  foreach(k=1:length(condSD),.combine=rbind) %:%
  foreach(h=1:length(P2),.combine=rbind, .inorder=FALSE) %:%
  foreach(c=1:length(corr),.combine=rbind, .inorder=FALSE) %:%
  foreach(n=1:length(condMain),.combine=rbind, .inorder=FALSE) %:%
  foreach(l=1:length(P3),.combine=rbind, .inorder=FALSE) %:%
  foreach(g=1:length(sample),.combine=rbind, .inorder=FALSE) %:%
  foreach(i=1:length(DataGen),.combine=rbind, .inorder=F) %:%
  foreach(j=1:length(DataGen[[i]]),.combine=rbind, .inorder=FALSE) %:%
  foreach(t=1:repdist,.combine=rbind,.packages=c("plyr","statip","data.table","BinNor","dplyr"),.options.snow = opts, .inorder=FALSE) %dopar% {
    f = function() {
      
      phackingFunction(DataGen[[i]][[j]](sample[[g]],corr[[c]]),"y1","h1",Ma_HCI =P2[[h]],outlierexclusion=condSD[[k]],Ma_CCI = P3[[l]],Main = T,Per = TRUE)
      
      
    }
    d1=data.frame(f(),Power12=h,Power13=l,Power123=2,OutlierExclusion=k,IndependentVariables=j,Type=i,SampleSize=sample[[g]],Main=1,Correlation=corr[[c]],it=t)
    
    if(h==2 & l==2){
      f = function() {
        
        phackingFunction(DataGen[[i]][[j]](sample[[g]],corr[[c]]),"y1","h1",outlierexclusion=condSD[[k]],Ma_HCI_CCI = T,Main = T,Per = TRUE)
        
        
      }
    }
    else{
      f = function() {
        nothing=NA
        
        nothing
      }
    }
    
    d2=data.frame(f(),Power12=h,Power13=l,Power123=1,OutlierExclusion=k,IndependentVariables=j,Type=i,SampleSize=sample[[g]],Main=1,Correlation=corr[[c]],it=t)
    return(rbind(d1,d2))
    
    
  }

## Close process bar
close(pb)
## Stop the workers 
stopCluster(cl)
registerDoSEQ()

## Choosing how many workers there should be used
cl <- makeSOCKcluster(40, autoStop = TRUE)

## Using the SNOW packed as this gives the ability to make a process bar
registerDoSNOW(cl)

## Making the process bar. The process bar will stand still towards the end, as it cannot take into account the time mapply will take
pb <- txtProgressBar(max=length(DataGenListBin)*length(DataGen)*length(condSD)*length(corr)*length(P2)*length(P3)*length(condMain)*length(sample)*repdist, style=3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress=progress)




resultsMFDist4<-
  foreach(k=1:length(condSD),.combine=rbind, .inorder=FALSE) %:%
  foreach(h=1:length(P2),.combine=rbind, .inorder=FALSE) %:%
  foreach(c=1:length(corr),.combine=rbind, .inorder=FALSE) %:%
  foreach(n=1:length(condMain),.combine=rbind, .inorder=FALSE) %:%
  foreach(l=1:length(P3),.combine=rbind, .inorder=FALSE) %:%
  foreach(g=1:length(sample),.combine=rbind, .inorder=FALSE) %:%
  foreach(i=1:length(DataGen),.combine=rbind, .inorder=F) %:%
  foreach(j=1:length(DataGen[[i]]),.combine=rbind, .inorder=FALSE) %:%
  foreach(t=1:repdist,.combine=rbind,.packages=c("plyr","statip","data.table","BinNor"),.options.snow = opts, .inorder=FALSE) %dopar% {
    f = function() {
      
      phackingFunction(DataGen[[i]][[j]](sample[[g]],corr[[c]]),"y1","h1",HCI =P2[[h]],outlierexclusion=condSD[[k]],CCI = P3[[l]],Main = F,Per = TRUE)
      
      
    }
    data1=data.frame(f(),Power2=h,Power3=l,Power12=2,Power13=2,Power23=2,Power123=2,OutlierExclusion=k,IndependentVariables=j,Type=i,SampleSize=sample[[g]],Main=2,Correlation=corr[[c]],it=t)
    if(h!=2 | l!=2){
      f = function() {
        
        phackingFunction(DataGen[[i]][[j]](sample[[g]],corr[[c]]),"y1","h1",Ma_HCI =P2[[h]],outlierexclusion=condSD[[k]],Ma_CCI = P3[[l]],Main = F,Per = TRUE)
        
        
      }
    }
    else{
      f = function() {
        nothing=NA
        
        nothing
      }
    }
    data2=data.frame(f(),Power2=2,Power3=2,Power12=h,Power13=l,Power23=2,Power123=2,OutlierExclusion=k,IndependentVariables=j,Type=i,SampleSize=sample[[g]],Main=2,Correlation=corr[[c]],it=t)
    
    if(h==2 & l==2){
      f = function() {
        
        phackingFunction(DataGen[[i]][[j]](sample[[g]],corr[[c]]),"y1","h1",outlierexclusion=condSD[[k]],Ma_HCI_CCI = T,Main = F,Per = TRUE)
        
        
      }
      data3=data.frame(f(),Power2=h,Power3=l,Power12=2,Power13=2,Power23=2,Power123=1,OutlierExclusion=k,IndependentVariables=j,Type=i,SampleSize=sample[[g]],Main=2,Correlation=corr[[c]],it=t)
      
      f=function(){
        phackingFunction(DataGen[[i]][[j]](sample[[g]],corr[[c]]),"y1","h1",outlierexclusion=condSD[[k]],HCI_CCI = T,Main = F,Per = TRUE)
        
      }
      data4=data.frame(f(),Power2=h,Power3=l,Power12=2,Power13=2,Power23=1,Power123=2,OutlierExclusion=k,IndependentVariables=j,Type=i,SampleSize=sample[[g]],Main=2,Correlation=corr[[c]],it=t)
      
    }
    else{
      f = function() {
        nothing=NA
        
        nothing
      }
      
      data3=data.frame(f(),Power2=h,Power3=l,Power12=2,Power13=2,Power23=2,Power123=1,OutlierExclusion=k,IndependentVariables=j,Type=i,SampleSize=sample[[g]],Main=2,Correlation=corr[[c]],it=t)
      data4=data.frame(f(),Power2=h,Power3=l,Power12=2,Power13=2,Power23=1,Power123=2,OutlierExclusion=k,IndependentVariables=j,Type=i,SampleSize=sample[[g]],Main=2,Correlation=corr[[c]],it=t)
      
    }
    
    
    
    return(rbind(data1,data2,data3,data4))
    
  }


## Close process bar
close(pb)

## Stop the workers 
stopCluster(cl)
registerDoSEQ()

resultsMFDist4<-resultsMFDist4[!(is.na(resultsMFDist4$f..)),]
resultsMTDist<-resultsMTDist[!(is.na(resultsMTDist$f..)),]

finalresults<-dplyr::bind_rows(resultsMTDist, resultsMFDist4)
finalresults[is.na(finalresults)] <- 2

finalresults$Set<-ifelse(finalresults$Power2==1 & finalresults$Power3==2 & finalresults$Power12==2 & finalresults$Power13==2 & finalresults$Power123==2 & finalresults$Power23==2, "HCI",
                         ifelse(finalresults$Power2==2 & finalresults$Power3==2 & finalresults$Power12==2 & finalresults$Power13==2& finalresults$Power123==2 & finalresults$Power23==2,"Ma",
                                ifelse(finalresults$Power2==2 & finalresults$Power3==1 & finalresults$Power12==2 & finalresults$Power13==2& finalresults$Power123==2 & finalresults$Power23==2, "CCI",
                                       ifelse(finalresults$Power2==2 & finalresults$Power3==2 & finalresults$Power12==1 & finalresults$Power13==2& finalresults$Power123==2 & finalresults$Power23==2,"Ma + HCI",
                                              ifelse(finalresults$Power2==2 & finalresults$Power3==2 & finalresults$Power12==2 & finalresults$Power13==1& finalresults$Power123==2 & finalresults$Power23==2,"Ma + CCI",
                                                     ifelse(finalresults$Power2==2 & finalresults$Power3==2 & finalresults$Power12==2 & finalresults$Power13==2 & finalresults$Power123==1 & finalresults$Power23==2,"Ma + HCI + CCI",
                                                            ifelse(finalresults$Power2==2 & finalresults$Power3==2 & finalresults$Power12==2 & finalresults$Power13==2 & finalresults$Power23==1,"HCI + CCI",0)))))))



## Since the simulation produce the effect where two sets are true at the same time, we delete them for these figures
finalresultsCorr=finalresults[!finalresults$Set==0,]
finalresultsCorr$DV=1
finalresultsCorr$OutlierExclusion=2

##### Sample size ####


## Selecting the sample sizes that should be used
sample = c(100,150,250,300)

## Setting the correlation between dependent and independent 
corr=c(0.2)


### Here it is added how many covariates there should be in the simulation
## Making the list for the Normal Data
source(here::here("CodeFinal","Data","Data Generation Normal.R"))
DataGenListNorm <- list(dataGen2)

## Making the list for the Bin Data
source(here::here("CodeFinal","Data","Data Generation Bin_v2-0.R"))
DataGenListBin <- list(dataGen2)

## Making the list for the Bin Normal Data
source(here::here("CodeFinal","Data","Data Generation BinNormal_v2-0.R"))
DataGenListBinNorm <- list(dataGen2)

## Making the list for the Normal Bin Data
source(here::here("CodeFinal","Data","Data Generation NormalBin.R"))
DataGenListNormBin <- list(dataGen2)

## Making the list for the Bin Data with effect coding
source(here::here("CodeFinal","Data","Data Generation Bin effect coding_v2-0.R"))
DataGenListBinEffect <- list(dataGen2)

## Making the list for the Bin Data with effect coding
source(here::here("CodeFinal","Data","Data Generation BinNormal effect coding_v2-0.R"))
DataGenListBinNormEffect <- list(dataGen2)


#### Run the simulation ####

## General for all


## The different condetions
P2<-c("TRUE","FALSE")
P3<-c("TRUE","FALSE")
condSD<-c("FALSE")
condMain<-c("TRUE","FALSE")

### Since there is a difference between the sets when Main is False and True two different simulations are made

#### Simulation for Main =T ####

## Collecting the different datatypes in one list
DataGen<-list(m1=DataGenListNorm,m2=DataGenListBin,m3=DataGenListNormBin,m4=DataGenListBinNorm,m5=DataGenListBinEffect,m6=DataGenListBinNormEffect)

#DataGen<-list(m1=DataGenListNorm,m2=DataGenListBin)

## Choosing how many workers there should be used
cl <- makeSOCKcluster(40, autoStop = TRUE)

## Using the SNOW packed as this gives the ability to make a process bar
registerDoSNOW(cl)

## Making the process bar. The process bar will stand still towards the end, as it cannot take into account the time mapply will take
pb <- txtProgressBar(max=length(DataGenListBin)*length(DataGen)*length(condSD)*length(corr)*length(P2)*length(P3)*length(condMain)*length(sample)*repdist, style=3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress=progress)



resultsMTDist<-
  foreach(k=1:length(condSD),.combine=rbind) %:%
  foreach(h=1:length(P2),.combine=rbind, .inorder=FALSE) %:%
  foreach(c=1:length(corr),.combine=rbind, .inorder=FALSE) %:%
  foreach(n=1:length(condMain),.combine=rbind, .inorder=FALSE) %:%
  foreach(l=1:length(P3),.combine=rbind, .inorder=FALSE) %:%
  foreach(g=1:length(sample),.combine=rbind, .inorder=FALSE) %:%
  foreach(i=1:length(DataGen),.combine=rbind, .inorder=F) %:%
  foreach(j=1:length(DataGen[[i]]),.combine=rbind, .inorder=FALSE) %:%
  foreach(t=1:repdist,.combine=rbind,.packages=c("plyr","statip","data.table","BinNor","dplyr"),.options.snow = opts, .inorder=FALSE) %dopar% {
    f = function() {
      
      phackingFunction(DataGen[[i]][[j]](sample[[g]],corr[[c]]),"y1","h1",Ma_HCI =P2[[h]],outlierexclusion=condSD[[k]],Ma_CCI = P3[[l]],Main = T,Per = TRUE)
      
      
    }
    d1=data.frame(f(),Power12=h,Power13=l,Power123=2,OutlierExclusion=k,IndependentVariables=j,Type=i,SampleSize=sample[[g]],Main=1,Correlation=corr[[c]],it=t)
    
    if(h==2 & l==2){
      f = function() {
        
        phackingFunction(DataGen[[i]][[j]](sample[[g]],corr[[c]]),"y1","h1",outlierexclusion=condSD[[k]],Ma_HCI_CCI = T,Main = T,Per = TRUE)
        
        
      }
    }
    else{
      f = function() {
        nothing=NA
        
        nothing
      }
    }
    
    d2=data.frame(f(),Power12=h,Power13=l,Power123=1,OutlierExclusion=k,IndependentVariables=j,Type=i,SampleSize=sample[[g]],Main=1,Correlation=corr[[c]],it=t)
    return(rbind(d1,d2))
    
    
  }

## Close process bar
close(pb)
## Stop the workers 
stopCluster(cl)
registerDoSEQ()

## Choosing how many workers there should be used
cl <- makeSOCKcluster(40, autoStop = TRUE)

## Using the SNOW packed as this gives the ability to make a process bar
registerDoSNOW(cl)

## Making the process bar. The process bar will stand still towards the end, as it cannot take into account the time mapply will take
pb <- txtProgressBar(max=length(DataGenListBin)*length(DataGen)*length(condSD)*length(corr)*length(P2)*length(P3)*length(condMain)*length(sample)*repdist, style=3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress=progress)




resultsMFDist<-
  foreach(k=1:length(condSD),.combine=rbind, .inorder=FALSE) %:%
  foreach(h=1:length(P2),.combine=rbind, .inorder=FALSE) %:%
  foreach(c=1:length(corr),.combine=rbind, .inorder=FALSE) %:%
  foreach(n=1:length(condMain),.combine=rbind, .inorder=FALSE) %:%
  foreach(l=1:length(P3),.combine=rbind, .inorder=FALSE) %:%
  foreach(g=1:length(sample),.combine=rbind, .inorder=FALSE) %:%
  foreach(i=1:length(DataGen),.combine=rbind, .inorder=F) %:%
  foreach(j=1:length(DataGen[[i]]),.combine=rbind, .inorder=FALSE) %:%
  foreach(t=1:repdist,.combine=rbind,.packages=c("plyr","statip","data.table","BinNor"),.options.snow = opts, .inorder=FALSE) %dopar% {
    f = function() {
      
      phackingFunction(DataGen[[i]][[j]](sample[[g]],corr[[c]]),"y1","h1",HCI =P2[[h]],outlierexclusion=condSD[[k]],CCI = P3[[l]],Main = F,Per = TRUE)
      
      
    }
    data1=data.frame(f(),Power2=h,Power3=l,Power12=2,Power13=2,Power23=2,Power123=2,OutlierExclusion=k,IndependentVariables=j,Type=i,SampleSize=sample[[g]],Main=2,Correlation=corr[[c]],it=t)
    if(h!=2 | l!=2){
      f = function() {
        
        phackingFunction(DataGen[[i]][[j]](sample[[g]],corr[[c]]),"y1","h1",Ma_HCI =P2[[h]],outlierexclusion=condSD[[k]],Ma_CCI = P3[[l]],Main = F,Per = TRUE)
        
        
      }
    }
    else{
      f = function() {
        nothing=NA
        
        nothing
      }
    }
    data2=data.frame(f(),Power2=2,Power3=2,Power12=h,Power13=l,Power23=2,Power123=2,OutlierExclusion=k,IndependentVariables=j,Type=i,SampleSize=sample[[g]],Main=2,Correlation=corr[[c]],it=t)
    
    if(h==2 & l==2){
      f = function() {
        
        phackingFunction(DataGen[[i]][[j]](sample[[g]],corr[[c]]),"y1","h1",outlierexclusion=condSD[[k]],Ma_HCI_CCI = T,Main = F,Per = TRUE)
        
        
      }
      data3=data.frame(f(),Power2=h,Power3=l,Power12=2,Power13=2,Power23=2,Power123=1,OutlierExclusion=k,IndependentVariables=j,Type=i,SampleSize=sample[[g]],Main=2,Correlation=corr[[c]],it=t)
      
      f=function(){
        phackingFunction(DataGen[[i]][[j]](sample[[g]],corr[[c]]),"y1","h1",outlierexclusion=condSD[[k]],HCI_CCI = T,Main = F,Per = TRUE)
        
      }
      data4=data.frame(f(),Power2=h,Power3=l,Power12=2,Power13=2,Power23=1,Power123=2,OutlierExclusion=k,IndependentVariables=j,Type=i,SampleSize=sample[[g]],Main=2,Correlation=corr[[c]],it=t)
      
    }
    else{
      f = function() {
        nothing=NA
        
        nothing
      }
      
      data3=data.frame(f(),Power2=h,Power3=l,Power12=2,Power13=2,Power23=2,Power123=1,OutlierExclusion=k,IndependentVariables=j,Type=i,SampleSize=sample[[g]],Main=2,Correlation=corr[[c]],it=t)
      data4=data.frame(f(),Power2=h,Power3=l,Power12=2,Power13=2,Power23=1,Power123=2,OutlierExclusion=k,IndependentVariables=j,Type=i,SampleSize=sample[[g]],Main=2,Correlation=corr[[c]],it=t)
      
    }
    
    
    
    return(rbind(data1,data2,data3,data4))
    
  }


## Close process bar
close(pb)

## Stop the workers 
stopCluster(cl)
registerDoSEQ()

resultsMFDist<-resultsMFDist[!(is.na(resultsMFDist$f..)),]
resultsMTDist<-resultsMTDist[!(is.na(resultsMTDist$f..)),]


finalresults<-dplyr::bind_rows(resultsMTDist, resultsMFDist)
finalresults[is.na(finalresults)] <- 2

finalresults$Set<-ifelse(finalresults$Power2==1 & finalresults$Power3==2 & finalresults$Power12==2 & finalresults$Power13==2 & finalresults$Power123==2 & finalresults$Power23==2, "HCI",
                         ifelse(finalresults$Power2==2 & finalresults$Power3==2 & finalresults$Power12==2 & finalresults$Power13==2& finalresults$Power123==2 & finalresults$Power23==2,"Ma",
                                ifelse(finalresults$Power2==2 & finalresults$Power3==1 & finalresults$Power12==2 & finalresults$Power13==2& finalresults$Power123==2 & finalresults$Power23==2, "CCI",
                                       ifelse(finalresults$Power2==2 & finalresults$Power3==2 & finalresults$Power12==1 & finalresults$Power13==2& finalresults$Power123==2 & finalresults$Power23==2,"Ma + HCI",
                                              ifelse(finalresults$Power2==2 & finalresults$Power3==2 & finalresults$Power12==2 & finalresults$Power13==1& finalresults$Power123==2 & finalresults$Power23==2,"Ma + CCI",
                                                     ifelse(finalresults$Power2==2 & finalresults$Power3==2 & finalresults$Power12==2 & finalresults$Power13==2 & finalresults$Power123==1 & finalresults$Power23==2,"Ma + HCI + CCI",
                                                            ifelse(finalresults$Power2==2 & finalresults$Power3==2 & finalresults$Power12==2 & finalresults$Power13==2 & finalresults$Power23==1,"HCI + CCI",0)))))))



## Since the simulation produce the effect where two sets are true at the same time, we delete them for these figures
finalresultsSample=finalresults[!finalresults$Set==0,]
finalresultsSample$DV=1
finalresultsSample$OutlierExclusion=2

finalresults=rbind(finalresultsMain,finalresultsSample,finalresultsCorr,finalresultsOut,finalresultsDV)


#### all sets active at the same time #### 

##### Things that can be changed in the simulation

## Selecting the sample sizes that should be used
sample = c(200)

## Setting the correlation between dependent and independent 
corr=c(0.2)


### Here it is added how many covariates there should be in the simulation
## Making the list for the Normal Data
source(here::here("CodeFinal","Data","Data Generation Normal.R"))
DataGenListNorm <- list(dataGen2)

## Making the list for the Bin Data
source(here::here("CodeFinal","Data","Data Generation Bin_v2-0.R"))
DataGenListBin <- list(dataGen2)



#### Run the simulation ####

## General for all


## The different condetions
P2<-c("TRUE")
P3<-c("TRUE")
condSD<-c("FALSE")
condMain<-c("TRUE","FALSE")

### Since there is a difference between the sets when Main is False and True two different simulations are made

#### Simulation for Main =T ####

## Collecting the different datatypes in one list
#DataGen<-list(m1=DataGenListNorm,m2=DataGenListBin,m3=DataGenListNormBin,m4=DataGenListBinNorm)

DataGen<-list(m1=DataGenListNorm,m2=DataGenListBin)

## Choosing how many workers there should be used
cl <- makeSOCKcluster(40, autoStop = TRUE)

## Using the SNOW packed as this gives the ability to make a process bar
registerDoSNOW(cl)

## Making the process bar. The process bar will stand still towards the end, as it cannot take into account the time mapply will take
pb <- txtProgressBar(max=length(DataGenListBin)*length(DataGen)*length(condSD)*length(corr)*length(P2)*length(P3)*length(condMain)*length(sample)*repdist, style=3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress=progress)



resultsFullset<-
  foreach(k=1:length(condSD),.combine=rbind) %:%
  foreach(h=1:length(P2),.combine=rbind, .inorder=FALSE) %:%
  foreach(c=1:length(corr),.combine=rbind, .inorder=FALSE) %:%
  foreach(n=1:length(condMain),.combine=rbind, .inorder=FALSE) %:%
  foreach(l=1:length(P3),.combine=rbind, .inorder=FALSE) %:%
  foreach(g=1:length(sample),.combine=rbind, .inorder=FALSE) %:%
  foreach(i=1:length(DataGen),.combine=rbind, .inorder=F) %:%
  foreach(j=1:length(DataGen[[i]]),.combine=rbind, .inorder=FALSE) %:%
  foreach(t=1:repdist,.combine=rbind,.packages=c("plyr","statip","data.table","BinNor","dplyr"),.options.snow = opts, .inorder=FALSE) %dopar% {
    f = function() {
      
      phackingFunction(DataGen[[i]][[j]](sample[[g]],corr[[c]]),"y1","h1",HCI = T,CCI=T,Ma_HCI=T, Ma_CCI=T
                       ,HCI_CCI=T,Ma_HCI_CCI=T,Main = condMain[[n]],Per = TRUE)
      
      
    }
    d1=data.frame(f(),Power12=h,Power13=l,Power123=2,OutlierExclusion=k,IndependentVariables=j,Type=i,SampleSize=sample[[g]],Main=n,Correlation=corr[[c]],it=t)
    
    d1
    
    
  }

## Close process bar
close(pb)
## Stop the workers 
stopCluster(cl)
registerDoSEQ()

write.csv(finalresults, file=gzfile(paste0(output,"/File/Results.csv.gz")))
write.csv(resultsFullset, file=gzfile(paste0(output,"/File/resultsFullSet.csv.gz")))
