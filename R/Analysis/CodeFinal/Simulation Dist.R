### Master file Dist 2.0###


## Code for p-hacking and simulation
## This code has both a function that can look at interactionterms, multiple dependt variables and there average
## and different "outlier" criterias. These interactions are only with one other variable, 
## but can be generalized such that it takes all interactions but the modelset will explode exponentially

#### Setting up the simulation ####
## Setting working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
set.seed(1234)

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
repdist=200
## Setting the correlation between dependent and independent 
corr=c(0.2)




## Loading the P-Hacking function
source(here::here("phackingFunction_v2-0.R"))

## Making the list for the Normal Data
source("Data Generation Normal.R")
DataGenListNorm <- list(dataGen3)

## Making the list for the Bin Data
source("Data Generation Bin_v2-0.R")
DataGenListBin <- list(dataGen3)

## Making the list for the Bin Data
source("Data Generation BinNormal_v2-0.R")
DataGenListBinNorm <- list(dataGen3)

## Making the list for the Bin Data
source("Data Generation NormalBin.R")
DataGenListNormBin <- list(dataGen3)


#### Run the simulation ####

## General for all


## The different condetions
P2<-c("TRUE","FALSE")
P3<-c("TRUE","FALSE")
condSD<-c("TRUE","FALSE")
condMain<-c("TRUE","FALSE")

### Since there is a difference between the sets when Main is False and True two different simulations are made

#### Simulation for Main =T ####

## Collecting the different datatypes in one list
#DataGen<-list(m1=DataGenListNorm,m2=DataGenListBin,m3=DataGenListNormBin,m4=DataGenListBinNorm)

DataGen<-list(m1=DataGenListNorm,m2=DataGenListBin)

## Choosing how many workers there should be used
cl <- makeSOCKcluster(20)

## Using the SNOW packed as this gives the ability to make a process bar
registerDoSNOW(cl)

## Making the process bar. The process bar will stand still towards the end, as it cannot take into account the time mapply will take
pb <- txtProgressBar(max=length(DataGenListBin)*length(DataGen)*length(condSD)*length(P2)*length(P3)*length(sample), style=3)
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
  foreach(t=1:repdist,.combine=rbind, .inorder=FALSE) %:%
  foreach(j=1:length(DataGen[[i]]),.combine=rbind,.packages=c("plyr","statip","data.table","BinNor","dplyr"), .inorder=FALSE) %dopar% {
    f = function() {
      
      phackingFunction(DataGen[[i]][[j]](sample[[g]],corr[[c]]),"y1","h1",Power_12 =P2[[h]],SD=condSD[[k]],Power_13 = P3[[l]],Main = T,Per = TRUE)
    
      
    }
    d1=data.frame(f(),Power12=h,Power13=l,Power123=2,OutlierExclusion=k,IndependentVariables=j,Type=i,SampleSize=sample[[g]],Main=1,Correlation=corr[[c]],it=t)

    if(h==2 & l==2){
      f = function() {
        
        phackingFunction(DataGen[[i]][[j]](sample[[g]],corr[[c]]),"y1","h1",SD=condSD[[k]],Power_123 = T,Main = T,Per = TRUE)
       
        
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
cl <- makeSOCKcluster(20)
## Using the SNOW packed as this gives the ability to make a process bar
registerDoSNOW(cl)

## Making the process bar. The process bar will stand still towards the end, as it cannot take into account the time mapply will take
pb <- txtProgressBar(max=length(DataGenListBin)*length(DataGen)*length(condSD)*length(P2)*length(P3)*length(sample), style=3)
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
  foreach(t=1:repdist,.combine=rbind, .inorder=FALSE) %:%
  foreach(j=1:length(DataGen[[i]]),.combine=rbind,.packages=c("plyr","statip","data.table","BinNor"), .inorder=FALSE) %dopar% {
    f = function() {
      
   phackingFunction(DataGen[[i]][[j]](sample[[g]],corr[[c]]),"y1","h1",Power_2 =P2[[h]],SD=condSD[[k]],Power_3 = P3[[l]],Main = F,Per = TRUE)
  
      
    }
    data1=data.frame(f(),Power2=h,Power3=l,Power12=2,Power13=2,Power23=2,Power123=2,OutlierExclusion=k,IndependentVariables=j,Type=i,SampleSize=sample[[g]],Main=2,Correlation=corr[[c]],it=t)
    if(h!=2 | l!=2){
    f = function() {
      
      phackingFunction(DataGen[[i]][[j]](sample[[g]],corr[[c]]),"y1","h1",Power_12 =P2[[h]],SD=condSD[[k]],Power_13 = P3[[l]],Main = F,Per = TRUE)
  
      
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
        
       phackingFunction(DataGen[[i]][[j]](sample[[g]],corr[[c]]),"y1","h1",SD=condSD[[k]],Power_123 = T,Main = F,Per = TRUE)
     
        
      }
      data3=data.frame(f(),Power2=h,Power3=l,Power12=2,Power13=2,Power23=2,Power123=1,OutlierExclusion=k,IndependentVariables=j,Type=i,SampleSize=sample[[g]],Main=2,Correlation=corr[[c]],it=t)
      
      f=function(){
        phackingFunction(DataGen[[i]][[j]](sample[[g]],corr[[c]]),"y1","h1",SD=condSD[[k]],Power_23 = T,Main = F,Per = TRUE)
      
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




### Calculate the average dist and make plots for appendix

figuredist<-as.data.table(finalresults[ finalresults$Correlation==0.2 & finalresults$OutlierExclusion==2,])
figuredist<-figuredist[figuredist$Power2==1 & figuredist$Power3==2 & figuredist$Power12==2 & figuredist$Power13==2|
                               figuredist$Power2==2 & figuredist$Power3==1 & figuredist$Power12==2 & figuredist$Power13==2|
                               figuredist$Power2==2 & figuredist$Power3==2 & figuredist$Power12==2 & figuredist$Power13==2|
                               figuredist$Power2==2 & figuredist$Power3==2 & figuredist$Power12==1 & figuredist$Power13==2|
                               figuredist$Power2==2 & figuredist$Power3==2 & figuredist$Power12==2 & figuredist$Power13==1,]



meanDist=figuredist[,list(mean=mean(f..)),by=c("Main","Set","Type")]

