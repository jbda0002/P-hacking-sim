### Master file ###


## Code for p-hacking and simulation
## This code has both a function that can look at interactionterms, multiple dependt variables and there average
## and different "outlier" criterias. These interactions are only with one other variable, 
## but can be generalized such that it takes all interactions but the modelset will explode exponentially

#### Setting up the simulation ####
## Setting working directory
setwd("C:/Users/jbda0002/Dropbox/Uppsala/Projects/P-Hacking paper/R")
#setwd("C:/Users/jbda0002/Documents/Projects/P-hacking/trunk/R")
set.seed(1234)

## Loading library
library(ggplot2)
library(data.table)
library(plyr)
library(reshape2)
library(Rlab)
library(foreach)
library(doParallel)



##### Things that can be changed in the simulation

## Selecting the sample sizes that should be used
sample = c(50,100,150)

## Setting the number of repretetion
rep=20
## Setting the correlation between dependent and independent 
per=0.2



## Loading the outlier Functions used in P-hacking function
source("Analysis/Code/Outlier Analysis.R")

## Loading the P-Hacking function
source("Analysis/Code/phackingFunction.R")

## Making the list for the Normal Data
source("Analysis/Code/Data Generation Normal.R")
DataGenListNorm <- list(dataGen1,dataGen2,dataGen3,dataGen4,dataGen5,dataGen6,dataGen7)

## Making the list for the Bin Data
source("Analysis/Code/Data Generation Bin.R")
DataGenListBin <- list(dataGen1,dataGen2,dataGen3,dataGen4,dataGen5,dataGen6,dataGen7)

## Making the list for the Bin Data
source("Analysis/Code/Data Generation BinNormal.R")
DataGenListBinNorm <- list(dataGen1,dataGen2,dataGen3,dataGen4,dataGen5,dataGen6,dataGen7)

## Making the list for the Bin Data
source("Analysis/Code/Data Generation NormalBin.R")
DataGenListNormBin <- list(dataGen1,dataGen2,dataGen3,dataGen4,dataGen5,dataGen6,dataGen7)


#### Run the simulation ####

## General for all


## The different condetions
condIn<-c("TRUE","FALSE")
condSD<-c("TRUE","FALSE")
condcor<-c("TRUE","FALSE")

## Making a list to store the results
finalresult<-list(finalresultNorm=c(),finalresultBin=c(),finalresultNormBin=c(),finalresultBinNorm=c())

## Collecting the different datatypes in one list
DataGen<-list(m1=DataGenListNorm,m2=DataGenListBin,m3=DataGenListNormBin,m4=DataGenListBinNorm)

cl <- makeCluster(4)
registerDoParallel(cl)
results=NULL
results<-
  foreach(k=1:length(condSD),.combine='rbind') %:%
    foreach(h=1:length(condIn),.combine='rbind') %:%
      foreach(i=1:length(DataGen), .combine='cbind') %:%
        foreach(j=1:length(DataGen[[i]]),.combine=cbind,.packages=c("plyr","statip")) %dopar% {
          f = function() {
          fun = function(x) mean(replicate(rep, phackingFunction(DataGen[[i]][[j]](x,per),"y1","x1",interaction = condIn[[h]] ,SD=condSD[[k]])))
          mapply(x=sample, function(x) fun(x))
    }
    f()
  }
