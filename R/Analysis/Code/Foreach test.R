### Master file ###


## Code for p-hacking and simulation
## This code has both a function that can look at interactionterms, multiple dependt variables and there average
## and different "outlier" criterias. These interactions are only with one other variable, 
## but can be generalized such that it takes all interactions but the modelset will explode exponentially

#### Setting up the simulation ####
## Setting working directory
#setwd("C:/Users/jbda0002/Dropbox/Uppsala/Projects/P-Hacking paper/R")
setwd("C:/Users/jbda0002/Documents/Projects/P-hacking/trunk/R")
set.seed(1234)

## Loading library
library(ggplot2)
library(data.table)
library(plyr)
library(reshape2)
library(Rlab)
library(foreach)
library(parallel)
library(doParallel)
library(tidyr)
library(doSNOW)



##### Things that can be changed in the simulation

## Selecting the sample sizes that should be used
sample = c(50,100,150,200,250,300,350)

## Setting the number of repretetion
rep=150
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

cl <- makeCluster(20)
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
    data.frame(Pr=f(),Interaction=h,OutlierExclusion=k,IndependentVariables=j,Type=i,SampleSize=sample)
        }

results<-as.data.frame(results)
stopCluster(cl)

## Normal
finalresultNorm<-rbind(results[c(1:6)],results[c(7:12)],results[13:18],results[19:24],results[25:30],results[31:36],results[37:42])
finalresultBin<-rbind(results[c(43:48)],results[c(49:54)],results[55:60],results[61:66],results[67:72],results[73:78],results[79:84])
finalresultNormBin<-rbind(results[c(85:90)],results[c(91:96)],results[97:102],results[103:108],results[109:114],results[115:120],results[121:126])
finalresultBinNorm<-rbind(results[c(127:132)],results[c(133:138)],results[139:144],results[145:150],results[151:156],results[157:162],results[163:168])



finalresult<-list(finalresultNorm=finalresultNorm,finalresultBin=finalresultBin,finalresultNormBin=finalresultNormBin,finalresultBinNorm=finalresultBinNorm
                )

## Chaning the names in the result file such that they are easier to understand
for (i in 1:length(finalresult)) {
  
  finalresult[[i]]$Interaction[finalresult[[i]]$Interaction == "1"] <- "In TRUE"
  finalresult[[i]]$Interaction[finalresult[[i]]$Interaction == "2"] <- "In FALSE"
  finalresult[[i]]$OutlierExclusion[finalresult[[i]]$OutlierExclusion == "1"] <- "SD TRUE"
  finalresult[[i]]$OutlierExclusion[finalresult[[i]]$OutlierExclusion == "2"] <- "SD FALSE"
}

### Making and saving the different figures 

figureNormal <-ggplot(aes(x=SampleSize, y=Pr, group=IndependentVariables, colour=IndependentVariables), data=finalresult$finalresultNorm)+
  geom_line(aes(colour=as.factor(IndependentVariables)),show.legend = FALSE) +
  geom_point(aes(colour=as.factor(IndependentVariables)),show.legend = FALSE)+
  scale_color_grey()+
  theme_bw()+
  facet_grid(OutlierExclusion~Interaction)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "none")+
  ylab("Percent of simulations with at least one model with significant random variable")+ 
  xlab("Sample size")+
  ggtitle("Depended normal, independet variable normal")+
  theme_classic()

figureNormal


## Bin ##
figureBin <-ggplot(aes(x=SampleSize, y=Pr, group=IndependentVariables, colour=IndependentVariables), data=finalresult$finalresultBin)+
  geom_line(aes(colour=as.factor(IndependentVariables)),show.legend = FALSE) +
  geom_point(aes(colour=as.factor(IndependentVariables)),show.legend = FALSE)+
  scale_color_grey()+
  theme_bw()+
  facet_grid(OutlierExclusion~Interaction)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab("Percent of simulations with at least one model with significant random variable")+
  xlab("Sample size")+
  labs(colour = "Number of Predictors") +
  ggtitle("Dependent variable 1/0, independet variable 1/0")+
  theme_classic()

figureBin

## Normal IV and H_1 bin ##
figureNormBin <-ggplot(aes(x=SampleSize, y=Pr, group=IndependentVariables, colour=IndependentVariables), data=finalresult$finalresultNormBin)+
  geom_line(aes(colour=as.factor(IndependentVariables)),show.legend = FALSE) +
  geom_point(aes(colour=as.factor(IndependentVariables)),show.legend = FALSE)+
  scale_color_grey()+
  theme_bw()+
  facet_grid(OutlierExclusion~Interaction)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab("Percent of simulations with at least one model with significant random variable")+ 
  xlab("Sample size")+
  labs(colour = "Number of Predictors") +
  ggtitle("Dependent variable normal, independet variable 1/0")+
  theme_classic()

figureNormBin

## Normal H_1 and IV bin ##
figureBinNorm <-ggplot(aes(x=SampleSize, y=Pr, group=IndependentVariables, colour=IndependentVariables), data=finalresult$finalresultBinNorm)+
  geom_line(aes(colour=as.factor(IndependentVariables)),show.legend = FALSE) +
  geom_point(aes(colour=as.factor(IndependentVariables)),show.legend = FALSE)+
  scale_color_grey()+
  theme_bw()+
  facet_grid(OutlierExclusion~Interaction)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab("Percent of simulations with at least one model with significant random variable")+
  xlab("Sample size")+
  labs(colour = "Number of Predictors") +
  ggtitle("Dependent variable 1/0, independet variable normal")+
  theme_classic()

figureBinNorm

#### Save the figures and the data

## Save the figures
ggsave(figureNormal,filename =file.path("Analysis/Result/Figures","figureNormal.jpeg"),width = 6.64,height = 5.70)
ggsave(figureBin,filename =file.path("Analysis/Result/Figures","figureBin.jpeg"),width = 6.64,height = 5.70)
ggsave(figureNormBin,filename =file.path("Analysis/Result/Figures","figureNormBin.jpeg"),width = 6.64,height = 5.70)
ggsave(figureBinNorm,filename =file.path("Analysis/Result/Figures","figureBinNorm.jpeg"),width = 6.64,height = 5.70)

## Save the result file
write.csv(finalresult,"Analysis/Result/ResultFile/Results.csv" )

