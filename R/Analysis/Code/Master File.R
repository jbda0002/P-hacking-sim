### Master file ###


## Code for p-hacking and simulation
## This code has both a function that can look at interactionterms, multiple dependt variables and there average
## and different "outlier" criterias. These interactions are only with one other variable, 
## but can be generalized such that it takes all interactions but the modelset will explode exponentially

#### Setting up the simulation ####
## Setting working directory
setwd("C:/Users/jbda0002/Dropbox/Uppsala/Projects/P-Hacking paper/R")
set.seed(1234)

## Loading library
library(ggplot2)
library(data.table)
library(plyr)
library(reshape2)
library(Rlab)

##### Things that can be changed in the simulation

## Selecting the sample sizes that should be used
sample = c(100,200)

## Setting the number of repretetion
rep=10
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

## Simulation ##
# Remember to notice in which order the data comes in
for (k in 1:length(condSD)) {
  for (h in 1:length(condIn)) {
    for (i in 1:length(DataGen)) {
      for(j in 1:length(DataGen[[i]])){
        res = mapply(function(x) mean(replicate(rep, phackingFunction(DataGen[[i]][[j]](x,per),"y1","x1", interaction = condIn[[h]] ,SD=condSD[[k]]))), x=sample)
        result = data.frame(sample,res,j,h,k)
        names(result)<-c("SampleSize","Pr","IndependentVariables","Interaction","OutlierExclusion")
        finalresult[[i]]=rbind(result,finalresult[[i]])
      }
    } 
  }
}

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

## Save the figures
ggsave(figureNormal,"Analysis/Result/Figures/figureNormal.jpeg",width = 580,height = 470)
ggsave(figureBin,"Analysis/Result/Figures/figureBin.jpeg",width = 580,height = 470)
ggsave(figureNormBin,"Analysis/Result/Figures/figureNormBin.jpeg",width = 580,height = 470)
ggsave(figureBinNorm,"Analysis/Result/Figures/figureBinNorm.jpeg",width = 580,height = 470)

## Save the result file
write.csv(finalresult,"Analysis/Result/ResultFile/Results.csv" )