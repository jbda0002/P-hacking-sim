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
library(parallel)
library(doParallel)
library(tidyr)
library(doSNOW)
library(tcltk)

##### Things that can be changed in the simulation

## Selecting the sample sizes that should be used
sample = c(50,100,150,200,250,300,350,400,450,500)

## Setting the number of repretetion
rep=1000
## Setting the correlation between dependent and independent 
per=0.2
perDV=0.5




## Loading the outlier Functions used in P-hacking function
source("Analysis/Code/Outlier Analysis.R")

## Loading the P-Hacking function
source("Analysis/Code/phackingFunction.R")

## Making the list for the Normal Data
source("Analysis/Code/Data Generation NormalDV.R")
DataGenListNormDV <- list(dataGen1,dataGen2,dataGen3,dataGen4,dataGen5,dataGen6,dataGen7)

## Making the list for the Bin Data
source("Analysis/Code/Data Generation NormalBinDV.R")
DataGenListNormBinDV <- list(dataGen1,dataGen2,dataGen3,dataGen4,dataGen5,dataGen6,dataGen7)


#### Run the simulation ####

## General for all

## The different condetions
condIn<-c("TRUE","FALSE")
condSD<-c("TRUE","FALSE")
condcor<-c("TRUE","FALSE")

## Making a list to store the results
finalresult<-list(finalresultNormDV=c(),finalresultNormBinDV=c())

## Collecting the different datatypes in one list
DataGen<-list(m1=DataGenListNormDV,m2=DataGenListNormBinDV)

## Choosing how many workers there should be used
cl <- makeSOCKcluster(20)

## Using the SNOW packed as this gives the ability to make a process bar
registerDoSNOW(cl)

## Making the process bar. The process bar will stand still towards the end, as it cannot take into account the time mapply will take
pb <- txtProgressBar(max=7*2*2*2*length(sample), style=3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress=progress)

## Running the simulation 

results<-
  foreach(k=1:length(condSD),.combine='rbind') %:%
  foreach(h=1:length(condIn),.combine='rbind') %:%
  foreach(g=1:length(sample), .combine='rbind') %:%
  foreach(i=1:length(DataGen), .combine='cbind') %:%
  foreach(j=1:length(DataGen[[i]]),.combine=cbind,.packages=c("plyr","statip"),.options.snow=opts) %dopar% {
    f = function() {
      mean(replicate(rep, phackingFunction(DataGen[[i]][[j]](sample[[g]],per,perDV),c("y1","y2"),"x1",interaction = condIn[[h]] ,SD=condSD[[k]])))
      
    }
    data.frame(Pr=f(),Interaction=h,OutlierExclusion=k,IndependentVariables=j,Type=i,SampleSize=sample[[g]])
  }

results<-as.data.frame(results)





## Close process bar
close(pb)

## Stop the workers 
stopCluster(cl); print("Cluster stopped.")

# insert serial backend, otherwise error in repetetive tasks
registerDoSEQ()

## Splitting the data into the different types of data
finalresultNormDV<-rbind(results[c(1:6)],results[c(7:12)],results[13:18],results[19:24],results[25:30],results[31:36],results[37:42])
finalresultNormBinDV<-rbind(results[c(43:48)],results[c(49:54)],results[55:60],results[61:66],results[67:72],results[73:78],results[79:84])


## Putting them into a list
finalresult<-list(finalresultNormDV=finalresultNormDV,finalresultNormBinDV=finalresultNormBinDV)

## Chaning the names in the result file such that they are easier to understand
for (i in 1:length(finalresult)) {
  
  finalresult[[i]]$Interaction[finalresult[[i]]$Interaction == "1"] <- "In TRUE"
  finalresult[[i]]$Interaction[finalresult[[i]]$Interaction == "2"] <- "In FALSE"
  finalresult[[i]]$OutlierExclusion[finalresult[[i]]$OutlierExclusion == "1"] <- "SD TRUE"
  finalresult[[i]]$OutlierExclusion[finalresult[[i]]$OutlierExclusion == "2"] <- "SD FALSE"
}

### Making and saving the different figures 

#### Figures ####
## Normal ##
figureNormalDV <-ggplot(aes(x=SampleSize, y=Pr, group=IndependentVariables, colour=IndependentVariables), data=finalresult$finalresultNormDV)+
  geom_line(aes(colour=as.factor(IndependentVariables)),show.legend = FALSE) +
  geom_point(aes(colour=as.factor(IndependentVariables)),show.legend = FALSE)+
  scale_color_grey()+
  theme_bw()+
  facet_grid(OutlierExclusion~Interaction)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "none")+
  ylab("corcent of simulations with at least one model with significant random variable")+ 
  xlab("Sample size")+
  ggtitle("Depended normal, independet variable normal")+
  theme_classic()

figureNormBinDV <-ggplot(aes(x=SampleSize, y=Pr, group=IndependentVariables, colour=IndependentVariables), data=finalresult$finalresultNormBinDV)+
  geom_line(aes(colour=as.factor(IndependentVariables)),show.legend = FALSE) +
  geom_point(aes(colour=as.factor(IndependentVariables)),show.legend = FALSE)+
  scale_color_grey()+
  theme_bw()+
  facet_grid(OutlierExclusion~Interaction)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab("corcent of simulations with at least one model with significant random variable")+  
  xlab("Sample size")+
  labs(colour = "Number of Predictors") +
  ggtitle("Dependent variable normal, independet variable 1/0")+
  theme_classic()

figureNormalDV
figureNormBinDV

## Save the figures
ggsave(figureNormalDV,filename =file.path("Analysis/Result/Figures","figureNormalDV.jpeg"),width = 6.64,height = 5.70)
ggsave(figureNormBinDV,filename =file.path("Analysis/Result/Figures","figureNormalBinDV.jpeg"),width = 6.64,height = 5.70)

## Save the result file
write.csv(finalresult,"Analysis/Result/ResultFile/ResultsDV.csv" )

