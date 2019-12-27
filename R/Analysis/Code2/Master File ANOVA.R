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
library(tidyr)
library(doSNOW)
library(tcltk)
library(parallel)
library(doParallel)
library(foreach)



##### Things that can be changed in the simulation

## Selecting the sample sizes that should be used
sample = c(50,100,150)

## Setting the number of repretetion
rep=100
## Setting the correlation between dependent and independent 
per=0.2




## Loading the P-Hacking function
#source(here::here("phackingFunction_v2-0.R"))

## Making the list for the Normal Data
source("Data Generation Normal.R")
DataGenListNorm <- list(dataGen2,dataGen3)

## Making the list for the Bin Data
source("Data Generation Bin.R")
DataGenListBin <- list(dataGen2,dataGen3)

## Making the list for the Bin Data
source("Data Generation BinNormal.R")
DataGenListBinNorm <- list(dataGen2,dataGen3)

## Making the list for the Bin Data
source("Data Generation NormalBin.R")
DataGenListNormBin <- list(dataGen2,dataGen3)


#### Run the simulation ####

## General for all


## The different condetions
P2<-c("TRUE","FALSE")
P3<-c("TRUE","FALSE")
condSD<-c("FALSE")
condcor<-c("TRUE","FALSE")

## Making a list to store the results
finalresult<-list(finalresultNorm=c(),finalresultBin=c(),finalresultNormBin=c(),finalresultBinNorm=c())

## Collecting the different datatypes in one list
DataGen<-list(m1=DataGenListNorm,m2=DataGenListBin,m3=DataGenListNormBin,m4=DataGenListBinNorm)

## Choosing how many workers there should be used
cl <- makeSOCKcluster(7)
#clusterExport(cl,c("DataGen","sample","condIn","condSD"))
## Using the SNOW packed as this gives the ability to make a process bar
registerDoSNOW(cl)

## Making the process bar. The process bar will stand still towards the end, as it cannot take into account the time mapply will take
pb <- txtProgressBar(max=length(DataGenListBin)*length(DataGen)*length(condSD)*length(P2)*length(P3)*length(sample), style=3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress=progress)


results<-
  foreach(k=1:length(condSD),.combine=rbind) %:%
  foreach(h=1:length(P2),.combine=rbind, .inorder=FALSE) %:%
  foreach(l=1:length(P3),.combine=rbind, .inorder=FALSE) %:%
  foreach(g=1:length(sample),.combine=rbind, .inorder=FALSE) %:%
  foreach(i=1:length(DataGen),.combine=rbind, .inorder=F) %:%
  foreach(j=1:length(DataGen[[i]]),.combine=rbind,.packages=c("plyr","statip","data.table"), .inorder=FALSE) %dopar% {
    f = function() {
      
      x<-replicate(rep, phackingFunctionA(DataGen[[i]][[j]](sample[[g]],per),"y1","h1",Power_2 =P2[[h]],SD=F,Power_3 = P3[[l]],Per=F),simplify = F)
      d<-as.data.table(t(as.data.table(x)))
      Stats<-as.data.table(sapply(d, function(x)c(mean = mean(x))))
      #Stats<-t(c(mean(x),(sd(x)/sqrt(length(x)))))
      ret<-list("lm"=Stats[1,1],"anova"=Stats[2,1])
      return(ret)
      
    }
    
    data.frame(f()$lm,f()$anova,Power2=h,Power3=l,OutlierExclusion=1,IndependentVariables=j,Type=i,SampleSize=sample[[g]])
  }




results<-as.data.frame(results)
names(results)[1]<-"LM"
names(results)[2]<-"ANOVA"

finalresult <- reshape2::melt(results, id.vars=c("Power2","Power3","OutlierExclusion","IndependentVariables","Type","SampleSize"), 
                      measure.vars=c("LM", "ANOVA"))
## Close process bar
close(pb)

## Stop the workers 
stopCluster(cl);print("Cluster stopped")
registerDoSEQ()
results

## Splitting the data into the different types of data

finalresultNorm<-finalresult[finalresult$Type==1,]
finalresultBin<-finalresult[finalresult$Type==2,]
finalresultNormBin<-finalresult[finalresult$Type==3,]
finalresultBinNorm<-finalresult[finalresult$Type==4,]


## Putting them into a list
finalresult<-list(finalresultNorm=finalresultNorm,finalresultBin=finalresultBin,finalresultNormBin=finalresultNormBin,finalresultBinNorm=finalresultBinNorm
                )

## Chaning the names in the result file such that they are easier to understand
for (i in 1:length(finalresult)) {
  
  finalresult[[i]]$Power2[finalresult[[i]]$Power2 == "1"] <- "Power2 True"
  finalresult[[i]]$Power2[finalresult[[i]]$Power2 == "2"] <- "Power2 FALSE"
  finalresult[[i]]$Power3[finalresult[[i]]$Power3 == "1"] <- "Power3 TRUE"
  finalresult[[i]]$Power3[finalresult[[i]]$Power3 == "2"] <- "Power3 FALSE"
  finalresult[[i]]$OutlierExclusion[finalresult[[i]]$OutlierExclusion == "1"] <- "SD TRUE"
  finalresult[[i]]$OutlierExclusion[finalresult[[i]]$OutlierExclusion == "2"] <- "SD FALSE"
}

### Making and saving the different figures 

figureNormal <-ggplot(aes(x=SampleSize, y=value, group=as.factor(IndependentVariables):variable, colour=as.factor(IndependentVariables):variable), data=finalresult$finalresultNorm)+
  geom_line(aes(colour=as.factor(IndependentVariables):variable),show.legend = T) +
  geom_point(aes(colour=as.factor(IndependentVariables):variable),show.legend = FALSE)+
  theme_bw()+
  facet_grid(Power2~Power3)+
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
  facet_grid(Power2~Power3)+
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
  facet_grid(Power2~Power3)+
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
  facet_grid(Power2~Power3)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab("Percent of simulations with at least one model with significant random variable")+
  xlab("Sample size")+
  labs(colour = "Number of Predictors") +
  ggtitle("Dependent variable 1/0, independet variable normal")+
  theme_classic()

figureBinNorm

#### Save the figures and the data

## Save the figures
ggsave(figureNormal,filename =file.path("Analysis/Result/Figures","figureNormalNew.jpeg"),width = 6.64,height = 5.70)
ggsave(figureBin,filename =file.path("Analysis/Result/Figures","figureBinNew.jpeg"),width = 6.64,height = 5.70)
ggsave(figureNormBin,filename =file.path("Analysis/Result/Figures","figureNormBinNew.jpeg"),width = 6.64,height = 5.70)
ggsave(figureBinNorm,filename =file.path("Analysis/Result/Figures","figureBinNormNew.jpeg"),width = 6.64,height = 5.70)

## Save the result file
write.csv(finalresult,"Analysis/Result/ResultFile/ResultsNew.csv" )



#### If Per= TRUE run this code instead for plotting the data

### Making and saving the different figures 
library(scales)
histNormal<-ggplot(aes(x=dist, colour=IndependentVariables),data = finalresult$finalresultNorm)+
  facet_grid(Power2~Power3+IndependentVariables)+
  geom_histogram(binwidth = 0.05,aes(y=length(DataGenListBin)*2*2*..count../sum(..count..)),show.legend = FALSE)+
  theme_bw()+
  theme_classic()+
  ylab("Percentage")+
  scale_y_continuous(limits = c(0, 1))
histNormal


histBin<-ggplot(aes(x=dist, colour=IndependentVariables),data = finalresult$finalresultBin)+
  facet_grid(Power2~Power3+IndependentVariables)+
  geom_histogram(binwidth = 0.05,aes(y=length(DataGenListBin)*2*2*..count../sum(..count..)),show.legend = FALSE)+
  theme_bw()+
  theme_classic()+
  ylab("Percentage")+
  scale_y_continuous(limits = c(0, 1))
histBin

histBinNorm<-ggplot(aes(x=dist, colour=IndependentVariables),data = finalresult$finalresultBinNorm)+
  facet_grid(Power2~Power3+IndependentVariables)+
  geom_histogram(binwidth = 0.05,aes(y=length(DataGenListBin)*2*2*..count../sum(..count..)),show.legend = FALSE)+
  theme_bw()+
  theme_classic()+
  ylab("Percentage")+
  scale_y_continuous(limits = c(0, 1))
histBinNorm


histNormBin<-ggplot(aes(x=dist, colour=IndependentVariables),data = finalresult$finalresultNormBin)+
  facet_grid(Power2~Power3+IndependentVariables)+
  geom_histogram(binwidth = 0.05,aes(y=length(DataGenListBin)*2*2*..count../sum(..count..)),show.legend = FALSE)+
  theme_bw()+
  theme_classic()+
  ylab("Percentage")+
  scale_y_continuous(limits = c(0, 1))
histNormBin

ggsave(histNormal,filename =file.path("Analysis/Result/Figures","figureNormalDist.jpeg"),width = 18,height = 9)
ggsave(histBin,filename =file.path("Analysis/Result/Figures","figureBinDist.jpeg"),width = 18,height = 9)
ggsave(histNormBin,filename =file.path("Analysis/Result/Figures","figureNormBinDist.jpeg"),width = 18,height = 9)
ggsave(histBinNorm,filename =file.path("Analysis/Result/Figures","figureBinNormDist.jpeg"),width = 18,height = 9)




