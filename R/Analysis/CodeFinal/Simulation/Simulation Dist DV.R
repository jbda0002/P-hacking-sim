### Master file Dist 2.0###
# here for several dependent variables

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

sample = c(200)

## Setting the number of repretetion
repdist=1000
## Setting the correlation between dependent and independent 
corr=c(0.2,0.3,0.4)
corrDV=0.5


## Due to memory problems only run dataGen4 by itself! 

## Loading the P-Hacking function
source(here::here("CodeFinal","phackingFunction_v2-0.R"))


### Here it is added how many covariates there should be in the simulation
## Making the list for the Normal Data
source(here::here("CodeFinal","Data","Data Generation NormalDV.R"))
DataGenListNorm <- list(dataGen2,dataGen3)

## Making the list for the Bin Data
source(here::here("CodeFinal","Data","Data Generation BinDV_v2-0.R"))
DataGenListBin <- list(dataGen2,dataGen3)

## Making the list for the Bin Data
source(here::here("CodeFinal","Data","Data Generation BinNormalDV_v2-0.R"))
DataGenListBinNorm <- list(dataGen2,dataGen3)

## Making the list for the Bin Data
source(here::here("CodeFinal","Data","Data Generation NormalBinDV.R"))
DataGenListNormBin <- list(dataGen2,dataGen3)



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
cl <- makeSOCKcluster(23)

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
cl <- makeSOCKcluster(23)
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
finalresults=finalresults[!finalresults$Set==0,]

### Calculate the average dist and make plots for appendix

## Add for the different levels of covariates and how sample size affects this kind of 

figuredist<-as.data.table(finalresults)
figuredist<-figuredist[figuredist$Power2==1 & figuredist$Power3==2 & figuredist$Power12==2 & figuredist$Power13==2|
                         figuredist$Power2==2 & figuredist$Power3==1 & figuredist$Power12==2 & figuredist$Power13==2|
                         figuredist$Power2==2 & figuredist$Power3==2 & figuredist$Power12==2 & figuredist$Power13==2|
                         figuredist$Power2==2 & figuredist$Power3==2 & figuredist$Power12==1 & figuredist$Power13==2|
                         figuredist$Power2==2 & figuredist$Power3==2 & figuredist$Power12==2 & figuredist$Power13==1,]


## Percent of models with significant effect
meanDist=figuredist[,list(mean=mean(f..)),by=c("Main","Set","Type","SampleSize", "IndependentVariables","OutlierExclusion","Correlation")]


write.csv(meanDist,'meanDistSampleDV.csv')

## Create Table 1
figuredist<-as.data.table(finalresults[finalresults$Correlation ==0.2 & finalresults$IndependentVariables==1
                                       & finalresults$Type!=3 & finalresults$Type!=4 & finalresults$Type!=5 & finalresults$Type!=6,])
figuredist<-figuredist[figuredist$Power2==1 & figuredist$Power3==2 & figuredist$Power12==2 & figuredist$Power13==2|
                         figuredist$Power2==2 & figuredist$Power3==1 & figuredist$Power12==2 & figuredist$Power13==2|
                         figuredist$Power2==2 & figuredist$Power3==2 & figuredist$Power12==2 & figuredist$Power13==2|
                         figuredist$Power2==2 & figuredist$Power3==2 & figuredist$Power12==1 & figuredist$Power13==2|
                         figuredist$Power2==2 & figuredist$Power3==2 & figuredist$Power12==2 & figuredist$Power13==1,]

Table1=figuredist[,list(mean=mean(f..)),by=c("Main","Set","Type")]

## Effect of using an extra covariate (figure for SM)


figuredist<-as.data.table(finalresults[finalresults$SampleSize==200 & finalresults$Correlation ==0.2 & finalresults$OutlierExclusion==2
                                       & finalresults$Type!=3 & finalresults$Type!=4 & finalresults$Type!=5 & finalresults$Type!=6,])
figuredist<-figuredist[figuredist$Power2==1 & figuredist$Power3==2 & figuredist$Power12==2 & figuredist$Power13==2|
                         figuredist$Power2==2 & figuredist$Power3==1 & figuredist$Power12==2 & figuredist$Power13==2|
                         figuredist$Power2==2 & figuredist$Power3==2 & figuredist$Power12==2 & figuredist$Power13==2|
                         figuredist$Power2==2 & figuredist$Power3==2 & figuredist$Power12==1 & figuredist$Power13==2|
                         figuredist$Power2==2 & figuredist$Power3==2 & figuredist$Power12==2 & figuredist$Power13==1,]

meanDist=figuredist[,list(mean=mean(f..)),by=c("Main","Set","Type","IndependentVariables")]
first=meanDist[meanDist$IndependentVariables==2]
second=meanDist[meanDist$IndependentVariables==1]

meanDistCov <- merge(first,second, by=c("Main","Set","Type")) 
meanDistCov$diff=meanDistCov$mean.x-meanDistCov$mean.y


meanDistCov$Main[meanDistCov$Main==1]<-"Main = TRUE"
meanDistCov$Main[meanDistCov$Main==2]<-"Main = FALSE"
meanDistCov$Type[meanDistCov$Type==1]<-"h1=Normal, Co=Normal"
meanDistCov$Type[meanDistCov$Type==2]<-"h1=Binary, Co=Binary"



meanDistCov$Set <- factor(meanDistCov$Set,levels = c("Ma", "HCI", "CCI", "Ma + HCI","Ma + CCI","HCI + CCI","Ma + HCI + CCI"))


Figure2_SM = ggplot(meanDistCov)+
  geom_bar(aes(x=Set,y=diff), stat = "identity",position="dodge")+
  facet_grid(Type~Main)+
  theme_apa()+
  xlab("Model set")+
  ylab("Difference between false-positive rate")+
  theme(axis.text.x = element_text(color = "grey20", size = 17, angle = 65, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 17, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 17, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 17, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        strip.text.x = element_text(color = "grey20", size = 15, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        strip.text.y = element_text(color = "grey20", size = 15, angle = 90, hjust = .5, vjust = .5, face = "plain"))
Figure2_SM


### The effect of an increase in sample (Figure to SM)


figuredist<-as.data.table(finalresults[finalresults$IndependentVariables==1 & finalresults$Correlation ==0.2 & finalresults$OutlierExclusion==2
                                       & finalresults$Type!=3 & finalresults$Type!=4 & finalresults$Type!=5 & finalresults$Type!=6,])
figuredist<-figuredist[figuredist$Power2==1 & figuredist$Power3==2 & figuredist$Power12==2 & figuredist$Power13==2|
                         figuredist$Power2==2 & figuredist$Power3==1 & figuredist$Power12==2 & figuredist$Power13==2|
                         figuredist$Power2==2 & figuredist$Power3==2 & figuredist$Power12==2 & figuredist$Power13==2|
                         figuredist$Power2==2 & figuredist$Power3==2 & figuredist$Power12==1 & figuredist$Power13==2|
                         figuredist$Power2==2 & figuredist$Power3==2 & figuredist$Power12==2 & figuredist$Power13==1,]

meanDist=figuredist[,list(mean=mean(f..)),by=c("Main","Set","Type","SampleSize")]

## Creating the different levels of the sample 
a1=meanDist[meanDist$SampleSize==100]
a2=meanDist[meanDist$SampleSize==150]
a3=meanDist[meanDist$SampleSize==200]
a4=meanDist[meanDist$SampleSize==250]
a5=meanDist[meanDist$SampleSize==300]

a1=merge(a1,a2, by=c("Main","Set","Type"))
a3=merge(a3,a4, by=c("Main","Set","Type"))
a1=merge(a1,a3, by=c("Main","Set","Type"))

meanDistSample=merge(a1,a5, by=c("Main","Set","Type"))

## Make the increase from an increase in sample
meanDistSample$Inc1=meanDistSample$mean.y.x-meanDistSample$mean.x.x
meanDistSample$Inc2=meanDistSample$mean.x.y-meanDistSample$mean.y.x
meanDistSample$Inc3=meanDistSample$mean.y.y-meanDistSample$mean.x.y
meanDistSample$Inc4=meanDistSample$mean-meanDistSample$mean.y.y

## Make the new data file for the plotting

first=meanDistSample[,c(1:3, 14)]
first$sample="100 - 150"
second=meanDistSample[,c(1:3, 15)]
second$sample="150 - 200"
third=meanDistSample[,c(1:3, 16)]
third$sample="200 - 250"
fourth=meanDistSample[,c(1:3, 17)]
fourth$sample="250 - 300"

## Rename
names(first)[4]="Inc"
names(second)[4]="Inc"
names(third)[4]="Inc"
names(fourth)[4]="Inc"

## Put together

meanDistSampleInc=do.call("rbind",list(first,second,third,fourth))

meanDistSampleInc$Main[meanDistSampleInc$Main==1]<-"Main = TRUE"
meanDistSampleInc$Main[meanDistSampleInc$Main==2]<-"Main = FALSE"
meanDistSampleInc$Type[meanDistSampleInc$Type==1]<-"h1=Normal, Co=Normal"
meanDistSampleInc$Type[meanDistSampleInc$Type==2]<-"h1=Binary, Co=Binary"



meanDistSampleInc$Set <- factor(meanDistSampleInc$Set,levels = c("Ma", "HCI", "CCI", "Ma + HCI","Ma + CCI","HCI + CCI","Ma + HCI + CCI"))


Figure3_SM<-ggplot(aes(x=sample, y=Inc,group = 1), data=meanDistSampleInc)+
  geom_line() +
  geom_point()+
  scale_color_grey()+
  facet_grid(Set~Main+Type)+
  ylab("")+
  xlab("Sample size increase")+
  theme_apa()+
  geom_hline(yintercept=0)+
  theme(axis.text.x = element_text(color = "grey20", size = 17, angle = 65, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 17, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 17, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 17, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        strip.text.x = element_text(color = "grey20", size = 15, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        strip.text.y = element_text(color = "grey20", size = 15, angle = 90, hjust = .5, vjust = .5, face = "plain"))

Figure3_SM


## The effect of higher correlations (Figure to SM)

## Calculate false-positive rates when using several dependent variables

falsepositverateData<-as.data.table(finalresults)
falsepositverateData<-falsepositverateData[falsepositverateData$Power2==1 & falsepositverateData$Power3==2 & falsepositverateData$Power12==2 & falsepositverateData$Power13==2|
                                             falsepositverateData$Power2==2 & falsepositverateData$Power3==1 & falsepositverateData$Power12==2 & falsepositverateData$Power13==2|
                                             falsepositverateData$Power2==2 & falsepositverateData$Power3==2 & falsepositverateData$Power12==2 & falsepositverateData$Power13==2|
                                             falsepositverateData$Power2==2 & falsepositverateData$Power3==2 & falsepositverateData$Power12==1 & falsepositverateData$Power13==2|
                                             falsepositverateData$Power2==2 & falsepositverateData$Power3==2 & falsepositverateData$Power12==2 & falsepositverateData$Power13==1,]

falsepositverateData$rate=ifelse(falsepositverateData$f..>0,1,0)

falsepostive=falsepositverateData[,list(mean=mean(rate)),by=c("Main","Set","Type","SampleSize","OutlierExclusion","Correlation","IndependentVariables")]

## Figures


### Figure 1

figureonedata<-falsepostive[falsepostive$SampleSize==200 & falsepostive$IndependentVariables==2,]

figureonedata$Main[figureonedata$Main==1]<-"Main = TRUE"
figureonedata$Main[figureonedata$Main==2]<-"Main = FALSE"
figureonedata$Type[figureonedata$Type==1]<-"h1=Normal, Co=Normal"
figureonedata$Type[figureonedata$Type==2]<-"h1=Binary, Co=Binary"
figureonedata$Type[figureonedata$Type==1]<-"h1=Normal, Co=Normal"
figureonedata$Type[figureonedata$Type==2]<-"h1=Binary, Co=Binary"
figureonedata$Type[figureonedata$Type==3]<-"h1=Normal, Co=Binary"
figureonedata$Type[figureonedata$Type==4]<-"h1=Binary, Co=Normal"
figureonedata$Type[figureonedata$Type==5]<-"h1=Binary, Co=Binary Effect"
figureonedata$Type[figureonedata$Type==6]<-"h1=Normal, Co=Binary Effect"



figureonedata$Set <- factor(figureonedata$Set,levels = c("Ma", "HCI", "CCI", "Ma + HCI","Ma + CCI","HCI + CCI","Ma + HCI + CCI"))
figureonedata$Pr<-as.numeric(figureonedata$mean)
Figure1 = ggplot(figureonedata)+
  geom_bar(aes(x=Set,y=Pr), stat = "identity",position="dodge")+
  facet_grid(Type~Main+Correlation)+
  theme_apa()+
  xlab("Model set")+
  ylab("False-positive rate")+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 65, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 10, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        strip.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        strip.text.y = element_text(color = "grey20", size = 10, angle = 90, hjust = .5, vjust = .5, face = "plain"))


Figure1


### The effect of sample size

figurethreedata<-falsepostive[ falsepostive$Correlation==0.2   & falsepostive$IndependentVariables==2
                               & falsepostive$Type!=3 & falsepostive$Type!=4 & falsepostive$Type!=5 & falsepostive$Type!=6,]

figurethreedata$Main[figurethreedata$Main==1]<-"Main = TRUE"
figurethreedata$Main[figurethreedata$Main==2]<-"Main = FALSE"
figurethreedata$Type[figurethreedata$Type==1]<-"h1=Normal, Co=Normal"
figurethreedata$Type[figurethreedata$Type==2]<-"h1=Binary, Co=Binary"



figurethreedata$Set <- factor(figurethreedata$Set,levels = c("Ma", "HCI", "CCI", "Ma + HCI","Ma + CCI","HCI + CCI","Ma + HCI + CCI"))
figurethreedata$Pr<-as.numeric(figurethreedata$mean)


Figure3<-ggplot(aes(x=SampleSize, y=Pr), data=figurethreedata[figurethreedata$Type=="h1=Binary, Co=Binary",])+
  geom_line(aes(colour=as.factor(IndependentVariables)),show.legend = FALSE) +
  geom_point(aes(colour=as.factor(IndependentVariables)),show.legend = FALSE)+
  scale_color_grey()+
  facet_grid(Set~Main)+
  ylab("")+
  xlab("Sample size")+
  theme_apa()+
  theme(axis.text.x = element_text(color = "grey20", size = 17, angle = 65, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 17, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 17, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 17, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        strip.text.x = element_text(color = "grey20", size = 15, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        strip.text.y = element_text(color = "grey20", size = 15, angle = 90, hjust = .5, vjust = .5, face = "plain"))

Figure3
