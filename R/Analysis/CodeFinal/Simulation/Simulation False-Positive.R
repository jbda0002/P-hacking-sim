### Master file ###
library(here)

## Code for p-hacking and simulation

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




##### Things that can be changed in the simulation

## Selecting the sample sizes that should be used
sample = c(100,150)

## Setting the number of repretetion
rep=10
## Setting the correlation between dependent and independent 
corr=c(0.2,0.3,0.4)




## Loading the P-Hacking function
source(here::here("CodeFinal","phackingFunction_v2-0.R"))


### Here it is added how many covariates there should be in the simulation
## Making the list for the Normal Data
source(here::here("CodeFinal","Data","Data Generation Normal.R"))
DataGenListNorm <- list(dataGen2,dataGen3,dataGen4)

## Making the list for the Bin Data
source(here::here("CodeFinal","Data","Data Generation Bin_v2-0.R"))
DataGenListBin <- list(dataGen2,dataGen3,dataGen4)

## Making the list for the Bin Data
source(here::here("CodeFinal","Data","Data Generation BinNormal_v2-0.R"))
DataGenListBinNorm <- list(dataGen2,dataGen3,dataGen4)

## Making the list for the Bin Data
source(here::here("CodeFinal","Data","Data Generation NormalBin.R"))
DataGenListNormBin <- list(dataGen2,dataGen3,dataGen4)

## Making the list for the Bin Data with effect coding
source(here::here("CodeFinal","Data","Data Generation Bin effect coding_v2-0.R"))
DataGenListBinEffect <- list(dataGen2,dataGen3,dataGen4)

## Making the list for the Bin Data with effect coding
source(here::here("CodeFinal","Data","Data Generation BinNormal effect coding_v2-0.R"))
DataGenListBinNormEffect <- list(dataGen2,dataGen3,dataGen4)


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
DataGen<-list(m1=DataGenListNorm,m2=DataGenListBin,m3=DataGenListNormBin,m4=DataGenListBinNorm,m4=DataGenListBinEffect,m5=DataGenListBinNormEffect)

## Choosing how many workers there should be used
cl <- makeSOCKcluster(20)

## Using the SNOW packed as this gives the ability to make a process bar
registerDoSNOW(cl)

## Making the process bar. The process bar will stand still towards the end, as it cannot take into account the time mapply will take
pb <- txtProgressBar(max=length(DataGenListBin)*length(DataGen)*length(condSD)*length(P2)*length(P3)*length(sample), style=3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress=progress)


resultsMT<-
  foreach(k=1:length(condSD),.combine=rbind) %:%
  foreach(h=1:length(P2),.combine=rbind, .inorder=FALSE) %:%
  foreach(c=1:length(corr),.combine=rbind, .inorder=FALSE) %:%
  foreach(l=1:length(P3),.combine=rbind, .inorder=FALSE) %:%
  foreach(g=1:length(sample),.combine=rbind, .inorder=FALSE) %:%
  foreach(i=1:length(DataGen),.combine=rbind, .inorder=F) %:%
  foreach(j=1:length(DataGen[[i]]),.combine=rbind,.packages=c("plyr","statip","data.table","BinNor"), .inorder=FALSE) %dopar% {
   
    ## Simulation for set: Ma, Ma + HCI and Ma + CCI
     f = function() {
      
      x<-replicate(rep, phackingFunction(DataGen[[i]][[j]](sample[[g]],corr[[c]]),"y1","h1",Ma_HCI =P2[[h]],outlierexclusion=condSD[[k]],Ma_CCI = P3[[l]],Main = T))
      Stats<-t(c(mean(x),(sd(x)/sqrt(length(x)))))
      Stats
      
     }
     ## Simulation for set: MA + HCI + CCI
    if(h==2 & l==2){
      u = function() {
        
        x<-replicate(rep, phackingFunction(DataGen[[i]][[j]](sample[[g]],corr[[c]]),"y1","h1",outlierexclusion=condSD[[k]],Ma_HCI_CCI = T,Main = T))
        Stats<-t(c(mean(x),(sd(x)/sqrt(length(x)))))
       
        Stats
        
      }
    }
    else{
      u = function() {
        nothing=t(list(NA,NA))
        nothing
      }
    }
   
    d1=data.frame(f(),Power12=h,Power13=l,Power123=2,OutlierExclusion=k,IndependentVariables=j,Type=i,SampleSize=sample[[g]],Main=1,Correlation=corr[[c]])
    d2=data.frame(u(),Power12=h,Power13=l,Power123=1,OutlierExclusion=k,IndependentVariables=j,Type=i,SampleSize=sample[[g]],Main=1,Correlation=corr[[c]])
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


resultsMF<-
  foreach(k=1:length(condSD),.combine=rbind) %:%
  foreach(h=1:length(P2),.combine=rbind, .inorder=FALSE) %:%
  foreach(c=1:length(corr),.combine=rbind, .inorder=FALSE) %:%
  foreach(l=1:length(P3),.combine=rbind, .inorder=FALSE) %:%
  foreach(g=1:length(sample),.combine=rbind, .inorder=FALSE) %:%
  foreach(i=1:length(DataGen),.combine=rbind, .inorder=F) %:%
  foreach(j=1:length(DataGen[[i]]),.combine=rbind,.packages=c("plyr","statip","data.table","BinNor"), .inorder=FALSE) %dopar% {
    
    ## Simulation for set: MA, HCI and CCI
    f = function() {
      
      x<-replicate(rep, phackingFunction(DataGen[[i]][[j]](sample[[g]],corr[[c]]),"y1","h1",HCI =P2[[h]],outlierexclusion=condSD[[k]],CCI = P3[[l]],Main = F))
      Stats<-t(c(mean(x),(sd(x)/sqrt(length(x)))))
      Stats
      
    }
    data1=data.frame(f(),Power2=h,Power3=l,Power12=2,Power13=2,Power23=2,Power123=2,OutlierExclusion=k,IndependentVariables=j,Type=i,SampleSize=sample[[g]],Main=2,Correlation=corr[[c]])
    
    ## Simulation for set: Ma + HCI and Ma + CCI
    if(h!=2 | l!=2){
    u = function() {
      
      x<-replicate(rep, phackingFunction(DataGen[[i]][[j]](sample[[g]],corr[[c]]),"y1","h1",Ma_HCI =P2[[h]],outlierexclusion=condSD[[k]],Ma_CCI = P3[[l]],Main = F))
      Stats<-t(c(mean(x),(sd(x)/sqrt(length(x)))))
      Stats
      
    }
    }
    else{
      u = function() {
        nothing=t(list(NA,NA))
        
        nothing
      }
      
    }
    
    data2=data.frame(u(),Power2=2,Power3=2,Power12=h,Power13=l,Power23=2,Power123=2,OutlierExclusion=k,IndependentVariables=j,Type=i,SampleSize=sample[[g]],Main=2,Correlation=corr[[c]])
    
    
    ## Simulation for set: Ma + HCI + CCI and HCI + CCI
     if(h==2 & l==2){
      u = function() {
        
        x<-replicate(rep, phackingFunction(DataGen[[i]][[j]](sample[[g]],corr[[c]]),"y1","h1",outlierexclusion=condSD[[k]],Ma_HCI_CCI = T,Main = F))
        Stats<-t(c(mean(x),(sd(x)/sqrt(length(x)))))
        
        return(Stats)
        
      }
      b=function(){
        x<-replicate(rep, phackingFunction(DataGen[[i]][[j]](sample[[g]],corr[[c]]),"y1","h1",outlierexclusion=condSD[[k]],HCI_CCI = T,Main = F))
        Stats<-t(c(mean(x),(sd(x)/sqrt(length(x)))))
        return(Stats)
      }
    }
    else{
      u = function() {
        nothing=t(list(NA,NA))
        
        nothing
      }
      b = function() {
        nothing=t(list(NA,NA))
        
        nothing
      }
    }
    
    data3=data.frame(u(),Power2=h,Power3=l,Power12=2,Power13=2,Power23=2,Power123=1,OutlierExclusion=k,IndependentVariables=j,Type=i,SampleSize=sample[[g]],Main=2,Correlation=corr[[c]])
    data4=data.frame(b(),Power2=h,Power3=l,Power12=2,Power13=2,Power23=1,Power123=2,OutlierExclusion=k,IndependentVariables=j,Type=i,SampleSize=sample[[g]],Main=2,Correlation=corr[[c]])
    
    
    return(rbind(data1,data2,data3,data4))
    
  }


## Close process bar
close(pb)

## Stop the workers 
stopCluster(cl)
registerDoSEQ()

resultsMF<-resultsMF[!(resultsMF$X1=="NA"),]
resultsMT<-resultsMT[!(resultsMT$X1=="NA"),]


finalresults<-dplyr::bind_rows(resultsMT, resultsMF)
finalresults[is.na(finalresults)] <- 2
names(finalresults)[1]<-"Pr"
names(finalresults)[2]<-"Sde"
#finalresults$IndependentVariables=finalresults$IndependentVariables + 1

## Creating the different Sets ## 

finalresults$Set<-ifelse(finalresults$Power2==1 & finalresults$Power3==2 & finalresults$Power12==2 & finalresults$Power13==2 & finalresults$Power123==2 & finalresults$Power23==2, "HCI",
                            ifelse(finalresults$Power2==2 & finalresults$Power3==2 & finalresults$Power12==2 & finalresults$Power13==2& finalresults$Power123==2 & finalresults$Power23==2,"Ma",
                                   ifelse(finalresults$Power2==2 & finalresults$Power3==1 & finalresults$Power12==2 & finalresults$Power13==2& finalresults$Power123==2 & finalresults$Power23==2, "CCI",
                                          ifelse(finalresults$Power2==2 & finalresults$Power3==2 & finalresults$Power12==1 & finalresults$Power13==2& finalresults$Power123==2 & finalresults$Power23==2,"Ma + HCI",
                                                 ifelse(finalresults$Power2==2 & finalresults$Power3==2 & finalresults$Power12==2 & finalresults$Power13==1& finalresults$Power123==2 & finalresults$Power23==2,"Ma + CCI",
                                                        ifelse(finalresults$Power2==2 & finalresults$Power3==2 & finalresults$Power12==2 & finalresults$Power13==2 & finalresults$Power123==1 & finalresults$Power23==2,"Ma + HCI + CCI",

                                                               
                                                                                                                              ifelse(finalresults$Power2==2 & finalresults$Power3==2 & finalresults$Power12==2 & finalresults$Power13==2 & finalresults$Power23==1,"HCI + CCI",0)))))))
## Since the simulation produce the effect where two sets are true at the same time, we delete them for these figures
finalresults=finalresults[!finalresults$Set==0,]

### Figure 1

figureonedata<-finalresults[finalresults$SampleSize==200 & finalresults$Correlation==0.2 & finalresults$OutlierExclusion==2 & finalresults$IndependentVariables=2,]

figureonedata$Main[figureonedata$Main==1]<-"Main = TRUE"
figureonedata$Main[figureonedata$Main==2]<-"Main = FALSE"
figureonedata$Type[figureonedata$Type==1]<-"h1=Normal, Co=Normal"
figureonedata$Type[figureonedata$Type==2]<-"h1=Binary, Co=Binary"
figureonedata$Type[figureonedata$Type==3]<-"h1=Binary, Co=Binary EF"


figureonedata$Set <- factor(figureonedata$Set,levels = c("Ma", "HCI", "CCI", "Ma + HCI","Ma + CCI","HCI + CCI","Ma + HCI + CCI"))
figureonedata$Pr<-as.numeric(figureonedata$Pr)
Figure1 = ggplot(figureonedata)+
  geom_bar(aes(x=Set,y=Pr), stat = "identity",position="dodge")+
  facet_grid(Type~Main)+
  theme_apa()+
  xlab("Model set")+
  ylab("False-positive rate")+
  theme(axis.text.x = element_text(color = "grey20", size = 17, angle = 65, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 17, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 17, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 17, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        strip.text.x = element_text(color = "grey20", size = 15, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        strip.text.y = element_text(color = "grey20", size = 15, angle = 90, hjust = .5, vjust = .5, face = "plain"))


Figure1
#### Figure 2 ####
## Average affect over number of covariates
figuretwodata<-as.data.table(finalresults[finalresults$SampleSize==200 & finalresults$Correlation==0.2 & finalresults$IndependentVariables=2,])


figuretwodata$Main[figuretwodata$Main==1]<-"Main = TRUE"
figuretwodata$Main[figuretwodata$Main==2]<-"Main = FALSE"
figuretwodata$Type[figuretwodata$Type==1]<-"h1=Normal, Co=Normal"
figuretwodata$Type[figuretwodata$Type==2]<-"h1=Binary, Co=Binary"
figuretwodata$Type[figuretwodata$Type==3]<-"h1=Binary, Co=Binary EF"
figuretwodata$OutlierExclusion[figuretwodata$OutlierExclusion==1]<-"TRUE"
figuretwodata$OutlierExclusion[figuretwodata$OutlierExclusion==2]<-"FALSE"

figuretwodata$Set <- factor(figuretwodata$Set,levels = c("Ma", "HCI", "CCI", "Ma + HCI","Ma + CCI","HCI + CCI","Ma + HCI + CCI"))
figuretwodata$Pr<-as.numeric(figuretwodata$Pr)

first=figuretwodata[figuretwodata$OutlierExclusion=="TRUE",c(1,2,6,8,10,15)]
second=figuretwodata[figuretwodata$OutlierExclusion=="FALSE",c(1,2,6,8,10,15)]

figuretwodatadiff <- merge(first,second, by=c("Main","Set","Type")) 
figuretwodatadiff$diff=figuretwodatadiff$Pr.x-figuretwodatadiff$Pr.y


Figure2 = ggplot(figuretwodatadiff)+
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
Figure2


### Figure 3 ###
## Here we will just make some examples
# In general there is no effect from sample size expect for Binary covarietes and not require main effect to be present
figurethreedata<-finalresults[ finalresults$Correlation==0.2 & finalresults$Type==2 & finalresults$OutlierExclusion==2 & finalresults$IndependentVariables=2,]


figurethreedata$Main[figurethreedata$Main==1]<-"Main = TRUE"
figurethreedata$Main[figurethreedata$Main==2]<-"Main = FALSE"
figurethreedata$Type[figurethreedata$Type==1]<-"h1=Normal, Co=Normal"
figurethreedata$Type[figurethreedata$Type==2]<-"h1=Binary, Co=Binary"
figureonedata$Type[figureonedata$Type==3]<-"h1=Binary, Co=Binary EF"


figurethreedata$Set <- factor(figurethreedata$Set,levels = c("Ma", "HCI", "CCI", "Ma + HCI","Ma + CCI","HCI + CCI","Ma + HCI + CCI"))
figurethreedata$Pr<-as.numeric(figurethreedata$Pr)

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



### Save figures and tables 

## Move a folder down
setwd('..')

ggsave(Figure1,filename =file.path("Result","Figures","v2.0","figure1.jpeg"),width = 12.5,height = 10)
ggsave(Figure2,filename =file.path("Result","Figures","v2.0","figure2.jpeg"),width = 12.5,height = 10)
ggsave(Figure3,filename =file.path("Result","Figures","v2.0","figure3.jpeg"),width = 8,height = 12)


## Save tables
df <- apply(figuretwodatadiff,2,as.character)
write.csv(as.data.frame(df),file.path("Result","ResultFile","OutlierDiff.csv"))


## Figures for the Appendix

### the effect of increase in correlation ###

figurefourdata<-finalresults[finalresults$SampleSize==200 & finalresults$OutlierExclusion==2  & finalresults$IndependentVariables=2,]

figurefourdata$Main[figurefourdata$Main==1]<-"Main = TRUE"
figurefourdata$Main[figurefourdata$Main==2]<-"Main = FALSE"
figurefourdata$Type[figurefourdata$Type==1]<-"h1=Normal, Co=Normal"
figurefourdata$Type[figurefourdata$Type==2]<-"h1=Binary, Co=Binary"
figurefourdata$Type[figurefourdata$Type==3]<-"h1=Binary, Co=Binary EF"


figurefourdata$Set <- factor(figurefourdata$Set,levels = c("Ma", "HCI", "CCI", "Ma + HCI","Ma + CCI","HCI + CCI","Ma + HCI + CCI"))
figurefourdata$Pr<-as.numeric(figurefourdata$Pr)

Figure4 = ggplot(figurefourdata)+
  geom_bar(aes(x=Set,y=Pr), stat = "identity",position="dodge")+
  facet_grid(Type + Main~Correlation)+
  theme_apa()+
  xlab("Model set")+
  ylab("False-positive rate")+
  theme(axis.text.x = element_text(color = "grey20", size = 17, angle = 65, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 17, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 17, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 17, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        strip.text.x = element_text(color = "grey20", size = 15, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        strip.text.y = element_text(color = "grey20", size = 15, angle = 90, hjust = .5, vjust = .5, face = "plain"))


Figure4


## Save figures
ggsave(Figure4,filename =file.path("Result","Figures","v2.0","figure4.jpeg"),width = 12.5,height = 12)
