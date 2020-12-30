##Load lib

## Set output directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
output = dirname(dirname(getwd()))
output=paste0(output,"/Result")

library(data.table)
library(ggplot2)
library(jtools)
library(ggpubr)
library(xtable)

##Load the data
fileplace=paste0(output,"/File/ResultsBC.csv.gz")
finalresults = read.table(gzfile(fileplace),sep=",",header = T)
## Figures for paper ##

## Make the false positve data

falsepositverateData<-as.data.table(finalresults)
falsepositverateData<-falsepositverateData[falsepositverateData$Power2==1 & falsepositverateData$Power3==2 & falsepositverateData$Power12==2 & falsepositverateData$Power13==2|
                                             falsepositverateData$Power2==2 & falsepositverateData$Power3==1 & falsepositverateData$Power12==2 & falsepositverateData$Power13==2|
                                             falsepositverateData$Power2==2 & falsepositverateData$Power3==2 & falsepositverateData$Power12==2 & falsepositverateData$Power13==2|
                                             falsepositverateData$Power2==2 & falsepositverateData$Power3==2 & falsepositverateData$Power12==1 & falsepositverateData$Power13==2|
                                             falsepositverateData$Power2==2 & falsepositverateData$Power3==2 & falsepositverateData$Power12==2 & falsepositverateData$Power13==1,]

falsepositverateData$rate=ifelse(falsepositverateData$f..>0,1,0)

falsepostive=falsepositverateData[,list(mean=mean(rate),FPR=mean(f..)),by=c("Main","Set","Type","SampleSize","OutlierExclusion","Correlation","IndependentVariables","DV")]

falsepostive$Main[falsepostive$Main==1]<-"Main = TRUE"
falsepostive$Main[falsepostive$Main==2]<-"Main = FALSE"
falsepostive$Type[falsepostive$Type==1]<-"h1=Normal, Co=Normal"
falsepostive$Type[falsepostive$Type==2]<-"h1=Binary, Co=Binary"
falsepostive$Type[falsepostive$Type==1]<-"h1=Normal, Co=Normal"
falsepostive$Type[falsepostive$Type==2]<-"h1=Binary, Co=Binary"
falsepostive$Type[falsepostive$Type==3]<-"h1=Normal, Co=Binary"
falsepostive$Type[falsepostive$Type==4]<-"h1=Binary, Co=Normal"
falsepostive$Type[falsepostive$Type==5]<-"h1=Binary, Co=Binary Effect"
falsepostive$Type[falsepostive$Type==6]<-"h1=Normal, Co=Binary Effect"
falsepostive$OutlierExclusion[falsepostive$OutlierExclusion==2]<-"FALSE"
falsepostive$OutlierExclusion[falsepostive$OutlierExclusion==1]<-"TRUE"



falsepostive$Set <- factor(falsepostive$Set,levels = c("Ma", "HCI", "CCI", "Ma + HCI","Ma + CCI","HCI + CCI","Ma + HCI + CCI"))
levels(falsepostive$Set) <- c("ME", "HCI", "CCI", "ME + HCI","ME + CCI","HCI + CCI","ME + HCI + CCI")
falsepostive$Pr<-as.numeric(falsepostive$mean)

## Figure 1A
# False-positive rate for each set with just two covariates and a sample size of 200


figuredata<-falsepostive[falsepostive$SampleSize==200 & falsepostive$OutlierExclusion=="FALSE" & falsepostive$IndependentVariables==1 & falsepostive$Correlation==0.2 & falsepostive$DV==1
                         & falsepostive$Type!="h1=Normal, Co=Binary" & falsepostive$Type!="h1=Binary, Co=Normal"
                         & falsepostive$Type!="h1=Binary, Co=Binary Effect" & falsepostive$Type!="h1=Normal, Co=Binary Effect",]

Figure1A = ggplot(figuredata,aes(x=Set))+
  geom_bar(aes(x=Set,y=Pr, fill = "FPP"), stat = "identity",position="dodge")+
  geom_bar(aes(x=Set,y=FPR, fill = "FPR"), stat = "identity",position="dodge")+
  scale_fill_manual(values=c("black","red"))+
  #geom_text( aes(y=round(FPR,3),label=round(FPR,3)), vjust=-2)+
  #geom_text( aes(y=round(Pr,3),label=round(Pr,3)), vjust=-1)+
  facet_grid(Type~Main)+
  theme_apa()+
  xlab("Model set")+
  ylab("Probability of FPP and FPR")+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 65, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 14, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 14, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        strip.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        strip.text.y = element_text(color = "grey20", size = 10, angle = 90, hjust = .5, vjust = .5, face = "plain"))

##Save data
fwrite(figuredata,paste0(output,"/Files/figuredata1ABon.csv"),sep=";")
Figure1A

## Figure 1B
# Effect of using outlier citeria, with two covariates and sample size at 200
figuredata<-falsepostive[falsepostive$SampleSize==200 & falsepostive$IndependentVariables==1 & falsepostive$Correlation==0.2 & falsepostive$DV==1
                         & falsepostive$Type!="h1=Normal, Co=Binary" & falsepostive$Type!="h1=Binary, Co=Normal"
                         & falsepostive$Type!="h1=Binary, Co=Binary Effect" & falsepostive$Type!="h1=Normal, Co=Binary Effect",]



first=figuredata[figuredata$OutlierExclusion=="TRUE"]
second=figuredata[figuredata$OutlierExclusion=="FALSE"]

figuredata <- merge(first,second, by=c("Main","Set","Type"),all=T, allow.cartesian=TRUE) 
figuredata$diff=figuredata$mean.x-figuredata$mean.y
figuredata$FPRdiff=figuredata$FPR.x-figuredata$FPR.y

Figure1B = ggplot(figuredata,aes(x=Set))+
  geom_bar(aes(x=Set,y=diff,fill="FPP"), stat = "identity",position="dodge")+
  geom_bar(aes(x=Set,y=FPRdiff,fill="FPR"), stat = "identity",position="dodge")+
  scale_fill_manual(values=c("black","red"))+
  #geom_text( aes(y=round(FPRdiff,3),label=round(FPRdiff,3)), vjust=-1)+
  #geom_text( aes(y=round(diff,3),label=round(diff,3)), vjust=-1)+
  facet_grid(Type~Main)+
  theme_apa()+
  xlab("Model set")+
  ylab("Difference in probability from base model")+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 65, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 14, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 14, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        strip.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        strip.text.y = element_text(color = "grey20", size = 10, angle = 90, hjust = .5, vjust = .5, face = "plain"))

##Save data
fwrite(figuredata,paste0(output,"/Files/figuredata1BBon.csv"),sep=";")
Figure1B

## Figure 1C
# Adding an extra covariate

figuredata<-as.data.table(falsepostive[falsepostive$SampleSize==200  & falsepostive$OutlierExclusion=="FALSE" & falsepostive$Correlation==0.2 & falsepostive$DV==1
                                       & falsepostive$Type!="h1=Normal, Co=Binary" & falsepostive$Type!="h1=Binary, Co=Normal"
                                       & falsepostive$Type!="h1=Binary, Co=Binary Effect" & falsepostive$Type!="h1=Normal, Co=Binary Effect",]
)

first=figuredata[figuredata$IndependentVariables==1]
second=figuredata[figuredata$IndependentVariables==2]

figuredata <- merge(first,second, by=c("Main","Set","Type"),all=T, allow.cartesian=TRUE) 
figuredata$diff=figuredata$mean.y-figuredata$mean.x
figuredata$FPRdiff=figuredata$FPR.y-figuredata$FPR.x


Figure1C = ggplot(figuredata,aes(x=Set))+
  geom_bar(aes(x=Set,y=diff,fill="FPP"), stat = "identity",position="dodge")+
  geom_bar(aes(x=Set,y=FPRdiff,fill="FPR"), stat = "identity",position="dodge")+
  scale_fill_manual(values=c("black","red"))+
  #geom_text( aes(y=round(FPRdiff,3),label=round(FPRdiff,3)), vjust=-1)+
  #geom_text( aes(y=round(diff,3),label=round(diff,3)), vjust=-1)+
  facet_grid(Type~Main)+
  theme_apa()+
  xlab("Model set")+
  ylab("Difference in probability from base model")+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 65, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 14, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 14, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        strip.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        strip.text.y = element_text(color = "grey20", size = 10, angle = 90, hjust = .5, vjust = .5, face = "plain"))

##Save data
fwrite(figuredata,paste0(output,"/Files/figuredata1CBon.csv"),sep=";")
Figure1C

## Figure 1D
# The effect of bigger sample


figuredata<-as.data.table(falsepostive[ falsepostive$OutlierExclusion=="FALSE" & falsepostive$Correlation==0.2 & falsepostive$IndependentVariables==1 & falsepostive$DV==1
                                        & falsepostive$Type!="h1=Normal, Co=Binary" & falsepostive$Type!="h1=Binary, Co=Normal"
                                        & falsepostive$Type!="h1=Binary, Co=Binary Effect" & falsepostive$Type!="h1=Normal, Co=Binary Effect"
                                        ,]
)
figuredata$Set <- factor(figuredata$Set,levels = c("ME", "HCI", "CCI", "ME + HCI","ME + CCI","HCI + CCI","ME + HCI + CCI"))
figuredata$Pr<-as.numeric(figuredata$mean)


Figure1D<-ggplot(aes(x=SampleSize), data=figuredata)+
  geom_line(aes( y=Pr,color="black")) +
  geom_point(aes( y=Pr),color="black")+
  geom_line(aes( y=FPR,color="red")) +
  geom_point(aes( y=FPR),color="red")+
  scale_color_manual(values = c("black","red"),labels = c("FPP", "FPR"))+
  facet_grid(Set~Main+Type)+
  ylab("Probability of FPP and FPR")+
  xlab("Sample size")+
  theme_apa()+
  theme(axis.text.x = element_text(color = "grey20", size = 7, angle = 65, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 10, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        strip.text.x = element_text(color = "grey20", size = 5, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        strip.text.y = element_text(color = "grey20", size = 7, angle = 330, hjust = .5, vjust = .5, face = "plain"))

##Save data
fwrite(figuredata,paste0(output,"/Files/figuredata1DBon.csv"),sep=";")

Figure1D


Figure1 = ggarrange(Figure1A, Figure1D, Figure1B,Figure1C, 
                    labels = c("A", "B", "C","D"),
                    ncol = 2, nrow = 2)
Figure1


ggsave(Figure1,filename = file.path(output,"Figures","Figure1Bon.jpeg"),width = 15,height = 17)
ggsave(Figure1A,filename = file.path(output,"Figures","Figure1ABon.jpeg"),width = 6,height = 7)
ggsave(Figure1B,filename = file.path(output,"Figures","Figure1BBon.jpeg"),width = 6,height = 7)
ggsave(Figure1C,filename = file.path(output,"Figures","Figure1CBon.jpeg"),width = 6,height = 7)
ggsave(Figure1D,filename = file.path(output,"Figures","Figure1DBon.jpeg"),width = 6,height = 7)


### Using the full model set ###

resultsFullset=fread(paste0(output,"/Files/resultsFullSetBon.csv"),sep=";")

falsepositverateDataFull<-as.data.table(resultsFullset)

falsepositverateDataFull$rate=ifelse(falsepositverateDataFull$f..>0,1,0)

falsepostiveFULL=falsepositverateDataFull[,list(FPP=mean(rate),FPR=mean(f..)),by=c("Main","Type","SampleSize","OutlierExclusion","Correlation","IndependentVariables")]
falsepostiveFULL$Main=ifelse(falsepostiveFULL$Main==1,"TRUE","FALSE")
falsepostiveFULL$Type=ifelse(falsepostiveFULL$Type==1,"Normal","Binomial")
falsepostiveFULL$IndependentVariables=falsepostiveFULL$IndependentVariables+1
names(falsepostiveFULL)=c("Main","Type","Sample","Outlier","Correlation","Number of Variables","FPP","FPR")
falsepostiveFULL=falsepostiveFULL[order(falsepostiveFULL$Main),]
print(xtable(falsepostiveFULL,digits = 2, type = "latex",caption =""), caption.placement = "top", include.rownames=FALSE, tabular.environment="longtable", file = "FullModelSet.tex")

falsepostiveFULL

#### Figures for the appendix #### 


## The effect of higher correlations ##

meanDist<-as.data.table(falsepostive[falsepostive$SampleSize==200  & falsepostive$OutlierExclusion =="FALSE" & falsepostive$IndependentVariables==1 & falsepostive$DV==1
                                     & falsepostive$Type!="h1=Binary, Co=Binary Effect" & falsepostive$Type!="h1=Normal, Co=Binary Effect",])


## Creating the different levels of the sample 
a1=meanDist[meanDist$Correlation==0.2]
a2=meanDist[meanDist$Correlation==0.3]
a3=meanDist[meanDist$Correlation==0.4]


a1=merge(a1,a2, by=c("Main","Set","Type"))
meanDistCorr=merge(a1,a3, by=c("Main","Set","Type"))


## Make the increase from an increase in sample
meanDistCorr$Inc1=meanDistCorr$mean.y-meanDistCorr$mean.x
meanDistCorr$Inc2=meanDistCorr$mean-meanDistCorr$mean.y

meanDistCorr$Inc1FPR=meanDistCorr$FPR.y-meanDistCorr$FPR.x
meanDistCorr$Inc2FPR=meanDistCorr$FPR-meanDistCorr$FPR.y

## Make the new data file for the plotting

first=meanDistCorr[,c(1:3, 28,30)]
first$Cor="0.2 - 0.3"
second=meanDistCorr[,c(1:3, 29,31)]
second$Cor="0.3 - 0.4"


## Rename
names(first)[4]="Inc"
names(second)[4]="Inc"

names(first)[5]="IncFPR"
names(second)[5]="IncFPR"


## Put together

meanDistCorrInc=do.call("rbind",list(first,second))

meanDistCorrInc$Set <- factor(meanDistCorrInc$Set,levels = c("ME", "HCI", "CCI", "ME + HCI","ME + CCI","HCI + CCI","ME + HCI + CCI"))


Figure2SI<-ggplot( data=meanDistCorrInc)+
  geom_bar(aes(x=Cor,y=Inc, fill = "FPP"), stat = "identity",position="dodge")+
  geom_bar(aes(x=Cor,y=IncFPR, fill = "FPR"), stat = "identity",position="dodge")+
  scale_fill_manual(values=c("black","red"))+
  facet_grid(Type~Main+Set)+
  ylab("Difference in probability from base model")+
  xlab("Correlation increase")+
  theme_apa()+
  geom_hline(yintercept=0)+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 65, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 10, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        strip.text.x = element_text(color = "grey20", size = 10, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        strip.text.y = element_text(color = "grey20", size = 7, angle = 330, hjust = .5, vjust = .5, face = "plain"))

Figure2SI

ggsave(Figure2SI,filename = file.path(output,"Figures","Figure2SIBon.jpeg"),width = 6,height = 7)

## Using several dependent variables
figuredata<-as.data.table(falsepostive[falsepostive$SampleSize==200  & falsepostive$OutlierExclusion=="FALSE" & falsepostive$Correlation==0.2 & falsepostive$IndependentVariables==1
                                       & falsepostive$Type!="h1=Binary, Co=Binary Effect" & falsepostive$Type!="h1=Normal, Co=Binary Effect",]
)

first=figuredata[figuredata$DV==1]
second=figuredata[figuredata$DV==2]

figuredata <- merge(first,second, by=c("Main","Set","Type"),all=T, allow.cartesian=TRUE) 
figuredata$diff=figuredata$mean.y-figuredata$mean.x
figuredata$FPRdiff=figuredata$FPR.y-figuredata$FPR.x


Figure3SI = ggplot(figuredata,aes(x=Set))+
  geom_bar(aes(x=Set,y=diff, fill = "FPP"), stat = "identity",position="dodge")+
  geom_bar(aes(x=Set,y=FPRdiff, fill = "FPR"), stat = "identity",position="dodge")+
  scale_fill_manual(values=c("black","red"))+
  #geom_text( aes(y=round(FPRdiff,3),label=round(FPRdiff,3)), vjust=-1)+
  #geom_text( aes(y=round(diff,3),label=round(diff,3)), vjust=-1)+
  facet_grid(Type~Main)+
  theme_apa()+
  xlab("Model set")+
  ylab("Difference in probability from base model")+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 65, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_blank(),
        strip.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        strip.text.y = element_text(color = "grey20", size = 7, angle = 330, hjust = .5, vjust = .5, face = "plain"))
Figure3SI
ggsave(Figure3SI,filename = file.path(output,"Figures","Figure3SIBon.jpeg"),width = 6,height = 7)


figuredata<-falsepostive[falsepostive$SampleSize==200 & falsepostive$OutlierExclusion=="FALSE" & falsepostive$IndependentVariables==2 & falsepostive$Correlation==0.2 & falsepostive$DV==1
                         ,]
## For all sets of variables

figuredata<-falsepostive[falsepostive$SampleSize==200 & falsepostive$OutlierExclusion=="FALSE" & falsepostive$IndependentVariables==1 & falsepostive$Correlation==0.2 & falsepostive$DV==1
                         ,]

Figure1ASI = ggplot(figuredata,aes(x=Set))+
  geom_bar(aes(x=Set,y=Pr, fill = "FPP"), stat = "identity",position="dodge")+
  geom_bar(aes(x=Set,y=FPR, fill = "FPR"), stat = "identity",position="dodge")+
  scale_fill_manual(values=c("black","red"))+
  #geom_text( aes(y=round(FPR,3),label=round(FPR,3)), vjust=-2)+
  #geom_text( aes(y=round(Pr,3),label=round(Pr,3)), vjust=-1)+
  facet_grid(Type~Main)+
  theme_apa()+
  xlab("Model set")+
  ylab("Probability of FPP and FPR")+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 65, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 14, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 14, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        strip.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        strip.text.y = element_text(color = "grey20", size = 7, angle = 330, hjust = .5, vjust = .5, face = "plain"))

Figure1ASI

## Figure 1B
# Effect of using outlier citeria, with two covariates and sample size at 200
figuredata<-falsepostive[falsepostive$SampleSize==200 & falsepostive$IndependentVariables==1 & falsepostive$Correlation==0.2 & falsepostive$DV==1
                         ,]



first=figuredata[figuredata$OutlierExclusion=="TRUE"]
second=figuredata[figuredata$OutlierExclusion=="FALSE"]

figuredata <- merge(first,second, by=c("Main","Set","Type"),all=T, allow.cartesian=TRUE) 
figuredata$diff=figuredata$mean.x-figuredata$mean.y
figuredata$FPRdiff=figuredata$FPR.x-figuredata$FPR.y

Figure1BSI = ggplot(figuredata,aes(x=Set))+
  geom_bar(aes(x=Set,y=diff,fill="FPP"), stat = "identity",position="dodge")+
  geom_bar(aes(x=Set,y=FPRdiff,fill="FPR"), stat = "identity",position="dodge")+
  scale_fill_manual(values=c("black","red"))+
  #geom_text( aes(y=round(FPRdiff,3),label=round(FPRdiff,3)), vjust=-1)+
  #geom_text( aes(y=round(diff,3),label=round(diff,3)), vjust=-1)+
  facet_grid(Type~Main)+
  theme_apa()+
  xlab("Model set")+
  ylab("Difference in probability from base model")+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 65, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 14, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 14, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        strip.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        strip.text.y = element_text(color = "grey20", size = 7, angle = 330, hjust = .5, vjust = .5, face = "plain"))

##Save data
Figure1BSI

## Figure 1C
# Adding an extra covariate

figuredata<-as.data.table(falsepostive[falsepostive$SampleSize==200  & falsepostive$OutlierExclusion=="FALSE" & falsepostive$Correlation==0.2 & falsepostive$DV==1
                                       ,]
)

first=figuredata[figuredata$IndependentVariables==1]
second=figuredata[figuredata$IndependentVariables==2]

figuredata <- merge(first,second, by=c("Main","Set","Type"),all=T, allow.cartesian=TRUE) 
figuredata$diff=figuredata$mean.y-figuredata$mean.x
figuredata$FPRdiff=figuredata$FPR.y-figuredata$FPR.x


Figure1CSI = ggplot(figuredata,aes(x=Set))+
  geom_bar(aes(x=Set,y=diff,fill="FPP"), stat = "identity",position="dodge")+
  geom_bar(aes(x=Set,y=FPRdiff,fill="FPR"), stat = "identity",position="dodge")+
  scale_fill_manual(values=c("black","red"))+
  #geom_text( aes(y=round(FPRdiff,3),label=round(FPRdiff,3)), vjust=-1)+
  #geom_text( aes(y=round(diff,3),label=round(diff,3)), vjust=-1)+
  facet_grid(Type~Main)+
  theme_apa()+
  xlab("Model set")+
  ylab("Difference in probability from base model")+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 65, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 14, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 14, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        strip.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        strip.text.y = element_text(color = "grey20", size = 7, angle = 330, hjust = .5, vjust = .5, face = "plain"))

##Save data
Figure1CSI

## Figure 1D
# The effect of bigger sample
figuredata<-as.data.table(falsepostive[ falsepostive$OutlierExclusion=="FALSE" & falsepostive$Correlation==0.2 & falsepostive$IndependentVariables==1 & falsepostive$DV==1
                                        ,]
)
figuredata$Set <- factor(figuredata$Set,levels = c("ME", "HCI", "CCI", "ME + HCI","ME + CCI","HCI + CCI","ME + HCI + CCI"))
figuredata$Pr<-as.numeric(figuredata$mean)


Figure1DSI<-ggplot(aes(x=SampleSize), data=figuredata)+
  geom_line(aes( y=Pr,color="black")) +
  geom_point(aes( y=Pr),color="black")+
  geom_line(aes( y=FPR,color="red")) +
  geom_point(aes( y=FPR),color="red")+
  scale_color_manual(values = c("black","red"),labels = c("FPP", "FPR"))+
  facet_grid(Set~Main+Type)+
  ylab("Probability of FPP and FPR")+
  xlab("Sample size")+
  theme_apa()+
  theme(axis.text.x = element_text(color = "grey20", size = 7, angle = 65, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 10, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        strip.text.x = element_text(color = "grey20", size = 5, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        strip.text.y = element_text(color = "grey20", size = 7, angle = 330, hjust = .5, vjust = .5, face = "plain"),
        legend.text=element_text(color = "grey20",size=5))
Figure1DSI
##Save data
ggsave(Figure1ASI,filename = file.path(output,"Figures","Figure1ASIBon.jpeg"),width = 6,height = 7)
ggsave(Figure1BSI,filename = file.path(output,"Figures","Figure1BSIBon.jpeg"),width = 6,height = 7)
ggsave(Figure1CSI,filename = file.path(output,"Figures","Figure1CSIBon.jpeg"),width = 6,height = 7)
ggsave(Figure1DSI,filename = file.path(output,"Figures","Figure1DSIBon.jpeg"),width = 6,height = 7)


