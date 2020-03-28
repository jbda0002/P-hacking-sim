##Load lib

library(data.table)
library(ggplot2)
library(jtools)
library(ggpubr)
library(Rmisc)

##Load the data

## Figures for paper ##

## Make the false positve data

falsepositverateData<-as.data.table(finalresults)
falsepositverateData<-falsepositverateData[falsepositverateData$Power2==1 & falsepositverateData$Power3==2 & falsepositverateData$Power12==2 & falsepositverateData$Power13==2|
                                             falsepositverateData$Power2==2 & falsepositverateData$Power3==1 & falsepositverateData$Power12==2 & falsepositverateData$Power13==2|
                                             falsepositverateData$Power2==2 & falsepositverateData$Power3==2 & falsepositverateData$Power12==2 & falsepositverateData$Power13==2|
                                             falsepositverateData$Power2==2 & falsepositverateData$Power3==2 & falsepositverateData$Power12==1 & falsepositverateData$Power13==2|
                                             falsepositverateData$Power2==2 & falsepositverateData$Power3==2 & falsepositverateData$Power12==2 & falsepositverateData$Power13==1,]

falsepositverateData$rate=ifelse(falsepositverateData$f..>0,1,0)

falsepostive=falsepositverateData[,list(mean=mean(rate)),by=c("Main","Set","Type","SampleSize","OutlierExclusion","Correlation","IndependentVariables","DV")]

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
falsepostive$OutlierExclusion[falsepostive$OutlierExclusion==2]<-"TRUE"
falsepostive$OutlierExclusion[falsepostive$OutlierExclusion==1]<-"FALSE"



falsepostive$Set <- factor(falsepostive$Set,levels = c("Ma", "HCI", "CCI", "Ma + HCI","Ma + CCI","HCI + CCI","Ma + HCI + CCI"))
falsepostive$Pr<-as.numeric(falsepostive$mean)


## Figure 1A
# False-positive rate for each set with just two covariates and a sample size of 200


figuredata<-falsepostive[falsepostive$SampleSize==200 & falsepostive$OutlierExclusion=="FALSE" & falsepostive$IndependentVariables==1 & falsepostive$Correlation==0.2 & falsepostive$DV==1
                         & falsepostive$Type!="h1=Normal, Co=Binary" & falsepostive$Type!="h1=Binary, Co=Normal"
                         & falsepostive$Type!="h1=Binary, Co=Binary Effect" & falsepostive$Type!="h1=Normal, Co=Binary Effect",]

Figure1A = ggplot(figuredata)+
  geom_bar(aes(x=Set,y=Pr), stat = "identity",position="dodge")+
  facet_grid(Type~Main)+
  theme_apa()+
  xlab("Model set")+
  ylab("False-positive rate")+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 65, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 10, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        strip.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        strip.text.y = element_text(color = "grey20", size = 10, angle = 90, hjust = .5, vjust = .5, face = "plain"))


Figure1A

## Figure 1B
# Effect of using outlier citeria, with two covariates and sample size at 200
figuredata<-as.data.table(falsepostive[falsepostive$SampleSize==200  & falsepostive$IndependentVariables==1 & falsepostive$Correlation==0.2 & falsepostive$DV==1 
                                       & falsepostive$Type!="h1=Normal, Co=Binary" & falsepostive$Type!="h1=Binary, Co=Normal"
                                       & falsepostive$Type!="h1=Binary, Co=Binary Effect" & falsepostive$Type!="h1=Normal, Co=Binary Effect",]
)


first=figuredata[figuredata$OutlierExclusion=="TRUE"]
second=figuredata[figuredata$OutlierExclusion=="FALSE"]

figuredata <- merge(first,second, by=c("Main","Set","Type"),all=T, allow.cartesian=TRUE) 
figuredata$diff=figuredata$mean.x-figuredata$mean.y


Figure1B = ggplot(figuredata)+
  geom_bar(aes(x=Set,y=diff), stat = "identity",position="dodge")+
  facet_grid(Type~Main)+
  theme_apa()+
  xlab("Model set")+
  ylab("Difference between false-positive rate")+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 65, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 14, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        strip.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        strip.text.y = element_text(color = "grey20", size = 10, angle = 90, hjust = .5, vjust = .5, face = "plain"))
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


Figure1C = ggplot(figuredata)+
  geom_bar(aes(x=Set,y=diff), stat = "identity",position="dodge")+
  facet_grid(Type~Main)+
  theme_apa()+
  xlab("Model set")+
  # ylab("Difference between false-positive rate")+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 65, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_blank(),
        strip.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        strip.text.y = element_text(color = "grey20", size = 10, angle = 90, hjust = .5, vjust = .5, face = "plain"))
Figure1C

## Figure 1D
# The effect of bigger sample


figuredata<-as.data.table(falsepostive[ falsepostive$OutlierExclusion=="FALSE" & falsepostive$Correlation==0.2 & falsepostive$IndependentVariables==1 & falsepostive$DV==1
                                        & falsepostive$Type!="h1=Normal, Co=Binary" & falsepostive$Type!="h1=Binary, Co=Normal"
                                        & falsepostive$Type!="h1=Binary, Co=Binary Effect" & falsepostive$Type!="h1=Normal, Co=Binary Effect"
                                        & falsepostive$Type!="h1=Normal, Co=Normal",]
)
figuredata$Set <- factor(figuredata$Set,levels = c("Ma", "HCI", "CCI", "Ma + HCI","Ma + CCI","HCI + CCI","Ma + HCI + CCI"))
figuredata$Pr<-as.numeric(figuredata$mean)


Figure1D<-ggplot(aes(x=SampleSize, y=Pr), data=figuredata)+
  geom_line(aes(colour=as.factor(IndependentVariables)),show.legend = FALSE) +
  geom_point(aes(colour=as.factor(IndependentVariables)),show.legend = FALSE)+
  scale_color_grey()+
  facet_grid(Set~Main+Type)+
  # ylab("False-positive rate")+
  xlab("Sample size")+
  theme_apa()+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 65, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 8, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_blank(),
        strip.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        strip.text.y = element_text(color = "grey20", size = 10, angle = 90, hjust = .5, vjust = .5, face = "plain"))

Figure1D

Figure1 = ggarrange(Figure1A, Figure1D, Figure1B,Figure1C, 
                    labels = c("A", "B", "C","D"),
                    ncol = 2, nrow = 2)
Figure1

setwd('..')
setwd('..')
ggsave(Figure1,filename = file.path("Result","Figures","Figure1.jpeg"),width = 15,height = 17)


### Using the full model set ###

falsepositverateDataFull<-as.data.table(resultsFullset)

falsepositverateDataFull$rate=ifelse(falsepositverateDataFull$f..>0,1,0)

falsepostiveFULL=falsepositverateDataFull[,list(mean=mean(rate)),by=c("Main","Type","SampleSize","OutlierExclusion","Correlation","IndependentVariables")]

falsepostiveFULL

### Calculate the average dist and make plots for appendix

## Add for the different levels of covariates and how sample size affects this kind of 

figuredist<-as.data.table(finalresults)
figuredist<-figuredist[figuredist$Power2==1 & figuredist$Power3==2 & figuredist$Power12==2 & figuredist$Power13==2|
                         figuredist$Power2==2 & figuredist$Power3==1 & figuredist$Power12==2 & figuredist$Power13==2|
                         figuredist$Power2==2 & figuredist$Power3==2 & figuredist$Power12==2 & figuredist$Power13==2|
                         figuredist$Power2==2 & figuredist$Power3==2 & figuredist$Power12==1 & figuredist$Power13==2|
                         figuredist$Power2==2 & figuredist$Power3==2 & figuredist$Power12==2 & figuredist$Power13==1,]
figuredist$Type[figuredist$Type==1]<-"h1=Normal, Co=Normal"
figuredist$Type[figuredist$Type==2]<-"h1=Binary, Co=Binary"
figuredist$Type[figuredist$Type==1]<-"h1=Normal, Co=Normal"
figuredist$Type[figuredist$Type==2]<-"h1=Binary, Co=Binary"
figuredist$Type[figuredist$Type==3]<-"h1=Normal, Co=Binary"
figuredist$Type[figuredist$Type==4]<-"h1=Binary, Co=Normal"
figuredist$Type[figuredist$Type==5]<-"h1=Binary, Co=Binary Effect"
figuredist$Type[figuredist$Type==6]<-"h1=Normal, Co=Binary Effect"

## Percent of models with significant effect
meanDist=figuredist[,list(mean=mean(f..)),by=c("Main","Set","Type","SampleSize", "IndependentVariables","OutlierExclusion","Correlation")]



## Create Table 1
table1data<-as.data.table(figuredist[figuredist$SampleSize==200 & figuredist$Correlation ==0.2 & figuredist$OutlierExclusion ==2 & figuredist$IndependentVariables==1
                                     & figuredist$Type!="h1=Normal, Co=Binary" & figuredist$Type!="h1=Binary, Co=Normal"
                                     & figuredist$Type!="h1=Binary, Co=Binary Effect" & figuredist$Type!="h1=Normal, Co=Binary Effect" ,])

Table1=table1data[,list(mean=mean(f..),confL=CI(f..)[3], confU=CI(f..)[1]),by=c("Main","Set","Type")]

write.csv(Table1,'Table1.csv')

## The effect of using outlier exclusion (Figure for SM)
tabledata<-as.data.table(figuredist[figuredist$SampleSize==200 & figuredist$Correlation ==0.2 &  figuredist$IndependentVariables==1
                                    & figuredist$Type!="h1=Normal, Co=Binary" & figuredist$Type!="h1=Binary, Co=Normal"
                                    & figuredist$Type!="h1=Binary, Co=Binary Effect" & figuredist$Type!="h1=Normal, Co=Binary Effect" ,])

meanDist=tabledata[,list(mean=mean(f..)),by=c("Main","Set","Type","OutlierExclusion")]
first=meanDist[meanDist$OutlierExclusion==1]
second=meanDist[meanDist$OutlierExclusion==2]

meanDistOut <- merge(first,second, by=c("Main","Set","Type")) 
meanDistOut$diff=meanDistOut$mean.x-meanDistOut$mean.y


meanDistOut$Set <- factor(meanDistOut$Set,levels = c("Ma", "HCI", "CCI", "Ma + HCI","Ma + CCI","HCI + CCI","Ma + HCI + CCI"))


Figure1_SM = ggplot(meanDistOut)+
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
Figure1_SM

## Effect of using an extra covariate (figure for SM)


tabledata<-as.data.table(figuredist[figuredist$SampleSize==200 & figuredist$Correlation ==0.2 & figuredist$OutlierExclusion ==2                                     & figuredist$Type!="h1=Normal, Co=Binary" & figuredist$Type!="h1=Binary, Co=Normal"
                                    & figuredist$Type!="h1=Binary, Co=Binary Effect" & figuredist$Type!="h1=Normal, Co=Binary Effect" ,])


meanDist=tabledata[,list(mean=mean(f..)),by=c("Main","Set","Type","IndependentVariables")]
first=meanDist[meanDist$IndependentVariables==2]
second=meanDist[meanDist$IndependentVariables==1]

meanDistCov <- merge(first,second, by=c("Main","Set","Type")) 
meanDistCov$diff=meanDistCov$mean.x-meanDistCov$mean.y


meanDistCov$Main[meanDistCov$Main==1]<-"Main = TRUE"
meanDistCov$Main[meanDistCov$Main==2]<-"Main = FALSE"

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


tabledata<-as.data.table(figuredist[figuredist$Correlation ==0.2 & figuredist$OutlierExclusion ==2 & figuredist$IndependentVariables==1
                                    & figuredist$Type!="h1=Normal, Co=Binary" & figuredist$Type!="h1=Binary, Co=Normal"
                                    & figuredist$Type!="h1=Binary, Co=Binary Effect" & figuredist$Type!="h1=Normal, Co=Binary Effect" ,])

meanDist=tabledata[,list(mean=mean(f..)),by=c("Main","Set","Type","SampleSize")]

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

figuredist<-as.data.table(figuredist[figuredist$SampleSize==200  & figuredist$OutlierExclusion ==2 & figuredist$IndependentVariables==1
                                     & figuredist$Type!="h1=Normal, Co=Binary" & figuredist$Type!="h1=Binary, Co=Normal"
                                     & figuredist$Type!="h1=Binary, Co=Binary Effect" & figuredist$Type!="h1=Normal, Co=Binary Effect" ,])

meanDist=figuredist[,list(mean=mean(f..)),by=c("Main","Set","Type","Correlation")]

## Creating the different levels of the sample 
a1=meanDist[meanDist$Correlation==0.2]
a2=meanDist[meanDist$Correlation==0.3]
a3=meanDist[meanDist$Correlation==0.4]


a1=merge(a1,a2, by=c("Main","Set","Type"))
meanDistCorr=merge(a1,a3, by=c("Main","Set","Type"))


## Make the increase from an increase in sample
meanDistCorr$Inc1=meanDistCorr$mean.y-meanDistCorr$mean.x
meanDistCorr$Inc2=meanDistCorr$mean-meanDistCorr$mean.y


## Make the new data file for the plotting

first=meanDistCorr[,c(1:3, 10)]
first$Cor="0.2 - 0.3"
second=meanDistSample[,c(1:3, 11)]
second$Cor="0.3 - 0.4"


## Rename
names(first)[4]="Inc"
names(second)[4]="Inc"


## Put together

meanDistCorrInc=do.call("rbind",list(first,second))

meanDistCorrInc$Main[meanDistCorrInc$Main==1]<-"Main = TRUE"
meanDistCorrInc$Main[meanDistCorrInc$Main==2]<-"Main = FALSE"
meanDistCorrInc$Type[meanDistCorrInc$Type==1]<-"h1=Normal, Co=Normal"
meanDistCorrInc$Type[meanDistCorrInc$Type==2]<-"h1=Binary, Co=Binary"
meanDistCorrInc$Type[meanDistCorrInc$Type==1]<-"h1=Normal, Co=Normal"
meanDistCorrInc$Type[meanDistCorrInc$Type==2]<-"h1=Binary, Co=Binary"
meanDistCorrInc$Type[meanDistCorrInc$Type==3]<-"h1=Normal, Co=Binary"
meanDistCorrInc$Type[meanDistCorrInc$Type==4]<-"h1=Binary, Co=Normal"
meanDistCorrInc$Type[meanDistCorrInc$Type==5]<-"h1=Binary, Co=Binary Effect"
meanDistCorrInc$Type[meanDistCorrInc$Type==6]<-"h1=Normal, Co=Binary Effect"


meanDistCorrInc$Set <- factor(meanDistCorrInc$Set,levels = c("Ma", "HCI", "CCI", "Ma + HCI","Ma + CCI","HCI + CCI","Ma + HCI + CCI"))


Figure4_SM<-ggplot(aes(x=Cor, y=Inc,group = 1), data=meanDistCorrInc)+
  geom_bar( stat = "identity",position="dodge") +
  scale_color_grey()+
  facet_grid(Type~Main+Set)+
  ylab("Increase in models with a significant hypothesis variable")+
  xlab("Correlation increase")+
  theme_apa()+
  geom_hline(yintercept=0)+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 65, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 10, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        strip.text.x = element_text(color = "grey20", size = 10, angle = 40, hjust = .5, vjust = .5, face = "plain"),
        strip.text.y = element_text(color = "grey20", size = 10, angle = 90, hjust = .5, vjust = .5, face = "plain"))

Figure4_SM

## Figures with False-positive rate for the appendix


### Figure 1

figureonedata<-falsepostive[falsepostive$SampleSize==200 & falsepostive$OutlierExclusion=="FALSE" & falsepostive$IndependentVariables==2,]


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

#### Figure 2 ####
## Average affect of using outlier criteria
figuretwodata<-as.data.table(falsepostive[falsepostive$SampleSize==200  & falsepostive$IndependentVariables==1,]
)

first=figuretwodata[figuretwodata$OutlierExclusion=="TRUE"]
second=figuretwodata[figuretwodata$OutlierExclusion=="FALSE"]

figuretwodatadiff <- merge(first,second, by=c("Main","Set","Type","IndependentVariables","Correlation")) 
figuretwodatadiff$diff=figuretwodatadiff$mean.x-figuretwodatadiff$mean.y


Figure2 = ggplot(figuretwodatadiff)+
  geom_bar(aes(x=Set,y=diff), stat = "identity",position="dodge")+
  facet_grid(Type~Main+Correlation)+
  theme_apa()+
  xlab("Model set")+
  ylab("Difference between false-positive rate")+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 65, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 10, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        strip.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        strip.text.y = element_text(color = "grey20", size = 10, angle = 90, hjust = .5, vjust = .5, face = "plain"))
Figure2

### The effect of sample size

figurethreedata<-falsepostive[ falsepostive$Correlation==0.2 & falsepostive$OutlierExclusion==2 & falsepostive$IndependentVariables==2,]

figurethreedata$Main[figurethreedata$Main==1]<-"Main = TRUE"
figurethreedata$Main[figurethreedata$Main==2]<-"Main = FALSE"
figurethreedata$Type[figurethreedata$Type==1]<-"h1=Normal, Co=Normal"
figurethreedata$Type[figurethreedata$Type==2]<-"h1=Binary, Co=Binary"
figurethreedata$Type[figurethreedata$Type==3]<-"h1=Normal, Co=Binary"
figurethreedata$Type[figurethreedata$Type==4]<-"h1=Binary, Co=Normal"
figurethreedata$Type[figurethreedata$Type==5]<-"h1=Binary, Co=Binary Effect"
figurethreedata$Type[figurethreedata$Type==6]<-"h1=Normal, Co=Binary Effect"


figurethreedata$Set <- factor(figurethreedata$Set,levels = c("Ma", "HCI", "CCI", "Ma + HCI","Ma + CCI","HCI + CCI","Ma + HCI + CCI"))
figurethreedata$Pr<-as.numeric(figurethreedata$mean)


Figure3<-ggplot(aes(x=SampleSize, y=Pr), data=figurethreedata)+
  geom_line(aes(colour=as.factor(IndependentVariables)),show.legend = FALSE) +
  geom_point(aes(colour=as.factor(IndependentVariables)),show.legend = FALSE)+
  scale_color_grey()+
  facet_grid(Set~Main+Type)+
  ylab("False-positive rate")+
  xlab("Sample size")+
  theme_apa()+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 65, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 8, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 10, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        strip.text.x = element_text(color = "grey20", size = 6, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        strip.text.y = element_text(color = "grey20", size = 10, angle = 90, hjust = .5, vjust = .5, face = "plain"))

Figure3


#### Figure 4 ####
## Average affect over number of covariates
figuretwodata<-as.data.table(falsepostive[falsepostive$SampleSize==200  & falsepostive$OutlierExclusion=="FALSE",]
)

first=figuretwodata[figuretwodata$IndependentVariables==1]
second=figuretwodata[figuretwodata$IndependentVariables==2]

figuretwodatadiff2 <- merge(first,second, by=c("Main","Set","Type","Correlation"),all=T, allow.cartesian=TRUE) 
figuretwodatadiff2$diff=figuretwodatadiff2$mean.y-figuretwodatadiff2$mean.x


Figure2 = ggplot(figuretwodatadiff2)+
  geom_bar(aes(x=Set,y=diff), stat = "identity",position="dodge")+
  facet_grid(Type~Main+Correlation)+
  theme_apa()+
  xlab("Model set")+
  ylab("Difference between false-positive rate")+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 65, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 10, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        strip.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        strip.text.y = element_text(color = "grey20", size = 10, angle = 90, hjust = .5, vjust = .5, face = "plain"))
Figure2
