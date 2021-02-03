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
library(latex2exp)
library(grid)

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

falsepostive$Main[falsepostive$Main==1]<-"With restrictions"
falsepostive$Main[falsepostive$Main==2]<-"Without restrictions"
falsepostive$Type[falsepostive$Type==1]<-"x1~Normal, z~Normal"
falsepostive$Type[falsepostive$Type==2]<-"x1~Binary, z~Binary"
falsepostive$Type[falsepostive$Type==3]<-"x1~Normal, z~Binary"
falsepostive$Type[falsepostive$Type==4]<-"x1~Binary, z~Normal"
falsepostive$Type[falsepostive$Type==5]<-"x1~Binary, z~Binary Effect"
falsepostive$Type[falsepostive$Type==6]<-"x1~Normal, z~Binary Effect"
falsepostive$OutlierExclusion[falsepostive$OutlierExclusion==2]<-"FALSE"
falsepostive$OutlierExclusion[falsepostive$OutlierExclusion==1]<-"TRUE"



falsepostive$Set <- factor(falsepostive$Set,levels = c("Ma", "HCI", "CCI", "Ma + HCI","Ma + CCI","HCI + CCI","Ma + HCI + CCI"))
levels(falsepostive$Set) <- c("x + z", "x * z", "z * z", "x + z + x * z","x + z + z * z","x * z + z * z","x + z + x * z + z * z")
falsepostive$Main = factor(falsepostive$Main,levels = c("Without restrictions","With restrictions"))
levels(falsepostive$Main ) <- c("Without restrictions","With restrictions")
falsepostive$Pr<-as.numeric(falsepostive$mean)

falsepostive$Type = factor(falsepostive$Type,levels = c("x1~Binary, z~Binary","x1~Normal, z~Normal","x1~Normal, z~Binary",
                                                        "x1~Binary, z~Normal","x1~Binary, z~Binary Effect","x1~Normal, z~Binary Effect"))

## Make labels
labs_full <- expression(italic("x") + italic("z"), italic("x") %*% italic("z"), italic("z") %*% italic("z"), italic("x") + italic("z") + italic("x") %*% italic("z")
                        ,italic("x") + italic("z") + italic("z") %*% italic("z"),
                        italic("x") %*% italic("z") + italic("z") %*% italic("z"),italic("x") + italic("z") + italic("x") %*% italic("z") + italic("z") %*% italic("z"))



labs <- c("$\\textit{x} + \\textit{z}$", "$\\textit{x} \\times \\textit{z}$", "$\\textit{z} \\times \\textit{z}$", "$\\textit{x} + \\textit{z} + \\textit{x} \\times \\textit{z}$", "$\\textit{x} + \\textit{z} + \\textit{z} \\times \\textit{z}$", 
          "$\\textit{x} \\times \\textit{z} + \\textit{z} \\times \\textit{z}$", "$\\textit{x} + \\textit{z} + \\textit{x} \\times \\textit{z} + \\textit{z} \\times \\textit{z}$")


## Make two labels of dist when there is only the two from the main paper that will be shown
labels_2=c('italic("x")%~%Binary~italic("z")%~%Binary','italic("x")%~%Normal~italic("z")%~%Normal')
labels_2latex=c("$\\textit{x} \\sim Binary, \\textit{z} \\sim Binary$","$\\textit{x} \\sim Normal , \\textit{z} \\sim Normal$")

labels_4latex=c("$\\textit{x} \\sim Binary, \\textit{z} \\sim Binary$","$\\textit{x} \\sim Normal , \\textit{z} \\sim Normal$",
                "$\\textit{x} \\sim Normal, \\textit{z} \\sim Binary$","$\\textit{x} \\sim Binary, \\textit{z} \\sim Normal$")

labels_re = c('Without~restrictions', 'With~restrictions')
labels_relatex = c('$Without$', '$With$')
## Make labels for the appendix

## Without effect coding
labels_all=c('italic("x")%~%Binary~italic("z")%~%Binary','italic("x")%~%Normal~italic("z")%~%Normal','italic("x")%~%Normal~italic("z")%~%Binary'
             ,'italic("x")%~%Binary~italic("z")%~%Normal')
#With effect coding
#labels_all=c('italic("x")%~%Binary~italic("z")%~%Binary','italic("x")%~%Normal~italic("z")%~%Normal','italic("x")%~%Normal~italic("z")%~%Binary'
#             ,'italic("x")%~%Binary~italic("z")%~%Normal','italic("x")%~%Binary~italic("z")%~%Binary~(effect~coded)','italic("x")%~%Normal~italic("z")%~%Binary~(effect~coded)')


## Figure 1A
# False-positive rate for each set with just two covariates and a sample size of 200


figuredata<-falsepostive[falsepostive$SampleSize==200 & falsepostive$OutlierExclusion=="FALSE" & falsepostive$IndependentVariables==1 & falsepostive$Correlation==0.2 & falsepostive$DV==1
                         & falsepostive$Type!="x1~Normal, z~Binary" & falsepostive$Type!="x1~Binary, z~Normal"
                         & falsepostive$Type!="x1~Binary, z~Binary Effect" & falsepostive$Type!="x1~Normal, z~Binary Effect",]

#Rename labels
figuredata$Type <- factor(figuredata$Type,
                          labels=labels_2)


figuredata$Main <- factor(figuredata$Main,
                          labels=labels_re)


Figure1A = ggplot(figuredata,aes(x=Set))+
  geom_bar(aes(x=Set,y=Pr, fill = "FPP"), stat = "identity",position="dodge")+
  geom_bar(aes(x=Set,y=FPR, fill = "FPR"), stat = "identity",position="dodge")+
  scale_fill_manual(values=c("black","red"))+
  facet_grid(Type~Main, scales = "free_x", labeller=label_parsed)+
  theme_apa()+
  xlab("Model set")+
  ylab("Probability (FPP) / Ratio (FPR)")+
  ylim(0,1) +
  scale_x_discrete(labels = c("x + z" = expression(italic("x") + italic("z")), 
                              "x * z"= expression(italic("x") %*% italic("z")), 
                              "z * z"= expression(italic("z") %*% italic("z")), 
                              "x + z + x * z"= expression(italic("x") + italic("z") + italic("x") %*% italic("z")),
                              "x + z + z * z"= expression(italic("x") + italic("z") + italic("z") %*% italic("z")),
                              "x * z + z * z"= expression(italic("x") %*% italic("z") + italic("x") %*% italic("z")),
                              "x + z + x * z + z * z"= expression(italic("x") + italic("z") + italic("x") %*% italic("z") + italic("z") %*% italic("z"))))+
  geom_hline(yintercept = 0.05, linetype="dashed")+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 65, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x=element_blank(),
        axis.title.y = element_text(color = "grey20", size = 14, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        strip.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        strip.text.y = element_text(color = "grey20", size = 10, angle = 90, hjust = .5, vjust = .5, face = "plain"))



## Make into a table
figuredata[,11]=NULL
figuredata$IndependentVariables = figuredata$IndependentVariables+1
figuredata$DV[figuredata$DV == 2 ] = 3

figuredata$Set <- factor(figuredata$Set,
                         labels=labs)

figuredata$Type <- factor(figuredata$Type,
                          labels=labels_2latex)


figuredata$Main <- factor(figuredata$Main,levels = c('Without~restrictions', 'With~restrictions'),
                          labels=labels_relatex)
figuredata=figuredata[
  with(figuredata, order(Main, Set)),
  ]
names(figuredata) = c("Restrictions" ,"Set", "Type" , "Sample Size" , "Outlier exclusion", "Correlation","Covariates","Dependent variables","FPP","FPR")


print(xtable(figuredata,digits = 2, type = "latex", align = c("l","l","c","c","c","c","c","c","c","c","c"),caption = "False positive probability (FPP) and false positive ratio (FPR) for the different model sets,  the presence of main effects when having interactions (i.e., With restrictions or No restrictions) and different distributions of the variable of interest and covariates. Sample size is set to 200, a correlation between the dependent variable and covariates is \\textit{r}=0.2 and using two covariates.",label = "tab:apptab1"), caption.placement = "top", include.rownames=FALSE,sanitize.text.function = identity, file = "Table1ABon.tex",scalebox = 0.8)

##Save data
fwrite(figuredata,paste0(output,"/Files/figuredata1A.csv"),sep=";")
Figure1A

## Figure 1B
# Effect of using outlier citeria, with two covariates and sample size at 200
figuredata<-falsepostive[falsepostive$SampleSize==200 & falsepostive$OutlierExclusion=="TRUE" & falsepostive$IndependentVariables==1 & falsepostive$Correlation==0.2 & falsepostive$DV==1
                         & falsepostive$Type!="x1~Normal, z~Binary" & falsepostive$Type!="x1~Binary, z~Normal"
                         & falsepostive$Type!="x1~Binary, z~Binary Effect" & falsepostive$Type!="x1~Normal, z~Binary Effect",]

#Rename labels
figuredata$Type <- factor(figuredata$Type,
                          labels=labels_2)

figuredata$Main <- factor(figuredata$Main,
                          labels=labels_re)

Figure1B = ggplot(figuredata,aes(x=Set))+
  geom_bar(aes(x=Set,y=Pr, fill = "FPP"), stat = "identity",position="dodge")+
  geom_bar(aes(x=Set,y=FPR, fill = "FPR"), stat = "identity",position="dodge")+
  scale_fill_manual(values=c("black","red"))+
  #geom_text( aes(y=round(FPR,3),label=round(FPR,3)), vjust=-2)+
  #geom_text( aes(y=round(Pr,3),label=round(Pr,3)), vjust=-1)+
  facet_grid(Type~Main, scales = "free", labeller=label_parsed)+
  theme_apa()+
  xlab("Model set")+
  ylab("Probability (FPP) / Ratio (FPR)")+
  ylim(0,1) +
  scale_x_discrete(labels = c("x + z" = expression(italic("x") + italic("z")),        
                              "x * z"= expression(italic("x") %*% italic("z")),                                
                              "z * z"= expression(italic("z") %*% italic("z")),                               
                              "x + z + x * z"= expression(italic("x") + italic("z") + italic("x") %*% italic("z")),                              
                              "x + z + z * z"= expression(italic("x") + italic("z") + italic("z") %*% italic("z")),                               
                              "x * z + z * z"= expression(italic("x") %*% italic("z") + italic("x") %*% italic("z")),                              
                              "x + z + x * z + z * z"= expression(italic("x") + italic("z") + italic("x") %*% italic("z") + italic("z") %*% italic("z"))))+
  geom_hline(yintercept = 0.05, linetype="dashed")+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 65, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x=element_blank(),
        axis.title.y = element_text(color = "grey20", size = 14, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        strip.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        strip.text.y = element_text(color = "grey20", size = 10, angle = 90, hjust = .5, vjust = .5, face = "plain"))


## Make into a table
figuredata[,11]=NULL
figuredata$IndependentVariables = figuredata$IndependentVariables+1
figuredata$DV[figuredata$DV == 2 ] = 3

figuredata$Set <- factor(figuredata$Set,
                         labels=labs)

figuredata$Type <- factor(figuredata$Type,
                          labels=labels_2latex)


figuredata$Main <- factor(figuredata$Main,levels = c('Without~restrictions', 'With~restrictions'),
                          labels=labels_relatex)
figuredata=figuredata[
  with(figuredata, order(Main, Set)),
  ]
names(figuredata) = c("Restrictions" ,"Set", "Type" , "Sample Size" , "Outlier exclusion", "Correlation","Covariates","Dependent variables","FPP","FPR")


print(xtable(figuredata,digits = 2, type = "latex", align = c("l","l","c","c","c","c","c","c","c","c","c"),caption = "False positive probability (FPP) and false positive ratio (FPR) for the different model sets when using multiple outlier criteria.",label = "tab:apptab2"), caption.placement = "top", include.rownames=FALSE,sanitize.text.function = identity, file = "Table1BBon.tex",scalebox = 0.8)

##Save data
fwrite(figuredata,paste0(output,"/Files/figuredata1B.csv"),sep=";")
Figure1B

## Figure 1C
# Adding an extra covariate

figuredata<-falsepostive[falsepostive$SampleSize==200 & falsepostive$OutlierExclusion=="FALSE" & falsepostive$IndependentVariables==2 & falsepostive$Correlation==0.2 & falsepostive$DV==1
                         & falsepostive$Type!="x1~Normal, z~Binary" & falsepostive$Type!="x1~Binary, z~Normal"
                         & falsepostive$Type!="x1~Binary, z~Binary Effect" & falsepostive$Type!="x1~Normal, z~Binary Effect",]

#Rename labels
figuredata$Type <- factor(figuredata$Type,
                          labels=labels_2)

figuredata$Main <- factor(figuredata$Main,
                          labels=labels_re)

Figure1C =ggplot(figuredata,aes(x=Set))+
  geom_bar(aes(x=Set,y=Pr, fill = "FPP"), stat = "identity",position="dodge")+
  geom_bar(aes(x=Set,y=FPR, fill = "FPR"), stat = "identity",position="dodge")+
  scale_fill_manual(values=c("black","red"))+
  #geom_text( aes(y=round(FPR,3),label=round(FPR,3)), vjust=-2)+
  #geom_text( aes(y=round(Pr,3),label=round(Pr,3)), vjust=-1)+
  facet_grid(Type~Main, scales = "free", labeller=label_parsed)+
  theme_apa()+
  xlab("Model set")+
  ylab("Probability (FPP) / Ratio (FPR)")+
  ylim(0,1) +
  scale_x_discrete(labels = c("x + z" = expression(italic("x") + italic("z")),                                
                              "x * z"= expression(italic("x") %*% italic("z")),                                
                              "z * z"= expression(italic("z") %*% italic("z")),                                
                              "x + z + x * z"= expression(italic("x") + italic("z") + italic("x") %*% italic("z")),                              
                              "x + z + z * z"= expression(italic("x") + italic("z") + italic("z") %*% italic("z")),                              
                              "x * z + z * z"= expression(italic("x") %*% italic("z") + italic("x") %*% italic("z")),                              
                              "x + z + x * z + z * z"= expression(italic("x") + italic("z") + italic("x") %*% italic("z") + italic("z") %*% italic("z"))))+
  geom_hline(yintercept = 0.05, linetype="dashed")+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 65, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x=element_blank(),
        axis.title.y = element_text(color = "grey20", size = 14, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        strip.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        strip.text.y = element_text(color = "grey20", size = 10, angle = 90, hjust = .5, vjust = .5, face = "plain"))
##Save data
fwrite(figuredata,paste0(output,"/Files/figuredata1C.csv"),sep=";")
Figure1C


## Make into a table
figuredata[,11]=NULL
figuredata$IndependentVariables = figuredata$IndependentVariables+1
figuredata$DV[figuredata$DV == 2 ] = 3

figuredata$Set <- factor(figuredata$Set,
                         labels=labs)

figuredata$Type <- factor(figuredata$Type,
                          labels=labels_2latex)


figuredata$Main <- factor(figuredata$Main,levels = c('Without~restrictions', 'With~restrictions'),
                          labels=labels_relatex)
figuredata=figuredata[
  with(figuredata, order(Main, Set)),
  ]
names(figuredata) = c("Restrictions" ,"Set", "Type" , "Sample Size" , "Outlier exclusion", "Correlation","Covariates","Dependent variables","FPP","FPR")


print(xtable(figuredata,digits = 2, type = "latex", align = c("l","l","c","c","c","c","c","c","c","c","c"),caption = "False positive probability (FPP) and false positive ratio (FPR) for the different model sets when adding a extra covariate compared to the baseline case.",label = "tab:apptab3"), caption.placement = "top", include.rownames=FALSE,sanitize.text.function = identity, file = "Table1CBon.tex",scalebox = 0.8)


## Figure 1D
# The effect of bigger sample

figuredata<-as.data.table(falsepostive[ falsepostive$OutlierExclusion=="FALSE" & falsepostive$Correlation==0.2 & falsepostive$IndependentVariables==1 & falsepostive$DV==1
                                        & falsepostive$Type!="x1~Normal, z~Binary" & falsepostive$Type!="x1~Binary, z~Normal"
                                        & falsepostive$Type!="x1~Binary, z~Binary Effect" & falsepostive$Type!="x1~Normal, z~Binary Effect"
                                        ,]
)

#Rename labels
figuredata$Type <- factor(figuredata$Type,
                          labels=labels_2)

figuredata$Main <- factor(figuredata$Main,
                          labels=labels_re)

figuredata$Set <- factor(figuredata$Set,
                         labels=labs_full)


Figure1D<-ggplot(aes(x=SampleSize), data=figuredata)+
  geom_line(aes( y=Pr,color="black")) +
  geom_point(aes( y=Pr),color="black")+
  geom_line(aes( y=FPR,color="red")) +
  geom_point(aes( y=FPR),color="red")+
  scale_color_manual(values = c("black","red"),labels = c("FPP", "FPR"))+
  facet_grid(Set~Main+Type, scales = "free", labeller=label_parsed)+
  ylab("Probability (FPP) / Ratio (FPR)")+
  xlab("Sample size")+
  theme_apa()+
  ylim(0,1) +
  geom_hline(yintercept = 0.05, linetype="dashed")+
  theme(axis.text.x = element_text(color = "grey20", size = 7, angle = 65, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 10, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        strip.text.x = element_text(color = "grey20", size = 8, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        strip.text.y = element_text(color = "grey20", size = 7, angle = 330, hjust = .5, vjust = .5, face = "plain"),
        legend.text = element_text( size = 7))

## Remove the empty facets

g = ggplotGrob(Figure1D)
g$layout
pos <- g$layout$name %in% c("panel-4-4","panel-4-5","panel-4-6","panel-3-6","panel-1-5","panel-3-7")
g$grobs[pos] <- list(nullGrob())

grid.newpage()
grid.draw(g)


##Save data
fwrite(figuredata,paste0(output,"/Files/figuredata1D.csv"),sep=";")

Figure1D


## Make into a table
figuredata[,11]=NULL
figuredata$IndependentVariables = figuredata$IndependentVariables+1
figuredata$DV[figuredata$DV == 2 ] = 3


figuredata$Type <- factor(figuredata$Type,
                          labels=labels_2latex)


figuredata$Set <- factor(figuredata$Set,
                         labels=labs)


figuredata$Main <- factor(figuredata$Main,levels = c('Without~restrictions', 'With~restrictions'),
                          labels=labels_relatex)
figuredata=figuredata[
  with(figuredata, order(Main, Set,SampleSize)),
  ]
names(figuredata) = c("Restrictions" ,"Set", "Type" , "Sample Size" , "Outlier exclusion", "Correlation","Covariates","Dependent variables","FPP","FPR")


print(xtable(figuredata,digits = 2, type = "latex", align = c("l","l","c","c","c","c","c","c","c","c","c"),caption = "False positive probability (FPP) and false positive ratio (FPR) for the different sample sizes"), caption.placement = "top", include.rownames=FALSE,sanitize.text.function = identity, file = "Table1DBon.tex",tabular.environment="longtable",floating = F)


Figure1 = ggarrange(Figure1A, grid.draw(g), Figure1B,Figure1C, 
                    labels = c("A", "B", "C","D"),
                    ncol = 2, nrow = 2)
Figure1


ggsave(Figure1,filename = file.path(output,"Figures","Figure1Bon.jpeg"),width = 15,height = 17)
ggsave(Figure1A,filename = file.path(output,"Figures","Figure1ABon.jpeg"),width = 6,height = 6)
ggsave(Figure1B,filename = file.path(output,"Figures","Figure1BBon.jpeg"),width = 6,height = 6)
ggsave(Figure1C,filename = file.path(output,"Figures","Figure1CBon.jpeg"),width = 6,height = 6)
ggsave(grid.draw(g),filename = file.path(output,"Figures","Figure1DBon.jpeg"),width = 9,height = 6)


### Using the full model set ###


fileplace=paste0(output,"/File/resultsFullSet.csv.gz")
resultsFullset = read.table(gzfile(fileplace),sep=",",header = T)
falsepositverateDataFull<-as.data.table(resultsFullset)

falsepositverateDataFull$rate=ifelse(falsepositverateDataFull$f..>0,1,0)

falsepostiveFULL=falsepositverateDataFull[,list(FPP=mean(rate),FPR=mean(f..)),by=c("Main","Type","SampleSize","OutlierExclusion","Correlation","IndependentVariables")]
falsepostiveFULL$Main=ifelse(falsepostiveFULL$Main==1,"TRUE","FALSE")
falsepostiveFULL$Type=ifelse(falsepostiveFULL$Type==1,"Normal","Binomial")
falsepostiveFULL$IndependentVariables=falsepostiveFULL$IndependentVariables+1
names(falsepostiveFULL)=c("Main","Type","Sample","Outlier","Correlation","Number of Variables","FPP","FPR")
falsepostiveFULL=falsepostiveFULL[order(falsepostiveFULL$Main),]
falsepostiveFULL$Sample = NULL 
falsepostiveFULL$Outlier = NULL 
falsepostiveFULL$Correlation = NULL 
falsepostiveFULL[,3] = NULL 
falsepostiveFULL$Main[falsepostiveFULL$Main=="FALSE"] = "Without"
falsepostiveFULL$Main[falsepostiveFULL$Main=="TRUE"] = "With"
names(falsepostiveFULL) = c("Restrictions on interactions" , "Type" , "FPP" , "FPR")

print(xtable(falsepostiveFULL,digits = 2, type = "latex",caption ="False positive probability (FPP) and false positive ratio (FPR) when looking at all the models possible when the sample size is 200, no outlier criteria is being used and having two covariates. When restrictions on interactions are on main effects should always be present when there is interactions, this is not the case when restrictions on interactions is off."), caption.placement = "top", include.rownames=FALSE, tabular.environment="longtable", file = "FullModelSetBon.tex")

falsepostiveFULL

#### Figures for the appendix #### 


## The effect of higher correlations ##

figuredata<-falsepostive[falsepostive$SampleSize==200 & falsepostive$OutlierExclusion=="FALSE" & falsepostive$IndependentVariables==1 & falsepostive$DV==1
                         
                         & falsepostive$Type!="x1~Binary, z~Binary Effect" & falsepostive$Type!="x1~Normal, z~Binary Effect",]


figuredata$Type <- factor(figuredata$Type,
                          labels=labels_all)

figuredata$Main <- factor(figuredata$Main,
                          labels=labels_re)


Figure2SI<-ggplot(figuredata,aes(x=Set))+
  geom_bar(aes(x=Set,y=Pr, fill = "FPP"), stat = "identity",position="dodge")+
  geom_bar(aes(x=Set,y=FPR, fill = "FPR"), stat = "identity",position="dodge")+
  scale_fill_manual(values=c("black","red"))+
  #geom_text( aes(y=round(FPR,3),label=round(FPR,3)), vjust=-2)+
  #geom_text( aes(y=round(Pr,3),label=round(Pr,3)), vjust=-1)+
  facet_grid(Correlation~Main+Type, scales = "free", labeller=label_parsed)+
  theme_apa()+
  xlab("Model set")+
  ylab("Probability (FPP) / Ratio (FPR)")+
  ylim(0,1) +
  scale_x_discrete(labels = c("x + z" = expression(italic("x") + italic("z")),                                "x * z"= expression(italic("x") %*% italic("z")),                                "z * z"= expression(italic("z") %*% italic("z")),                                "x + z + x * z"= expression(italic("x") + italic("z") + italic("x") %*% italic("z")),                               "x + z + z * z"= expression(italic("x") + italic("z") + italic("z") %*% italic("z")),                               "x * z + z * z"= expression(italic("x") %*% italic("z") + italic("x") %*% italic("z")),                               "x + z + x * z + z * z"= expression(italic("x") + italic("z") + italic("x") %*% italic("z") + italic("z") %*% italic("z"))))+
  geom_hline(yintercept = 0.05, linetype="dashed")+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 65, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x=element_blank(),
        axis.title.y = element_text(color = "grey20", size = 14, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        strip.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        strip.text.y = element_text(color = "grey20", size = 10, angle = 90, hjust = .5, vjust = .5, face = "plain"))

Figure2SI

ggsave(Figure2SI,filename = file.path(output,"Figures","Figure2SIBon.jpeg"),width = 14,height = 8)

## Make into a table
figuredata[,11]=NULL
figuredata$IndependentVariables = figuredata$IndependentVariables+1
figuredata$DV[figuredata$DV == 2 ] = 3

figuredata$Set <- factor(figuredata$Set,
                         labels=labs)

figuredata$Type <- factor(figuredata$Type,
                          labels=labels_4latex)


figuredata$Main <- factor(figuredata$Main,levels = c('Without~restrictions', 'With~restrictions'),
                          labels=labels_relatex)
figuredata=figuredata[
  with(figuredata, order(Main, Set,Correlation)),
  ]
names(figuredata) = c("Restrictions" ,"Set", "Type" , "Sample Size" , "Outlier exclusion", "Correlation","Covariates","Dependent variables","FPP","FPR")


print(xtable(figuredata,digits = 2, type = "latex", align = c("l","l","c","c","c","c","c","c","c","c","c"),caption = "False positive probability (FPP) and false positive ratio (FPR) for the different model sets with different levels of correlations between the dependent variable and the covariates.",label = "tab:apptab5"), caption.placement = "top", include.rownames=FALSE, tabular.environment="longtable",sanitize.text.function = identity, file = "Table2SIBon.tex",floating = F)


## Using several dependent variables
figuredata<-falsepostive[falsepostive$SampleSize==200 & falsepostive$OutlierExclusion=="FALSE" & falsepostive$IndependentVariables==1 & falsepostive$Correlation==0.2 & falsepostive$DV==2
                         
                         & falsepostive$Type!="x1~Binary, z~Binary Effect" & falsepostive$Type!="x1~Normal, z~Binary Effect",]


figuredata$Type <- factor(figuredata$Type,
                          labels=labels_all)

figuredata$Main <- factor(figuredata$Main,
                          labels=labels_re)

Figure3SI = ggplot(figuredata,aes(x=Set))+
  geom_bar(aes(x=Set,y=Pr, fill = "FPP"), stat = "identity",position="dodge")+
  geom_bar(aes(x=Set,y=FPR, fill = "FPR"), stat = "identity",position="dodge")+
  scale_fill_manual(values=c("black","red"))+
  #geom_text( aes(y=round(FPR,3),label=round(FPR,3)), vjust=-2)+
  #geom_text( aes(y=round(Pr,3),label=round(Pr,3)), vjust=-1)+
  facet_grid(Type~Main, scales = "free", labeller=label_parsed)+
  theme_apa()+
  xlab("Model set")+
  ylab("Probability (FPP) / Ratio (FPR)")+
  ylim(0,1) +
  scale_x_discrete(labels = c("x + z" = expression(italic("x") + italic("z")),                                "x * z"= expression(italic("x") %*% italic("z")),                                "z * z"= expression(italic("z") %*% italic("z")),                                "x + z + x * z"= expression(italic("x") + italic("z") + italic("x") %*% italic("z")),                               "x + z + z * z"= expression(italic("x") + italic("z") + italic("z") %*% italic("z")),                               "x * z + z * z"= expression(italic("x") %*% italic("z") + italic("x") %*% italic("z")),                               "x + z + x * z + z * z"= expression(italic("x") + italic("z") + italic("x") %*% italic("z") + italic("z") %*% italic("z"))))+
  geom_hline(yintercept = 0.05, linetype="dashed")+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 65, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x=element_blank(),
        axis.title.y = element_text(color = "grey20", size = 14, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        strip.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        strip.text.y = element_text(color = "grey20", size = 10, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        legend.text = element_text( size = 7))
Figure3SI
ggsave(Figure3SI,filename = file.path(output,"Figures","Figure3SIBon.jpeg"),width = 6,height = 9)


## Make into a table
figuredata[,11]=NULL
figuredata$IndependentVariables = figuredata$IndependentVariables+1
figuredata$DV[figuredata$DV == 2 ] = 3

figuredata$Set <- factor(figuredata$Set,
                         labels=labs)

figuredata$Type <- factor(figuredata$Type,
                          labels=labels_4latex)


figuredata$Main <- factor(figuredata$Main,levels = c('Without~restrictions', 'With~restrictions'),
                          labels=labels_relatex)
figuredata=figuredata[
  with(figuredata, order(Main, Set)),
  ]
names(figuredata) = c("Restrictions" ,"Set", "Type" , "Sample Size" , "Outlier exclusion", "Correlation","Covariates","Dependent variables","FPP","FPR")


print(xtable(figuredata,digits = 2, type = "latex", align = c("l","l","c","c","c","c","c","c","c","c","c"),caption = "False positive probability (FPP) and false positive ratio (FPR) for the different model sets when using two dependent variables and the average of the two (meaning three dependent variables in total).",label = "tab:apptab6"), caption.placement = "top", include.rownames=FALSE,sanitize.text.function = identity, file = "TableSI3Bon.tex",scalebox = 0.8)

## For all sets of variables

figuredata<-falsepostive[falsepostive$SampleSize==200 & falsepostive$OutlierExclusion=="FALSE" & falsepostive$IndependentVariables==1 & falsepostive$Correlation==0.2 & falsepostive$DV==1
                         & falsepostive$Type!="x1~Binary, z~Binary Effect" & falsepostive$Type!="x1~Normal, z~Binary Effect",]

figuredata$Type <- factor(figuredata$Type,
                          labels=labels_all)

figuredata$Main <- factor(figuredata$Main,
                          labels=labels_re)

Figure1ASI = ggplot(figuredata,aes(x=Set))+
  geom_bar(aes(x=Set,y=Pr, fill = "FPP"), stat = "identity",position="dodge")+
  geom_bar(aes(x=Set,y=FPR, fill = "FPR"), stat = "identity",position="dodge")+
  scale_fill_manual(values=c("black","red"))+
  #geom_text( aes(y=round(FPR,3),label=round(FPR,3)), vjust=-2)+
  #geom_text( aes(y=round(Pr,3),label=round(Pr,3)), vjust=-1)+
  facet_grid(Type~Main, scales = "free", labeller=label_parsed)+
  theme_apa()+
  xlab("Model set")+
  ylab("Probability (FPP) / Ratio (FPR)")+
  ylim(0,1) +
  scale_x_discrete(labels = c("x + z" = expression(italic("x") + italic("z")),                                "x * z"= expression(italic("x") %*% italic("z")),                                "z * z"= expression(italic("z") %*% italic("z")),                                "x + z + x * z"= expression(italic("x") + italic("z") + italic("x") %*% italic("z")),                               "x + z + z * z"= expression(italic("x") + italic("z") + italic("z") %*% italic("z")),                               "x * z + z * z"= expression(italic("x") %*% italic("z") + italic("x") %*% italic("z")),                               "x + z + x * z + z * z"= expression(italic("x") + italic("z") + italic("x") %*% italic("z") + italic("z") %*% italic("z"))))+
  geom_hline(yintercept = 0.05, linetype="dashed")+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 65, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x=element_blank(),
        axis.title.y = element_text(color = "grey20", size = 14, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        strip.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        strip.text.y = element_text(color = "grey20", size = 7, angle = 330, hjust = .5, vjust = .5, face = "plain"),
        legend.text = element_text( size = 7))

Figure1ASI

## Figure 1B
# Effect of using outlier citeria, with two covariates and sample size at 200
figuredata<-falsepostive[falsepostive$SampleSize==200 & falsepostive$OutlierExclusion=="TRUE" & falsepostive$IndependentVariables==1 & falsepostive$Correlation==0.2 & falsepostive$DV==1                   
                         & falsepostive$Type!="x1~Binary, z~Binary Effect" & falsepostive$Type!="x1~Normal, z~Binary Effect",]

figuredata$Type <- factor(figuredata$Type,
                          labels=labels_all)

figuredata$Main <- factor(figuredata$Main,
                          labels=labels_re)

Figure1BSI = ggplot(figuredata,aes(x=Set))+
  geom_bar(aes(x=Set,y=Pr, fill = "FPP"), stat = "identity",position="dodge")+
  geom_bar(aes(x=Set,y=FPR, fill = "FPR"), stat = "identity",position="dodge")+
  scale_fill_manual(values=c("black","red"))+
  #geom_text( aes(y=round(FPR,3),label=round(FPR,3)), vjust=-2)+
  #geom_text( aes(y=round(Pr,3),label=round(Pr,3)), vjust=-1)+
  facet_grid(Type~Main, scales = "free", labeller=label_parsed)+
  theme_apa()+
  xlab("Model set")+
  ylab("Probability (FPP) / Ratio (FPR)")+
  ylim(0,1) +
  scale_x_discrete(labels = c("x + z" = expression(italic("x") + italic("z")),                                "x * z"= expression(italic("x") %*% italic("z")),                                "z * z"= expression(italic("z") %*% italic("z")),                                "x + z + x * z"= expression(italic("x") + italic("z") + italic("x") %*% italic("z")),                               "x + z + z * z"= expression(italic("x") + italic("z") + italic("z") %*% italic("z")),                               "x * z + z * z"= expression(italic("x") %*% italic("z") + italic("x") %*% italic("z")),                               "x + z + x * z + z * z"= expression(italic("x") + italic("z") + italic("x") %*% italic("z") + italic("z") %*% italic("z"))))+
  geom_hline(yintercept = 0.05, linetype="dashed")+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 65, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x=element_blank(),
        axis.title.y = element_text(color = "grey20", size = 14, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        strip.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        strip.text.y = element_text(color = "grey20", size = 7, angle = 330, hjust = .5, vjust = .5, face = "plain"),
        legend.text = element_text( size = 7))


##Save data
Figure1BSI

## Figure 1C
# Adding an extra covariate

figuredata<-falsepostive[falsepostive$SampleSize==200 & falsepostive$OutlierExclusion=="FALSE" & falsepostive$IndependentVariables==2 & falsepostive$Correlation==0.2 & falsepostive$DV==1
                         & falsepostive$Type!="x1~Binary, z~Binary Effect" & falsepostive$Type!="x1~Normal, z~Binary Effect",]


figuredata$Type <- factor(figuredata$Type,
                          labels=labels_all)

figuredata$Main <- factor(figuredata$Main,
                          labels=labels_re)

Figure1CSI =ggplot(figuredata,aes(x=Set))+
  geom_bar(aes(x=Set,y=Pr, fill = "FPP"), stat = "identity",position="dodge")+
  geom_bar(aes(x=Set,y=FPR, fill = "FPR"), stat = "identity",position="dodge")+
  scale_fill_manual(values=c("black","red"))+
  #geom_text( aes(y=round(FPR,3),label=round(FPR,3)), vjust=-2)+
  #geom_text( aes(y=round(Pr,3),label=round(Pr,3)), vjust=-1)+
  facet_grid(Type~Main, scales = "free", labeller=label_parsed)+
  theme_apa()+
  xlab("Model set")+
  ylab("Probability (FPP) / Ratio (FPR)")+
  ylim(0,1) +
  scale_x_discrete(labels = c("x + z" = expression(italic("x") + italic("z")),                                "x * z"= expression(italic("x") %*% italic("z")),                                "z * z"= expression(italic("z") %*% italic("z")),                                "x + z + x * z"= expression(italic("x") + italic("z") + italic("x") %*% italic("z")),                               "x + z + z * z"= expression(italic("x") + italic("z") + italic("z") %*% italic("z")),                               "x * z + z * z"= expression(italic("x") %*% italic("z") + italic("x") %*% italic("z")),                               "x + z + x * z + z * z"= expression(italic("x") + italic("z") + italic("x") %*% italic("z") + italic("z") %*% italic("z"))))+
  geom_hline(yintercept = 0.05, linetype="dashed")+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 65, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x=element_blank(),
        axis.title.y = element_text(color = "grey20", size = 14, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        strip.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        strip.text.y = element_text(color = "grey20", size = 7, angle = 330, hjust = .5, vjust = .5, face = "plain"),
        legend.text = element_text( size = 7))


##Save data
Figure1CSI

## Figure 1D
# The effect of bigger sample
figuredata<-as.data.table(falsepostive[ falsepostive$OutlierExclusion=="FALSE" & falsepostive$Correlation==0.2 & falsepostive$IndependentVariables==1 & falsepostive$DV==1
                                        & falsepostive$Type!="x1~Binary, z~Binary Effect" & falsepostive$Type!="x1~Normal, z~Binary Effect",]
)

figuredata$Type <- factor(figuredata$Type,
                          labels=labels_all)

figuredata$Main <- factor(figuredata$Main,
                          labels=labels_re)

figuredata$Set <- factor(figuredata$Set,
                         labels=labs_full)

Figure1DSI<-ggplot(aes(x=SampleSize), data=figuredata)+
  geom_line(aes( y=Pr,color="black")) +
  geom_point(aes( y=Pr),color="black")+
  geom_line(aes( y=FPR,color="red")) +
  geom_point(aes( y=FPR),color="red")+
  scale_color_manual(values = c("black","red"),labels = c("FPP", "FPR"))+
  facet_grid(Set~Main+Type, scales = "free", labeller=label_parsed)+
  ylab("Probability (FPP) / Ratio (FPR)")+
  xlab("Sample size")+
  theme_apa()+
  ylim(0,1) +
  geom_hline(yintercept = 0.05, linetype="dashed")+
  theme(axis.text.x = element_text(color = "grey20", size = 7, angle = 65, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 10, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        strip.text.x = element_text(color = "grey20", size = 5, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        strip.text.y = element_text(color = "grey20", size = 7, angle = 330, hjust = .5, vjust = .5, face = "plain"),
        legend.text=element_text(color = "grey20",size=5),
        legend.position = "none")
Figure1DSI
##Save data
ggsave(Figure1ASI,filename = file.path(output,"Figures","Figure1ASIBon.jpeg"),width = 8,height = 7)
ggsave(Figure1BSI,filename = file.path(output,"Figures","Figure1BSIBon.jpeg"),width = 8,height = 7)
ggsave(Figure1CSI,filename = file.path(output,"Figures","Figure1CSIBon.jpeg"),width = 8,height = 7)
ggsave(Figure1DSI,filename = file.path(output,"Figures","Figure1DSIBon.jpeg"),width = 12,height = 7)


## Make table with all the results


falsepostive=falsepostive[order(falsepostive$Main),]

falsepostive[,11]=NULL
falsepostive$IndependentVariables = falsepostive$IndependentVariables+1
falsepostive$DV[falsepostive$DV == 2 ] = 3
names(falsepostive) = c("Restrictions on interactions" ,"Set", "Type" , "Sample Size" , "Outlier exclusion", "Correlation","Number of covariates","Number of dependent variables","FPP","FPR")

print(xtable(falsepostive,digits = 2, type = "latex",caption ="False positive probability (FPP) and false positive ratio (FPR) when looking at all the different sets under the different condetions. When restrictions on interactions are on main effects should always be present when there is interactions, this is not the case when restrictions on interactions is off."), caption.placement = "top", include.rownames=FALSE, tabular.environment="longtable", file = "AllsetsBon.tex")
