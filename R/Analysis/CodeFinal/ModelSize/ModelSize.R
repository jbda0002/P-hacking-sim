setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
## Number of covarietes

covarites = c(2,3,4,5,6)

#Main = F
#Ma
Ma = function(n){
  number = 2^n
  number
}
## CCI
CCI = function(n){
  number = 2^(n*(n-1)/2-1)
  number
}
## HCI
HCI = function(n){
  number = 2^n-1
  number
}
## Ma + CCI
Ma_CCI = function(n){
  number = (2^n-1)*(2^(n*(n-1)/2)-1)
  number
}
## Ma + HCI
Ma_HCI = function(n){
  number = (2^n-1)^2
  number
}
## HCI + CCI
HCI_CCI = function(n){
  number = (2^n-1)*(2^(n*(n-1)/2)-1)
  number
}
## Ma + HCI + CCI
Ma_HCI_CCI = function(n){
  number = (2^n-1)^2*(2^(n*(n-1)/2)-1)
  number
}

TableModelsFalse = NULL
## Make the table
for (i in covarites) {
  number_of_models=c(Ma(i),HCI(i),CCI(i),Ma_HCI(i),Ma_CCI(i),HCI_CCI(i),Ma_HCI_CCI(i))
  fullset=sum(number_of_models)
  set=c(i,number_of_models,fullset)
  TableModelsFalse=rbind(TableModelsFalse,set)
}
TableModelsFalse=as.data.frame(TableModelsFalse)
names(TableModelsFalse)=c("Number of covariates" , "ME" ,"HCI","CCI", "ME+HCI" , "ME+CCI","HCI+CCI", "ME+HCI+CCI", "Number of models")

## Model set size when there is no specefic variable of intrest
TableModelsFalseML = NULL
## Make the table
for (i in covarites) {
  number_of_models=c(Ma(i),CCI(i),Ma_CCI(i))
  fullset=sum(number_of_models)
  set=c(i,number_of_models,fullset)
  TableModelsFalseML=rbind(TableModelsFalseML,set)
}
TableModelsFalseML=as.data.frame(TableModelsFalseML)
names(TableModelsFalseML)=c("Number of covariates" , "ME" ,"CCI" , "ME+CCI", "Number of models")


## Main = T 

#Ma
Ma = function(n){
  number = 2^n
  number
}


## Ma + CCI
Ma_CCI = function(n){
  number=NULL
  for (i in 2:n) {
    
    number=c(number,choose(n,i)*(2^(i*(i-1)/2)-1))
  }
  number = sum(number)
  number
}
## Ma + HCI
Ma_HCI = function(n){
  number=NULL
  for (i in 1:n) {
    number=c(number,choose(n,i)*(2^(i)-1))
  }
  number = sum(number)
  number
}

## Ma + HCI + CCI
Ma_HCI_CCI = function(n){
  number=NULL
  for (i in 1:n) {
    number=c(number,choose(n,i)*(2^(i*(i-1)/2)-1)*(2^i-1))
  }
  number = sum(number)
  number
}

TableModelsTrue = NULL
## Make the table
for (i in covarites) {
  number_of_models=c(Ma(i),Ma_HCI(i),Ma_CCI(i),Ma_HCI_CCI(i))
  fullset=sum(number_of_models)
  set=c(i,number_of_models,fullset)
  TableModelsTrue=rbind(TableModelsTrue,set)
}
TableModelsTrue=as.data.frame(TableModelsTrue)
rownames(TableModelsTrue)=NULL
names(TableModelsTrue)=c("Number of covariates", "ME","ME+HCI", "ME+CCI", "ME+HCI+CCI", "Number of models")

## When there is no specefic variable of interest
TableModelsTrueML = NULL
## Make the table
for (i in covarites) {
  number_of_models=c(Ma(i),Ma_CCI(i))
  fullset=sum(number_of_models)
  set=c(i,number_of_models,fullset)
  TableModelsTrueML=rbind(TableModelsTrueML,set)
}
TableModelsTrueML=as.data.frame(TableModelsTrueML)
rownames(TableModelsTrueML)=NULL
names(TableModelsTrueML)=c("Number of covariates", "ME", "ME+CCI", "Number of models")


## Save tables 
comment          <- list()
comment$pos      <- list()
comment$pos[[1]] <- c(nrow(variables))
comment$command  <- c(paste("\\hline \n", 
                            "{\\textbf{Note: }ME = models with the main effects only; ME + HCI = models with the main effects and interactions between the variable of interest and covariates; ME + CCI = models with the main effects and interactions between covariates; ME + HCI + CCI = models with the main effects and the interactions between the variable of interest and covariates and the interactions between covariates. \n",
                            sep = ""))
caption_True = "The total number of models for any given set considering the different number of covariates with the restriction that the main effects should always be present when there are the interaction effects."

library(xtable)
print(xtable(TableModelsTrue,digits = 0, type = "latex",
             caption =caption_True,align = c("l","l","c","c","c","c","c"),auto = T ), caption.placement = "top",add.to.row =comment,include.rownames=FALSE, file = "ModelNumberTrue.tex", table.placement = "!h")
print(xtable(TableModelsTrueML,digits = 0, type = "latex",
             caption ="" ,auto = T), caption.placement = "top",include.rownames=FALSE, file = "ModelNumberTrueML.tex", table.placement = "!h")
print(xtable(TableModelsFalse,digits = 0, type = "latex",caption ="",auto = T), caption.placement = "top", include.rownames=FALSE, file = "ModelNumberFalse.tex", table.placement = "!h",scalebox='0.8')
print(xtable(TableModelsFalseML,digits = 0, type = "latex",caption ="",auto = T), caption.placement = "top", include.rownames=FALSE, file = "ModelNumberFalseML.tex", table.placement = "!h")
