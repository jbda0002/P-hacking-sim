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
names(TableModelsFalse)=c("Number of covariates" , "Ma" ,"HCI","CCI", "Ma+HCI" , "Ma+CCI","HCI+CCI", "Ma+HCI+CCI", "Number of models")

## Main = T 
#Main = F
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
names(TableModelsTrue)=c("Number of covariates", "Ma","Ma+HCI", "Ma+CCI", "Ma+HCI+CCI", "Number of models")


## Save tables 


library(xtable)
print(xtable(TableModelsTrue,digits = 0, type = "latex",
             auto=T,include.rownames=FALSE), file = "ModelNumber.tex")
print(xtable(TableModelsFalse,digits = 0, type = "latex", include.rownames=FALSE), file = "ModelNumberFalse.tex")
