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
names(TableModelsFalse)=c("Model sets" , "ME" ,"X * Cov","Cov * Cov", "ME + X * Cov" , "ME + Cov * Cov","X * Cov + Cov * Cov", "ME + X * Cov + Cov * Cov", "Number of total models")

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
names(TableModelsFalseML)=c("Model sets" , "ME" ,"Cov * Cov" , "ME + Cov * Cov", "Number of total models")


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
names(TableModelsTrue)=c("Model sets", "ME","ME + X * Cov", "ME + Cov * Cov", "ME+ X * Cov + Cov * Cov", "Number of total models")

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
names(TableModelsTrueML)=c("Model sets", "ME", "ME + Cov * Cov", "Number of total models")

## Transpose the table other otherwise it become sto wide
TableModelsTrue_t = as.data.frame(t(TableModelsTrue))
names(TableModelsTrue_t) <- as.matrix(TableModelsTrue_t[1, ])
TableModelsTrue_t <- TableModelsTrue_t[-1, ]

TableModelsFalse_t = as.data.frame(t(TableModelsFalse))
names(TableModelsFalse_t) <- as.matrix(TableModelsFalse_t[1, ])
TableModelsFalse_t <- TableModelsFalse_t[-1, ]


## Save tables 
comment          <- list()
comment$pos      <- list()
comment$pos[[1]] <- c(nrow(TableModelsTrue_t))
comment$command  <- c(paste("\\hline \n", 
                            "\\multicolumn{6}{p{12cm}}{\\footnotesize{Note: ME = models with main effects only; ME + X * Cov = models with main effects and interactions between the variable of interest and covariates; ME + Cov * Cov = models with main effects and interactions between covariates; ME + X * Cov + Cov * Cov = models with main effects and interactions between the variable of interest and covariates and the interactions between covariates.}} \n",
                            sep = ""))
caption_True = "The total number of models for any given set considering the different number of covariates with the restriction that the main effects should always be present when there are interaction effects."
caption_TrueML = "The total number of models when there is no variable of interest for any given set considering the different number of covariates with the restriction that the main effects should always be present when there are interaction effects"
library(xtable)


print(xtable(TableModelsTrue_t,digits = 0, type = "latex",
             caption = caption_True, align = c("l","c","c","c","c","c"),auto = T ), caption.placement = "top",include.rownames=T, file = "ModelNumberTrue.tex", table.placement = "!h",add.to.row =comment)
print(xtable(t(TableModelsTrueML),digits = 0, type = "latex",
             caption =caption_TrueML ,auto = T), caption.placement = "top",include.rownames=T, file = "ModelNumberTrueML.tex", table.placement = "!h")

## When MAIN = F
comment          <- list()
comment$pos      <- list()
comment$pos[[1]] <- c(nrow(TableModelsFalse_t))
comment$command  <- c(paste("\\hline \n", 
                            "\\multicolumn{6}{p{13cm}}{\\footnotesize{Note: ME = models with main effects only; X * Cov = models with interactions between the variable of interest and covariates; Cov * Cov = models with interactions between covariates;  ME + X * Cov = models with main effects and interactions between the variable of interest and covariates; ME + Cov * Cov = models with main effects and interactions between covariates; X * Cov + Cov * Cov = models with interactions between covariates and variable of interest and interactions between covariates; ME + X * Cov + Cov * Cov = models with main effects and interactions between the variable of interest and covariates and the interactions between covariates.}} \n",
                            sep = ""))
caption_False = "The total number of models for any given set considering the different number of covariates and with no restriction that main effects should be present when having interaction effects."
caption_FalseML = "The total number of models when there is no variable of interest for any given set considering the different number of covariates and with no restriction that main effects should be present when having interaction effects."

print(xtable(TableModelsFalse_t,digits = 0, type = "latex",caption =caption_False, align = c("l","c","c","c","c","c"), auto = T), caption.placement = "top", include.rownames=T, file = "ModelNumberFalse.tex", table.placement = "!h",add.to.row =comment)
print(xtable(t(TableModelsFalseML),digits = 0, type = "latex",caption =caption_FalseML,auto = T), caption.placement = "top", include.rownames=T, file = "ModelNumberFalseML.tex", table.placement = "!h")



## Put table into one big

FUllModels = as.data.frame(rbind(TableModelsTrue_t,TableModelsFalse_t))

## Remove the repeated part
FUllModels = setDT(FUllModels, keep.rownames = TRUE)[]
names(FUllModels) = c("Model set","2","3","4","5","6")

comment          <- list()
comment$pos      <- list()
comment$pos[[1]] <- c(nrow(FUllModels))
comment$command  <- c(paste("\\hline \n", 
                            "\\multicolumn{6}{p{13cm}}{\\footnotesize{Note: ME = models with main effects only; X * Cov = models with interactions between the variable of interest and covariates; Cov * Cov = models with interactions between covariates;  ME + X * Cov = models with main effects and interactions between the variable of interest and covariates; ME + Cov * Cov = models with main effects and interactions between covariates; X * Cov + Cov * Cov = models with interactions between covariates and variable of interest and interactions between covariates; ME + X * Cov + Cov * Cov = models with main effects and interactions between the variable of interest and covariates and the interactions between covariates.}} \n",
                            sep = ""))

#print(xtable(FUllModels,digits = 0, type = "latex",caption =caption_False, align = c("l","c","c","c","c","c","c"), auto = T), caption.placement = "top", include.rownames=F, file = "FUllModels.tex", table.placement = "!h",add.to.row =comment)
