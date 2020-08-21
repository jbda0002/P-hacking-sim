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
  number = 2^(n*(n-1)/2)
  number
}
## HCI
HCI = function(n){
  number = 2^n-1
  number
}
## Ma + CCI
Ma_CCI = function(n){
  number = (2^n-1)*(2^(n*(n-1)/2))
  number
}
## Ma + HCI
Ma_HCI = function(n){
  number = (2^n-1)^2
  number
}
## HCI + CCI
HCI_CCI = function(n){
  number = (2^n-1)*(2^(n*(n-1)/2))
  number
}
## Ma + HCI + CCI
Ma_HCI_CCI = function(n){
  number = (2^n-1)^2*(2^(n*(n-1)/2))
  number
}

TableModelsFalse = NULL
## Make the table
for (i in covarites) {
  number_of_models=c(Ma(i),HCI(i),CCI(i),Ma_HCI(i),Ma_CCI(i),HCI_CCI(i),Ma_HCI_CCI(i))
  fullset=sum(number_of_models)
  set=c(number_of_models,fullset)
  TableModelsFalse=rbind(TableModelsFalse,set)
}

## Main = T 
#Main = F
#Ma
Ma = function(n){
  number = 2^n
  number
}


## Ma + CCI
Ma_CCI = function(n){
  number = (2^n-1)*(2^(n*(n-1)/2))
  number
}
## Ma + HCI
Ma_HCI = function(n){
  number = (2^n-1)^2
  number
}
## HCI + CCI
HCI_CCI = function(n){
  number = (2^n-1)*(2^(n*(n-1)/2))
  number
}
## Ma + HCI + CCI
Ma_HCI_CCI = function(n){
  number = (2^n-1)^2*(2^(n*(n-1)/2))
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
