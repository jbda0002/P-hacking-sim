pvaluecapture<-function(coefficients,holder,H_1){
  co<-as.data.frame(coefficients)
  co$names<-rownames(co)
  co<-as.data.table(co)
  
  # Make a data.table with only the interaction terms 
  Int<-paste(H_1,":",sep = "")
  inter <- co[like(names,Int)]
  if(nrow(inter)>=1){
    for (i in 1:nrow(inter)) {
      #Put the p-values of the interaction terms into the holder
      p<-inter[i,4] 
      holder[i]<-p
    }
  }
  return(holder)
}

pvaluecaptureaov<-function(coefficients,holder,H_1){
  co<-as.data.frame(coefficients)
  co$names<-rownames(co)
  co<-as.data.table(co)
  
  # Make a data.table with only the interaction terms 
  Int<-paste(H_1,":",sep = "")
  inter <- co[like(names,Int)]
  if(nrow(inter)>=1){
    for (i in 1:nrow(inter)) {
      #Put the p-values of the interaction terms into the holder
      p<-inter[i,5] 
      holder[i]<-p
    }
  }
  return(holder)
}
