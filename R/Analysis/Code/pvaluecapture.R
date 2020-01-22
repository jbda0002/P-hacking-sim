pvaluemain<-function(models,Formulas,Out,SD,Main,H_1){
  ModelName=NULL
  RModelF=NULL
  RModelI=NULL
  if(SD==T){
    Outlier= NULL
  }
  else{
    Outlier=Out
  }
  
  
  for(i in 1:length(models)){
    
    
    #Finding the name of the model
    
    ModelName<-rbind(Formulas[[i]],ModelName)
    
    if(SD==TRUE){
      Outlier<-rbind(Out,Outlier)
    }
    
    #Collecting the p-value
    coef <-summary(models[[i]])$coefficients
    RModelF<-rbind(coef[2,4],RModelF)
    
    #Make a holder for the p-values
    holder=NA
    #See if ther interaction with H_1 becomes significant
    if(Main==TRUE){
      if(grepl("*",Formulas[[i]], fixed=TRUE)==TRUE){
        
        holder<-pvaluecapture(coef,holder,H_1)
      }
    }
    if(Main==FALSE){  
      if(grepl(paste(H_1,":",sep = ""),Formulas[[i]], fixed=TRUE)==TRUE){
        holder<-pvaluecapture(coef,holder,H_1)
      }
    }
    names(holder)=paste("p",1:length(holder),sep = "")
    RModelI<-dplyr::bind_rows(holder,RModelI)
  }
  
  #Combine all the p-values
  TheModels<-cbind(ModelName,RModelF,RModelI,Outlier)
  return(TheModels)
}


pvaluecapture<-function(coefficients,holder,H_1){
  co<-data.frame(names = rownames(coefficients), coefficients)

  # Make a data.table with only the interaction terms 
  inter <- dplyr::filter(co, grepl("h1:",names)) 
  if(nrow(inter)>=1){
    holder <- data.frame(t(inter[,5]))
  }
  return(holder)
}

