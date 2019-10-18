### P-hacking function ###
## In this p-hacking function there is only one interaction term, and it is the combinantion with all models. 
## This means that there will be models where there is a interaction term with no main-effect term.

phackingFunction<-function(data,y,H_1,interaction = TRUE,SD=FALSE,Per=FALSE){
  ## Loading liberaries
  library(data.table)
  library(ggplot2)
  ## Creating datasets with outliers deleted if sd=TRUE 
  # This part of the code can also be optimized
  if(SD==TRUE){
    #Calculate means and sd for all variables
    #### Does this on individual variables or maybe aggegated like now but on the H_1
    all.means <- sapply(data,mean)
    all.sd<-sapply(data, sd)
    
    #sd*2
    #Miller, 1991
    outlier = ifelse(data > all.means+all.sd*2, 1, 0)
    outlier <-ifelse(rowSums(outlier)>=1,1,0)
    dataoutlier2<-data
    dataoutlier2$outlier<-outlier
    dataoutlier2<-dataoutlier2[!dataoutlier2$outlier==1,]
    dataoutlier2$outlier <- NULL
    
    #sd*2.5
    #Miller, 1991
    outlier = ifelse(data > all.means+all.sd*2.5, 1, 0)
    outlier <-ifelse(rowSums(outlier)>=1,1,0)
    dataoutlier25<-data
    dataoutlier25$outlier<-outlier
    dataoutlier25<-dataoutlier25[!dataoutlier25$outlier==1,]
    dataoutlier25$outlier <- NULL
    
    #sd*3
    #Howell, 1998 - Statistical methods in human sciences
    outlier = ifelse(data > all.means+all.sd*3, 1, 0)
    outlier <-ifelse(rowSums(outlier)>=1,1,0)
    dataoutlier3<-data
    dataoutlier3$outlier<-outlier
    dataoutlier3<-dataoutlier3[!dataoutlier3$outlier==1,]
    dataoutlier3$outlier <- NULL
    
    #Outlier 4
    # outside 1.5 times interquartile range - above and below
    dataoutlier4<-data
    dataoutlier4<-remove_all_outliers1(dataoutlier4)
    
    dataoutlier<-list(dataoutlier2,dataoutlier25,dataoutlier3,dataoutlier4)
  }
  
  #Making objects for saving model versions 
  RModelF = NULL
  RModelI = NULL
  ModelName =NULL
  
  #Collecting and counting all the different variables, except the DV and H_1
  Cols <- names(data)
  Cols <- Cols[! Cols %in% c(y,H_1)] 
  n <- length(Cols)
  
  #Making different combinations of the variables
  Combin <- unlist(
    lapply(1:n, function(i)combn(1:n,i,simplify=FALSE)), recursive=FALSE)
  
  # Look if there are multiple DV
  
  mvDV<-ifelse(length(y)>=2,TRUE,FALSE)
  
  if(mvDV==TRUE){
    #Make average 
    data<-data[c(y,H_1,Cols)]
    data$yavg<-1/length(y)*rowSums(data[1:length(y)])
    
    if(SD==TRUE){
      for (j in 1:length(dataoutlier)) {
        dataoutlier[[j]]<-dataoutlier[[j]][c(y,H_1,Cols)]
        dataoutlier[[j]]$yavg<-1/length(y)*rowSums(dataoutlier[[j]][1:length(y)])
      }
      
    }
    # Add this to the list of dependt variables
    y=c(y,"yavg")
    
    start=NULL
    
    for (i in 1:length(y)) {
      start_i<- paste(c(y[i],H_1),collapse = " ~ ")
      start_i<- paste(c(start_i," + "),collapse = "")
      start<-rbind(start,start_i)
    }
    
  }
  else{
    #Starting of regression
    start<- paste(c(y,H_1),collapse = " ~ ")
    start<- paste(c(start," + "),collapse = "")
  }
  
  
  #Paste all the combinations of the different regressions into a form that can be read by lm()
  Form <- sapply(Combin,function(i)
    paste(start,paste(Cols[i],collapse=" + ")))
  
  
  if(interaction==TRUE){
    
    #Starting of interaction term
    CombinInter <- unlist(
      lapply(1, function(i)combn(1:n,i,simplify=FALSE)), recursive=FALSE)
    
    CombH1<-paste(c(H_1,":"),collapse = "")
    
    #All interactions terms
    Interactions <- sapply(CombinInter,function(i) 
      paste(CombH1,paste(Cols[i],collapse=":")))
    
    # Make all the combinations of the interaction terms and formulas from before
    
    # -----------------------------------------
    # test med en version hvor der ikke er interaktioner uden main effects
    # --------------------------------------------
    FormulasInteraction<-apply(expand.grid(Form, Interactions), 1, paste, collapse="+")
    #Set them together in one string
    Formulas<-c(Form,FormulasInteraction)
    
    
  }
  else{
    Formulas=Form
  }
  
  
  #Running all the models
  models<-lapply(Formulas,function(i)
    lm(as.formula(i),data=data))
  
  #Collecting the p-values
  
  for(i in 1:length(models)){
    
    
    #Finding the name of the model
    mod<-Formulas[[i]]
    ModelName<-rbind(mod,ModelName)
    
    #Collecting the p-value
    coef <-summary(models[[i]])$coefficients
    p_h_1<-coef[2,4] 
    RModelF<-rbind(p_h_1,RModelF)
    
    #See if ther interaction with H_1 becomes significant
    if(grepl(":",mod)==TRUE){
      p_h_2<-coef[nrow(coef),4] 
    }
    else{
      p_h_2<-1 
    }
    RModelI<-rbind(p_h_2,RModelI)
  }
  
  
  #Combine all the p-values
  TheModels<-cbind(ModelName,RModelF,RModelI)
  TheModels<-as.data.table(TheModels)
  
  #making them from factor to numeric
  TheModels$V2<-as.numeric(as.character(TheModels$V2))
  TheModels$V3<-as.numeric(as.character(TheModels$V3))
  TheModels$Outlier<-"Non removed"
  
  RModelF = NULL
  RModelI = NULL
  ModelName =NULL
  Outliers= NULL
  
  
  
  if(SD==TRUE){
    for(j in 1:length(dataoutlier)){
      models_out<-lapply(Formulas,function(i)
        lm(as.formula(i),data=dataoutlier[[j]]))
      for(i in 1:length(models_out)){
        
        
        #Finding the name of the model
        mod<-Formulas[[i]]
        ModelName<-rbind(mod,ModelName)
        Outliers<-rbind(j,Outliers)
        
        #Collecting the p-value
        coef <-summary(models_out[[i]])$coefficients
        p_h_1<-coef[2,4] 
        RModelF<-rbind(p_h_1,RModelF)
        
        #See if ther interaction with H_1 becomes significant
        if(grepl(":",mod)==TRUE){
          p_h_2<-coef[nrow(coef),4] 
        }
        else{
          p_h_2<-1 
        }
        RModelI<-rbind(p_h_2,RModelI)
      }
    }
    TheModels_outlier<-cbind(ModelName,RModelF,RModelI,Outliers)
    TheModels_outlier<-as.data.table(TheModels_outlier)
    
    #making them from factor to numeric
    TheModels_outlier$V2<-as.numeric(as.character(TheModels_outlier$V2))
    TheModels_outlier$V3<-as.numeric(as.character(TheModels_outlier$V3))
    colnames(TheModels_outlier)[4]<-"Outlier"
    
    #Make the names such that they mach up to the outlier criteria. 
    
    
    #Combine the 2 datastets
    TheModels<-rbind(TheModels_outlier,TheModels)
  }
  
  
  #Order the models if one wants all the models out in the end
  TheModels<- TheModels[with(TheModels, order(V2)), ]
  
  
  
  Models <- TheModels[(TheModels$V2<=0.05|TheModels$V3<=0.05  ),]
  
  if(Per==FALSE){
    res <- ifelse(nrow(Models)>=1,1,0)
  }
  else{
    res <- nrow(Models)/nrow(TheModels)
  }
  
  
  return(res)
}
