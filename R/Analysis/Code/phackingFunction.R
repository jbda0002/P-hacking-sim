### P-hacking function ###
## In this function if interaction is TRUE there will be an interaction between all main effects and the H_1 variable
## Furtheremore, there is four different outlier criterias there will be tested
## large1 and large2 cannot be TRUE at the same time - if using any of these the modelset will explode!!

## Setting interaction = TRUE: Use Power(1) and Power(2) but not the combinations
## Setting Large1 = TRUE:  Use Power(1) and Power(2) and the combinations
## Setting Large2 = TRUE: Use Power(1), Power(2) and Power(3) without there combinations
## Setting Large3 = TRUE: Use Power(1), Power(2) and Power(3) and there combinations
phackingFunction<-function(data,y,H_1,interaction = TRUE,SD=FALSE,Per=FALSE,large1=FALSE,large2=FALSE,large3=FALSE){
  ## Loading liberaries
  library(data.table)
  library(dplyr)
  ## Creating datasets with outliers deleted if sd=TRUE 
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
  

  
  #Collecting and counting all the different variables, except the DV and H_1
  Cols <- names(data)
  Cols <- Cols[! Cols %in% c(y,H_1)] 
  n <- length(Cols)
  
  #Making objects for saving model versions. RModelI has the dimensions that is needed to collect all the p-values
  RModelF = NULL
  RModelI = as.data.frame(matrix(0, ncol = n, nrow = 0))
  ModelName =NULL
  
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
    #Starting of regression if there is not several dependent variables
    start<- paste(c(y,H_1),collapse = " ~ ")
    start<- paste(c(start," + "),collapse = "")
  }
  
  ## Make all the regressions without interaction terms. 
  Formulas=sapply(Combin,function(i)
    paste(start,paste(Cols[i],collapse=" + ")))
  Power1=Formulas
  
  if(large1==TRUE ){
    interaction=TRUE
  }
  
  if(large2==TRUE ){
    interaction=TRUE
  }
  if(large3==TRUE ){
    interaction=TRUE
    large1=TRUE
    large2=TRUE
  }
  
  if(interaction==TRUE){
    ## This is Power(1) and Power(2)
    #Starting of interaction term
    CombinInter <- unlist(
      lapply(1, function(i)combn(1:n,i,simplify=FALSE)), recursive=FALSE)
    
    ## Put together the random variable and all the dependent variables 
    CombH1<-paste(c(H_1,"*"),collapse = "")
    
    #All interactions terms
    Interactions <- sapply(CombinInter,function(i) 
      paste(CombH1,paste(Cols[i],collapse="")))
    
    ## Make the models with the interaction terms 
    FormulasIN <- sapply(Combin,function(i)
      paste(start,paste(Interactions[i],collapse=" + ")))
    
    Formulas=c(Formulas,FormulasIN)
    Power2=FormulasIN
    
    if(large1==TRUE){
      ## Put together Power(1) and Power(2)
      ## There are models there are repeated here, so not a effecient set of models
      
      
      # Make all combinations of the two-way interactions
      Interactions2 <- unlist(
        lapply(1:length(Interactions), function(i)combn(1:length(Interactions),i,simplify=FALSE)), recursive=FALSE)
      # But in the names of the variables and but a plus inbetween
      Interactions_2=sapply(Interactions2,function(i)
        paste(paste(Interactions[i],collapse="+")))
      
      
      FormulasIN12=expand.grid(Power1,Interactions_2)
      Power12=apply(FormulasIN12, 1, paste, collapse="+") 
      
      
      Formulas=c(Power12,Power2,Power1)
      
    }
    if(large2==TRUE){
      ## Make Power(3) to put together with Power(1) and Power(2)
      
      #Make all the combinations of two-way interactions
      CombinTwo <- unlist(
        lapply(2, function(i)combn(1:n,i,simplify=FALSE)), recursive=FALSE)
      
      # Make all the combinations with the names from Cols
      Twoway=sapply(CombinTwo,function(i)
        paste(paste(Cols[i],collapse="*")))
      
      # Make all combinations of the two-way interactions
      CombinTwo_2 <- unlist(
        lapply(1:length(CombinTwo), function(i)combn(1:length(CombinTwo),i,simplify=FALSE)), recursive=FALSE)
      # But in the names of the variables and but a plus inbetween
      Twoway_2=sapply(CombinTwo_2,function(i)
         paste(paste(Twoway[i],collapse="+")))
      
      
      Power3=sapply(CombinTwo_2,function(i)
        paste(start,paste(Twoway_2[i],collapse=" + ")))
      
      Formulas=c(Power1,Power2,Power3)
      
    }

    if(large3==TRUE){
   
    Power23=expand.grid(Interactions_2,Twoway_2)
    Power13=expand.grid(Power1,Twoway_2)
    
    ## Make all combinations, but where the main effect for the interaction is always there
    Formulas23=apply(Power23, 1, paste, collapse="+")
    Power13=apply(Power13, 1, paste, collapse="+")
    
    Power23=paste(start, Formulas23, sep="")
    Power123=expand.grid(Power1,Formulas23)
    
    Formulas=c(apply(Power123, 1, paste, collapse="+"),Power1,Power2,Power3,Power12,Power13,Power23)
    
    }
    ## Combine with the models there have no interaction terms 
    
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
    
    #Make a holder for the p-values
    holder<-as.data.frame(matrix(1, ncol = n, nrow = 1))
    
    #See if ther interaction with H_1 becomes significant
    if(grepl("*",mod, fixed=TRUE)==TRUE){
      coef<-as.data.frame(coef)
      coef$names<-rownames(coef)
      coef<-as.data.table(coef)
      
      # Make a data.table with only the interaction terms 
      inter <- coef[like(names,"x1:")]
      
      if(nrow(inter)>=1){
      for (i in 1:nrow(inter)) {
        #Put the p-values of the interaction terms into the holder
        p<-inter[i,4] 
        holder[i]<-p
      }
      }
    }
 
    RModelI<-rbind(holder,RModelI)
  }
  
  
  #Combine all the p-values
  TheModels<-cbind(ModelName,RModelF,RModelI)
  TheModels<-as.data.table(TheModels)
  
  #Make a variable to indicate that there are nothing from the data removed
  TheModels$Outlier<-"Non removed"
  
  # Clean out the holders for the result 
  RModelF = NULL
  RModelI = NULL
  ModelName =NULL
  Outlier= NULL
  
  
  
  if(SD==TRUE){
    for(j in 1:length(dataoutlier)){
      
      ## Run all the models with the different data
      models_out<-lapply(Formulas,function(i)
        lm(as.formula(i),data=dataoutlier[[j]]))
      for(i in 1:length(models_out)){
        
        
        #Finding the name of the model
        mod<-Formulas[[i]]
        ModelName<-rbind(mod,ModelName)
        
        ## Make the variable to indicate which outlier criteria 
        Outlier<-rbind(j,Outlier)
        
        #Collecting the p-value
        coef <-summary(models_out[[i]])$coefficients
        p_h_1<-coef[2,4] 
        RModelF<-rbind(p_h_1,RModelF)
        
        #Make a holder for the p-values
        holder<-as.data.frame(matrix(1, ncol = n, nrow = 1))
        
        #See if ther interaction with H_1 becomes significant
        if(grepl("*",mod, fixed=TRUE)==TRUE){
          coef<-as.data.frame(coef)
          coef$names<-rownames(coef)
          coef<-as.data.table(coef)
          
          # Make a data.table with only the interaction terms 
          inter <- coef[like(names,"x1:")]
          if(nrow(inter)>=1){
          for (i in 1:nrow(inter)) {
            #Put the p-values of the interaction terms into the holder
            p<-inter[i,4] 
            holder[i]<-p
          }
          
        }
        }
        RModelI<-rbind(holder,RModelI)
      }
    }
    TheModels_outlier<-cbind(ModelName,RModelF,RModelI,Outlier)
    TheModels_outlier<-as.data.table(TheModels_outlier)
    
    #Combine the 2 datastets
    TheModels<-rbind(TheModels_outlier,TheModels)
  }
  
  
  #Order the models if one wants all the models out in the end
  TheModels<- TheModels[with(TheModels, order(RModelF)), ]
  
  Models<-TheModels[apply(TheModels[, 2:ncol(TheModels)] <= 0.05, 1, any, na.rm=TRUE), ]
  

  
  if(Per==FALSE){
    res <- ifelse(nrow(Models)>=1,1,0)
  }
  else{
    res <- nrow(Models)/nrow(TheModels)
  }
  
  
  return(res)
}
