### P-hacking function ###
## This function has the ability to p-hack data with the help of the research degrees of freedom described in 
## Christensen, J. D., Orquin, J. L., Perkovic, S., Lagerkvist, C. J.,

## The different options:
## Inserting multiple dependent variables the function wil test of all them including the average of them
## Setting Main = False: This will allow models where there are interaction terms without the corresponding main effect
## Power_12 = TRUE: This gives you the combination of Power(1)+Power(1+2) 
## Power_2 = TRUE: This can only be used if Main = FALSE, this gives the models in Power(2). If TRUE and Main = TRUE it will give you the same as Power_12=TRUE
## Power_13 = TRUE: This gives the combinations in Power(1+3)
## Power_3 = TRUE: This gives Power(3) if Main=FALSE otherwise it will give the same as Power_13=TRUE
## Power_23 = TRUE: This gives you Power(2+3). This is however only possible if Main = FALSE
## Power_123 = TRUE: Gives you the models in Power(1+2+3). Using this will explode the model set. 
## SD = TRUE: Uses four different outlier criteria


### Function that are being used in the function. 
## Sonja: I'm not sure where to have these in all this. I'm looking into making more functions in the other parts as well
## I haven't figured out the factor part yet, but sure it will come :) - mainly how to extract the p-values without that the code becomes really long

#setwd("/Users/au549385/Dropbox/P-Hacking paper/R/Analysis/Code")
source(here::here("Dropbox", "P-Hacking paper", "R", "Analysis", "Code", "remove_outliers_sd.R"))
source(here::here("Dropbox", "P-Hacking paper", "R", "Analysis", "Code", "remove_outliers_range.R"))
source(here::here("Dropbox", "P-Hacking paper", "R", "Analysis", "Code", "pvaluecapture.R"))
source(here::here("Dropbox", "P-Hacking paper", "R", "Analysis", "Code", "addmaineffects.R"))

# ## Jacob
# source(here::here("remove_outliers_sd.R"))
# source(here::here( "remove_outliers_range.R"))
# source(here::here("pvaluecapture.R"))
# source(here::here("addmaineffects.R"))

# source("remove_outliers_sd.R")
# source("remove_outliers_range.R")

phackingFunction<-function(data,y,H_1,Power_2 = FALSE,Power_3=FALSE,Power_12=FALSE, Power_13=FALSE
                           ,Power_23=FALSE,Power_123=FALSE
                           ,SD=FALSE,Per=FALSE,Main=TRUE,pvalue=0.05){
  
  library(dplyr)
  ## delete outliers if SD = TRUE 
  
  if(SD==TRUE){
    
    #### Does this on individual variables or maybe aggegated like now but on the H_1
    #comment 1
    
    # sd * 2 (Miller, 1991)
    outlier1 <- remove_outliers_sd(data, 2)
    
    # sd * 2.5 (Miller, 1991)
    outlier2 <- remove_outliers_sd(data, 2.5)
    
    # sd * 3 (Howell, 1998)
    outlier3 <- remove_outliers_sd(data, 3)
    
    # outside 1.5 times interquartile range (above and below)
    outlier4 <- remove_numeric_outliers(data)
    
    # combine all outlier data sets in a list
    dataoutlier <-
      list(outlier1, outlier2, outlier3, outlier4)
  }
  
  #Collecting and counting all the different variables, except the DV and H_1
  Cols <- names(data)
  Cols <- Cols[! Cols %in% c(y,H_1)] 
  n <- length(Cols)
  
  #Making objects for saving model versions. RModelI has the dimensions that is needed to collect all the p-values
  ModelName = NULL
  
  ## If h1 is a factor, then the placeholer for the interaction must be changed
  if(sapply(data[grep(H_1, colnames(data))], is.factor)==TRUE){ 
    
    level=sapply(data[grep(H_1, colnames(data))], nlevels)-1 
    RModelI = as.data.frame(matrix(0, ncol = n*level, nrow = 0))
    RModelF=as.data.frame(matrix(0, ncol = level, nrow = 0))
  }
  
  if(sapply(data[grep(H_1, colnames(data))], is.factor)==F){ 
    
    RModelI = as.data.frame(matrix(0, ncol = n, nrow = 0))
    RModelF = NULL
    RModelIaov = as.data.frame(matrix(0, ncol = n, nrow = 0))
    RModelFaov = NULL
  }
  #Making different combinations of the variables
  Combin <- unlist(
    lapply(1:n, function(i)combn(1:n,i,simplify=FALSE)), recursive=FALSE)
  
  # Look if there are multiple DV
  ## MOve this to the start of the code, such that we don't need the for loop
  
  mvDV <- ifelse(length(y) >= 2, TRUE, FALSE)
  
  if (mvDV == TRUE) {
    #Make average
    data <- data[c(y, H_1, Cols)]
    data$yavg <-
      1 / length(y) * rowSums(data[1:length(y)]) #double check that it outputs correct result
    
    if (SD == TRUE) {
      for (j in 1:length(dataoutlier)) {
        dataoutlier[[j]] <- dataoutlier[[j]][c(y, H_1, Cols)]
        dataoutlier[[j]]$yavg <-
          1 / length(y) * rowSums(dataoutlier[[j]][1:length(y)])
      }
      
    }
    # Add this to the list of dependent variables
    y = c(y, "yavg")
    
    start = NULL
    
    for (i in 1:length(y)) {
      start_i <- paste(c(y[i], "~", H_1, "+"), collapse = " ")
      start <- rbind(start, start_i)
    }
    
  } else {
    #Starting of regression if there is no several dependent variables
    start <- paste(c(y, "~", H_1, "+"), collapse = " ")
  }
  
  ## Make all the regressions without interaction terms. 
  Formulas=sapply(Combin,function(i)
    paste(start,paste(Cols[i],collapse=" + "))) #I removed + sign here and paste0 (because paste0 didn't create space between + and the next element)
  Power1=Formulas #why assigned to power1?
  
  if(Power_2==TRUE & Main == TRUE){
    Power_12=TRUE
  }
  
  if(Power_3==TRUE & Main == TRUE){
    Power_13=TRUE
  }
  
  
  if(Main==TRUE){
    
    if(Power_12==TRUE){
      ## This is Power(1) and Power(2)
      #Starting of interaction term
      CombinInter <- unlist(
        lapply(1, function(i)combn(1:n,i,simplify=FALSE)), recursive=FALSE)
      
      ## Put together the random variable and all the dependent variables 
      CombH1<-paste(c(H_1,"*"),collapse =" ")
      
      #All interactions terms
      Interactions <- sapply(CombinInter,function(i) 
        paste(CombH1,paste(Cols[i],collapse=" ")))
      
      ## Make the models with the interaction terms 
      FormulasIN <- sapply(Combin,function(i)
        paste(start,paste(Interactions[i],collapse=" + "))) #I removed plus
      
      ## Adding the main effects where it is possible
      
      FoirmulasInC<-InterceptMain(Combin,Cols,FormulasIN)
      
      ## Combine Power(1+2) with Power(1) and Power(2)
      Formulas=c(Power1,FormulasIN,FoirmulasInC)
      Power12=FoirmulasInC
      
      Power2=FormulasIN
      
    }
    if(Power_13==TRUE){
      ## Make Power(3) to put together with Power(1) and Power(2)
      
      #Make all the combinations of two-way interactions
      CombinTwo <- unlist(
        lapply(2, function(i)combn(1:n,i,simplify=FALSE)), recursive=FALSE)
      
      # Make all the combinations with the names from Cols
      Twoway=sapply(CombinTwo,function(i)
        paste0(paste0(Cols[i],collapse="*")))
      
      # Make all combinations of the two-way interactions
      CombinTwo_2 <- unlist(
        lapply(1:length(CombinTwo), function(i)combn(1:length(CombinTwo),i,simplify=FALSE)), recursive=FALSE)
      # But in the names of the variables and but a plus inbetween
      Twoway_2=sapply(CombinTwo_2,function(i)
        paste0(paste0(Twoway[i],collapse=" + ")))
      
      ## ##### Change name to something that makes sense #####
      Power3=sapply(CombinTwo_2,function(i)
        paste0(start,paste0(Twoway_2[i],collapse=" + ")))
      
      ## Adding the main effects where it is possible
      FoirmulasInC<-InterceptMain(Combin,Cols,Power3)
      
      ## Combine Power(1+3) with Power(1) and Power(3)
      Formulas=c(Power1,Power3,FoirmulasInC)
      Power13=c(Power3,FoirmulasInC)
      
      
      if(Power_12==TRUE){
        Formulas=c(Power1,Power2,Power12,Power13)
      }
      else{
        Formulas=c(Power1,Power13)
      }
    }
    if(Power_123==TRUE){
      
      CombinInter <- unlist(
        lapply(1, function(i)combn(1:n,i,simplify=FALSE)), recursive=FALSE)
      
      ## Put together the random variable and all the dependent variables 
      CombH1<-paste0(c(H_1,"*"),collapse = "")
      
      #All interactions terms
      Interactions <- sapply(CombinInter,function(i) 
        paste0(CombH1,paste0(Cols[i],collapse="")))
      
      FormulasIN <- sapply(Combin,function(i)
        paste0(start,paste0(Interactions[i],collapse=" + ")))
      ## Make Power(3) to put together with Power(1) and Power(2)
      
      #Make all the combinations of two-way interactions
      CombinTwo <- unlist(
        lapply(2, function(i)combn(1:n,i,simplify=FALSE)), recursive=FALSE)
      
      # Make all the combinations with the names from Cols
      Twoway=sapply(CombinTwo,function(i)
        paste0(paste0(Cols[i],collapse="*")))
      
      # Make all combinations of the two-way interactions
      CombinTwo_2 <- unlist(
        lapply(1:length(CombinTwo), function(i)combn(1:length(CombinTwo),i,simplify=FALSE)), recursive=FALSE)
      # But in the names of the variables and but a plus inbetween
      Twoway_2=sapply(CombinTwo_2,function(i)
        paste0(paste0(Twoway[i],collapse=" + ")))
      
      Power23=expand.grid(FormulasIN,Twoway_2)
      FormulasIN=apply(Power23, 1, paste0, collapse=" + ")
      
      ## Adding the main effects where it is possible
      FoirmulasInC<-InterceptMain(Combin,Cols,FormulasIN)
      
      ## Combine Power(1+2) with Power(1) and Power(2)
      if(Power_12==TRUE & Power_13==TRUE){
        Formulas=c(Power1,Power2,Power13,FormulasIN,FoirmulasInC,Power12)
      }
      if(Power_12==FALSE & Power_13==TRUE){
        Formulas=c(Power1,Power13,FormulasIN,FoirmulasInC)
      }
      if(Power_12==TRUE & Power_13==FALSE){
        Formulas=c(Power1,Power2,FormulasIN,FoirmulasInC,Power12)
      }
      if(Power_12==FALSE & Power_13==FALSE){
        Formulas=c(Power1,FormulasIN,FoirmulasInC)
      }
      
      Power123=c(FormulasIN,FoirmulasInC)
      
    }
    
  }
  else{
    
    if(Power_2==TRUE){
      ## This is Power(1) and Power(2)
      #Starting of interaction term
      CombinInter <- unlist(
        lapply(1, function(i)combn(1:n,i,simplify=FALSE)), recursive=FALSE)
      
      ## Put together the random variable and all the dependent variables 
      CombH1<-paste0(c(H_1,":"),collapse ="")
      
      #All interactions terms
      Interactions <- sapply(CombinInter,function(i) 
        paste0(CombH1,paste0(Cols[i],collapse="")))
      
      ## Make the models with the interaction terms 
      FormulasIN <- sapply(Combin,function(i)
        paste0(start,paste0(Interactions[i],collapse=" + ")))
      Power2=FormulasIN
      
      if(Power_12==TRUE){
        # Make all combinations of the two-way interactions
        Interactions2 <- unlist(
          lapply(1:length(Interactions), function(i)combn(1:length(Interactions),i,simplify=FALSE)), recursive=FALSE)
        
        # But in the names of the variables and but a plus inbetween
        Interactions_2=sapply(Interactions2,function(i)
          paste0(paste0(Interactions[i],collapse=" + ")))
        
        
        FormulasIN12=expand.grid(Power1,Interactions_2)
        Power12=apply(FormulasIN12, 1, paste0, collapse=" + ") 
        
        
        Formulas=c(Power12,Power2,Power1)
      }
      else{
        Formulas=c(Power1,Power2)
      }
    }
    if(Power_3==TRUE){
      ## Make Power(3) to put together with Power(1) 
      
      #Make all the combinations of two-way interactions
      CombinTwo <- unlist(
        lapply(2, function(i)combn(1:n,i,simplify=FALSE)), recursive=FALSE)
      
      # Make all the combinations with the names from Cols
      Twoway=sapply(CombinTwo,function(i)
        paste0(paste0(Cols[i],collapse=":")))
      
      # Make all combinations of the two-way interactions
      CombinTwo_2 <- unlist(
        lapply(1:length(CombinTwo), function(i)combn(1:length(CombinTwo),i,simplify=FALSE)), recursive=FALSE)
      # But in the names of the variables and but a plus inbetween
      Twoway_2=sapply(CombinTwo_2,function(i)
        paste0(paste0(Twoway[i],collapse=" + ")))
      
      Power3=sapply(CombinTwo_2,function(i)
        paste0(start,paste0(Twoway_2[i],collapse=" + ")))
      
      if(Power_13==TRUE){
        FormulasIN13=expand.grid(Power1,Twoway_2)
        Power13=apply(FormulasIN13, 1, paste0, collapse=" + ") 
      }
      
      
      if(Power_13==TRUE){
        Formulas=c(Formulas,Power3,Power13)
      }
      if(Power_13==FALSE){
        Formulas=c(Formulas,Power3)
      }
      
    }
    if(Power_23==TRUE){
      #Make all the combinations of two-way interactions
      CombinTwo <- unlist(
        lapply(2, function(i)combn(1:n,i,simplify=FALSE)), recursive=FALSE)
      
      # Make all the combinations with the names from Cols
      Twoway=sapply(CombinTwo,function(i)
        paste0(paste0(Cols[i],collapse=":")))
      
      # Make all combinations of the two-way interactions
      CombinTwo_2 <- unlist(
        lapply(1:length(CombinTwo), function(i)combn(1:length(CombinTwo),i,simplify=FALSE)), recursive=FALSE)
      # But in the names of the variables and but a plus inbetween
      Twoway_2=sapply(CombinTwo_2,function(i)
        paste0(paste0(Twoway[i],collapse=" + ")))
      
      CombinInter <- unlist(
        lapply(1, function(i)combn(1:n,i,simplify=FALSE)), recursive=FALSE)
      
      ## Put together the random variable and all the dependent variables 
      CombH1<-paste0(c(H_1,":"),collapse ="")
      
      #All interactions terms
      Interactions <- sapply(CombinInter,function(i) 
        paste0(CombH1,paste0(Cols[i],collapse="")))
      
      ## Make the models with the interaction terms 
      FormulasIN <- sapply(Combin,function(i)
        paste0(start,paste0(Interactions[i],collapse=" + ")))
      
      # Make all combinations of the two-way interactions
      Interactions2 <- unlist(
        lapply(1:length(Interactions), function(i)combn(1:length(Interactions),i,simplify=FALSE)), recursive=FALSE)
      
      # But in the names of the variables and but a plus inbetween
      Interactions_2=sapply(Interactions2,function(i)
        paste0(paste0(Interactions[i],collapse=" + ")))
      
      
      Power23=expand.grid(Interactions_2,Twoway_2)
      ## Make all combinations, but where the main effect for the interaction is always there
      Formulas23=apply(Power23, 1, paste0, collapse=" + ")
      Power23=paste0(start, Formulas23, sep="")
      
      Formulas<-c(Formulas,Power23)
    }
    if(Power_123==TRUE){
      
      #Make all the combinations of two-way interactions
      CombinTwo <- unlist(
        lapply(2, function(i)combn(1:n,i,simplify=FALSE)), recursive=FALSE)
      
      # Make all the combinations with the names from Cols
      Twoway=sapply(CombinTwo,function(i)
        paste0(paste0(Cols[i],collapse=":")))
      
      # Make all combinations of the two-way interactions
      CombinTwo_2 <- unlist(
        lapply(1:length(CombinTwo), function(i)combn(1:length(CombinTwo),i,simplify=FALSE)), recursive=FALSE)
      # But in the names of the variables and but a plus inbetween
      Twoway_2=sapply(CombinTwo_2,function(i)
        paste0(paste0(Twoway[i],collapse=" + ")))
      
      CombinInter <- unlist(
        lapply(1, function(i)combn(1:n,i,simplify=FALSE)), recursive=FALSE)
      
      ## Put together the random variable and all the dependent variables 
      CombH1<-paste0(c(H_1,":"),collapse ="")
      
      #All interactions terms
      Interactions <- sapply(CombinInter,function(i) 
        paste0(CombH1,paste0(Cols[i],collapse="")))
      
      
      # Make all combinations of the two-way interactions
      Interactions2 <- unlist(
        lapply(1:length(Interactions), function(i)combn(1:length(Interactions),i,simplify=FALSE)), recursive=FALSE)
      
      # But in the names of the variables and but a plus inbetween
      Interactions_2=sapply(Interactions2,function(i)
        paste0(paste0(Interactions[i],collapse=" + ")))
      
      
      ### Move the code down ####  
      Power23=expand.grid(Interactions_2,Twoway_2)
      
      ## Make all combinations, but where the main effect for the interaction is always there
      Formulas23=apply(Power23, 1, paste0, collapse=" + ")
      
      Power23=paste0(start, Formulas23, sep="")
      Power123=expand.grid(Power1,Formulas23)
      
      Formulas=c(apply(Power123, 1, paste0, collapse=" + "),Formulas)
      
      
      
    }
  }
  
  
  #Running all the models with OLS

    models<-lapply(Formulas,function(i)
      lm(as.formula(i),data=data))

  
  
  #Running all models with ANCOVA

    modelsaov<-lapply(Formulas,function(i)
      aov(as.formula(i),data=data))
 
  
  
  
  #Collecting the p-values
  
  for(i in 1:length(models)){
    
    
    #Finding the name of the model
    mod<-Formulas[[i]]
    ModelName<-rbind(mod,ModelName)
    
    #Collecting the p-value
    coef <-summary(models[[i]])$coefficients
    coefaov <-as.data.frame(summary(modelsaov[[i]])[[1]])
    p_h_1<-coef[2,4] 
    p_h_1aov=coefaov[1,5]
    RModelF<-rbind(p_h_1,RModelF)
    RModelFaov<-rbind(p_h_1aov,RModelFaov)
    
    #Make a holder for the p-values
    holder<-as.data.frame(matrix(NA, ncol = n, nrow = 1))
    holderaov<-as.data.frame(matrix(NA, ncol = n, nrow = 1))
    #See if ther interaction with H_1 becomes significant
    if(Main==TRUE){
      if(grepl("*",mod, fixed=TRUE)==TRUE){
        
        holder<-pvaluecapture(coef,holder,H_1)
        holderaov<-pvaluecaptureaov(coefaov,holderaov,H_1)
      }
    }
    if(Main==FALSE){  
      Int<-paste(H_1,":",sep = "")
      
      if(grepl(Int,mod, fixed=TRUE)==TRUE){
        holder<-pvaluecapture(coef,holder,H_1)
      }
    }
    RModelI<-rbind(holder,RModelI)
    RModelIaov<-rbind(holderaov,RModelIaov)
  }
  
  
  #Combine all the p-values
  TheModels<-cbind(ModelName,RModelF,RModelI)
  TheModelsaov<-cbind(ModelName,RModelFaov,RModelIaov)
  
  
  #Make a variable to indicate that there are nothing from the data removed
  TheModels$Outlier<-"Non removed"
  TheModelsaov$Outlier<-"Non removed"
  
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
        holder<-as.data.frame(matrix(NA, ncol = n, nrow = 1))
        
        #See if ther interaction with H_1 becomes significant
        if(Main==TRUE){
          if(grepl("*",mod, fixed=TRUE)==TRUE){
            holder<-pvaluecapture(coef,holder,H_1)
          }
        }
        if(Main==FALSE){
          Int<-paste(H_1,":",sep = "")
          if(grepl(Int,mod, fixed=TRUE)==TRUE){
            holder<-pvaluecapture(coef,holder,H_1)
          }
        }
        RModelI<-rbind(holder,RModelI)
      }
    }
    TheModels_outlier<-cbind(ModelName,RModelF,RModelI,Outlier)
    
    
    #Combine the 2 datastets
    TheModels<-rbind(TheModels_outlier,TheModels)
  }
  
  
  #Order the models if one wants all the models out in the end
  TheModels<-as.data.table(TheModels)
  TheModels<- TheModels[with(TheModels, order(RModelF)), ]
  
  TheModelsaov<-as.data.table(TheModelsaov)
  TheModelsaov<- TheModelsaov[with(TheModelsaov, order(RModelFaov)), ]
  
  ##Change the name of the data.frame
  names<-c("Model","Main effect",paste("Interaction",1:n,sep = " "),"Outlier")
  names(TheModels)<-names
  names(TheModelsaov)<-names
  
  Models<-TheModels[apply(TheModels[, 2:ncol(TheModels)] <= pvalue, 1, any, na.rm=TRUE), ]
  Modelsaov<-TheModelsaov[apply(TheModelsaov[, 2:ncol(TheModelsaov)] <= pvalue, 1, any, na.rm=TRUE), ]
  
  ## This part is only for the simulation
  if(Per==FALSE){
    res1 <- ifelse(nrow(Models)>=1,1,0)
    res2 <- ifelse(nrow(Modelsaov)>=1,1,0)
    res<-c(res1,res2)
  }
  else{
    res <- nrow(Models)/nrow(TheModels)
  }
  
  
  return(res)
}
