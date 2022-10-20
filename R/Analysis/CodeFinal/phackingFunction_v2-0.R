### P-hacking function ###
## This function has the ability to p-hack data with the help of the research degrees of freedom described in 
## Christensen, J. D., Orquin, J. L., Perkovic, S., Lagerkvist, C. J.,

## The different options:
## Inserting multiple dependent variables the function wil test of all them including the average of them
## Setting Main = False: This will allow models where there are interaction terms without the corresponding main effect
## Ma_HCI = TRUE: This gives you the set MA + HCI
## HCI = TRUE: This can only be used if Main = FALSE, this gives the models in HCI. If TRUE and Main = TRUE it will give you the same as Ma_HCI=TRUE
## Ma_CCI = TRUE: This gives the set MA + HCI
## CCI = TRUE: This gives the set CCI if Main=FALSE otherwise it will give the same as Ma_CCI=TRUE
## HCI_CCI = TRUE: This gives youthe set HCI + CCI. This is however only possible if Main = FALSE. If Main = FALSE then HCI_CCI = TRUE will be ignored
## Ma_HCI_CCI = TRUE: Gives you the models in the set MA + HCI + CCI. Using this will greatly increase the model set, so only use this with very few covariates. 
## outlierexclusion = TRUE: Uses four different outlier criteria.
## variable_ignore: list of variables in your data_frame that the phackingFunction shall ignore and not include.

### Function that are being used in the function. 
source(here::here("R","Analysis","CodeFinal","HelpFunctions","remove_outliers_sd.R")) 
source(here::here("R","Analysis","CodeFinal","HelpFunctions","remove_outliers_range.R"))
source(here::here("R","Analysis","CodeFinal","HelpFunctions","pvaluecapture.R"))
source(here::here("R","Analysis","CodeFinal","HelpFunctions","addmaineffects.R"))


phackingFunction<-function(data,y,H_1,HCI = FALSE,CCI=FALSE,Ma_HCI=FALSE, Ma_CCI=FALSE
                           ,HCI_CCI=FALSE,Ma_HCI_CCI=FALSE
                           ,outlierexclusion=FALSE,Per=FALSE,Main=TRUE,pvalue=0.05,variables_ignore=c()){
  
  library(dplyr)
  ## delete outliers if outlierexclusion = TRUE 
  
  if(outlierexclusion==TRUE){
    
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
  Cols <- Cols[! Cols %in% c(y,H_1,variables_ignore)] 
  n <- length(Cols)
  
  #Making different combinations of the variables
  Combin <- unlist(
    lapply(1:n, function(i)combn(1:n,i,simplify=FALSE)), recursive=FALSE)
  
  # Look if there are multiple DV
  mvDV <- ifelse(length(y) >= 2, TRUE, FALSE)
  
  if (mvDV == TRUE) {
    #Make average
    data <- data[c(y, H_1, Cols)]
    data$yavg <-
      1 / length(y) * rowSums(data[1:length(y)]) #double check that it outputs correct result
    
    if (outlierexclusion == TRUE) {
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
    paste(start,paste(Cols[i],collapse=" + "))) 
  Ma=Formulas 
  
  if(HCI==TRUE & Main == TRUE){
    Ma_HCI=TRUE
  }
  
  if(CCI==TRUE & Main == TRUE){
    Ma_CCI=TRUE
  }
  
  
  if(Ma_HCI==TRUE & HCI==F & Main == F){
    HCI=TRUE
    print("HCI has been set to TRUE") ## Change wording
  }
  
  if(Ma_CCI==TRUE & CCI==F & Main == F){
    CCI=TRUE
    print("CCI has been set to TRUE") ## Change wording
  }
  
  ## This is needed if there is only one covariate
  FoirmulasInC=NULL
  
  if(Main==TRUE){
    
    if(Ma_HCI==TRUE){
      ## This is Ma and HCI
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
      
      if(length(n)>1){
        FoirmulasInC<-InterceptMain(Combin,Cols,FormulasIN)
      }
      ## Combine Power(1+2) with Power(1) and Power(2)
      Formulas=c(Ma,FormulasIN,FoirmulasInC)
      Power12=FoirmulasInC
      
      Power2=FormulasIN
      
    }
    if(Ma_CCI==TRUE){
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
      if(length(n)>1){
        FoirmulasInC<-InterceptMain(Combin,Cols,Power3)
      }
      ## Combine Power(1+3) with Power(1) and Power(3)
      Formulas=c(Ma,Power3,FoirmulasInC)
      Power13=c(Power3,FoirmulasInC)
      
      
      if(Ma_HCI==TRUE){
        Formulas=c(Ma,Power2,Power12,Power13)
      }
      else{
        Formulas=c(Ma,Power13)
      }
    }
    if(Ma_HCI_CCI==TRUE){
      
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
      if(length(n)>1){
        FoirmulasInC<-InterceptMain(Combin,Cols,FormulasIN)
      }
      ## Combine Power(1+2) with Power(1) and Power(2)
      if(Ma_HCI==TRUE & Ma_CCI==TRUE){
        Formulas=c(Ma,Power2,Power13,FormulasIN,FoirmulasInC,Power12)
      }
      if(Ma_HCI==FALSE & Ma_CCI==TRUE){
        Formulas=c(Ma,Power13,FormulasIN,FoirmulasInC)
      }
      if(Ma_HCI==TRUE & Ma_CCI==FALSE){
        Formulas=c(Ma,Power2,FormulasIN,FoirmulasInC,Power12)
      }
      if(Ma_HCI==FALSE & Ma_CCI==FALSE){
        Formulas=c(Ma,FormulasIN,FoirmulasInC)
      }
      
      Power123=c(FormulasIN,FoirmulasInC)
      
    }
    
  }
  else{
    
    if(HCI==TRUE){
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
      
      if(Ma_HCI==TRUE){
        # Make all combinations of the two-way interactions
        Interactions2 <- unlist(
          lapply(1:length(Interactions), function(i)combn(1:length(Interactions),i,simplify=FALSE)), recursive=FALSE)
        
        # But in the names of the variables and but a plus inbetween
        Interactions_2=sapply(Interactions2,function(i)
          paste0(paste0(Interactions[i],collapse=" + ")))
        
        
        FormulasIN12=expand.grid(Ma,Interactions_2)
        Power12=apply(FormulasIN12, 1, paste0, collapse=" + ") 
        
        
        Formulas=c(Power12,Power2,Ma)
      }
      else{
        Formulas=c(Ma,Power2)
      }
    }
    if(CCI==TRUE){
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
      
      if(Ma_CCI==TRUE){
        FormulasIN13=expand.grid(Ma,Twoway_2)
        Power13=apply(FormulasIN13, 1, paste0, collapse=" + ") 
      }
      
      
      if(Ma_CCI==TRUE){
        Formulas=c(Formulas,Power3,Power13)
      }
      if(Ma_CCI==FALSE){
        Formulas=c(Formulas,Power3)
      }
      
    }
    if(HCI_CCI==TRUE){
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
      
      Power23=apply(expand.grid(start, Formulas23), 1, paste, collapse="")
      
      
      
      
      
      Formulas<-c(Formulas,Power23)
    }
    if(Ma_HCI_CCI==TRUE){
      
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
      Power23=apply(expand.grid(start, Formulas23), 1, paste, collapse="")
      
      Power123=expand.grid(Ma,Formulas23)
      
      Formulas=c(apply(Power123, 1, paste0, collapse=" + "),Formulas)
      
      
      
    }
  }
  
  
  #Running all the models
  models<-lapply(Formulas,function(i)
    lm(as.formula(i),data=data))
  
  
  #Combine all the p-values
  TheModels<-pvaluemain(models,Formulas,"None removed",F,Main,H_1)
  
  if(outlierexclusion==TRUE){
    for(j in 1:length(dataoutlier)){
      
      ## Run all the models with the different data
      models_out<-lapply(Formulas,function(i)
        lm(as.formula(i),data=dataoutlier[[j]]))
      
      TheModelsoutlier=pvaluemain(models_out,Formulas,j,T,Main,H_1)
      
      TheModels<-rbind(TheModelsoutlier,TheModels)
    }
    
  }
  
  #Order the models if one wants all the models out in the end
  TheModels<-as.data.table(TheModels)
  TheModels<- TheModels[with(TheModels, order(RModelF)), ]
  
  # Delete column if only NA
  TheModels=TheModels %>% select_if(~sum(!is.na(.)) > 0)
  
  ##Change the name of the data.frame
  
  if(length(TheModels)>3){
    names<-c("Model","Main effect",paste("Interaction",1:n,sep = " "),"Outlier") ## If there is no interactions then it will come with an error since names are too long
    names(TheModels)<-names
  }
  
  
  
  Models<-TheModels[apply(TheModels[, 2:(ncol(TheModels)-1)] <= pvalue, 1, any, na.rm=TRUE), ]
  
  
  ## This part is only for the simulation
  if(Per==FALSE){
    res <- ifelse(nrow(Models)>=1,1,0)
  }
  else{
    res <- nrow(Models)/nrow(TheModels)
  }
  
  
  return(res)
}
