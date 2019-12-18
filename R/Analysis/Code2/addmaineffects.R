InterceptMain<-function(Combinations,names,formel){ #need fixing/optimizing
  
  Test= sapply(Combinations,function(i)
    paste0(names[i],collapse="|"))
  
  ## Make object to capture the functions where to add main effect
  FoirmulasInC=NULL
  for (j in 1:length(Test)) {
    ## Test if variable is part of the interaction term
    loop=grep(Test[j], formel,invert=TRUE)
    
    ## For those where the term is not part of the interaction term, add them
    for (k in loop){
      
      ## Paste on the main effects that are needed
      Form= paste0(formel[k],paste0("+",Test[j]))
      ## Change the | to a +
      Form=gsub("\\|", "+", Form)
      ## Put them into a list
      FoirmulasInC=rbind(Form,FoirmulasInC)
    }
  }
  return(FoirmulasInC)
}