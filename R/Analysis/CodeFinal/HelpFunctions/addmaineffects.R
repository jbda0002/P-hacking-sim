InterceptMain <-
  function(Combinations, names, formula) {
    
    ## Creating all the combinations of variables 
    Test = sapply(Combinations, function(i)
      paste(names[i], collapse = "|"))
    
    ## See where there can be added a covariate
    loop = sapply(Test, function(y)
      grep(y, formula, invert = T))
    
    loop[sapply(loop, function(x) length(x)==0)] <- NULL
    
    ## Add the covariate
    Form = unlist(
      mapply(x = loop, y = 1:(length(Test)-1), function(x, y)
        paste(formula[x], "+", Test[y])),
      recursive = F,
      use.names = F
    )
    
    
    FoirmulasInC = gsub("\\|", " + ", Form)
    
    return(FoirmulasInC)
  }
