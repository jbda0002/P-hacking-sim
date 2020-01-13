InterceptMain <-
  function(Combinations, names, formula) {
    
    ## Creating all the combinations of variables 
    Test = sapply(Combinations, function(i)
      paste(names[i], collapse = "|"))
    
    ## See where there can be added a covariate
    loop = sapply(Test, function(y)
      grep(y, formula, invert = T))
    
    ## Add the covariate
    Form = unlist(
      mapply(x = loop, y = 1:length(Test), function(x, y)
        paste(formula[x], "+", Test[y])),
      recursive = F,
      use.names = F
    )
    
    
    Form = gsub("\\|", " + ", Form)
    
    ## Only save the once that have a whole function
    FoirmulasInC = Form[grep("~", Form)]
    
    return(FoirmulasInC)
  }
