### Outlier Functions ### 


#### Outlier functions ####

## These can be build in when it is going to be a packed
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}
remove_all_outliers1 <- function(df){
  # We only want the numeric columns
  df[,sapply(df, is.numeric)] <- lapply(df[,sapply(df, is.numeric)], remove_outliers)
  df[complete.cases(df), ]
  
}