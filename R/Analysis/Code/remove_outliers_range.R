# functions for identifying and removing outliers based on 1.5 times interquartile range

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs = c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

remove_numeric_outliers <- function(df) {
  df[, sapply(df, is.numeric)] <-
    lapply(df[, sapply(df, is.numeric)], remove_outliers)
  df[complete.cases(df), ]
}
