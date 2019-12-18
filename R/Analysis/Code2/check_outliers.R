# check whether there is an outlier based on specified SD
check_outliers <- function(x, y, sd) {
  sapply(x, function(i)
    (i > y[1] + sd * y[2]))
}