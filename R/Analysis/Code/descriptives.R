library(data.table)

# compute descriptives (mean and sd) for variables in a data set
descriptives =function(data){ data.table(sapply(data, function(x)
  c(
    mean = mean(x, na.rm = T), sd = sd(x, na.rm = T)
  )))
}
