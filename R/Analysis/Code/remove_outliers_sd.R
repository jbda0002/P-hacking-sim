# identify and remove outliers based on a specific sd

 source(here::here("descriptives.R"))
 source(here::here("check_outliers.R"))

remove_outliers_sd <- function(data, sd) {
  outlier <-
    as.data.frame(mapply(check_outliers, abs(data), descriptives(data), sd))
  outlier <- cbind(data, outlier)
  outlier <- outlier[!Reduce(`|`, lapply(outlier, grepl,
                                           pattern = "TRUE")), ]
  return(outlier)
}
