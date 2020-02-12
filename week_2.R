pollutantmean <- function(directory, pollutant, id = 1:332) {
  means <- numeric()
  options(digits = 10)
  for(i in seq_along(id)){
    data <- read.csv(paste(directory, "/", formatC(id[i], flag = 0, digits=2), 
                           ".csv", sep=""))
    means <- c(means,data[[pollutant]])
  }
  mean(means, na.rm = TRUE)
}



complete <- function(directory, id = 1:332){
  df <- data.frame()
  for(i in seq_along(id)){
    data <- read.csv(paste(directory, "/", formatC(id[i], flag = 0, width = 3), 
                           ".csv", sep=""))
    row <- data.frame(id[i], nobs = sum(complete.cases(data)))
    df <- rbind(df,row)
  }
  df
}

corr <- function(directory, treshold = 0){
  comp <- complete(directory)
  use <- comp[comp["nobs"] > treshold, ]$id
  res <- numeric(0)
  if(length(use)>0){
    for (i in use){
      data <- read.csv(paste(directory, "/", formatC(i, flag = 0, digits=2), 
                             ".csv", sep=""))
      data_na <- data[complete.cases(data), ]
      res <- c(res,cor(data_na$sulfate, data_na$nitrate))
    }
  }
  res
}



