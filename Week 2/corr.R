corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files.
  ##
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all 
  ## variables) required to compute a correlation between
  ## nitrate and sulfate; the default is 0.
  ## 
  ## Returns a numeric vector of correslation.

  nob.temp <- numeric()
  results <- numeric()
  for (id_num in 1:332)
  {
    if (nchar(id_num) == 1) 
    {
      id_num <- paste0("00",id_num)
    } 
    else if (nchar(id_num) == 2)
    {
      id_num <- paste0("0",id_num)
    }
    CSV.holder <- read.csv(file.path(getwd(),directory, paste0(id_num,".CSV")))
    nob.temp <- sum(complete.cases(CSV.holder))
    if (nob.temp > threshold) {
      sulf <- c(unlist(CSV.holder[complete.cases(CSV.holder),]["sulfate"], use.names = FALSE))
      nitr <- c(unlist(CSV.holder[complete.cases(CSV.holder),]["nitrate"], use.names = FALSE))
      results <- c(results, cor(sulf, nitr))
    }
  }  
  results
}