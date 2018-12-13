complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files.
  ##
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used.
  ##
  ## Return a data frame with the count of the complete cases (nobs)
  ## for each id indicated. Data is returned as:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  nobs <- list()
  for (id_num in id)
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
    nobs <- c(nobs, sum(complete.cases(CSV.holder)))
  }  
  data.frame("id" = id, "nobs" = unlist(nobs))
}