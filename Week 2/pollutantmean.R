pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files.
  ##
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate."
  ##
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used.
  ##
  ## Return the mean of the pollutant across all monitors listed
  ## in the 'id' vector (ignoring NA values).
  ## 
  ## Adds up the values of the pollutant of interest and divides
  ## by the total number of samples measured.
  
  #
  pol_total <- 0
  pol_count <- 0
  for (id_num in id) {
    if (nchar(id_num) == 1) {
      id_num <- paste0("00",id_num)
    } else if (nchar(id_num) == 2) {
      id_num <- paste0("0",id_num)
    }
    CSV.holder <- read.csv(file.path(getwd(),directory, paste0(id_num,".CSV")))
    pol.holder <- CSV.holder[pollutant]
    pol_total <- pol_total + sum(pol.holder[!is.na(pol.holder)])
    pol_count <- pol_count + length(pol.holder[!is.na(pol.holder)])
  }
  pol_total/pol_count
}

