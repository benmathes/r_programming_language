getmonitor <- function(id, directory, summarize = FALSE) {
  ## 'id' is a vector of length 1 indicating the monitor ID
  ## number. The user can specify 'id' as either an integer, a
  ## character, or a numeric.
  
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'summarize' is a logical indicating whether a summary of
  ## the data should be printed to the console; the default is
  ## FALSE
  
  ## if id is just "1" instead of "001", "10" instead of "010"
  id <- as.character(id)
  if (nchar(id) == 1) {
    id <- paste("00", id, sep="")
  }
  else if (nchar(id) == 2) {
    id <- paste("0", id, sep="")
  }
  
  monitor <- read.csv(paste(directory, "/", id, ".csv", sep=""))
  if (summarize) {
    print(summary(monitor))
  }
  return(monitor)
}


non_na <- function(s,n) { 
  if (s != "NA" && n != "NA") { 
    return(TRUE) 
  } 
  else { 
    return(FALSE) 
  }
}