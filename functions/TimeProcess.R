# function to process date format
TimeProcess <- function(data.raw, col.name){
  temp <- data.raw[col.name]
  # get hour and minute column
  temp$H <- lapply(temp[,1], function(x) substr(as.character(x),1,(nchar(as.character(x))-2)))
  temp$M <- lapply(temp[,1], function(x) substr(as.character(x),(nchar(as.character(x))-1),(nchar(as.character(x)))))
  # represent the time in hour
  temp$all <- as.numeric(temp$H)+as.numeric(temp$M)/60
  # add back to data
  data.raw[col.name] <- temp$all
  return(data.raw)
}