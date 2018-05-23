# function to process date format
DateProcess2 <- function(data.raw, col.name){
  library(stringr)
  date.temp <- as.data.frame(str_split_fixed(as.character(data.raw[,col.name]), " ", 2))
  data.raw[,col.name]=as.Date(date.temp$V1, "%Y-%m-%d")
  return(data.raw)
}