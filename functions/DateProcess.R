# function to process date format
DateProcess <- function(data.raw, col.name){
  library(stringr)
  date.temp <- as.data.frame(str_split_fixed(as.character(data.raw[,col.name]), " ", 2))
  data.raw[,col.name]=as.Date(date.temp$V1, "%d/%m/%Y")
  return(data.raw)
}