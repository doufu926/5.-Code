# location feature
FeatureLocation <- function(data.all,cust.target){
  #################################################################
  ## customer location
  temp <- cust.target
  # get address from raw data
  temp$location <- data.all$Customer.address[match(cust.target$VIN_NO,data.all$VIN_NO)]
  temp$location <- as.character(temp$location)
  # get post code only
  temp$location <- sapply(temp$location, function(a) substr(a,nchar(a)-4,nchar(a)))
  # replace space in strings
  library(stringr)
  temp$location <- str_replace_all(temp$location,fixed(" "), "")
  # check incorrect values
  library(Hmisc)
  a <- as.data.frame(sapply(temp$location, all.is.numeric))
  temp$location[!a]="NAN"
  temp$location[nchar(temp$location)!=5]="NAN"
  #################################################################
  ## calculate distance from dealer by batch
  library(gmapsdistance)
  set.api.key("AIzaSyBd6htFNGpenOHaqp1bfY8fmuZaoS9stow")
  # run google api by batch to avoid limit
  # calculate driving time and distance to dealership
  for (i in 1:(floor(nrow(temp)/200)+1)){
    print(i)
    if (i==1){
      distance.temp <- sapply(temp$location[1:200], function(x) gmapsdistance(origin = paste(x,"+Thailand",sep = ""),destination = "10540+Thailand",mode = "driving"))
      distance <- distance.temp
      Sys.sleep(1)
    }else if(i!=(floor(nrow(temp)/200)+1)){
      distance.temp <- sapply(temp$location[((i-1)*200+1):(i*200)], function(x) gmapsdistance(origin = paste(x,"+Thailand",sep = ""),destination = "10540+Thailand",mode = "driving"))
      distance <- cbind(distance,distance.temp)    
      Sys.sleep(1)
    }else{
      distance.temp <- sapply(temp$location[((i-1)*200+1):nrow(temp)], function(x) gmapsdistance(origin = paste(x,"+Thailand",sep = ""),destination = "10540+Thailand",mode = "driving"))
      distance <- cbind(distance,distance.temp)  
    }
  }
  distance=as.data.frame(t(distance))
  cust.target$location <- distance$Time/60/60
}