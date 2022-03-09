checkFHD <- mat.or.vec(2, max(length(FlightData), 2))
if(is.list(FlightData)){
for(i in 1:length(FlightData)){
if(length(FlightData[[i]][1, ]) == 1002){
  checkFHD[1, i] <- 1
}
if(length(FlightData[[i]][, 2]) >= 100){
  checkFHD[2, i] <- 1
}
}
}else{
  for(i in 1:length(FlightData)){
    if(length(FlightData[1, ]) == 1002){
      checkFHD[1, i] <- 1
    }
    if(length(FlightData[, 2]) >= 100){
      checkFHD[2, i] <- 1
    }
  }
}
checkFHD <- rowSums(checkFHD)

