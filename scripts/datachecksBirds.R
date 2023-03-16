checkBirdData <- mat.or.vec(length(BirdData[1,]), max(length(BirdData[,1]), 2))
for(i in 1:length(BirdData[,1])){
  checkBirdData[1, i] <- 1
  if(any(BirdData[i,"Avoidance"] >= 0)&any(BirdData[i,"Avoidance"] <= 1)){
    checkBirdData[2, i] <- 1
  }
  if(any(BirdData[i,"AvoidanceSD"] >= 0)){
    checkBirdData[3, i] <- 1
  }
  if(any(BirdData[i,"Body_Length"] > 0)){
    checkBirdData[4, i] <- 1
  }
  if(any(BirdData[i,"Body_LengthSD"] >= 0)){
    checkBirdData[5, i] <- 1
  }
  if(any(BirdData[i,"Wingspan"] > 0)){
    checkBirdData[6, i] <- 1
  }
  if(any(BirdData[i,"WingspanSD"] >= 0)){
    checkBirdData[7, i] <- 1
  }
  if(any(BirdData[i,"Flight_Speed"] > 0)){
    checkBirdData[8, i] <- 1
  }
  if(any(BirdData[i,"Flight_SpeedSD"] >= 0)){
    checkBirdData[9, i] <- 1
  }
  if(any(BirdData[i,"Prop_Upwind"] > 0)){
    checkBirdData[10, i] <- 1
  }
  if(any(BirdData[i,"Prop_UpwindSD"] >= 0)){
    checkBirdData[11, i] <- 1
  }
  if(any(BirdData[i,"Flight"] == "Flapping")|any(BirdData[i,"Flight"] == "Gliding")){
    checkBirdData[12, i] <- 1
  }
}
checkBirdData <- rowSums(checkBirdData)

