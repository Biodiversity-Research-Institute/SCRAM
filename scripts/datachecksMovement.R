checkMovement <- mat.or.vec(2, max(length(MovementData), 2))
if(is.list(MovementData)){
for(i in 1:length(MovementData)){
if(length(MovementData[[i]][1, ]) == 13){
  checkMovement[1, i] <- 1
}
if(length(MovementData[[i]][, 2]) >= 10){
  checkMovement[2, i] <- 1
}
}
}else{
  for(i in 1:length(MovementData)){
    if(length(MovementData[1, ]) == 13){
      checkMovement[1, i] <- 1
    }
    if(length(MovementData[, 2]) >= 10){
      checkMovement[2, i] <- 1
    }
  }
}
checkMovement <- rowSums(checkMovement)


