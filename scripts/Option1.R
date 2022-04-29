# Option 1
# ATG - check to see why sampledSpeciesCount[i, h]/1000
# We see this warning: Warning in hours$Flux[h] <- (sampledSpeciesCount[i, h]/1000) * TotalFrontalArea :
# number of items to replace is not a multiple of replacement length
for (h in 1:nrow(hours)){ 
  hours$Flux[h] <- (sampledSpeciesCount[i, h]/1000)*TotalFrontalArea
}

PCH <- sum(FH.dat[ceiling(height)])
Option1_Transits <- hours$Flux*PCH*Operational

#Option1_Transits <- hours$Flux*sampledBirdParams$PCH[i]*Operational
Option1_collisions_No_Avoid <- Option1_Transits*(P_Collision/100)

Option1_CollisionRate <- data.frame(matrix(data = 0, nrow = 12, ncol = 2))
names(Option1_CollisionRate) <- c("Month", "Collisions")
Option1_CollisionRate$Month <- monthLabels

if(LargeArrayCorrection == "yes"){
  Option1_CollisionRate[, 2] <- as.numeric(Option1_collisions_No_Avoid*(1-sampledBirdParams$Avoidance[i])*L_ArrayCF)
} else {
  Option1_CollisionRate[, 2] <- as.numeric(Option1_collisions_No_Avoid*(1-sampledBirdParams$Avoidance[i]))
}

if(sum(Option1_CollisionRate[, 2], na.rm=TRUE)==0){
  Option1_CollisionRate[!is.na(Option1_CollisionRate[,2 ]), 2] <- 0.001
}

