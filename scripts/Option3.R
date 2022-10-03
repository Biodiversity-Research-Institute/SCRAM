# Modified 22 Feb 2022 by ATG to add turbine units for clarity

# Option 3
for (h in 1:nrow(hours)){ 
  # ATG - hours$flux is different Band or Stoch CRM, CF calculated as a n/km - more of a transit rate
  # hours$Flux[h] <- (sampledSpeciesCount[i, h]/1000)*TotalFrontalArea  #flip around to divide frontal area by 1000 below to be more clear
  # ATG - TotalFrontalArea (really a linear feature, not area) is in meters so convert to km and multiply by the monthly density in sq. km yields
  # the number of birds crossing the total turbine line per km? Since this is a total value crossing the line of turbines it does not get
  # scaled to time like the Band Model?
  hours$Flux[h] <- (sampledSpeciesCount[i, h])*TotalFrontalArea/1000
}
# HD.d.y converts height above SL to the appropriate point along the flight height distribution, which must be given in m increments
# this is mutliplied by sampledRotorRadius to keep it in dimensionless units (because x = X/R, y = Y/R)
HD.d.y <- FH.dat[ceiling(height)]*sampledTurbine$RotorRadius_m[i]

#ATG - checked below code from original against above, seems the same, just simpler
# HD.d.y.stochCRM <- (FH.dat[floor(height)+1] + ((FH.dat[floor(height)+1] - FH.dat[ceiling(height)+1]) * (floor(height)-height))) * sampledTurbine$RotorRadius[i]

# flux integral
FluxMin <- HD.d.y[1]*(2*((1 - HD.y[1]*HD.y[1])^0.5))/2 # risk at lowest point on rotor blade
FluxInt <- FluxMin + HD.d.y[41]*(2*((1 - HD.y[41]*HD.y[41])^0.5)) / 2 # risk at highest point on rotor blade
for(r in 2:40){
  FluxInt <- FluxInt + HD.d.y[r]*(2*((1 - HD.y[r]*HD.y[r])^0.5))  # fill in intermediate heights
}
FluxInt <- FluxInt*0.05*(2/pi)

#ptm <- proc.time()
# collision flux
# up wind
CollMinUP <- HD.d.y[1]*xrisksum2(HD.y[1], 0.05, "up")/2 # risk at lowest point on rotor blade
CollIntUP <- CollMinUP + HD.d.y[41]*xrisksum2(HD.y[41], 0.05, "up")/2 # risk at highest point on rotor blade
for(v in 2:40){
  CollIntUP <- CollIntUP + HD.d.y[v]*xrisksum2(HD.y[v], 0.05, "up")  # fill in intermediate heights
}
CollIntUP <- CollIntUP*0.05*(2/pi)

# down wind
CollMinDown <- HD.d.y[1]*xrisksum2(HD.y[1], 0.05, "up")/2 # risk at lowest point on rotor blade
CollIntDown <- CollMinDown + HD.d.y[41]*xrisksum2(HD.y[41], 0.05, "down")/2 # risk at highest point on rotor blade
for(w in 2:40){
  CollIntDown <- CollIntDown + HD.d.y[w]*xrisksum2(HD.y[w], 0.05, "down")  # fill in intermediate heights
}
CollIntDown <- CollIntDown*0.05*(2/pi)

# average collision integral
CollInt <- (TurbineData$Prop_Upwind[t]*CollIntUP) + ((1-TurbineData$Prop_Upwind[t])*CollIntDown)
CollRiskDist <- CollInt/FluxInt # average collision risk for single rotor transit

# calculate collisions
Option3_collisions_No_Avoid <- hours$Flux*CollInt*Operational
# Option3_CollisionRate <- data.frame(matrix(data = 0, nrow = 12, ncol = 2))
# ATG - change 0's no NAs
Option3_CollisionRate <- data.frame(matrix(data = NA, nrow = 12, ncol = 2))
names(Option3_CollisionRate) <- c("Month", "Collisions")
Option3_CollisionRate$Month <- month.abb

if(LargeArrayCorrection == "yes"){
  Option3_CollisionRate[, 2] <- as.numeric(Option3_collisions_No_Avoid*(1-sampledBirdParams$Avoidance[i])*L_ArrayCF)
} else {
  Option3_CollisionRate[, 2] <- as.numeric(Option3_collisions_No_Avoid*(1-sampledBirdParams$Avoidance[i]))
}

if(sum(Option3_CollisionRate[, 2], na.rm=TRUE)==0){
  Option3_CollisionRate[!is.na(Option3_CollisionRate[, 2]), 2] <- 0.001
}

