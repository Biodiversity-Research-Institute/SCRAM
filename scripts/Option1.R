# Option 1

# ATG - check to see why sampledSpeciesCount[i, h]/1000
# We see this warning: Warning in hours$Flux[h] <- (sampledSpeciesCount[i, h]/1000) * TotalFrontalArea :
# number of items to replace is not a multiple of replacement length
# This is due to the multiple wind farm inputs runs generating multiple values for TotalFrontalArea - need to select just for each run
# Fixed where TotalFrontalArea is calculated
# "i" is brought in from Iterating i - over random sample iterations (i)  - in band model code        
for (h in 1:nrow(hours)){ 
  # ATG - hours$flux is not a flux parameter like it was in Band or Stoch CRM, CF calculated as a weird density, but divided by 1000 and multiplied by
  # frontal length - it's NOT an area, change to calculating bird density 
  # hours$Flux[h] <- (sampledSpeciesCount[i, h]/1000)*TotalFrontalArea  #flip around to divide frontal area by 1000 below to be more clear
  # ATG - TotalFrontalArea (really a linear feauture, not area) is in meters so convert to km and multiply by the monthly density in sq. km yields
  # the number of birds crossing the total turbine line per km? Since this is a total value crossing the line of turbines it does not get
  # scaled to time like the Band Model?
  hours$Flux[h] <- (sampledSpeciesCount[i, h])*TotalFrontalArea/1000  #birds/km, the total monthly traffic rate - there is no time here #NOTE: this is just birds, not birds/km (EMA)
}

# PCH = Calculate the total proportion the species is in the RSZ
# ATG - this may not make sense as sum(FH.dat[ceiling(height)]) only picks values every 5 meters, but flight height dist is every meter
# In further discussions this may be because we are factoring in the 41 lengths along the turbined blades
PCH <- sum(FH.dat[ceiling(height)])
# flt_height_bins <- ceiling(height)
# PCH <- sum(FH.dat[seq(min(flt_height_bins), max(flt_height_bins), 1)])

# Calculate the number of animals/km * proportion of animals in RSZ * mean operational monthly proportion giving the traffic rate in the RSZ 
# only during operation periods - so the transiting birds/km
Option1_Transits <- hours$Flux*PCH*Operational

#Now bring in the probability of collision to yield the collisions/km for the wind farm
#Option1_Transits <- hours$Flux*sampledBirdParams$PCH[i]*Operational
Option1_collisions_No_Avoid <- Option1_Transits*(P_Collision/100)

# Option1_CollisionRate <- data.frame(matrix(data = 0, nrow = 12, ncol = 2))
# ATG - change 0's no NAs
Option1_CollisionRate <- data.frame(matrix(data = NA, nrow = 12, ncol = 2))
names(Option1_CollisionRate) <- c("Month", "Collisions")
Option1_CollisionRate$Month <- month.abb

# Account for the avoidance rate to remove those animals that avoided the wind farm and also the large array correction factor if needed
if(LargeArrayCorrection == "yes"){
  Option1_CollisionRate[, 2] <- as.numeric(Option1_collisions_No_Avoid*(1-sampledBirdParams$Avoidance[i])*L_ArrayCF)
} else {
  Option1_CollisionRate[, 2] <- as.numeric(Option1_collisions_No_Avoid*(1-sampledBirdParams$Avoidance[i]))
}

if(sum(Option1_CollisionRate[, 2], na.rm=TRUE)==0){
  Option1_CollisionRate[!is.na(Option1_CollisionRate[,2 ]), 2] <- 0.001
}

