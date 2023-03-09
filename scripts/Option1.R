# Option 1

# PCH = Calculate the total proportion the species is in the RSZ
# ATG - this may not make sense as sum(FH.dat[ceiling(height)]) only picks values every 5 meters, but flight height dist is every meter
# In further discussions this may be because we are factoring in the 41 lengths along the turbined blades
PCH <- sum(FH.dat[ceiling(height)])
# flt_height_bins <- ceiling(height)
# PCH <- sum(FH.dat[seq(min(flt_height_bins), max(flt_height_bins), 1)])

# Model will fork whether a migrant or a resident bird calculating risk using the more traditional flux model for 
# resident species that forage offshore and a more simplistic model for when we think there is directed flight with no ability
# to collide more than once
if (migr_res == "resident"){
  #traditional flux calcs
  Option1_Transits <- (hours$Flux * PCH) * Operational
  names(Option1_Transits) <- month.abb
} else { #migrants
  for (m in 1:nrow(num_birds_WF_permonth)){
    # num_birds_WF_permonth$cumulative[m] <- sampledSpeciesCount[i, m] * (TotalWindFarmArea/1e+06)
    # Change to be more like the Band 2012 migratory front calculations. Calculate birds/km / rotor diameter to get vertical density of front (bird/sq km)
    # Then multiply by the total frontal area to get the number of birds in the wind farm vertical space
    # num_birds_WF_permonth$cumulative[m] <- (sampledSpeciesCount[i, m] / (2 * sampledTurbine$RotorRadius_m[i] / 1000)) * (TotalFrontalArea_sqrm/1e+06)
    num_birds_WF_permonth$cumulative[m] <- (bird_flux_perkm[i, m] / (2 * sampledTurbine$RotorRadius_m[i] / 1000)) * (TotalFrontalArea_sqrm/1e+06)
    
    days_in_month <- lubridate::days_in_month(month_vals[[m]])
    num_birds_WF_perday_iters[m] <- num_birds_WF_permonth$cumulative[m]/days_in_month
  }

  # Calculate the number of animals/km * proportion of animals in RSZ * mean operational monthly proportion giving the traffic rate in the RSZ 
  # only during operation periods - so the transiting birds/km
  Option1_Transits <- num_birds_WF_permonth$cumulative*PCH*Operational
  
}

#Now bring in the probability of collision to yield the collisions/km for the wind farm
#Option1_Transits <- hours$Flux*sampledBirdParams$PCH[i]*Operational
Option1_collisions_No_Avoid <- Option1_Transits * (P_Collision)

Option1_CollisionRate <- data.frame(Month = month.abb, monthly_collisions = NA)

# Account for the avoidance rate to remove those animals that avoided the wind farm and also the large array correction factor if needed
if(LargeArrayCorrection == "yes"){
  Option1_CollisionRate$monthly_collisions <- as.numeric(Option1_collisions_No_Avoid*(1-sampledBirdParams$Avoidance[i])*L_ArrayCF)
} else {
  Option1_CollisionRate$monthly_collisions <- as.numeric(Option1_collisions_No_Avoid*(1-sampledBirdParams$Avoidance[i]))
}

#calcualate daily values
Option1_CollisionRate$daily_collisions <- Option1_CollisionRate$monthly_collisions/day_vals

if(sum(Option1_CollisionRate[, 2], na.rm=TRUE)==0){
  Option1_CollisionRate[!is.na(Option1_CollisionRate$monthly_collisions), 2] <- 0.001
  Option1_CollisionRate[!is.na(Option1_CollisionRate$daily_collisions), 2] <- 0.00003
}


