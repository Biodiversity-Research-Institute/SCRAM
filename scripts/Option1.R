# Option 1

# ATG - check to see why sampledSpeciesCount[i, h]/1000
# We see this warning: Warning in hours$Flux[h] <- (sampledSpeciesCount[i, h]/1000) * TotalFrontalArea :
# number of items to replace is not a multiple of replacement length
# This is due to the multiple wind farm inputs runs generating multiple values for TotalFrontalArea - need to select just for each run
# Fixed where TotalFrontalArea is calculated
# "i" is brought in from Iterating i - over random sample iterations (i)  - in band model code  

for (m in 1:nrow(num_birds_WF_permonth)){ 
  
  # for (h in 1:nrow(hours)){ 
    
  # ATG - hours$flux is not a flux parameter like it was in Band or Stoch CRM, CF calculated as a weird density, but divided by 1000 and multiplied by
  # frontal length - it's NOT an area, change to calculating bird density 
  # hours$Flux[h] <- (sampledSpeciesCount[i, h]/1000)*TotalFrontalArea  #flip around to divide frontal area by 1000 below to be more clear
  # ATG - TotalFrontalArea (really a linear feauture, not area) is in meters so convert to km and multiply by the monthly density in sq. km yields
  # the number of birds crossing the total turbine line per km? Since this is a total value crossing the line of turbines it does not get
  # scaled to time like the Band Model?
  # hours$Flux[h] <- (sampledSpeciesCount[i, h])*TotalFrontalArea/1000  #birds/km, the total monthly traffic rate - there is no time here #NOTE: this is just birds, not birds/km (EMA)
  #ATG - change TotalFrontalArea to an area
  # this is the value of the number of birds predicted that could intersect turbines
  #num_birds_WF_permonth$cumulative[m] = hours$Flux[h] original code
  num_birds_WF_permonth$cumulative[m] <- ((sampledSpeciesCount[i, m])/1e+06)*TotalFrontalArea
  # num_birds_WF_permonth$cumulative[m] <- ((num_birds_cell_perday_iters[i, m])/1e+06)*TotalFrontalArea
  
  days_in_month <- lubridate::days_in_month(month_vals[[m]])
  num_birds_WF_perday_iters[i, m] <- num_birds_WF_permonth$cumulative[m]/days_in_month
  #ATG - in Stoch CRM: 
  # tot_frontal_area <- n_turbines * pi * rotor_radius^2
  # # convert density to birds/m^2
  # bird_dens_sqrm <- bird_dens/1e+06
  # active_secs <- (daynight_hrs$Day + noct_activity * daynight_hrs$Night) * 3600
  # flight_speed * (bird_dens_sqrm/(2*rotor_radius)) * tot_frontal_area * active_secs
  
  #They divide density by the rotor diameter and multiply by the flight speed - we don't account for active time or flight speed, why?
  
  }

# PCH = Calculate the total proportion the species is in the RSZ
# ATG - this may not make sense as sum(FH.dat[ceiling(height)]) only picks values every 5 meters, but flight height dist is every meter
# In further discussions this may be because we are factoring in the 41 lengths along the turbined blades
PCH <- sum(FH.dat[ceiling(height)])
# flt_height_bins <- ceiling(height)
# PCH <- sum(FH.dat[seq(min(flt_height_bins), max(flt_height_bins), 1)])

# Calculate the number of animals/km * proportion of animals in RSZ * mean operational monthly proportion giving the traffic rate in the RSZ 
# only during operation periods - so the transiting birds/km
Option1_Transits <- num_birds_WF_permonth$cumulative*PCH*Operational

#Now bring in the probability of collision to yield the collisions/km for the wind farm
#Option1_Transits <- hours$Flux*sampledBirdParams$PCH[i]*Operational
Option1_collisions_No_Avoid <- Option1_Transits*(P_Collision/100)

# Option1_CollisionRate <- data.frame(matrix(data = 0, nrow = 12, ncol = 2))
# ATG - change 0's to NAs
Option1_CollisionRate <- data.frame(matrix(data = NA, nrow = 12, ncol = 3))
names(Option1_CollisionRate) <- c("Month", "monthly_collisions", "daily_collisions")
Option1_CollisionRate$Month <- month.abb

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


