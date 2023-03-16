# Samples parameters for the turbine --------------------------------------
  # NB: Masden comment - if no measure of variance provided, only the mean value is used
  
# Radius  -----------------------------------------------------------------
# Modified 22 Feb 2022 by ATG to add turbine units for clarity
# 27 April 22 - added in wind speed data

if(!is.na(TurbineData$RotorRadiusSD[t])){
  sampledTurbine$RotorRadius_m <- sampleRotorRadius(iter, TurbineData$RotorRadius_m[t], TurbineData$RotorRadiusSD_m[t])
} else {
  sampledTurbine$RotorRadius_m <- rep(TurbineData$RotorRadius_m[t], iter)
}

# Wind speed data
# ATG - missing from data tables from CF - added
if(!is.na(TurbineData$RotorRadiusSD[t])){
  sampledTurbine$WindSpeed_mps <- sampleWindSpeed(iter, TurbineData$RotorRadius_m[t], TurbineData$RotorRadiusSD_m[t])
} else {
  sampledTurbine$RotorRadius_m <- rep(TurbineData$RotorRadius_m[t], iter)
}

#### BC ##### -- adding option of sampling from a prob distns alone  & adding simulated windspeed values to records =====================
# Rotor speed & Pitch -------------------------------------------------------------

# Rotor speed
#sampledTurbine$RotorSpeed <- sampleRotnSpeed(iter, TurbineData$RotationSpeed[t], TurbineData$RotationSpeedSD[t])
windSpeedRating <- sampleRotnSpeed(iter, TurbineData$WindSpeed_mps[t], TurbineData$WindSpeedSD_mps[t])

#ATG - added reference below to record to parameters table
sampledTurbine$WindSpeed_mps <- windSpeedRating

# get rotor speed according to diameter
# 5.5 is the TSR; 60 is for converting radians/s to rpm
sampledTurbine$RotorSpeed_rpm <-  (5.5*windSpeedRating*60)/(pi*2*sampledTurbine$RotorRadius_m[i])

# Pitch
sampledTurbine$Pitch <- samplePitch(iter, TurbineData$Pitch[t], TurbineData$PitchSD[t])

sampledTurbine$Pitch = sampledTurbine$Pitch*pi/180 #### Transform Pitch from degrees to radians, needed for Collision Risk Sheet


# Hub height --------------------------------------------------------------
if(!is.na(TurbineData$HubHeightAddSD[t])){
  sampledTurbine$HubHeight_m <- sampleHubHeightAdd(iter, TurbineData$HubHeightAdd_m[t], TurbineData$HubHeightAddSD_m[t])
} else {
  sampledTurbine$HubHeight_m <- rep(TurbineData$HubHeightAdd_m[t], iter)
}
sampledTurbine$HubHeight_m <- sampledTurbine$RotorRadius_m + sampledTurbine$HubHeight_m


# Blade width -------------------------------------------------------------
if(!is.na(TurbineData$BladeWidthSD_m[t])){
  
  sampledTurbine$BladeWidth_m <- sampleBladeWidth(iter, TurbineData$BladeWidth_m[t], TurbineData$BladeWidthSD_m[t])
  
} else {
  
  sampledTurbine$BladeWidth_m <- rep(TurbineData$BladeWidth_m[t], iter)
  
}
  
# Monthly estimates below here --------------------------------------------
  
for(currentMonth in month.abb){
  
  # separate out the current month mean and SD. Species.count is already filtered for current species
  #workingMean <- TurbineData %>% select(contains(currentMonth)) %>% select(contains('Mean'))
  workingMean <- TurbineData[t , paste(currentMonth, "OpMean", sep="")]
  #workingMean <- TurbineData[, grep(currentMonth, colnames(TurbineData))]
  #workingMean <- workingMean[, grep('Mean', colnames(workingMean))]
  
  #workingSD <- TurbineData %>% select(contains(currentMonth)) %>% select(contains('SD'))
  #workingSD <- TurbineData[, grep(currentMonth, colnames(TurbineData))]
  #workingSD <- workingMean[, grep('SD', colnames(workingMean))]
  workingSD <- TurbineData[t , paste(currentMonth, "OpSD", sep="")]
  
  #workingOp <- TurbineData %>% select(contains(currentMonth)) %>% select(-contains('SD'), -contains('Mean'))
  workingOp <- TurbineData[t , paste(currentMonth, "Op", sep="")]

  # if we have an SD, then we sample, otherwise just the mean
  #if(!is.na(workingSD[1,1])){
  if(!is.na(workingSD)){
    
    workingVect <- sampleOp(iter, workingMean, workingSD) 
    
    ### === BC BUG FIXING === ### workingOp needs to be converted from a data.frame otherwise it doesn't return the vector of differences (only the 1st difference)
    #ATG - original from CF was workingOp*(100 - workingVect) which yielded a percentage squared which is further divided by 100 only later in band model code
    #fixed to reflect multiplying proportions
    # sampledTurbine[, grep(currentMonth, names(sampledTurbine))] <- workingOp*(100 - workingVect) Old CF Code
    sampledTurbine[, grep(currentMonth, names(sampledTurbine))] <- (workingOp/100*(1 - workingVect/100))*100
    
    # will explicitly rep mean, although not needed as filling into the DF
    ### === BC BUG FIXING === ### workingOp needs to be converted from a data.frame (see above)
  # } else {sampledTurbine[, grep(currentMonth, names(sampledTurbine))] <- workingOp}*(100 - rep(workingMean, iter)) old CF code
    #ATG -  also the } was in the wrong place below when SD missing
  } else {
    sampledTurbine[, grep(currentMonth, names(sampledTurbine))] <- (workingOp/100*(1 - rep(workingMean/100, iter)))*100
    }
}


