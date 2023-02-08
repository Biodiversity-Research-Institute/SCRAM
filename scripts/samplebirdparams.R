# Modified
# ATG - 20 Dec 2022 - calculated monthly species counts (cumulative daily) for display

# fill bird parameters with sampled values --------------------------------
  #= [NB Masden note: if no measure of variance provided, only the mean value is used
  #= here replace all ifelse with if. Only a constant needs evaluating for the if - so ifelse inefficient


# Wing span ---------------------------------------------------------------

  if(!is.na(species.dat$WingspanSD)){
    
    sampledBirdParams$WingSpan <- sampleWingSpan(iter, species.dat$Wingspan, species.dat$WingspanSD)
    
  } else {
      sampledBirdParams$WingSpan <- rep(species.dat$Wingspan, iter)
    }
    
   
# body length -------------------------------------------------------------

  if(!is.na(species.dat$Body_LengthSD)){
    sampledBirdParams$BodyLength <- sampleBirdLength(iter, species.dat$Body_Length, species.dat$Body_LengthSD)
    
  } else {
      sampledBirdParams$BodyLength <- rep(species.dat$Body_Length, iter)
    }

# flight speed ------------------------------------------------------------

if(!is.na(species.dat$Flight_SpeedSD)){
  sampledBirdParams$FlightSpeed <- sampleFlightSpeed(iter, species.dat$Flight_Speed, species.dat$Flight_SpeedSD)   ##### BC ####
} else {
  sampledBirdParams$FlightSpeed <- rep(species.dat$Flight_Speed, iter)
}

# proportion collision (PCH is propoprtion collision risk height) ----------------------------------------------------
  #if(!is.na(species.dat$Prop_CRH_ObsSD)){
  #  sampledBirdParams$PCH <- sampleCRH(iter, species.dat$Prop_CRH_Obs, species.dat$Prop_CRH_ObsSD)
  #} else {
  #    sampledBirdParams$PCH <- rep(species.dat$Prop_CRH_Obs, iter)
  #  }

# Nocturnal activity ------------------------------------------------------

  #if(!is.na(species.dat$Nocturnal_ActivitySD)){
  #  sampledBirdParams$NocturnalActivity <- sampleNocturnal(iter, species.dat$Nocturnal_Activity, species.dat$Nocturnal_ActivitySD)   ###  BC ###
  #} else {
  #    sampledBirdParams$NocturnalActivity <- rep(species.dat$Nocturnal_Activity, iter)
  #  }


# Avoidance ---------------------------------------------------------------

if(!is.na(species.dat$AvoidanceSD)){
  sampledBirdParams$Avoidance <- sampleAvoidance(iter, species.dat$Avoidance, species.dat$AvoidanceSD)
} else {
  sampledBirdParams$Avoidance <- rep(species.dat$Avoidance, iter)
}
sampledBirdParams$Avoidance[sampledBirdParams$Avoidance > 1] <- 1
sampledBirdParams$Avoidance[sampledBirdParams$Avoidance < 0] <- 0


# Monthly density estimates below here --------------------------------------------

if(c_densOpt == "truncNorm"){
  grid_cell_area <- movement_type$area
  for(currentMonth in month.abb){
      days_in_month <- lubridate::days_in_month(month_vals[[currentMonth]])
    
      # separate out the current month mean and SD. species.count is already filtered for current species
      #workingMean <- species.count %>% select(contains(currentMonth)) %>% select(-contains('SD'))
      workingMean <-  species.count[ , paste(currentMonth)]
      
      #workingSD <- species.count %>% select(contains(currentMonth)) %>% select(contains('SD'))
      workingSD <- species.count[ , paste(currentMonth, "SD", sep="")]
      
      if(survey_data==1){
        if(!is.na(workingSD)){
          workingVect <- max(sampleCount_tnorm(iter, workingMean, workingSD), 0)  
          #create vector of sampled population counts based on the number of iteration requested
          sampledSpeciesCount[,grep(currentMonth, names(sampledSpeciesCount))] <- workingVect
          sampledSpeciesCount[sampledSpeciesCount < 0] <- 0
        }else{
          sampledSpeciesCount[,grep(currentMonth, names(sampledSpeciesCount))] <- rep(workingMean, iter)
          sampledSpeciesCount[sampledSpeciesCount < 0] <- 0
        }
      }else{
      # if we have an SD, then we sample, otherwise just the mean
      #if(!is.na(workingSD[1,1])){

      if(!is.na(workingSD)){
        workingVect <- sampleCount_tnorm(iter, workingMean, workingSD)
        #Calculate the number of animals in a grid cell/sq km
        #movement_type[3] is the area of the grid cell in square km
        #multiply popn. number * prop. occurrence in a cell / area of cell
        #sampledSpeciesCount[,grep(currentMonth, names(sampledSpeciesCount))] <- workingVect
        
        #Calculate # animals in a cell per day
        num_birds_cell_perday_iters[,grep(currentMonth, names(num_birds_cell_perday_iters))] <- (workingVect*MovementSpec[movement.boot.sample, paste(currentMonth)])/days_in_month

        # sampledSpeciesCount[,grep(currentMonth, names(sampledSpeciesCount))] <- (workingVect*MovementSpec[movement.boot.sample, paste(currentMonth)])/movement_type[3]
        #look at occupancy across the entire month, average it out and then distribute the population at the monthly scale
        sampledSpeciesCount[,grep(currentMonth, names(sampledSpeciesCount))] <- (workingVect*MovementSpec[movement.boot.sample, paste(currentMonth)])/(grid_cell_area*days_in_month)
        
        # will explicitly rep mean, although not needed as filling into the DF
      }else{
        num_birds_cell_perday_iters[,grep(currentMonth, names(num_birds_cell_perday_iters))] <- (rep(workingMean, iter)*MovementSpec[movement.boot.sample, paste(currentMonth)])/days_in_month
        #sampledSpeciesCount[,grep(currentMonth, names(sampledSpeciesCount))] <- rep(workingMean, iter)
        
        # sampledSpeciesCount[,grep(currentMonth, names(sampledSpeciesCount))] <- (rep(workingMean, iter)*MovementSpec[movement.boot.sample, paste(currentMonth)])/movement_type[3]
        #look at occupancy across the entire month, average it out and then distribute the population at the monthly scale
        sampledSpeciesCount[,grep(currentMonth, names(sampledSpeciesCount))] <- (rep(workingMean, iter)*MovementSpec[movement.boot.sample, paste(currentMonth)])/(grid_cell_area*days_in_month)
        }
      #sampledSpeciesCount[,grep(currentMonth, names(sampledSpeciesCount))]*MovementSpec[ , paste(currentMonth)]
      }
  }
}

if(c_densOpt == "reSamp"){
  for(currentMonth in month.abb){
    #workingVect <- sampleCount_resample(n = iter, countsSample = species.count %>% select(contains(currentMonth)))
    workingVect <- sampleCount_resample(n = iter, countsSample = species.count[ , c(paste(currentMonth), paste(currentMonth, "SD", sep=""))])
    
    sampledSpeciesCount[,grep(currentMonth, names(sampledSpeciesCount))] <- workingVect
  }
}

if(c_densOpt == "pcntiles"){
  for(currentMonth in month.abb){
    #cPcntls <- species.count %>% select(referenceProbs, contains(currentMonth))
    #cPcntls <- species.count[, c(referenceProbs, paste(currentMonth), paste(currentMonth, "SD", sep=""))]
    #cPcntls <- species.count[, c(grep("referenceProbs", names(species.count)), grep(currentMonth, names(species.count)))]
    cPcntls <- species.count[, c(which(names(species.count)==referenceProbs), grep(currentMonth, names(species.count)))]
    
    workingVect <- sampleCount_pctiles(iter, probs = cPcntls[, 1], countsPctls = cPcntls[, 2])
    
    sampledSpeciesCount[,grep(currentMonth, names(sampledSpeciesCount))] <- workingVect
  }
}


