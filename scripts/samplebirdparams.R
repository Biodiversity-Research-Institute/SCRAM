# Modified
# ATG - 20 Dec 2022 - calculated monthly species counts (cumulative daily) for display
# ATG - 24 Feb 2023 - changed to migrant flux model like Band 2012 where birds move across broad front

# fill bird parameters with sampled values --------------------------------
  #= [NB Masden note: if no measure of variance provided, only the mean value is used
  #= here replace all ifelse with if. Only a constant needs evaluating for the if - so ifelse inefficient


# Avoidance ---------------------------------------------------------------

if(!is.na(species.dat$AvoidanceSD)){
  sampledBirdParams$Avoidance <- sampleAvoidance(iter, species.dat$Avoidance, species.dat$AvoidanceSD)
} else {
  sampledBirdParams$Avoidance <- rep(species.dat$Avoidance, iter)
}
sampledBirdParams$Avoidance[sampledBirdParams$Avoidance > 1] <- 1
sampledBirdParams$Avoidance[sampledBirdParams$Avoidance < 0] <- 0

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

# if(!is.na(species.dat$Nocturnal_ActivitySD)){
#   sampledBirdParams$NocturnalActivity <- sampleNocturnal(iter, species.dat$Nocturnal_Activity, species.dat$Nocturnal_ActivitySD)   ###  BC ###
# } else {
#   sampledBirdParams$NocturnalActivity <- rep(species.dat$Nocturnal_Activity, iter)
# }

# Proportion upwind flight ------------------------------------------------------

if(!is.na(species.dat$Prop_UpwindSD)){
  sampledBirdParams$PropUpwind <- samplePropUpwind(iter, species.dat$Prop_Upwind, species.dat$Prop_UpwindSD)   ###  BC ###
} else {
  sampledBirdParams$PropUpwind <- rep(species.dat$Prop_Upwind, iter)
}


# Monthly density estimates below here --------------------------------------------
grid_cell_area <- ModelCell$area

if(c_densOpt == "truncNorm"){
  for(currentMonth in month.abb){
      days_in_month <- lubridate::days_in_month(month_vals[[currentMonth]])
    
      # separate out the current month mean and SD. species.count is already filtered for current species
      species.count_mean <-  species.count[ , paste(currentMonth)]
      species.count_SD <- species.count[ , paste(currentMonth, "SD", sep="")]


      if(!is.na(species.count_SD)){
        species.count_vect <- sampleCount_tnorm(iter, species.count_mean, species.count_SD)

        # Calculate the number of animals in a grid cell per month
        num_birds_cell_permonth_iters[,grep(currentMonth, names(num_birds_cell_permonth_iters))] <- species.count_vect*MovementSpec[movement.boot.sample, paste(currentMonth)]
      }else{
        # Calculate the number of animals in a grid cell per month
        # will explicitly rep mean, although not needed as filling into the DF
        num_birds_cell_permonth_iters[,grep(currentMonth, names(num_birds_cell_permonth_iters))] <- rep(species.count_mean, iter)*MovementSpec[movement.boot.sample, paste(currentMonth)]
      }
      # due to overprediction of offshore flux due to stationary coastal birds, offset using the the proportion of
      # transient movements (to total states)
      num_birds_cell_permonth_iters[,grep(currentMonth, names(num_birds_cell_permonth_iters))] <- num_birds_cell_permonth_iters[,grep(currentMonth, names(num_birds_cell_permonth_iters))] * transient_prop
        
      if (migr_res == "resident"){
        #Calculate the number of animals in a grid cell/sq km
        # divide population by grid cell area in sq km * the occupancy
        bird_dens_sqrkm[,grep(currentMonth, names(bird_dens_sqrkm))] <- num_birds_cell_permonth_iters[,grep(currentMonth, names(num_birds_cell_permonth_iters))] / grid_cell_area
        
      } else { #migrants
        # Change to be more like the Band 2012 migratory front calculations. Calculate birds/km where you estimate number in the cell / avg width of cell in km
        bird_flux_perkm[,grep(currentMonth, names(bird_flux_perkm))] <- num_birds_cell_permonth_iters[,grep(currentMonth, names(num_birds_cell_permonth_iters))] / sqrt(grid_cell_area)

      }
    
      #Calculate # animals in a cell per month and day
      num_birds_cell_perday_iters[,grep(currentMonth, names(num_birds_cell_perday_iters))] <-  num_birds_cell_permonth_iters[,grep(currentMonth, names(num_birds_cell_permonth_iters))] / days_in_month
  }
}

# if(c_densOpt == "reSamp"){
#   for(currentMonth in month.abb){
#     #species.count_vect <- sampleCount_resample(n = iter, countsSample = species.count %>% select(contains(currentMonth)))
#     species.count_vect <- sampleCount_resample(n = iter, countsSample = species.count[ , c(paste(currentMonth), paste(currentMonth, "SD", sep=""))])
#     
#     sampledSpeciesCount[,grep(currentMonth, names(sampledSpeciesCount))] <- species.count_vect
#   }
# }
# 
# if(c_densOpt == "pcntiles"){
#   for(currentMonth in month.abb){
#     #cPcntls <- species.count %>% select(referenceProbs, contains(currentMonth))
#     #cPcntls <- species.count[, c(referenceProbs, paste(currentMonth), paste(currentMonth, "SD", sep=""))]
#     #cPcntls <- species.count[, c(grep("referenceProbs", names(species.count)), grep(currentMonth, names(species.count)))]
#     cPcntls <- species.count[, c(which(names(species.count)==referenceProbs), grep(currentMonth, names(species.count)))]
#     
#     species.count_vect <- sampleCount_pctiles(iter, probs = cPcntls[, 1], countsPctls = cPcntls[, 2])
#     
#     sampledSpeciesCount[,grep(currentMonth, names(sampledSpeciesCount))] <- species.count_vect
#   }
# }


