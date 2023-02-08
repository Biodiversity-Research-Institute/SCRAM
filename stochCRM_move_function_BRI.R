# stochastic Band function ------------------------------------------------

#' master file that operates as a simple function
#' Author: CF modifed code from CRD
#' Created: 07/16/2020
#' Modifed 22 Feb 2022 by ATG to add units to Turbined Data fields for clarity
#'    27 Jan 23 - cleaned up code and addressed issues with calculations of density and flux
#' Arguments:
#'  - workingDirectory: parent directory that contains data and script directories
#'  
#'  - results_folder: output folder for tables/plots; defaults to "results" in working directory
#'  
#'  - BirdDataFile: currently species-by-parameter CSV file, found in "data/BirdData.csv"
#'  -- [contains: Species 	
#'               Avoidance AvoidanceSD
#'               Body_Length	Body_LengthSD	
#'               Wingspan	WingspanSD	
#'               Flight_Speed	Flight_SpeedSD	
#'               Flight]
#'

#'  - TurbineDataFile: currently contains turbine model-by-parameter CSV, found in "data/TurbineData.csv"
#'  -- [contains: TurbineModel	Blades	
#'                RotationSpeed	RotationSpeedSD	RotorRadius	RotorRadiusSD	
#'                HubHeightAdd	HubHeightAddSD	BladeWidth	BladeWidthSD	
#'                Pitch	PitchSD	
#'                JanOp	JanOpMean	JanOpSD	
#'                FebOp	FebOpMean	FebOpSD ... etc to Dec]
#'                
#'  - CountDataFile: currently contains species-by-parameters CSV, found in "data/CountData.csv"  
#'  -- [contains: Species	Jan	JanSD	Feb	FebSD ... etc to Dec]
#'  
#'  - FlightDataFile: currently contains height-by-species CSV , found in "data/FlightHeight.csv" 
#'  -- [contains: Height (300 values)]
#'  
#'  - iter: integer constant > 0. number of iterations - the number of stochastic draws to take
#'  
#'  - CRSpecies: character 'vector' of species to consider, looks to match/define other species name, so must match throughout
#'  
#'  - TPower: constant. How much power will the wind farm generate (MW)
#'  
#'  - LargeArrayCorrection: character ["yes" or "no"]
#'  
#'  - WFWidth: constant. "width" of wind farm used in Large Array Correction (km)
#'  
#'  - Prop_Upwind: constant, ought to be 0-1 bounded as roportion of flights upwind - default of 0.5.
#'  
#'  - Latitude: numeric. Decimal latitude.
#'  

# start of function -------------------------------------------------------
stochasticBand <- function(
  results_folder = "results", 
  BirdData, #= tablereact4,
  TurbineData, #= tablereact2,
  CountData, #= tablereact8,
  movement_type, #= movement_type2,
  FlightData, #= tablereact6,
  iter = 100,
  ModelType,
  CRSpecies,
  LargeArrayCorrection = "yes",
  Options_select, #= radioreact2,
  progress, #= progress,
  interruptor, #= interruptor,
  survey_data = 0,
  runlocal = FALSE
  # results_folder = "results", 

) {
  if(length(which(CRSpecies=="Other"))>0){
    CRSpecies <- CountData[, "Species"]
  }else{
    CRSpecies <- CRSpecies
  }
  if(!identical(CRSpecies[order(CRSpecies)], unlist(lapply(1:length(FlightData), function(x) FlightData[[x]][1,"Species"]))[order(unlist(lapply(1:length(FlightData), function(x) FlightData[[x]][1,"Species"])))])
     |length(which(CRSpecies[order(CRSpecies)] %in% CountData[,"Species"]==FALSE)) > 0
    ){
      #return(list(SpeciesCheck = CRSpecies[order(CRSpecies)]))
     return(list(SpeciesCheck = CRSpecies[order(CRSpecies)]))
    }else{
  
  source("scripts/datachecksTurbines.R", local=T)
  source("scripts/datachecksBirds.R", local=T)
  source("scripts/datachecksFHD.R", local=T)
  source("scripts/datachecksCounts.R", local=T)

  # user provides site-level passage rates as counts instead of movement model output (see manual)
  #source("scripts/datachecksMovement.R", local=T)
  # run checks on input data files; if there are any failures, run this loop for an error message
  if(length(which(check < length(TurbineData[,1])))>0|length(which(checkBirdData < length(BirdData[,1])))>0|sum(checkFHD) != (2*length(FlightData))|sum(checkCounts) != 2){
    TurbineError <- c("Num_Turbines","TurbineModel", "Num_Blades", "RotorRadius_m", "RotorRadiusSD_m", "HubHeightAdd_m", "HubHeightAddSD_m", "BladeWidth_m", "BladeWidthSD_m", "WindSpeed_mps", "WindSpeedSD_mps", 
                      "Pitch", "PitchSD", "WFWidth_km", "Latitude", "Longitude", "Prop_Upwind")[which(check < length(TurbineData[,1]))]
    BirdDataError <- c("Species", "Avoidance", "AvoidanceSD", "Body_Length", "Body_LengthSD", "Wingspan", "WingspanSD", "Flight_Speed", "Flight_SpeedSD", 
                       "Flight")[which(checkBirdData < length(BirdData[,1]))]
    if(sum(checkFHD) < (2*length(FlightData))){
    FHDError <- "flight height distributions"  
    }else{
      FHDError <- NULL 
    }
    if(sum(checkCounts) < 2){
      CountsError <- "species count data"  
    }else{
      CountsError <- NULL 
    }
    # muted when user provides passage rates instead of movement model output (see manual)
    #if(sum(checkMovement) < (2*length(MovementData))){
    #  MovementError <- "movement data"  
    #}else{
    #  MovementError <- NULL 
    #}
    return(list(TurbineError = TurbineError, BirdDataError = BirdDataError, FHDError = FHDError, CountsError = CountsError))
    # if all checks pass run main script
  }else{
  # preamble ---------------------------------------------------------------
  # start timer
  start.time <- Sys.time()
   
  ### potential to improve efficiency and stability here ###
  S <- iter*20 # this is number of samples, increased to ensure enough valid values (used in get_rotor_plus_pitch_auto.R)
  
  # initialize objects to store simulation replicates of monthly collisions, for each option, for current species and turbine
  monthCollsnReps <- list()
  # monthCollsnReps_opt3 <- list()
  dailyCollsnReps <- list()
  # dailyCollsnReps_opt3 <- list()
  
  sampledParamsBird <- list()
  sampledParamsTurbine <- list()
  #set variable to hold daily estimates of birds in WF
  num_birds_WF_perday <- list()
  num_birds_cell_perday <- list()
  movement.boot.sample_list <- list()
  
  # num_birds_WF <- list() # data.frame(month.abb)

  # read data sources -------------------------------------------------------
  # read in count data for each species, bird biometric data, flight height distributions and turbine characteristics
  #source("scripts/helpers_read data.R", local=T)
  # helpers no longer needed so run the only command from script not run elsewhere
  row.names(BirdData) <-  BirdData$Species
  
  # produces a data frame with number of hours daylight and night per month
  # day length calculations not needed with motus implementation
  #source("scripts/DayLength.R", local=T) 
  # when DayLength.R is muted, create the data frame for storing monthly abundance (or "flux") information
  # hours = data.frame(month.abb)
  # names(hours) = c("Month")
  num_birds_WF_permonth = data.frame(month.abb)
  names(num_birds_WF_permonth) = c("Month")
  month_vals <- setNames(1:12, month.abb)
  day_vals <- sapply(X=1:12, FUN=function(m) lubridate::days_in_month(m))
  day_vals_iter <- as.data.frame(t(replicate(iter, day_vals)))
    
  # load sampling functions for stochastic bits -----------------------------
  source("scripts/helpers_sampling functions.R", local=T)
  
  # Prob collision functions and associated bits ---------------------------
  # create dataframe giving the width of the chord relative to its maximum width at given points along the radius of the rotor
  # rad denotes the point along the blade (in terms of the radius of the rotor blade) for the corresponding 
  ## value in vector c
  # 21 values long for 20 positive values of the radius and 0
  rad = round(seq(0,1,0.05),2) 
  circ = c(0.69,0.73,0.79,0.88,0.96,1,0.98,0.92,0.85,0.8,0.75,0.7,0.64,0.58,0.52,0.47,0.41,0.37,0.3,0.24,0)
  coverC = data.frame(cbind(rad, circ))
  
  # bring in call functions needed to calculate collision risk along blade
  source("scripts/PCollFunctions.R", local=T)
  
  # load a lookup table for nicer looking species names
  SpeciesLabels <- read.csv("data/SpeciesLabels.csv", sep=",", header=FALSE)
  
  # calculations ------------------------------------------------------------
  ##set progress bar
  #pb   <- txtProgressBar(1, iter*length(CRSpecies)*nrow(TurbineData), style=3)
  
  ###create overall results summary table###
  # resultsSummary = data.frame(matrix(data = 0, ncol = 8, nrow = length(CRSpecies)*nrow(TurbineData)))
  resultsSummary = data.frame(matrix(data = NA, ncol = 8, nrow = length(CRSpecies)*nrow(TurbineData)))
  names(resultsSummary) = c("Species", "Turbine", "Option", "Mean", "SD", "CV", "Median", "IQR")

  # Start of the species loop -----------------------------------------------    
  for (s in 1:length(CRSpecies)){

    #if(as.numeric(movement_type[1])==0){
    #if(is.list(MovementData)){
    #  MovementSpec <- MovementData[[which(lapply(1:length(CRSpecies), function(x) MovementData[[x]][1, 1])==CRSpecies[s])[1]]]
    #}else{
    #  MovementSpec <- MovementData
    #}
    #}else{
      # MovementSpec <- read.csv(paste("data/movements/MovementBaked_", CRSpecies[s], "_", movement_type[2], ".csv", sep=""), header=T)
  
    if (ModelType == "first"){
      # ATG - use Evan's truncated models 
      # MovementSpec <- read.csv(paste("data/movements/MovementBaked_", CRSpecies[s], "_trunc", movement_type[2], ".csv", sep=""), header=T)
      # MovementSpec <- read.csv(paste("data/movements/MovementBaked_", CRSpecies[s], "_trunc", movement_type$id, ".csv", sep=""), header=T)
      #read from zipped file to reduce number of files to upload to R Shiny and handle generally
      MovementSpec <- read.csv(unz(paste0("data/movements/", CRSpecies[s], "_movements_trunc.zip"), 
                                            paste0("MovementBaked_", CRSpecies[s], "_trunc", movement_type$id, ".csv")), header=T)
      
    } else {
    #use Evan's last position models
      # MovementSpec <- read.csv(paste("data/movements/MovementBaked_", CRSpecies[s], "_last", movement_type[2], ".csv", sep=""), header=T)
      # MovementSpec <- read.csv(paste("data/movements/MovementBaked_", CRSpecies[s], "_last", movement_type$id, ".csv", sep=""), header=T)
      #read from zipped file to reduce number of files to upload to R Shiny and handle generally
      MovementSpec <- read.csv(unz(paste0("data/movements/", CRSpecies[s], "_movements_last.zip"), 
                                            paste0("MovementBaked_", CRSpecies[s], "_last", movement_type$id, ".csv")), header=T)
      
    }

    #}
    
    species.dat <- subset(BirdData, Species == CRSpecies[s])
    
    species.dat$FlightNumeric <- ifelse(species.dat$Flight == 'Flapping', 1, 0)
    
    # select the correct sampling option
    #c_densOpt <- filter(DensityOpt, specLabel == CRSpecies[s])$userOption
    # cf added version that accomplishes the same goal without 'dplyr'
    #c_densOpt <- DensityOpt[DensityOpt$specLabel == CRSpecies[s], 'userOption']
    ### to run without user interface, set c_densOpt to "truncNorm" ###
    c_densOpt <- "truncNorm"
    
    if(c_densOpt == "truncNorm"){
      species.count = subset(CountData, Species == CRSpecies[s])  
    }
    
    # if(c_densOpt == "reSamp"){
    #   # cf added option that accomplishes the same goal without 'data.table'
    #   #species.count <- fread("data/birdDensityData_samples.csv") %>%
    #   #  filter(specLabel == CRSpecies[s])
    #   species.count <- read.csv("data/birdDensityData_samples.csv")
    #   species.count <- species.count[species.count$specLabel == CRSpecies[s], ]
    # }
    # 
    # if(c_densOpt == "pcntiles"){
    #   # cf added option that accomplishes the same goal without 'data.table'
    #   #species.count <- fread("data/birdDensityData_refPoints.csv") %>%
    #   #  filter(specLabel == CRSpecies[s])
    #   species.count <- read.csv("data/birdDensityData_refPoints.csv")
    #   species.count <- species.count[species.count$specLabel == CRSpecies[s], ]
    # }
    
    Flap_Glide <- ifelse(species.dat$Flight == "Flapping", 1, 2/pi)
    
    # get species-specific flight height distributions
    #ht<-paste("data/", CRSpecies[s],"_ht_dflt.csv", sep='')
    #FlightHeightSpec <- read.csv(ht, header = T)
    if(is.list(FlightData)){
      FlightHeightSpec <- FlightData[[which(lapply(1:length(CRSpecies), function(x) FlightData[[x]][1,1])==CRSpecies[s])[1]]]
    }else{
      FlightHeightSpec <- FlightData
    }
    flight.boot <- 3:dim(FlightHeightSpec)[2] # skip second column since it contains heights, and first column because it has the species ID
    flight.boot.sample <- sample(flight.boot, iter, replace=T)
    
    movement.boot <- 1:dim(MovementSpec)[1] 
    movement.boot.sample <- sample(movement.boot, iter, replace=T)
    movement.boot.sample_list[[CRSpecies[s]]] <- movement.boot.sample
    # create data frame for bird-related parameters
    # sampledBirdParams = data.frame(matrix(data = 0, ncol = 7, nrow = iter))
    # ATG - change 0's no NAs
    sampledBirdParams = data.frame(matrix(data = NA, ncol = 7, nrow = iter))
    names(sampledBirdParams) = c("Avoidance", "WingSpan", "BodyLength", "FlightSpeed")
    
    # create data frame for count/density data
    # sampledSpeciesCount = data.frame(matrix(data = 0, ncol = 12, nrow = iter))
    sampledSpeciesCount = data.frame(matrix(data = NA, ncol = 12, nrow = iter))
    # ATG - change 0's no NAs
    names(sampledSpeciesCount) = month.abb
    
    #create variable to hold species counts for selected grid
    num_birds_cell_perday_iters <- sampledSpeciesCount
    
    num_birds_WF_perday_iters <- sampledSpeciesCount
    
    # create data frame for density data
    # densitySummary=data.frame(matrix(data = 0, ncol = nrow(TurbineData)*3, nrow = iter))
    # ATG - change 0's no NAs
    densitySummary=data.frame(matrix(data = NA, ncol = nrow(TurbineData)*3, nrow = iter))
    
    ##add names of columns later in turbine loop###
    
    # sample bird parameters
    source("scripts/samplebirdparams.R", local=T)
    
    # Start of turbine model loop ---------------------------------------------------
    for (t in 1:nrow(TurbineData))  {
      #' objects indexed by t
      #' TurbineData
      
      # moved out of t loop to precede day length calculations
      # create data frame for turbine data
      # sampledTurbine <- data.frame(matrix(data = 0, ncol = 18, nrow = iter))  
      # ATG - data to 0s which may be a problem. Set to NA to see if showing up in data down the line
      sampledTurbine <- data.frame(matrix(data = NA, ncol = 18, nrow = iter))  
      
      names(sampledTurbine) = c("RotorRadius_m", "HubHeight_m", "BladeWidth_m", "WindSpeed_mps", "RotorSpeed_rpm", "Pitch", 
                                "JanOp", "FebOp", "MarOp", "AprOp", "MayOp", "JunOp", "JulOp",   
                                "AugOp", "SepOp", "OctOp", "NovOp", "DecOp")
      
      # create results tables
      # tab1 <- data.frame(matrix(data = 0, ncol = 12, nrow = iter))
      Option1_Collisions_iter <- data.frame(matrix(data = NA, ncol = 12, nrow = iter))
      # ATG - change 0's to NA
      names(Option1_Collisions_iter) <- month.abb
      Option3_Collisions_iter <- Option1_Collisions_iter
      
      Option1_Collisions_iter_daily <- Option3_Collisions_iter_daily <- Option1_Collisions_iter
      
      # create objects to store PColl and CollInt 
      # sampledPColl <- data.frame(matrix(data = 0, ncol = 1, nrow = iter))
      sampledPColl <- data.frame(matrix(data = NA, ncol = 1, nrow = iter))
      names(sampledPColl) <- "PColl"
      
      # sampledCollInt <- data.frame(matrix(data = 0, ncol = 1, nrow = iter))
      sampledCollInt <- data.frame(matrix(data = NA, ncol = 1, nrow = iter))
      names(sampledCollInt) <- "CollInt"
      
      # sample turbine parameters based on their sampling dists
      # sample from wind parameters, then uses pitch/speed curves
      #source("scripts/get_rotor_plus_pitch_auto.R", local=T)
      
      # outputs large (size S) rotor speeds and pitch - sampled into the DF
      source("scripts/sampleturbineparams.R", local=T)

      
      #MonthlyOperational <- sampledTurbine %>% select(contains("Op", ignore.case = F))
      # cf added a version that accomplished the same goal without using 'dplyr'
      MonthlyOperational <- sampledTurbine[, grep("Op", colnames(sampledTurbine))]
      MeanOperational <- apply(MonthlyOperational, 1, mean)
      
      # Iterating i - over random samples  --------------------------------------        
      for(i in 1:iter){ 
        # mute the next two lines to run locally
        if(runlocal == FALSE){
        progress$inc(1/(iter*length(CRSpecies)*nrow(TurbineData)), detail = paste(SpeciesLabels[SpeciesLabels[,1] == CRSpecies[s], 2], " (species ", s, " of ", length(CRSpecies), "), turbine models ", t, " of ", nrow(TurbineData), ": ", round((i/(iter))*100), "%", sep=""))
        interruptor$execInterrupts()
        }
        
          #' objects that are index by i
          #' MonthlyOperational
          #' sampledTurbine
          #' sampledBirdParams
          #' sampledSpeciesCount
          
          # create single numeric outputs to speed up pcoll function       
          # coverC
          currentRad <- coverC$rad
          currentCirc <- coverC$circ
          
          # fixed turbine pars
          currentBlades <- TurbineData$Num_Blades[t]
          
          # sampled turbine pars
          currentRotorRadius <- sampledTurbine$RotorRadius_m[i]
          currentBladeWidth <- sampledTurbine$BladeWidth_m[i]
          currentRotorSpeed <- sampledTurbine$RotorSpeed_rpm[i]
          currentPitch <- sampledTurbine$Pitch[i]
          
          
          # fixed bird parameter
          currentFlightNumeric <- species.dat$FlightNumeric
          
          # sampled bird parameters
          currentWingSpan <- sampledBirdParams$WingSpan[i]
          currentFlightSpeed <- sampledBirdParams$FlightSpeed[i]
          currentBirdLength <- sampledBirdParams$BodyLength[i]

          # estimate collision risk - options appear here ------------------------------
          ############## STEP ONE - Calculate the collision risk in the absence of avoidance action
          source("scripts/ProbabilityCollision.R", local=T)

          ############## STEP TWO - Calculate Flux Factor - the number of birds passing a turbine in each month
          # first calculate turbine frontal area
          # NTurbines = round (TurbineData$TPower[t] / TurbineData$TurbineModel[t]) ### Number of turbines of given Output required to produce target output
          # BOEM requested that NTurbines be an input instead of total power
          # ATG - need to account for each run (differnt parameter runs)
          NTurbines = TurbineData$Num_Turbines[t]
          
          # ATG - CF changed to a linear function , number of turbines * the rotor diameter - huh? Revert to rotor area
          # TotalFrontalArea = NTurbines*sampledTurbine$RotorRadius_m[i]*2
          #ATG - 26 Jan 23 - change to area and see what that does!
          # TotalFrontalArea = NTurbines * pi * sampledTurbine$RotorRadius[i] ^2
          
          # TotalFrontalArea = NTurbines * ((pi * sampledTurbine$RotorRadius[i]^2)/(2*sampledTurbine$RotorRadius[i])) * (currentFlightSpeed *12*3600)
          TotalFrontalArea = NTurbines * (2 * sampledTurbine$RotorRadius[i]) * (currentFlightSpeed *12*3600)
          
          
          # cf added an option that changes the rotor area from circular to square to better model the FHD in individual-based options
          TotalFrontalAreaProb <- NTurbines*(sampledTurbine$RotorRadius_m[i]*2)^2

          ############## STEP THREE - Calculate Large Array Correction Factor
          # calculate number of turbine rows - manually enter if appropriate
          NTurbRows <- NTurbines ^ 0.5
          
          #ATG - check MeanOperational[i]/100 code I think it actually might need to be  100^2
          #also in the original stochastic CRM code there was a difference from how MeanOperational was calculated:
          # Original: MeanOperational = as.numeric(workingOp) - workingVect  (where workingVect is the sampled mean/SD downtime) e.g, 96% - 6% = 90%
          # CollideR: MeanOperational =  workingOp*(100 - workingVect) = 96% monthly operation * (100 - 6% sampled downtime) = 9024%^2
          # However, this code that was directly copied from original to CollideR: MeanOperational[i]/100 results in a proportion 0-1) of operation and 
          # in CF CollideR a percentaage. So I think that this is indeed wrong. The next question is the subtle difference between subtracting the downtime 
          # from total operational time, vs. multiplying two percentages (or  proportions). I think his argument is that downtime is independent of the 
          # time turbines can be operational due to good wind speeds and thus should be multiplicative. E.g., a turbine can be operating 96% of the time 
          # in a month due to good wind speeds, but within that month it's down 6% of the time due to maintenance that it could be operating 
          # thus should be 0.96 * (1 - 0.06) = 0.9024 or 90.24% operational per month.
          
          CollRiskSinglePassage <- NTurbines * (pi * sampledTurbine$RotorRadius_m[i]^2)/(2 * sampledTurbine$RotorRadius_m[i] * TurbineData$WFWidth_km[t] * 1000) * 
            (P_Collision/100) * (MeanOperational[i]/100) * (1-sampledBirdParams$Avoidance[i])
          
          #ATG - below seems to be an error in translation of the math from Band 2012 calc of the large 
          # array correction factor. I have confirmed it was an issue at least starting with Masden 2015 code
          # I looked at spreadsheet calcs for Band 2012 and the original equations.
          # L_ArrayCF_suspect <- 1 - (NTurbRows - 1) / (2*NTurbRows) * CollRiskSinglePassage +
          #   (NTurbRows - 1) * (2*NTurbRows)/(6 * NTurbRows^2) * (CollRiskSinglePassage ^2)

          L_ArrayCF <- 1 - (NTurbRows - 1) / (2*NTurbRows) * CollRiskSinglePassage + 
            (NTurbRows - 1) * (NTurbRows - 2)/(6 * NTurbRows^2) * (CollRiskSinglePassage ^2)
          
          FH.dat <- FlightHeightSpec[,flight.boot.sample[i]] ## using bootstraps
          # cf created a separate script to set up the flight height distribution and 
          # rotor height parameters for the Options scripts
          source("scripts/OptionSetup.R", local=T)
          
          #if(length(which(Options_select == 1))>0){
          if(Options_select == '1'){
            # Option 1 ----------------------------------------------------------------
            #######################	use option 1 - site-specific flight height information	##########################
            source("scripts/Option1.R", local=T)
            ## add results to overall species/turbine results table
            Option1_Collisions_iter[i,] = Option1_CollisionRate$monthly_collisions
            
            #store P_Coll
            sampledPColl[i,] <- P_Collision/100
            
            #daily collision rates
            Option1_Collisions_iter_daily[i,] <- Option1_CollisionRate$daily_collisions

          }

          #if(length(which(Options_select == 3))>0){
          if(Options_select == '3'){
            # Option 3 ----------------------------------------------------------------
            #######################	use option 3 - modelled flight height distribution ###############################
            ####################### taking account of variation in risk along the rotor blades #######################
            source("scripts/Option3.R", local=T)
            ## add results to overall species/turbine results table
            Option3_Collisions_iter[i,] = Option3_CollisionRate$monthly_collisions 
            
            #Store Collision Integral
            sampledCollInt[i,] <- CollInt
            
            #daily collision rates
            Option3_Collisions_iter_daily[i,] <- Option3_CollisionRate$daily_collisions

          }

      } # end of i over iter
      # end of the random sampling iterations i --------------------------------
      
      source("scripts/turbineSpeciesOutputs.R", local=T)

      # store simulation replicates under each option, for current species and turbine  ===========
      cSpec <- CRSpecies[s]
      cTurbModel <- paste0("turbModel", TurbineData$TurbineModel[t])
      
      if(Options_select == '1'){
        monthCollsnReps[[cSpec]][[cTurbModel]] <- Option1_Collisions_iter
        dailyCollsnReps[[cSpec]][[cTurbModel]] <- Option1_Collisions_iter_daily
      }
      if(Options_select == '3'){
        monthCollsnReps[[cSpec]][[cTurbModel]] <- Option3_Collisions_iter
        dailyCollsnReps[[cSpec]][[cTurbModel]] <- Option3_Collisions_iter_daily
      }

      sampledParamsBird[[cSpec]][[cTurbModel]] <- sampledBirdParamsIters
      sampledParamsTurbine[[cSpec]][[cTurbModel]] <- sampledTurbineParamsIters
      num_birds_cell_perday[[cSpec]][[cTurbModel]] <- num_birds_cell_perday_iters
      num_birds_WF_perday[[cSpec]][[cTurbModel]] <- num_birds_WF_perday_iters

    } # end of t over number of turbines
    # End of the turbine loop -------------------------------------------------
    
    
    # output species plots of density by option with curves for turbine model
    # plot density by Option (useful if there several turbine models)
    #if (nrow(TurbineData)>1){
    #  source("scripts/species_turbine_plots.R", local = T) 
    #}
    
    # relabel sampledBirdParams by species name
    assign(paste(CRSpecies[s],"params", sep="_"), sampledBirdParams)
    
    # relabel sampledSpeciesCount by species name
    assign(paste(CRSpecies[s],"counts", sep="_"), sampledSpeciesCount)
    
  } # end of the species loop over s

  run.time <- Sys.time() - start.time
  print(run.time)

  # return inputs and outputs as output for storing, reporting, and graphing ===========
  return(list(monthCollsnReps = monthCollsnReps, dailyCollsnReps = dailyCollsnReps,
              sampledParamsBird = sampledParamsBird, sampledParamsTurbine = sampledParamsTurbine, 
              Options_select = Options_select, movement.boot.sample_list=movement.boot.sample_list, 
              num_birds_cell_perday = num_birds_cell_perday, num_birds_WF_perday = num_birds_WF_perday, 
              resultsSummary = resultsSummary, Turbines = TurbineData$TurbineModel, CRSpecies = CRSpecies))
  }
  }
}


# EXTRA CODE --------------------------------------------------------------
# unmute the following 5 lines to set up results folder locally for saving results and run metadata
# create folders and paths ------------------------------------------------
# create results folder
# if(results_folder == "") results_folder <- Sys.Date() ## if no name given for results folder, use today's date
# if(results_folder !="") dir.create(results_folder) ## if name given for results folder use that and create folder

# make input, figures, and tables folders
# dir.create(paste(results_folder, "figures", sep="/"))
# dir.create(paste(results_folder, "tables", sep="/"))
# dir.create(paste(results_folder, "input", sep="/"))
# 
# moved out of first loop to precede day length calculations
# create data frame for turbine data
#sampledTurbine <- data.frame(matrix(data = 0, ncol = 23, nrow = iter))
#names(sampledTurbine) = c("RotorRadius", "HubHeight", "BladeWidth", "WindSpeed", "RotorSpeed", "Pitch", 
#                          "WFWidth", "Prop_Upwind", "Latitude", "TPower",
#                          "JanOp", "FebOp", "MarOp", "AprOp", "MayOp", "JunOp", "JulOp",   
#                          "AugOp", "SepOp", "OctOp", "NovOp", "DecOp")

# original progress bars not used with approach that uses promises
#updateProgress_Spec,
#updateProgress_Iter,
# following progress bar lines not used with implementation that uses promises
#incProgress(1/(iter*length(CRSpecies)), detail = paste(CRSpecies[s], " (species ", s, " of ", length(CRSpecies), "): ", i/iter*100, "%", sep=""))
# progress bar update for iterations; not used with implementation that uses promises
#if (is.function(updateProgress_Spec)) {
#  text <- gsub("_", " ", CRSpecies[s])
#  updateProgress_Spec(value = s/(length(CRSpecies)), detail = text)
#}

# progress bar update for iterations
#if (is.function(updateProgress_Iter)) {
#  text <- NULL # paste0("Working through iteration ", i)
#  updateProgress_Iter(value = i/iter, detail = text)
#}

##progress bar for iterations##

#setTxtProgressBar(pb, s*t+i)
#setTxtProgressBar(pb, (s*nrow(TurbineData)-(nrow(TurbineData)-t))*iter-(iter-i))
# this version of the progress bar not used with implementation that uses promises
# reset counter of progress bar for iterations =====================
#if (is.function(updateProgress_Iter)) {
#  text <- NULL # paste0("Working through iteration ", i)
#  updateProgress_Iter(value = 0, detail = text)
#}

# #if(length(which(Options_select == 2))>0){
# if(Options_select == '2'){
#   # Option 2 ----------------------------------------------------------------
#   ####################### use option 2 - modelled flight height distribution		###########################
#   source("scripts/Option2.R", local=T)
#   ## add results to overall species/turbine results table
#   tab2[i,]=Option2_CollisionRate[,2]
# }
# #if(length(which(Options_select == 4))>0){
# if(Options_select == '4'){
#   # Option 4a ----------------------------------------------------------------
#   #######################	use option 3 - modelled flight height distribution ###############################
#   #######################	taking account of variation in risk along the rotor blades #######################
#   source("scripts/Option4a.R", local=T)
#   ## add results to overall species/turbine results table
#   tab4[i,]=Option4a_CollisionRate[,2]  
# }

# #if(length(which(Options_select == 5))>0){
# if(Options_select == '5'){
#   # Option 5 ----------------------------------------------------------------
#   #######################	do model using option 3 - modelled flight height distribution	############################
#   #######################	taking account of variation in risk along the rotor blades ###############################
#   source("scripts/Option5.R", local=T)
#   ## add results to overall species/turbine results table
#   tab5[i,]=Option5_CollisionRate[,2] 
# }

# #if(length(which(Options_select == 6))>0){
# if(Options_select == '6'){
#   # Option 6 ----------------------------------------------------------------
#   #######################	use option 3 - modelled flight height distribution ###############################
#   #######################	taking account of variation in risk along the rotor blades #######################
#   source("scripts/Option6.R", local=T)
#   ## add results to overall species/turbine results table
#   tab6[i,]=Option6_CollisionRate[,2] 
# }


