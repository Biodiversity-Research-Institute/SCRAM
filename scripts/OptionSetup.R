# get flight height data for species of interest using generic/'averaged' distribution curve or bootstraps
# Modified 22 Feb 2022 by ATG to add turbine units for clarity
# Modified to correct issue with operational calculations

#FH.dat = FlightHeight[,c(grep(CRSpecies[s], colnames(FlightHeight)))]
FH.dat <- FlightHeightSpec[,flight.boot.sample[i]] ## using bootstraps

### HD.Y is 41 values long, or 20 values of the radius up and 20 down, and 0
HD.y <- round(seq(-1,1,0.05), 2)

### height off the surface of the water for each height band (which can be used to approximate the integral)
height <- sampledTurbine$HubHeight_m[i] + (HD.y*sampledTurbine$RotorRadius_m[i]) 
#+ TurbineData$TideOff[t]

# calculate potential transits through rotor area each month
Operational <- unlist(MonthlyOperational[i, ])
# divide by 10000 because in sampleturbineparams.R, probabilities in the form of proportion*100 are multiplied
# ATG - changed code above so that Monthly operational is a percentage so just divide by 100
# Operational <- Operational/10000  #old CF Code
Operational <- Operational/100  #operational proportion (0--1)



