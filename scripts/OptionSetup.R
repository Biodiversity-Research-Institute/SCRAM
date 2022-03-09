# get flight height data for species of interest using generic/'averaged' distribution curve or bootstraps
# Modified 22 Feb 2022 by ATG to add turbine units for clarity

#FH.dat = FlightHeight[,c(grep(CRSpecies[s], colnames(FlightHeight)))]
FH.dat <- FlightHeightSpec[,flight.boot.sample[i]] ## using bootstraps

### HD.Y is 41 values long, or 20 values of the radius up and 20 down, and 0
HD.y <- round(seq(-1,1,0.05), 2)

### height off the surface of the water for each height band (which can be used to approximate the integral)L
height <- sampledTurbine$HubHeight_m[i] + (HD.y*sampledTurbine$RotorRadius_m[i]) 
#+ TurbineData$TideOff[t]

# calculate potential transits through rotor area each month
Operational <- unlist(MonthlyOperational[i, ])
# divide by 10000 because in sampleturbineparams.R, probabilities in the form of proportion*100 are multiplied
Operational <- Operational/10000


