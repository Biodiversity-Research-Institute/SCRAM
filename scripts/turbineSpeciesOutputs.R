# Output tables and box plots ---------------------------------------------
###relabel sampledTurbine by turbine name###
# Modified 22 Feb 2022 by ATG to add turbine units for clarity
# 02 Feb 23 - changed tab1 to Option1_Collisions_iter and removed code for other options to reduce unneeded code

assign(paste(TurbineData$TurbineModel[t], "params", sep="_"), sampledTurbine)

###relabel results table
assign(paste(CRSpecies[s],TurbineData$TurbineModel[t],"opt1", "results", sep="_"), Option1_Collisions_iter)
assign(paste(CRSpecies[s],TurbineData$TurbineModel[t],"opt3", "results", sep="_"), Option3_Collisions_iter)

#== Table 1 generation
monthlySummaryOpt1 = data.frame(matrix(data = 0, ncol = 6, nrow = 12))
names(monthlySummaryOpt1) = c("Month", "Mean", "SD", "CV", "Median", "IQR")
monthlySummaryOpt1[,1] = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",  "Aug", "Sep", "Oct", "Nov", "Dec")
monthlySummaryOpt1[1,2:6] = c(mean(Option1_Collisions_iter$Jan, na.rm=TRUE), sd(Option1_Collisions_iter$Jan, na.rm=TRUE), CV(mean(Option1_Collisions_iter$Jan, na.rm=TRUE),sd(Option1_Collisions_iter$Jan, na.rm=TRUE)), median(Option1_Collisions_iter$Jan, na.rm=TRUE), IQR(Option1_Collisions_iter$Jan, na.rm=TRUE))
monthlySummaryOpt1[2,2:6] = c(mean(Option1_Collisions_iter$Feb, na.rm=TRUE), sd(Option1_Collisions_iter$Feb, na.rm=TRUE), CV(mean(Option1_Collisions_iter$Feb, na.rm=TRUE),sd(Option1_Collisions_iter$Feb, na.rm=TRUE)), median(Option1_Collisions_iter$Feb, na.rm=TRUE), IQR(Option1_Collisions_iter$Feb, na.rm=TRUE))
monthlySummaryOpt1[3,2:6] = c(mean(Option1_Collisions_iter$Mar, na.rm=TRUE), sd(Option1_Collisions_iter$Mar, na.rm=TRUE), CV(mean(Option1_Collisions_iter$Mar, na.rm=TRUE),sd(Option1_Collisions_iter$Mar, na.rm=TRUE)), median(Option1_Collisions_iter$Mar, na.rm=TRUE), IQR(Option1_Collisions_iter$Mar, na.rm=TRUE))
monthlySummaryOpt1[4,2:6] = c(mean(Option1_Collisions_iter$Apr, na.rm=TRUE), sd(Option1_Collisions_iter$Apr, na.rm=TRUE), CV(mean(Option1_Collisions_iter$Apr, na.rm=TRUE),sd(Option1_Collisions_iter$Apr, na.rm=TRUE)), median(Option1_Collisions_iter$Apr, na.rm=TRUE), IQR(Option1_Collisions_iter$Apr, na.rm=TRUE))
monthlySummaryOpt1[5,2:6] = c(mean(Option1_Collisions_iter$May, na.rm=TRUE), sd(Option1_Collisions_iter$May, na.rm=TRUE), CV(mean(Option1_Collisions_iter$May, na.rm=TRUE),sd(Option1_Collisions_iter$May, na.rm=TRUE)), median(Option1_Collisions_iter$May, na.rm=TRUE), IQR(Option1_Collisions_iter$May, na.rm=TRUE))
monthlySummaryOpt1[6,2:6] = c(mean(Option1_Collisions_iter$Jun, na.rm=TRUE), sd(Option1_Collisions_iter$Jun, na.rm=TRUE), CV(mean(Option1_Collisions_iter$Jun, na.rm=TRUE),sd(Option1_Collisions_iter$Jun, na.rm=TRUE)), median(Option1_Collisions_iter$Jun, na.rm=TRUE), IQR(Option1_Collisions_iter$Jun, na.rm=TRUE))
monthlySummaryOpt1[7,2:6] = c(mean(Option1_Collisions_iter$Jul, na.rm=TRUE), sd(Option1_Collisions_iter$Jul, na.rm=TRUE), CV(mean(Option1_Collisions_iter$Jul, na.rm=TRUE),sd(Option1_Collisions_iter$Jul, na.rm=TRUE)), median(Option1_Collisions_iter$Jul, na.rm=TRUE), IQR(Option1_Collisions_iter$Jul, na.rm=TRUE))
monthlySummaryOpt1[8,2:6] = c(mean(Option1_Collisions_iter$Aug, na.rm=TRUE), sd(Option1_Collisions_iter$Aug, na.rm=TRUE), CV(mean(Option1_Collisions_iter$Aug, na.rm=TRUE),sd(Option1_Collisions_iter$Aug, na.rm=TRUE)), median(Option1_Collisions_iter$Aug, na.rm=TRUE), IQR(Option1_Collisions_iter$Aug, na.rm=TRUE))
monthlySummaryOpt1[9,2:6] = c(mean(Option1_Collisions_iter$Sep, na.rm=TRUE), sd(Option1_Collisions_iter$Sep, na.rm=TRUE), CV(mean(Option1_Collisions_iter$Sep, na.rm=TRUE),sd(Option1_Collisions_iter$Sep, na.rm=TRUE)), median(Option1_Collisions_iter$Sep, na.rm=TRUE), IQR(Option1_Collisions_iter$Sep, na.rm=TRUE))
monthlySummaryOpt1[10,2:6] = c(mean(Option1_Collisions_iter$Oct, na.rm=TRUE), sd(Option1_Collisions_iter$Oct, na.rm=TRUE), CV(mean(Option1_Collisions_iter$Oct, na.rm=TRUE),sd(Option1_Collisions_iter$Oct, na.rm=TRUE)), median(Option1_Collisions_iter$Oct, na.rm=TRUE), IQR(Option1_Collisions_iter$Oct, na.rm=TRUE))
monthlySummaryOpt1[11,2:6] = c(mean(Option1_Collisions_iter$Nov, na.rm=TRUE), sd(Option1_Collisions_iter$Nov, na.rm=TRUE), CV(mean(Option1_Collisions_iter$Nov, na.rm=TRUE),sd(Option1_Collisions_iter$Nov, na.rm=TRUE)), median(Option1_Collisions_iter$Nov, na.rm=TRUE), IQR(Option1_Collisions_iter$Nov, na.rm=TRUE))
monthlySummaryOpt1[12,2:6] = c(mean(Option1_Collisions_iter$Dec, na.rm=TRUE), sd(Option1_Collisions_iter$Dec, na.rm=TRUE), CV(mean(Option1_Collisions_iter$Dec, na.rm=TRUE),sd(Option1_Collisions_iter$Dec, na.rm=TRUE)), median(Option1_Collisions_iter$Dec, na.rm=TRUE), IQR(Option1_Collisions_iter$Dec, na.rm=TRUE))

#== Table 3 generation
monthlySummaryOpt3 = data.frame(matrix(data = 0, ncol = 6, nrow = 12))
names(monthlySummaryOpt3) = c("Month", "Mean", "SD", "CV", "Median", "IQR")
monthlySummaryOpt3[,1] = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",  "Aug", "Sep", "Oct", "Nov", "Dec")
monthlySummaryOpt3[1,2:6] = c(mean(Option3_Collisions_iter$Jan, na.rm=TRUE), sd(Option3_Collisions_iter$Jan, na.rm=TRUE), CV(mean(Option3_Collisions_iter$Jan, na.rm=TRUE),sd(Option3_Collisions_iter$Jan, na.rm=TRUE)), median(Option3_Collisions_iter$Jan, na.rm=TRUE), IQR(Option3_Collisions_iter$Jan, na.rm=TRUE))
monthlySummaryOpt3[2,2:6] = c(mean(Option3_Collisions_iter$Feb, na.rm=TRUE), sd(Option3_Collisions_iter$Feb, na.rm=TRUE), CV(mean(Option3_Collisions_iter$Feb, na.rm=TRUE),sd(Option3_Collisions_iter$Feb, na.rm=TRUE)), median(Option3_Collisions_iter$Feb, na.rm=TRUE), IQR(Option3_Collisions_iter$Feb, na.rm=TRUE))
monthlySummaryOpt3[3,2:6] = c(mean(Option3_Collisions_iter$Mar, na.rm=TRUE), sd(Option3_Collisions_iter$Mar, na.rm=TRUE), CV(mean(Option3_Collisions_iter$Mar, na.rm=TRUE),sd(Option3_Collisions_iter$Mar, na.rm=TRUE)), median(Option3_Collisions_iter$Mar, na.rm=TRUE), IQR(Option3_Collisions_iter$Mar, na.rm=TRUE))
monthlySummaryOpt3[4,2:6] = c(mean(Option3_Collisions_iter$Apr, na.rm=TRUE), sd(Option3_Collisions_iter$Apr, na.rm=TRUE), CV(mean(Option3_Collisions_iter$Apr, na.rm=TRUE),sd(Option3_Collisions_iter$Apr, na.rm=TRUE)), median(Option3_Collisions_iter$Apr, na.rm=TRUE), IQR(Option3_Collisions_iter$Apr, na.rm=TRUE))
monthlySummaryOpt3[5,2:6] = c(mean(Option3_Collisions_iter$May, na.rm=TRUE), sd(Option3_Collisions_iter$May, na.rm=TRUE), CV(mean(Option3_Collisions_iter$May, na.rm=TRUE),sd(Option3_Collisions_iter$May, na.rm=TRUE)), median(Option3_Collisions_iter$May, na.rm=TRUE), IQR(Option3_Collisions_iter$May, na.rm=TRUE))
monthlySummaryOpt3[6,2:6] = c(mean(Option3_Collisions_iter$Jun, na.rm=TRUE), sd(Option3_Collisions_iter$Jun, na.rm=TRUE), CV(mean(Option3_Collisions_iter$Jun, na.rm=TRUE),sd(Option3_Collisions_iter$Jun, na.rm=TRUE)), median(Option3_Collisions_iter$Jun, na.rm=TRUE), IQR(Option3_Collisions_iter$Jun, na.rm=TRUE))
monthlySummaryOpt3[7,2:6] = c(mean(Option3_Collisions_iter$Jul, na.rm=TRUE), sd(Option3_Collisions_iter$Jul, na.rm=TRUE), CV(mean(Option3_Collisions_iter$Jul, na.rm=TRUE),sd(Option3_Collisions_iter$Jul, na.rm=TRUE)), median(Option3_Collisions_iter$Jul, na.rm=TRUE), IQR(Option3_Collisions_iter$Jul, na.rm=TRUE))
monthlySummaryOpt3[8,2:6] = c(mean(Option3_Collisions_iter$Aug, na.rm=TRUE), sd(Option3_Collisions_iter$Aug, na.rm=TRUE), CV(mean(Option3_Collisions_iter$Aug, na.rm=TRUE),sd(Option3_Collisions_iter$Aug, na.rm=TRUE)), median(Option3_Collisions_iter$Aug, na.rm=TRUE), IQR(Option3_Collisions_iter$Aug, na.rm=TRUE))
monthlySummaryOpt3[9,2:6] = c(mean(Option3_Collisions_iter$Sep, na.rm=TRUE), sd(Option3_Collisions_iter$Sep, na.rm=TRUE), CV(mean(Option3_Collisions_iter$Sep, na.rm=TRUE),sd(Option3_Collisions_iter$Sep, na.rm=TRUE)), median(Option3_Collisions_iter$Sep, na.rm=TRUE), IQR(Option3_Collisions_iter$Sep, na.rm=TRUE))
monthlySummaryOpt3[10,2:6] = c(mean(Option3_Collisions_iter$Oct, na.rm=TRUE), sd(Option3_Collisions_iter$Oct, na.rm=TRUE), CV(mean(Option3_Collisions_iter$Oct, na.rm=TRUE),sd(Option3_Collisions_iter$Oct, na.rm=TRUE)), median(Option3_Collisions_iter$Oct, na.rm=TRUE), IQR(Option3_Collisions_iter$Oct, na.rm=TRUE))
monthlySummaryOpt3[11,2:6] = c(mean(Option3_Collisions_iter$Nov, na.rm=TRUE), sd(Option3_Collisions_iter$Nov, na.rm=TRUE), CV(mean(Option3_Collisions_iter$Nov, na.rm=TRUE),sd(Option3_Collisions_iter$Nov, na.rm=TRUE)), median(Option3_Collisions_iter$Nov, na.rm=TRUE), IQR(Option3_Collisions_iter$Nov, na.rm=TRUE))
monthlySummaryOpt3[12,2:6] = c(mean(Option3_Collisions_iter$Dec, na.rm=TRUE), sd(Option3_Collisions_iter$Dec, na.rm=TRUE), CV(mean(Option3_Collisions_iter$Dec, na.rm=TRUE),sd(Option3_Collisions_iter$Dec, na.rm=TRUE)), median(Option3_Collisions_iter$Dec, na.rm=TRUE), IQR(Option3_Collisions_iter$Dec, na.rm=TRUE))

# save tables  -----------------------------------------------------------
# fileName<-paste(TurbineData$TurbineModel[t],CRSpecies[s],"monthlySummaryOpt1.csv", sep="_")
# write.csv (monthlySummaryOpt1, paste(results_folder,"tables",  fileName, sep="/"))
# fileName<-paste(TurbineData$TurbineModel[t],CRSpecies[s],"monthlySummaryOpt3.csv", sep="_")
# write.csv (monthlySummaryOpt3, paste(results_folder,"tables",  fileName, sep="/"))


# Add data to density tables ----------------------------------------------
densitySummary[,(t-1)*3+1]=rowSums(Option1_Collisions_iter)
densitySummary[,(t-1)*3+3]=rowSums(Option3_Collisions_iter)


# Results summary table ---------------------------------------------------
resultsSummary[(s*nrow(TurbineData)-(nrow(TurbineData)-t))*3-2,] <- c(CRSpecies[s], TurbineData$TurbineModel[t], 1, 
                                                                      mean(rowSums(Option1_Collisions_iter, na.rm=TRUE), na.rm=TRUE), sd(rowSums(Option1_Collisions_iter, na.rm=TRUE), na.rm=TRUE), 
                                                                      CV(mean(rowSums(Option1_Collisions_iter, na.rm=TRUE), na.rm=TRUE), sd(rowSums(Option1_Collisions_iter, na.rm=TRUE), na.rm=TRUE)), 
                                                                      median(rowSums(Option1_Collisions_iter, na.rm=TRUE), na.rm=TRUE), IQR(rowSums(Option1_Collisions_iter, na.rm=TRUE), na.rm=TRUE)) #option 1

resultsSummary[(s*nrow(TurbineData)-(nrow(TurbineData)-t))*3-0,] <- c(CRSpecies[s], TurbineData$TurbineModel[t], 3, 
                                                                      mean(rowSums(Option3_Collisions_iter, na.rm=TRUE), na.rm=TRUE), sd(rowSums(Option3_Collisions_iter, na.rm=TRUE), na.rm=TRUE), 
                                                                      CV(mean(rowSums(Option3_Collisions_iter, na.rm=TRUE), na.rm=TRUE), sd(rowSums(Option3_Collisions_iter, na.rm=TRUE), na.rm=TRUE)), 
                                                                      median(rowSums(Option3_Collisions_iter, na.rm=TRUE), na.rm=TRUE), IQR(rowSums(Option3_Collisions_iter, na.rm=TRUE), na.rm=TRUE)) #option 3


# Summaries of input parameters -------------------------------------------
#== Birds
sampledBirdParamsSummary = data.frame(matrix(data = 0, ncol = 5, nrow = 4))
names(sampledBirdParamsSummary) = c("Parameter","Mean", "SD", "Median", "IQR")
sampledBirdParamsSummary[, 1] = c("Avoidance", "WingSpan", "BodyLength", "FlightSpeed")
sampledBirdParamsSummary[1, 2:5] = c(mean(sampledBirdParams$Avoidance), sd(sampledBirdParams$Avoidance), median(sampledBirdParams$Avoidance), IQR(sampledBirdParams$Avoidance))
sampledBirdParamsSummary[2, 2:5] = c(mean(sampledBirdParams$WingSpan), sd(sampledBirdParams$WingSpan), median(sampledBirdParams$WingSpan), IQR(sampledBirdParams$WingSpan))
sampledBirdParamsSummary[3, 2:5] = c(mean(sampledBirdParams$BodyLength), sd(sampledBirdParams$BodyLength), median(sampledBirdParams$BodyLength), IQR(sampledBirdParams$BodyLength))
sampledBirdParamsSummary[4, 2:5] = c(mean(sampledBirdParams$FlightSpeed), sd(sampledBirdParams$FlightSpeed), median(sampledBirdParams$FlightSpeed), IQR(sampledBirdParams$FlightSpeed))


#== write out bird parameter tables
# fileName<-paste(TurbineData$TurbineModel[t],CRSpecies[s],"sampledBirdParameters.csv", sep="_")
# write.csv (sampledBirdParamsSummary, paste(results_folder,"tables", fileName, sep="/"))

sampledBirdParamsIters = data.frame(matrix(data = 0, ncol = 4, nrow = iter))
names(sampledBirdParamsIters) = c("Avoidance",  "WingSpan", "BodyLength", "FlightSpeed")
sampledBirdParamsIters[, 1] = sampledBirdParams$Avoidance
sampledBirdParamsIters[, 2] = sampledBirdParams$WingSpan
sampledBirdParamsIters[, 3] = sampledBirdParams$BodyLength
sampledBirdParamsIters[, 4] = sampledBirdParams$FlightSpeed

# fileName<-paste(TurbineData$TurbineModel[t],CRSpecies[s],"sampledBirdParametersIters.csv", sep="_")
# write.csv (sampledBirdParamsIters, paste(results_folder,"tables", fileName, sep="/"), row.names=FALSE)


# Turbine parameters ------------------------------------------------------
sampledTurbineParamsSummary = data.frame(matrix(data = 0, ncol = 5, nrow = 23))
names(sampledTurbineParamsSummary) = c("Parameter","Mean", "SD", "Median", "IQR")
# pitch_rad is used instead of pitch for column label
sampledTurbineParamsSummary[, 1] = c("RotorRadius_m", "HubHeight_m", "BladeWidth_m", "WindSpeed_mps", "RotorSpeed_rpm", "Pitch_rad", "WFWidth_km", 
                                     "Latitude", "Longitude", "Prop_Upwind", "Num_Turbines", "JanOp", "FebOp", "MarOp", "AprOp", "MayOp", "JunOp", "JulOp", 
                                     "AugOp", "SepOp", "OctOp", "NovOp", "DecOp")
sampledTurbineParamsSummary[1, 2:5] = c(mean(sampledTurbine$RotorRadius_m), sd(sampledTurbine$RotorRadius_m), median(sampledTurbine$RotorRadius_m), IQR(sampledTurbine$RotorRadius_m))
sampledTurbineParamsSummary[2, 2:5] = c(mean(sampledTurbine$HubHeight_m), sd(sampledTurbine$HubHeight_m), median(sampledTurbine$HubHeight_m), IQR(sampledTurbine$HubHeight_m))
sampledTurbineParamsSummary[3, 2:5] = c(mean(sampledTurbine$BladeWidth_m), sd(sampledTurbine$BladeWidth_m), median(sampledTurbine$BladeWidth_m), IQR(sampledTurbine$BladeWidth_m))
sampledTurbineParamsSummary[4, 2:5] = c(mean(sampledTurbine$WindSpeed_mps), sd(sampledTurbine$WindSpeed_mps), median(sampledTurbine$WindSpeed_mps), IQR(sampledTurbine$WindSpeed_mps))
sampledTurbineParamsSummary[5, 2:5] = c(mean(sampledTurbine$RotorSpeed_rpm), sd(sampledTurbine$RotorSpeed_rpm), median(sampledTurbine$RotorSpeed_rpm), IQR(sampledTurbine$RotorSpeed_rpm))
sampledTurbineParamsSummary[6, 2:5] = c(mean(sampledTurbine$Pitch), sd(sampledTurbine$Pitch), median(sampledTurbine$Pitch), IQR(sampledTurbine$Pitch))
sampledTurbineParamsSummary[7, 2:5] = c(mean(TurbineData$WFWidth_km), sd(TurbineData$WFWidth_km), median(TurbineData$WFWidth_km), IQR(TurbineData$WFWidth_km))
sampledTurbineParamsSummary[8, 2:5] = c(mean(TurbineData$Latitude), sd(TurbineData$Latitude), median(TurbineData$Latitude), IQR(TurbineData$Latitude))
sampledTurbineParamsSummary[9, 2:5] = c(mean(TurbineData$Longitude), sd(TurbineData$Longitude), median(TurbineData$Longitude), IQR(TurbineData$Longitude))
sampledTurbineParamsSummary[10, 2:5] = c(mean(TurbineData$Prop_Upwind), sd(TurbineData$Prop_Upwind), median(TurbineData$Prop_Upwind), IQR(TurbineData$Prop_Upwind))
# sampledTurbineParamsSummary[10, 2:5] = c(mean(TurbineData$TPower), sd(TurbineData$TPower), median(TurbineData$TPower), IQR(TurbineData$TPower))  #removed and switched to Numn_turbines
sampledTurbineParamsSummary[11, 2:5] = c(mean(TurbineData$Num_Turbines), sd(TurbineData$Num_Turbines), median(TurbineData$Num_Turbines), IQR(TurbineData$Num_Turbines))
sampledTurbineParamsSummary[12, 2:5] = c(mean(sampledTurbine$JanOp), sd(sampledTurbine$JanOp), median(sampledTurbine$JanOp), IQR(sampledTurbine$JanOp))
sampledTurbineParamsSummary[13, 2:5] = c(mean(sampledTurbine$FebOp), sd(sampledTurbine$FebOp), median(sampledTurbine$FebOp), IQR(sampledTurbine$FebOp))
sampledTurbineParamsSummary[14, 2:5] = c(mean(sampledTurbine$MarOp), sd(sampledTurbine$MarOp), median(sampledTurbine$MarOp), IQR(sampledTurbine$MarOp))
sampledTurbineParamsSummary[15, 2:5] = c(mean(sampledTurbine$AprOp), sd(sampledTurbine$AprOp), median(sampledTurbine$AprOp), IQR(sampledTurbine$AprOp))
sampledTurbineParamsSummary[16, 2:5] = c(mean(sampledTurbine$MayOp), sd(sampledTurbine$MayOp), median(sampledTurbine$MayOp), IQR(sampledTurbine$MayOp))
sampledTurbineParamsSummary[17, 2:5] = c(mean(sampledTurbine$JunOp), sd(sampledTurbine$JunOp), median(sampledTurbine$JunOp), IQR(sampledTurbine$JunOp))
sampledTurbineParamsSummary[18, 2:5] = c(mean(sampledTurbine$JulOp), sd(sampledTurbine$JulOp), median(sampledTurbine$JulOp), IQR(sampledTurbine$JulOp))
sampledTurbineParamsSummary[19, 2:5] = c(mean(sampledTurbine$AugOp), sd(sampledTurbine$AugOp), median(sampledTurbine$AugOp), IQR(sampledTurbine$AugOp))
sampledTurbineParamsSummary[20, 2:5] = c(mean(sampledTurbine$SepOp), sd(sampledTurbine$SepOp), median(sampledTurbine$SepOp), IQR(sampledTurbine$SepOp))
sampledTurbineParamsSummary[21, 2:5] = c(mean(sampledTurbine$OctOp), sd(sampledTurbine$OctOp), median(sampledTurbine$OctOp), IQR(sampledTurbine$OctOp))
sampledTurbineParamsSummary[22, 2:5] = c(mean(sampledTurbine$NovOp), sd(sampledTurbine$NovOp), median(sampledTurbine$NovOp), IQR(sampledTurbine$NovOp))
sampledTurbineParamsSummary[23, 2:5] = c(mean(sampledTurbine$DecOp), sd(sampledTurbine$DecOp), median(sampledTurbine$DecOp), IQR(sampledTurbine$DecOp))

#== write out turbine parameter tables
# fileName<-paste(TurbineData$TurbineModel[t],CRSpecies[s],"sampledTurbineParameters.csv", sep="_")
# write.csv (sampledTurbineParamsSummary, paste(results_folder, "tables", fileName, sep="/"))

sampledTurbineParamsIters = data.frame(matrix(data = 0, ncol = 23, nrow = iter))
names(sampledTurbineParamsIters) = c("RotorRadius_m", "HubHeight_m", "BladeWidth_m", "WindSpeed_mps", "RotorSpeed_rpm", "Pitch_rad", "WFWidth_km", 
                                     "Latitude", "Longitude", "Prop_Upwind", "Num_Turbines", "JanOp", "FebOp", "MarOp", "AprOp", "MayOp", "JunOp", "JulOp", 
                                     "AugOp", "SepOp", "OctOp", "NovOp", "DecOp")
sampledTurbineParamsIters[, 1] = sampledTurbine$RotorRadius_m
sampledTurbineParamsIters[, 2] = sampledTurbine$HubHeight_m
sampledTurbineParamsIters[, 3] = sampledTurbine$BladeWidth_m
sampledTurbineParamsIters[, 4] = sampledTurbine$WindSpeed_mps
sampledTurbineParamsIters[, 5] = sampledTurbine$RotorSpeed_rpm
sampledTurbineParamsIters[, 6] = sampledTurbine$Pitch
sampledTurbineParamsIters[, 7] = TurbineData$WFWidth_km
sampledTurbineParamsIters[, 8] = TurbineData$Latitude
sampledTurbineParamsIters[, 9] = TurbineData$Longitude
sampledTurbineParamsIters[, 10] = TurbineData$Prop_Upwind
# sampledTurbineParamsIters[, 10] = TurbineData$TPower
sampledTurbineParamsIters[, 11] = TurbineData$Num_Turbines
sampledTurbineParamsIters[, 12] = sampledTurbine$JanOp
sampledTurbineParamsIters[, 13] = sampledTurbine$FebOp
sampledTurbineParamsIters[, 14] = sampledTurbine$MarOp
sampledTurbineParamsIters[, 15] = sampledTurbine$AprOp
sampledTurbineParamsIters[, 16] = sampledTurbine$MayOp
sampledTurbineParamsIters[, 17] = sampledTurbine$JunOp
sampledTurbineParamsIters[, 18] = sampledTurbine$JulOp
sampledTurbineParamsIters[, 19] = sampledTurbine$AugOp
sampledTurbineParamsIters[, 20] = sampledTurbine$SepOp
sampledTurbineParamsIters[, 21] = sampledTurbine$OctOp
sampledTurbineParamsIters[, 22] = sampledTurbine$NovOp
sampledTurbineParamsIters[, 23] = sampledTurbine$DecOp

#== write out turbine parameter tables
# fileName<-paste(TurbineData$TurbineModel[t],CRSpecies[s],"sampledTurbineParametersIters.csv", sep="_")
# write.csv (sampledTurbineParamsIters, paste(results_folder, "tables", fileName, sep="/"), row.names=FALSE)



  