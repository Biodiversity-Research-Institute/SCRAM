# Output tables and box plots ---------------------------------------------
###relabel sampledTurbine by turbine name###
# Modified 22 Feb 2022 by ATG to add turbine units for clarity

assign(paste(TurbineData$TurbineModel[t], "params", sep="_"), sampledTurbine)

###relabel results table
assign(paste(CRSpecies[s],TurbineData$TurbineModel[t],"opt1", "results", sep="_"), tab1)
assign(paste(CRSpecies[s],TurbineData$TurbineModel[t],"opt2", "results", sep="_"), tab2)
assign(paste(CRSpecies[s],TurbineData$TurbineModel[t],"opt3", "results", sep="_"), tab3)

#== Table 1 generation
monthlySummaryOpt1 = data.frame(matrix(data = 0, ncol = 6, nrow = 12))
names(monthlySummaryOpt1) = c("Month", "Mean", "SD", "CV", "Median", "IQR")
monthlySummaryOpt1[,1] = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",  "Aug", "Sep", "Oct", "Nov", "Dec")
monthlySummaryOpt1[1,2:6] = c(mean(tab1$Jan, na.rm=TRUE), sd(tab1$Jan, na.rm=TRUE), CV(mean(tab1$Jan, na.rm=TRUE),sd(tab1$Jan, na.rm=TRUE)), median(tab1$Jan, na.rm=TRUE), IQR(tab1$Jan, na.rm=TRUE))
monthlySummaryOpt1[2,2:6] = c(mean(tab1$Feb, na.rm=TRUE), sd(tab1$Feb, na.rm=TRUE), CV(mean(tab1$Feb, na.rm=TRUE),sd(tab1$Feb, na.rm=TRUE)), median(tab1$Feb, na.rm=TRUE), IQR(tab1$Feb, na.rm=TRUE))
monthlySummaryOpt1[3,2:6] = c(mean(tab1$Mar, na.rm=TRUE), sd(tab1$Mar, na.rm=TRUE), CV(mean(tab1$Mar, na.rm=TRUE),sd(tab1$Mar, na.rm=TRUE)), median(tab1$Mar, na.rm=TRUE), IQR(tab1$Mar, na.rm=TRUE))
monthlySummaryOpt1[4,2:6] = c(mean(tab1$Apr, na.rm=TRUE), sd(tab1$Apr, na.rm=TRUE), CV(mean(tab1$Apr, na.rm=TRUE),sd(tab1$Apr, na.rm=TRUE)), median(tab1$Apr, na.rm=TRUE), IQR(tab1$Apr, na.rm=TRUE))
monthlySummaryOpt1[5,2:6] = c(mean(tab1$May, na.rm=TRUE), sd(tab1$May, na.rm=TRUE), CV(mean(tab1$May, na.rm=TRUE),sd(tab1$May, na.rm=TRUE)), median(tab1$May, na.rm=TRUE), IQR(tab1$May, na.rm=TRUE))
monthlySummaryOpt1[6,2:6] = c(mean(tab1$Jun, na.rm=TRUE), sd(tab1$Jun, na.rm=TRUE), CV(mean(tab1$Jun, na.rm=TRUE),sd(tab1$Jun, na.rm=TRUE)), median(tab1$Jun, na.rm=TRUE), IQR(tab1$Jun, na.rm=TRUE))
monthlySummaryOpt1[7,2:6] = c(mean(tab1$Jul, na.rm=TRUE), sd(tab1$Jul, na.rm=TRUE), CV(mean(tab1$Jul, na.rm=TRUE),sd(tab1$Jul, na.rm=TRUE)), median(tab1$Jul, na.rm=TRUE), IQR(tab1$Jul, na.rm=TRUE))
monthlySummaryOpt1[8,2:6] = c(mean(tab1$Aug, na.rm=TRUE), sd(tab1$Aug, na.rm=TRUE), CV(mean(tab1$Aug, na.rm=TRUE),sd(tab1$Aug, na.rm=TRUE)), median(tab1$Aug, na.rm=TRUE), IQR(tab1$Aug, na.rm=TRUE))
monthlySummaryOpt1[9,2:6] = c(mean(tab1$Sep, na.rm=TRUE), sd(tab1$Sep, na.rm=TRUE), CV(mean(tab1$Sep, na.rm=TRUE),sd(tab1$Sep, na.rm=TRUE)), median(tab1$Sep, na.rm=TRUE), IQR(tab1$Sep, na.rm=TRUE))
monthlySummaryOpt1[10,2:6] = c(mean(tab1$Oct, na.rm=TRUE), sd(tab1$Oct, na.rm=TRUE), CV(mean(tab1$Oct, na.rm=TRUE),sd(tab1$Oct, na.rm=TRUE)), median(tab1$Oct, na.rm=TRUE), IQR(tab1$Oct, na.rm=TRUE))
monthlySummaryOpt1[11,2:6] = c(mean(tab1$Nov, na.rm=TRUE), sd(tab1$Nov, na.rm=TRUE), CV(mean(tab1$Nov, na.rm=TRUE),sd(tab1$Nov, na.rm=TRUE)), median(tab1$Nov, na.rm=TRUE), IQR(tab1$Nov, na.rm=TRUE))
monthlySummaryOpt1[12,2:6] = c(mean(tab1$Dec, na.rm=TRUE), sd(tab1$Dec, na.rm=TRUE), CV(mean(tab1$Dec, na.rm=TRUE),sd(tab1$Dec, na.rm=TRUE)), median(tab1$Dec, na.rm=TRUE), IQR(tab1$Dec, na.rm=TRUE))

#== Table 2 generation
monthlySummaryOpt2 = data.frame(matrix(data = 0, ncol = 6, nrow = 12))
names(monthlySummaryOpt2) = c("Month", "Mean", "SD", "CV", "Median", "IQR")
monthlySummaryOpt2[,1] = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",  "Aug", "Sep", "Oct", "Nov", "Dec")
monthlySummaryOpt2[1,2:6] = c(mean(tab2$Jan, na.rm=TRUE), sd(tab2$Jan, na.rm=TRUE), CV(mean(tab2$Jan, na.rm=TRUE),sd(tab2$Jan, na.rm=TRUE)), median(tab2$Jan, na.rm=TRUE), IQR(tab2$Jan, na.rm=TRUE))
monthlySummaryOpt2[2,2:6] = c(mean(tab2$Feb, na.rm=TRUE), sd(tab2$Feb, na.rm=TRUE), CV(mean(tab2$Feb, na.rm=TRUE),sd(tab2$Feb, na.rm=TRUE)), median(tab2$Feb, na.rm=TRUE), IQR(tab2$Feb, na.rm=TRUE))
monthlySummaryOpt2[3,2:6] = c(mean(tab2$Mar, na.rm=TRUE), sd(tab2$Mar, na.rm=TRUE), CV(mean(tab2$Mar, na.rm=TRUE),sd(tab2$Mar, na.rm=TRUE)), median(tab2$Mar, na.rm=TRUE), IQR(tab2$Mar, na.rm=TRUE))
monthlySummaryOpt2[4,2:6] = c(mean(tab2$Apr, na.rm=TRUE), sd(tab2$Apr, na.rm=TRUE), CV(mean(tab2$Apr, na.rm=TRUE),sd(tab2$Apr, na.rm=TRUE)), median(tab2$Apr, na.rm=TRUE), IQR(tab2$Apr, na.rm=TRUE))
monthlySummaryOpt2[5,2:6] = c(mean(tab2$May, na.rm=TRUE), sd(tab2$May, na.rm=TRUE), CV(mean(tab2$May, na.rm=TRUE),sd(tab2$May, na.rm=TRUE)), median(tab2$May, na.rm=TRUE), IQR(tab2$May, na.rm=TRUE))
monthlySummaryOpt2[6,2:6] = c(mean(tab2$Jun, na.rm=TRUE), sd(tab2$Jun, na.rm=TRUE), CV(mean(tab2$Jun, na.rm=TRUE),sd(tab2$Jun, na.rm=TRUE)), median(tab2$Jun, na.rm=TRUE), IQR(tab2$Jun, na.rm=TRUE))
monthlySummaryOpt2[7,2:6] = c(mean(tab2$Jul, na.rm=TRUE), sd(tab2$Jul, na.rm=TRUE), CV(mean(tab2$Jul, na.rm=TRUE),sd(tab2$Jul, na.rm=TRUE)), median(tab2$Jul, na.rm=TRUE), IQR(tab2$Jul, na.rm=TRUE))
monthlySummaryOpt2[8,2:6] = c(mean(tab2$Aug, na.rm=TRUE), sd(tab2$Aug, na.rm=TRUE), CV(mean(tab2$Aug, na.rm=TRUE),sd(tab2$Aug, na.rm=TRUE)), median(tab2$Aug, na.rm=TRUE), IQR(tab2$Aug, na.rm=TRUE))
monthlySummaryOpt2[9,2:6] = c(mean(tab2$Sep, na.rm=TRUE), sd(tab2$Sep, na.rm=TRUE), CV(mean(tab2$Sep, na.rm=TRUE),sd(tab2$Sep, na.rm=TRUE)), median(tab2$Sep, na.rm=TRUE), IQR(tab2$Sep, na.rm=TRUE))
monthlySummaryOpt2[10,2:6] = c(mean(tab2$Oct, na.rm=TRUE), sd(tab2$Oct, na.rm=TRUE), CV(mean(tab2$Oct, na.rm=TRUE),sd(tab2$Oct, na.rm=TRUE)), median(tab2$Oct, na.rm=TRUE), IQR(tab2$Oct, na.rm=TRUE))
monthlySummaryOpt2[11,2:6] = c(mean(tab2$Nov, na.rm=TRUE), sd(tab2$Nov, na.rm=TRUE), CV(mean(tab2$Nov, na.rm=TRUE),sd(tab2$Nov, na.rm=TRUE)), median(tab2$Nov, na.rm=TRUE), IQR(tab2$Nov, na.rm=TRUE))
monthlySummaryOpt2[12,2:6] = c(mean(tab2$Dec, na.rm=TRUE), sd(tab2$Dec, na.rm=TRUE), CV(mean(tab2$Dec, na.rm=TRUE),sd(tab2$Dec, na.rm=TRUE)), median(tab2$Dec, na.rm=TRUE), IQR(tab2$Dec, na.rm=TRUE))

#== Table 3 generation
monthlySummaryOpt3 = data.frame(matrix(data = 0, ncol = 6, nrow = 12))
names(monthlySummaryOpt3) = c("Month", "Mean", "SD", "CV", "Median", "IQR")
monthlySummaryOpt3[,1] = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",  "Aug", "Sep", "Oct", "Nov", "Dec")
monthlySummaryOpt3[1,2:6] = c(mean(tab3$Jan, na.rm=TRUE), sd(tab3$Jan, na.rm=TRUE), CV(mean(tab3$Jan, na.rm=TRUE),sd(tab3$Jan, na.rm=TRUE)), median(tab3$Jan, na.rm=TRUE), IQR(tab3$Jan, na.rm=TRUE))
monthlySummaryOpt3[2,2:6] = c(mean(tab3$Feb, na.rm=TRUE), sd(tab3$Feb, na.rm=TRUE), CV(mean(tab3$Feb, na.rm=TRUE),sd(tab3$Feb, na.rm=TRUE)), median(tab3$Feb, na.rm=TRUE), IQR(tab3$Feb, na.rm=TRUE))
monthlySummaryOpt3[3,2:6] = c(mean(tab3$Mar, na.rm=TRUE), sd(tab3$Mar, na.rm=TRUE), CV(mean(tab3$Mar, na.rm=TRUE),sd(tab3$Mar, na.rm=TRUE)), median(tab3$Mar, na.rm=TRUE), IQR(tab3$Mar, na.rm=TRUE))
monthlySummaryOpt3[4,2:6] = c(mean(tab3$Apr, na.rm=TRUE), sd(tab3$Apr, na.rm=TRUE), CV(mean(tab3$Apr, na.rm=TRUE),sd(tab3$Apr, na.rm=TRUE)), median(tab3$Apr, na.rm=TRUE), IQR(tab3$Apr, na.rm=TRUE))
monthlySummaryOpt3[5,2:6] = c(mean(tab3$May, na.rm=TRUE), sd(tab3$May, na.rm=TRUE), CV(mean(tab3$May, na.rm=TRUE),sd(tab3$May, na.rm=TRUE)), median(tab3$May, na.rm=TRUE), IQR(tab3$May, na.rm=TRUE))
monthlySummaryOpt3[6,2:6] = c(mean(tab3$Jun, na.rm=TRUE), sd(tab3$Jun, na.rm=TRUE), CV(mean(tab3$Jun, na.rm=TRUE),sd(tab3$Jun, na.rm=TRUE)), median(tab3$Jun, na.rm=TRUE), IQR(tab3$Jun, na.rm=TRUE))
monthlySummaryOpt3[7,2:6] = c(mean(tab3$Jul, na.rm=TRUE), sd(tab3$Jul, na.rm=TRUE), CV(mean(tab3$Jul, na.rm=TRUE),sd(tab3$Jul, na.rm=TRUE)), median(tab3$Jul, na.rm=TRUE), IQR(tab3$Jul, na.rm=TRUE))
monthlySummaryOpt3[8,2:6] = c(mean(tab3$Aug, na.rm=TRUE), sd(tab3$Aug, na.rm=TRUE), CV(mean(tab3$Aug, na.rm=TRUE),sd(tab3$Aug, na.rm=TRUE)), median(tab3$Aug, na.rm=TRUE), IQR(tab3$Aug, na.rm=TRUE))
monthlySummaryOpt3[9,2:6] = c(mean(tab3$Sep, na.rm=TRUE), sd(tab3$Sep, na.rm=TRUE), CV(mean(tab3$Sep, na.rm=TRUE),sd(tab3$Sep, na.rm=TRUE)), median(tab3$Sep, na.rm=TRUE), IQR(tab3$Sep, na.rm=TRUE))
monthlySummaryOpt3[10,2:6] = c(mean(tab3$Oct, na.rm=TRUE), sd(tab3$Oct, na.rm=TRUE), CV(mean(tab3$Oct, na.rm=TRUE),sd(tab3$Oct, na.rm=TRUE)), median(tab3$Oct, na.rm=TRUE), IQR(tab3$Oct, na.rm=TRUE))
monthlySummaryOpt3[11,2:6] = c(mean(tab3$Nov, na.rm=TRUE), sd(tab3$Nov, na.rm=TRUE), CV(mean(tab3$Nov, na.rm=TRUE),sd(tab3$Nov, na.rm=TRUE)), median(tab3$Nov, na.rm=TRUE), IQR(tab3$Nov, na.rm=TRUE))
monthlySummaryOpt3[12,2:6] = c(mean(tab3$Dec, na.rm=TRUE), sd(tab3$Dec, na.rm=TRUE), CV(mean(tab3$Dec, na.rm=TRUE),sd(tab3$Dec, na.rm=TRUE)), median(tab3$Dec, na.rm=TRUE), IQR(tab3$Dec, na.rm=TRUE))

# save tables  -----------------------------------------------------------
# fileName<-paste(TurbineData$TurbineModel_MW[t],CRSpecies[s],"monthlySummaryOpt1.csv", sep="_")
# write.csv (monthlySummaryOpt1, paste(results_folder,"tables",  fileName, sep="/"))
# fileName<-paste(TurbineData$TurbineModel_MW[t],CRSpecies[s],"monthlySummaryOpt2.csv", sep="_")
# write.csv (monthlySummaryOpt2, paste(results_folder,"tables",  fileName, sep="/"))
# fileName<-paste(TurbineData$TurbineModel_MW[t],CRSpecies[s],"monthlySummaryOpt3.csv", sep="_")
# write.csv (monthlySummaryOpt3, paste(results_folder,"tables",  fileName, sep="/"))


# Add data to density tables ----------------------------------------------
densitySummary[,(t-1)*3+1]=rowSums(tab1)
densitySummary[,(t-1)*3+2]=rowSums(tab2)
densitySummary[,(t-1)*3+3]=rowSums(tab3)
densitySummary[,(t-1)*3+4]=rowSums(tab4)
densitySummary[,(t-1)*3+5]=rowSums(tab5)
densitySummary[,(t-1)*3+6]=rowSums(tab6)


# Create and save boxplots ------------------------------------------------
# fileName<-paste(TurbineData$TurbineModel[t],CRSpecies[s], sep="_")
# fileName<-paste(fileName, ".png", sep="")
# png(paste(results_folder, "figures", fileName, sep="/"),width=500,height=900,res=100)
# 
# par(mfrow = c( 6, 1))
# boxplot(tab1, outline=F, ylim=c(0,max(max(tab1), max(tab2), max(tab3), max(tab4), max(tab5), max(tab6))),ylab="Number of collisions", main="Option 1")
# boxplot(tab2, outline=F, ylim=c(0,max(max(tab1), max(tab2), max(tab3), max(tab4), max(tab5), max(tab6))),ylab="Number of collisions", main="Option 2")
# boxplot(tab3, outline=F, ylim=c(0,max(max(tab1), max(tab2), max(tab3), max(tab4), max(tab5), max(tab6))),ylab="Number of collisions", main="Option 3")
# boxplot(tab4, outline=F, ylim=c(0,max(max(tab1), max(tab2), max(tab3), max(tab4), max(tab5), max(tab6))),ylab="Number of collisions", main="Option 4")
# boxplot(tab5, outline=F, ylim=c(0,max(max(tab1), max(tab2), max(tab3), max(tab4), max(tab5), max(tab6))),ylab="Number of collisions", main="Option 5")
# boxplot(tab6, outline=F, ylim=c(0,max(max(tab1), max(tab2), max(tab3), max(tab4), max(tab5), max(tab6))),ylab="Number of collisions", main="Option 6")
# dev.off()


# Make density plots -----------------------------------------------------
# fileName<-paste(TurbineData$TurbineModel[t],CRSpecies[s], "density",sep="_")
# fileName<-paste(fileName, ".png", sep="")
# png(paste(results_folder, "figures", fileName, sep="/"),width=800,height=600,res=100)
# 
# plot(density(rowSums(tab1)), main="", xlab="Number of Collisions", ylab ="Probability Density", col="chocolate",
# xlim=(c(0, max(max(density(rowSums(tab1))$x),max(density(rowSums(tab2))$x),max(density(rowSums(tab3))$x)))),
# ylim=(c(0, max(max(density(rowSums(tab1))$y),max(density(rowSums(tab2))$y),max(density(rowSums(tab3))$y)))))
# 
# lines(density(rowSums(tab2)), col="darkgoldenrod", lty=2)
# lines(density(rowSums(tab3)), col="darkorange4", lty=3)
# legend("topright", c("Option 1", "Option 2", "Option 3"), cex=0.8, lty=1:3, col = c("chocolate", "darkgoldenrod", "darkorange4"))
# dev.off()


# Results summary table ---------------------------------------------------
resultsSummary[(s*nrow(TurbineData)-(nrow(TurbineData)-t))*3-2,] <- c(CRSpecies[s], TurbineData$TurbineModel_MW[t], 1, 
                                                                      mean(rowSums(tab1, na.rm=TRUE), na.rm=TRUE), sd(rowSums(tab1, na.rm=TRUE), na.rm=TRUE), 
                                                                      CV(mean(rowSums(tab1, na.rm=TRUE), na.rm=TRUE), sd(rowSums(tab1, na.rm=TRUE), na.rm=TRUE)), 
                                                                      median(rowSums(tab1, na.rm=TRUE), na.rm=TRUE), IQR(rowSums(tab1, na.rm=TRUE), na.rm=TRUE)) #option 1
resultsSummary[(s*nrow(TurbineData)-(nrow(TurbineData)-t))*3-1,] <- c(CRSpecies[s], TurbineData$TurbineModel_MW[t], 2, 
                                                                      mean(rowSums(tab2, na.rm=TRUE), na.rm=TRUE), sd(rowSums(tab2, na.rm=TRUE), na.rm=TRUE), 
                                                                      CV(mean(rowSums(tab2, na.rm=TRUE), na.rm=TRUE), sd(rowSums(tab2, na.rm=TRUE), na.rm=TRUE)), 
                                                                      median(rowSums(tab2, na.rm=TRUE), na.rm=TRUE), IQR(rowSums(tab2, na.rm=TRUE), na.rm=TRUE)) #option 2
resultsSummary[(s*nrow(TurbineData)-(nrow(TurbineData)-t))*3-0,] <- c(CRSpecies[s], TurbineData$TurbineModel_MW[t], 3, 
                                                                      mean(rowSums(tab3, na.rm=TRUE), na.rm=TRUE), sd(rowSums(tab3, na.rm=TRUE), na.rm=TRUE), 
                                                                      CV(mean(rowSums(tab3, na.rm=TRUE), na.rm=TRUE), sd(rowSums(tab3, na.rm=TRUE), na.rm=TRUE)), 
                                                                      median(rowSums(tab3, na.rm=TRUE), na.rm=TRUE), IQR(rowSums(tab3, na.rm=TRUE), na.rm=TRUE)) #option 3


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
# fileName<-paste(TurbineData$TurbineModel_MW[t],CRSpecies[s],"sampledBirdParameters.csv", sep="_")
# write.csv (sampledBirdParamsSummary, paste(results_folder,"tables", fileName, sep="/"))

sampledBirdParamsIters = data.frame(matrix(data = 0, ncol = 4, nrow = iter))
names(sampledBirdParamsIters) = c("Avoidance",  "WingSpan", "BodyLength", "FlightSpeed")
sampledBirdParamsIters[, 1] = sampledBirdParams$Avoidance
sampledBirdParamsIters[, 2] = sampledBirdParams$WingSpan
sampledBirdParamsIters[, 3] = sampledBirdParams$BodyLength
sampledBirdParamsIters[, 4] = sampledBirdParams$FlightSpeed

# fileName<-paste(TurbineData$TurbineModel_MW[t],CRSpecies[s],"sampledBirdParametersIters.csv", sep="_")
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
# fileName<-paste(TurbineData$TurbineModel_MW[t],CRSpecies[s],"sampledTurbineParameters.csv", sep="_")
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
# fileName<-paste(TurbineData$TurbineModel_MW[t],CRSpecies[s],"sampledTurbineParametersIters.csv", sep="_")
# write.csv (sampledTurbineParamsIters, paste(results_folder, "tables", fileName, sep="/"), row.names=FALSE)


  