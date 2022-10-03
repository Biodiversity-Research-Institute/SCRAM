#Turbine parameter rough data checks
# Modified 22 Feb 2022 by ATG to add turbine units for clarity

check <- mat.or.vec(length(TurbineData[1,]), max(length(TurbineData[,1]), 2))
for(i in 1:length(TurbineData[,1])){
# if(TurbineData[i, "TurbineModel"] > 0){
#   check[1, i] <- 1
# }
  #change to just a model value or name
  check[1, i] <- 1
if(any(TurbineData[i,"Num_Blades"] > 1)&any(TurbineData[i,"Num_Blades"] < 7)){
  check[2, i] <- 1
}
if(any(TurbineData[i,"RotorRadius_m"] > 1)){
  check[3, i] <- 1
}
if(any(TurbineData[i,"RotorRadiusSD_m"] >= 0)){
  check[4, i] <- 1
}
if(any(TurbineData[i,"HubHeightAdd_m"] > 1)){
  check[5, i] <- 1
}
if(any(TurbineData[i,"HubHeightAddSD_m"] >= 0)){
  check[6, i] <- 1
}
if(any(TurbineData[i,"BladeWidth_m"] > 0)){
  check[7, i] <- 1
}
if(any(TurbineData[i,"BladeWidthSD_m"] >= 0)){
  check[8, i] <- 1
}
if(any(TurbineData[i,"WindSpeed_mps"] > 0)){
  check[9, i] <- 1
}
if(any(TurbineData[i,"WindSpeedSD_mps"] >= 0)){
  check[10, i] <- 1
}
if(any(TurbineData[i,"Pitch"] > 0)){
  check[11, i] <- 1
}
if(any(TurbineData[i,"PitchSD"] >= 0)){
  check[12, i] <- 1
}
if(any(TurbineData[i,"WFWidth_km"] > 1)){
  check[13, i] <- 1
}
if(any(TurbineData[i,"Latitude"] > 23)&any(TurbineData[1,"Latitude"] < 55)){
  check[14, i] <- 1
}
  check[15, ] <- 1
  
if(any(TurbineData[i,"Prop_Upwind"] >= 0)&any(TurbineData[1,"Prop_Upwind"] <= 1)){
  check[16, i] <- 1
}
# if(any(TurbineData[i,"TPower"] > 0)){
#   check[17, i] <- 1
# }
if(any(TurbineData[i,"Num_Turbines"] > 0)){
    check[17, i] <- 1
  }
}
check[(length(TurbineData[1,]) - 38):length(TurbineData[1,]), ] <- 1
check <- rowSums(check)

