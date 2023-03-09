GSA_approx <- function(CRMlist, optionsGSA){
  ar_out_tab_sort <- list(list())
  for(i in 1:length(CRMlist[['CRSpecies']])){
    for(e in 1:length(CRMlist[['Turbines']])){
  # get the number of collisions per year for each iteration of the CRM model output
  # ATG - some cols are NA, so need na.RM for sums
  annual_colls <- rowSums(CRMlist[["monthCollsnReps"]][[CRMlist[['CRSpecies']][i]]][[paste0("turbModel", CRMlist[['Turbines']][1])]], na.rm = T)
  # combine turbine and bird parameters for a global sensitivity analysis
  params_iter <- cbind(CRMlist[["sampledParamsTurbine"]][[CRMlist[['CRSpecies']][i]]][[paste0("turbModel", CRMlist[['Turbines']][1])]],
                       CRMlist[["sampledParamsBird"]][[CRMlist[['CRSpecies']][i]]][[paste0("turbModel", CRMlist[['Turbines']][1])]])
  
  # create a function to standardize dependent and independent variables
  standardize <- function(x){
    var_stand <- (x - mean(x))/(sd(x))
    var_stand[is.na(var_stand)] <- 0
    var_stand
  }
  
  # standardize independent variables
  params_iter_stand <- apply(params_iter, 2, standardize)
  # estimate a linear model using standardized versions of dependent and independent variables
  lm_out <- lm(standardize(annual_colls) ~ params_iter_stand)
  
  # create a function to estimate the "contribution" of each variable to the total R2 (sensu Borcard 2002)
  # warnings from the cor() function are surpressed (some parameters have a sd of 0)
  ar <- function(x){
    lm_out$coefficients[paste('params_iter_stand', names(params_iter_stand[1,])[x], sep="")]*
      suppressWarnings(cor(annual_colls, params_iter_stand[, names(params_iter_stand[1,])[x]]))
  }
  
  # create a vector for x to use in lapply to apply ar() to each independent variable
  x_ar <- 1:length(params_iter_stand[1,])
  ar_out <- lapply(x_ar, ar)
  
  # combine parameter names and results of ar()
  ar_out_tab <- cbind(names(params_iter_stand[1,]), round(as.numeric(ar_out), 3))
  # create column labels
  colnames(ar_out_tab) <- c("Parameter", "Contribution")
  # sort independent variables by their "contribution" from most to least
  ar_out_tab_sort[[CRMlist[['CRSpecies']][i]]][[CRMlist[['Turbines']][e]]] <- ar_out_tab[order(ar_out_tab[,2], na.last=TRUE, decreasing = TRUE),]
  }
  }
  # return the 10 variables with the greatest contributions
  #ar_out_tab_sort[1:10, ]
  ar_out_tab_sort
  # test to make sure the sum of contributions is close to 1
  #sum(as.numeric(ar_out), na.rm=TRUE)
}

