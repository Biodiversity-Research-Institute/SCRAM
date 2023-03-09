### Sampling Functions ###
#### --- Extensive Modifications by BC from Masden and updated by CF ---- #####
#'
#' Including:
#'  - truncated normal for wingspan, bird length & flight speed
#'  - beta for Basic and Extended Avoidance, CHR & Nocturnal avoidance
#'  - 3 options for generating monthly densities: (i) truncated normal; (ii) sample of the distribution ; (iii) distribution percentiles

# Sampling functions for birds -------------------------------------------
#= wingspan
sampleWingSpan <- function(n, meanspan, sdspan){
  #rnorm(n, meanspan, sdspan)
  rtnorm_dmp(n, mu = meanspan, sd = sdspan)
}

#= bird length
sampleBirdLength <- function(n, meanlength, sdlength){
  #rnorm(n, meanlength, sdlength)
  rtnorm_dmp(n, mu = meanlength, sd = sdlength)
}

sampleCRH <- function(n, meanCRH, sdCRH){
  #rnorm(n, meanCRH, sdCRH)
  rbeta_dmp(n, p = meanCRH, sd = sdCRH)
}

# #= Bird heights
#   
#   sampleBirdFlight <- function(n, meanflight, sdflight){
#     
#     rnorm(n, meanflight, sdflight)
#     
#   }

#= Flight Speed                                                         
sampleFlightSpeed <- function(n, meanflspeed, sdflspeed){
  rtnorm_dmp(n, mu = meanflspeed, sd = sdflspeed)  
}

#= Nocturnal
sampleNocturnal <- function(n, meannoc, sdnoc){
  #rnorm(n, meannoc, sdnoc)
  rbeta_dmp(n, p = meannoc, sd = sdnoc)
}

#= Avoidance (basic and extended)
sampleAvoidance <- function(n, meanavoid, sdavoid){
  #rnorm(n, meanavoid, sdavoid)
  rbeta_dmp(n, p = meanavoid, sd = sdavoid)                       
}

#= Prop_Upwind
samplePropUpwind <- function(n, meanupwind, sdupwind){
  rbeta_dmp(n, p = meanupwind, sd = sdupwind)                       
}

#= Bird counts                          
# sampleCount <- function(n, meancount, sdcount){
#   
#   if(sdcount > 0 & meancount > 0){
#     return(rtnorm(n, meancount, sdcount, 0))
#   }else{
#     if(sdcount==0 & meancount >=0){
#       return(rep(meancount, n))
#     }
#     if(sdcount==0 & meancount < 0){
#       return(rep(0,n))
#     }
#   }
# }

# sample from a truncated normal bounded at 0
sampleCount_tnorm <- function(n, meancount, sdcount){
  rtnorm_dmp(n, mu = meancount, sd = sdcount)
}

# resampling with replacement from a vector of random realizations of counts from an unspecified distribution
sampleCount_resample <- function(n, countsSample){
  #dplyr::sample_n(tbl = countsSample, size = n, replace = TRUE)
  countsSample[sample(1:nrow(countsSample), size = n, replace = TRUE),]
}

# resample from empirical cdf based on set of quantiles
sampleCount_pctiles <- function(n, probs, countsPctls){
  #' based on the Inverse Transform Sampling tecnhique, by sampling random probabilities from an uniform distribution
  #' and interpolate (cubic) the count samples from the percentiles provided by the user (taken as the empirical cdf)
  x_intPoints <- runif(n, min(probs), max(probs))
  #y_intPoints <- interp1(probs, countsPctls, xi = x_intPoints, method = "cubic")
  y_intPoints <- spline(probs, countsPctls, method = "fmm", xout=x_intPoints, ties = mean)$y
  return(y_intPoints)
}


# Sampling functions for turbine pars -------------------------------------
#= Blade width
sampleBladeWidth <- function(n, meanwidth, sdwidth){
  rnorm(n, meanwidth, sdwidth)
}

#= rotor radius
sampleRotorRadius <- function(n, meanrotor, sdrotor){
  rnorm(n, meanrotor, sdrotor)
}

#= Hub height
sampleHubHeightAdd <- function(n, meanadd, sdadd){
  rnorm(n, meanadd, sdadd)
}

#= operation times?
sampleOp <- function(n, meanop, sdop){
  rnorm(n, meanop, sdop)
}


#= Aditional sampling functions for turbine params for integration of stocasticity based on a prob distn alone 
#= rotation speed
sampleRotnSpeed <-function(n, meanRotSpeed, sdRotSpeed){
  rtnorm_dmp(n, mu = meanRotSpeed, sd = sdRotSpeed)
}

#= Blade pitch
samplePitch <-function(n, meanPitch, sdPitch){
  rtnorm_dmp(n, mu = meanPitch, sd = sdPitch)
}


#ATG - added wind speed sampling function similar to above - not accounted for otherwise
sampleWindSpeed <-function(n, meanWindSpeed, sdWindSpeed){
  rtnorm_dmp(n, mu = meanWindSpeed, sd = sdWindSpeed)
}


# Misc --------------------------------------------------------------------
#= basic CV in 100%
CV <- function(mean, sd){
  (sd/mean)*100
}


#### sampling functions for the truncated normal and beta distribution
# generate random samples from a beta distribution, parameterized as mean and sd, and returning NAs if conditions are not met
rbeta_dmp <- function(n, p, sd){
  eta <- p*(1-p)/sd^2 - 1
  alpha <- eta*p
  beta <- eta*(1-p)
  betaMeanVarCond <- sd^2 < p*(1-p)
  if(is.na(p) | is.na(sd)){
    out <- rep(NA, n)
    warning("NA values for p and/or sd - NAs produced")
  }else{
    if(p >= 0 & p <= 1){
      if(sd < 0){
        out <- rep(NA, n)
        warning("stdev < 0 - NAs produced")
      }
      if(sd == 0){
        out <- rep(p, n)
      }
      if(sd > 0){
        if(betaMeanVarCond){
          out <- rbeta(n, shape1 = alpha, shape2 = beta)
        }else{
          out <- rep(NA, n)
          warning("condition var < p*(1 - p) not met - NAs produced")
        }
      }
    }else{
      out <- rep(NA, n)
      warning("p < 0 | p > 1 - NAs produced")
    }
  }
  return(out)
}


rtnorm_dmp <- function(number, mu, sd, truc_side = 'positive'){
  temp_u <- runif(number, min = 0, max = 1)
  if(truc_side == 'positive'){
    tr_norm_value <- temp_u - temp_u*pnorm(0, mean = mu, sd = sd)+pnorm(0, mean = mu, sd = sd)}
  if(truc_side == 'negative'){
    tr_norm_value <- temp_u*pnorm(0,mean = mu, sd = sd)}
  qnorm(tr_norm_value, mu, sd)
}


  