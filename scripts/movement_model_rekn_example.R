##################################################################################################################################
### Script to 1) prepare Motus data files for analyses, 2) run movement model in JAGS, 
### 3) use posterior estimates to obtain occupancy estimates for user-specified spatial units, and 
### 4) create simple plots showing mean and variance of results over space. 

### Depending on which components are run, requires: 'false_pos_filter.R', 'remove_false_pos.R', 
### 'remove_dup_bursts.R', 'serial_time.R', 'MOTUS_JAGS.R', 'Pr_occupancy_season.R'
##################################################################################################################################

###Authors: Evan Adams, Chris Field
###Date: 11/17/2023

## load required packages
library(R2jags)
library(R2WinBUGS)
library(tidyverse)
library(sf)
library(raster)
library(rgeos)

## read and combine Motus data for Red Knot, Roseate Tern, Common Tern, Piping Plover for movement analyses ######################
# rekn
# proj_14_2016_rekn_final <- readRDS("data/proj_14_2016_rekn_final.rds") #I commented some data sets for testing purposes
# proj_15_2016_rekn_final <- readRDS("data/proj_15_2016_rekn_final.rds")
proj_38_2016_rekn_final <- readRDS("data/proj_38_2016_rekn_final.rds")
# proj_88_2016_rekn_final <- readRDS("data/proj_88_2016_rekn_final.rds")
allindvs <- rbind(proj_38_2016_rekn_final)
# allindvs <- rbind(proj_14_2016_rekn_final, proj_15_2016_rekn_final, proj_38_2016_rekn_final, proj_88_2016_rekn_final)
# remove the one detection from 2017 so that the filters do not think the earliest detection was in January
allindvs <- allindvs[-which(as.numeric(substr(allindvs$ts_gmt, 1, 4))==2017), ]
allindvs <- allindvs[!is.na(allindvs$lat)&!is.na(allindvs$lon), ]

# rote
# proj_14_2015_rote_final <- read.csv("data/Appendix_H-Motus_Detection_Data_ROST_2015_v20190708.csv", header=TRUE, stringsAsFactors = FALSE)
# proj_14_2016_rote_final <- read.csv("data/Appendix_H-Motus_Detection_Data_ROST_2016_v20190708.csv", header=TRUE, stringsAsFactors = FALSE)
# proj_14_2017_rote_final <- read.csv("data/Appendix_H-Motus_Detection_Data_ROST_2017_v20190708.csv", header=TRUE, stringsAsFactors = FALSE)
# allindvs <- rbind(proj_14_2015_rote_final, proj_14_2016_rote_final, proj_14_2017_rote_final)
# allindvs <- allindvs[!is.na(allindvs$lat)&!is.na(allindvs$lon), ]

# pipl
# proj_14_2015_pipl_final <- read.csv("data/Appendix_H-Motus_Detection_Data_PIPL_2015.csv", header=TRUE, stringsAsFactors = FALSE)
# proj_14_2016_pipl_final <- read.csv("data/Appendix_H-Motus_Detection_Data_PIPL_2016.csv", header=TRUE, stringsAsFactors = FALSE)
# proj_14_2017_pipl_final <- read.csv("data/Appendix_H-Motus_Detection_Data_PIPL_2017.csv", header=TRUE, stringsAsFactors = FALSE)
# allindvs <- rbind(proj_14_2015_pipl_final, proj_14_2016_pipl_final, proj_14_2017_pipl_final)
# allindvs <- allindvs[!is.na(allindvs$lat)&!is.na(allindvs$lon), ]
# 
# # cote
# proj_14_2015_cote_final <- read.csv("data/Appendix_H-Motus_Detection_Data_COTE_2015_v20190708.csv", header=TRUE, stringsAsFactors = FALSE)
# proj_14_2016_cote_final <- read.csv("data/Appendix_H-Motus_Detection_Data_COTE_2016_v20190708.csv", header=TRUE, stringsAsFactors = FALSE)
# proj_14_2017_cote_final <- read.csv("data/Appendix_H-Motus_Detection_Data_COTE_2017_v20190708.csv", header=TRUE, stringsAsFactors = FALSE)
# allindvs <- rbind(proj_14_2015_cote_final, proj_14_2016_cote_final, proj_14_2017_cote_final)
# allindvs <- allindvs[!is.na(allindvs$lat)&!is.na(allindvs$lon), ]


## create columns for time variables for movement model ##########################################################################
# create a new column for hour of detection with minutes represented by a decimal
hours_dec <- round(as.numeric(substr(allindvs$ts_gmt, 12, 13)) + 
                     (as.numeric(substr(allindvs$ts_gmt, 15, 16))/60), 2)
allindvs <- cbind(allindvs, hours_dec)

# create a new column for month
month <- as.numeric(substr(allindvs$ts_gmt, 6, 7))
allindvs <- cbind(allindvs, month)

# create a new column for day
day <- as.numeric(substr(allindvs$ts_gmt, 9, 10))
allindvs <- cbind(allindvs, day)

# create a new column for year
year <- as.numeric(substr(allindvs$ts_gmt, 1, 4))
allindvs<- cbind(allindvs, year)

# order the detections by indiviudal, year, day, hour, and minute
allindvs_ordered <- allindvs[order(allindvs$id, allindvs$year, substr(allindvs$ts_gmt, 6, 11)), ]


## pass data through filters for false positives, duplicate bursts, and stationary individuals ###################################
# using filtering scripts that run on flat data frames instead until Motus releases pre-filtered downloadable data

source('false_pos_filter.R')
burst_length <- false_pos_filter(allindvs_ordered)
allindvs_ordered <- cbind(allindvs_ordered, burst_length)

# remove false positives 
source('remove_false_pos.R') 
allindvs_ordered_filtered <- remove_false_pos(allindvs_ordered, 3)

# remove duplicate detections by defining bursts as all detections within a 24-hour period
source('remove_dup_bursts.R')
allindvs_ordered_filtered_nodups <- remove_dup_bursts(allindvs_ordered_filtered, "day")

## create variables for indexing the JAGS movement model (variable terminology as in Baldwin et al. 2018) #############################################
# create a vector (Sind_obs) that references, for each individual, the position of the vector (for the observed data) that denotes its first detection
id_index <- mat.or.vec(length(allindvs_ordered_filtered_nodups[,1]), 1)
allindvs_ordered_filtered_nodups <- cbind(allindvs_ordered_filtered_nodups, id_index)
  # create an index for individual
uni_inds <- unique(allindvs_ordered_filtered_nodups$id)
Sind_obs <- unique(allindvs_ordered_filtered_nodups$id)
for(i in 1:length(uni_inds)){
  allindvs_ordered_filtered_nodups[allindvs_ordered_filtered_nodups$id==uni_inds[i], 'id_index'] <- i
  Sind_obs[i] <- min(which(allindvs_ordered_filtered_nodups$id==uni_inds[i]))
}

source('serial_time.R')
allindvs_ordered_filtered_nodups <- serial_time(allindvs_ordered_filtered_nodups, 'day')
saveRDS(allindvs_ordered_filtered_nodups, 'allindvs_ordered_filtered_nodups_rekn.rds')
season_length <- max(allindvs_ordered_filtered_nodups$days_since)

# number of individuals
N <- length(uni_inds)

# create a vector (Xidx) that references, for each individual, the position of the vector (for the regular time steps) that denotes its first detection
Xidx <- mat.or.vec(N, 1)
# Xidx2 indexes the last day of the season, with respect to x, for each individual
Xidx2 <- mat.or.vec(N, 1)
# Xidx3 is used for posterior checks of movement model 
Xidx3 <- mat.or.vec(N, 1)
Zidx <- mat.or.vec(N, 1)
for(i in 1:N){
  Xidx[i] <- min(allindvs_ordered_filtered_nodups[allindvs_ordered_filtered_nodups$id==uni_inds[i], 'days_since']) + season_length*(i-1)
  Xidx2[i] <- season_length*i
  Xidx3[i] <- max(allindvs_ordered_filtered_nodups[allindvs_ordered_filtered_nodups$id==uni_inds[i], 'days_since'])
  Zidx[i] <- min(allindvs_ordered_filtered_nodups[allindvs_ordered_filtered_nodups$id==uni_inds[i], 'days_since'])
}

# create the idx file, which indexes for each observation, which "regular" day it is related to, 
# taking into account the fact that the data for all individuals is specified as one vector
a <- 1
for(i in 1:N){
  # season length * number of individuals already recorded is added to the days since the start of the season
  a <- c(a, (allindvs_ordered_filtered_nodups[allindvs_ordered_filtered_nodups$id==uni_inds[i], 'days_since']+((season_length)*(i-1))))
}
idx <- a[-1]

# locations
y <- allindvs_ordered_filtered_nodups[, c("lat", "lon")]
# there is an additional position added to the end of the vector to ensure that the index loops stay in bounds (as in Baldwin et al. 2018)
Xidx <- c(Xidx, (season_length*N+1))
Xidx2 <- c(Xidx2, (season_length*N+1))
# there is an additional position added to the end of the vector to ensure that the index loops stay in bounds (as in Baldwin et al. 2018)
Yidx <- c(Sind_obs, (length(y[,1])+1))[1:(N+1)]
Y <- season_length

# priors for x and b, specifying NAs for nodes that are not observed
x1 <- rep(NA, season_length*N)
x2 <- rep(NA, season_length*N)
x <- cbind(x1, x2)
b <- rep(NA, season_length*N)
for(k in 1:N){
  x[(Xidx[k]+1):Xidx2[k], 1] <- mean(y[(Yidx[k]+1):(Yidx[k+1]-1),1])
  x[(Xidx[k]+1):Xidx2[k], 2] <- mean(y[(Yidx[k]+1):(Yidx[k+1]-1),2])
  b[(Xidx[k]+1):(Xidx2[k]-1)] <- 1
}

lat_min <- min(y[,1]) - sd(y[,1])/3
lat_max <- max(y[,1]) + sd(y[,1])/3
lon_min <- min(y[,2]) - sd(y[,2])/3
lon_max <- max(y[,2]) + sd(y[,2])/3

# number of iterations for the JAGS model
n.iter <- 20000
n.burnin <- 10000
nc <- 1
n.thin <- 5

save.image('rekn_2023.RData')

## run JAGS model ################################################################################################################
# setwd("~/")
# 2 state

##################################################################################################################################
### JAGS model and associated posterior checks with plots
##################################################################################################################################

# general two-state model structure modified from Baldwin et al. 2018
MOVE <- function(){
  #observation priors
  sd ~ dunif(0, 0.001)
  tau <- 1/(sd*sd)
  
  #state transition prior
  phi ~ dunif(0, 1)
  
  # variance-covariance matrix
  sigma_c[1, 1, 1] ~ dexp(1) # it cannot be negative
  sigma_c[1, 2, 1] <- 0
  sigma_c[2, 1, 1] <- 0
  sigma_c[2, 2, 1] ~ dexp(1)
  sigma_c[1, 1, 2] ~ dexp(1)
  sigma_c[1, 2, 2] <- 0
  sigma_c[2, 1, 2] <- 0
  sigma_c[2, 2, 2] ~ dexp(1)
  
  Rho[1, 1, 1] <- 1.0
  Rho[1, 2, 1] ~ dunif(-0.8, 0.8)
  Rho[2, 1, 1] <- Rho[1, 2, 1]
  Rho[2, 2, 1] <- 1.0
  Rho[1, 1, 2] <- 1.0
  Rho[1, 2, 2] ~ dunif(-0.8, 0.8)
  Rho[2, 1, 2] <- Rho[1, 2, 2]
  Rho[2, 2, 2] <- 1.0
  
  Sigma[1:2, 1:2, 1] <- sigma_c[,,1] %*% Rho[,,1] %*% sigma_c[,,1]
  Sigma[1:2, 1:2, 2] <- sigma_c[,,2] %*% Rho[,,2] %*% sigma_c[,,2]
  
  # priors on strength of autoregressive component for two states
  gamma[1] ~ dunif(0, 1)
  gamma[2] ~ dunif(0, 1)
  
  # priors for drift component (one for x and one for y dim.); units in dec. deg.
  D[1, 1] ~ dnorm(0, 0.1)
  D[1, 2] ~ dnorm(0, 0.1)
  D[2, 1] ~ dnorm(0, 0.1)
  D[2, 2] ~ dnorm(0, 0.1)
  
  # N is the individual index
  for(k in 1:N){
    first.loc[k, 1] <- y[Yidx[k], 1]
    first.loc[k, 2] <- y[Yidx[k], 2]
    # mute measurement error components
    #logpsi[k] ~ dunif(-10, 10)
    #psi[k] <- exp(logpsi[k])
    b2[Xidx[k]] <- 1
    b[Xidx[k]] <- 2
    for(j in 1:2){
      # mute the measurement error component
      #x[Xidx[k], j] ~ dt(first.loc[k, j], itau2[Xidx[k], j]*psi[k], nu[Xidx[k], j])
      x[Xidx[k], j] <- first.loc[k, j]
    }
    # get x for the second recorded location, which does not use autoregressive component
    x[(Xidx[k]+1), 1:2] ~ dmnorm.vcov(x[Xidx[k],], Sigma[, , b[Xidx[k]]])
    
    # mute loglikelihood monitor for first two locations
    #log_lik[(Xidx[k])] <- logdensity.mnorm(x[(Xidx[k]), 1:2], x[(Xidx[k]), ], iSigma[, ]) 
    #log_lik[(Xidx[k]+1)] <- logdensity.mnorm(x[(Xidx[k]+1), 1:2], x[(Xidx[k]+1), ], iSigma[, ])
    # for regular time steps, t
    for(t in (Xidx[k]+1):(Xidx2[k]-1)){ 
      phi2[t] <- max(phi*b2[t-1], 0.001)
      b2[t] ~ dbern(phi2[t])
      b[t] <- b2[t] + 1
      # displacement at time t + 1, from t, includes an autoregressive component, drift, and a max hop distance
      # for x and y dims. 
      # combine x and y dims. and add displacement to the previous time step, t, to feed into dmnorm()
      displace[t, 1] <- max(-10, min((x[t, 1] - x[t-1, 1])*gamma[b[t]] + D[1, b[t]], 10))
      displace[t, 2] <- max(-10, min((x[t, 2] - x[t-1, 2])*gamma[b[t]] + D[2, b[t]], 10))
      # get x for t + 1
      x.mn[t, 1:2] <- x[t, 1:2] + displace[t, 1:2]
      x[t+1, 1:2] ~ dmnorm.vcov(x.mn[t, ], Sigma[, , b[t]])
    }
    
    for(i in (Yidx[k]+1):(Yidx[k+1]-1)){ 
      for(j in 1:2){
        # mute the component that interpolates multiple detection within a single time step
        #yhat[i, j] <- w[i]*x[idx[i], j] + (1 - w[i])*x[idx[i+1], j] 
        yhat[i, j] <- x[idx[i], j]
        # mute the measurement error component
        #y[i, j] ~ dt(yhat[i, j], itau2[i, j]*psi[k], nu[i, j])
        y[i, j] ~ dnorm(yhat[i, j], tau)
      }
    }
  }
}

if (is.R()){
  filename <- file.path(tempdir(), "MOVE.bug")}
write.model(MOVE, filename)
inits <- list(list(gamma=c(rbeta(1, 2, 1.5), NA), dev=0.5, x=x, D=matrix(c(0, 0, 0, 0), 2, 2), sigma_c=array(c(0.1, NA, NA, 0.1, 0.1, NA, NA, 0.1), c(2, 2, 2)), phi=0.1, sd=0.001, #gamma=c(0.5, 0.5), ##sd=0.000001 #remove sd prior when constant
                   Rho=array(c(NA, NA, 0.01, NA, NA, NA, 0.01, NA), c(2, 2, 2))))
data <- list("Xidx","Xidx2", "Yidx", "y", "idx","N") 
parameters <- c("gamma", "Sigma", "D", "b", "x", "phi")
MOVE <- jags(data=data, inits=inits, parameters.to.save=parameters, filename,
             n.chains=nc, n.burnin=n.burnin, n.iter=n.iter+n.burnin, n.thin=nt, DIC=TRUE)
MOVE.mcmc <- as.mcmc(MOVE)

save(MOVE, file = 'rekn_2state_2023.RData')


######CONVERTING THE RESULTS INTO OCCUPANCY PROBABILITY#######
#load up the movement model
load('rekn_2state.RData')

# create a vector of the number of individuals tagged by each month,
# to ensure number of individuals observed is divided by the correct denominator to get occupancy
first_month <- mat.or.vec(12, length(uni_inds))
# calculating the number of individuals detected per month, this will be our denominator for the occupancy claculation

ind_bymonth <- allindvs_ordered_filtered_nodups %>%
  group_by(id, month) %>%
  summarize(n = n()) %>%
  group_by(month) %>%
  summarize(n = n())
  #pivot_wider(names_from = month, values_from = n)

N_bymonth <- c()

for(i in 1:12){

  if(i %in% ind_bymonth$month){
  N_bymonth[i] <- ind_bymonth$n[which(ind_bymonth$month == i)]
  } else{

  N_bymonth[i] <- 0}
}



## use posterior estimates from JAGS model to obtain estimates for user-defined spatial units ####################################
# convert output from JAGS to create arrays (one for lat; one for lon) that have the posterior samples (rows)
# for each time step of the movement model (columns), and for each individual (depth)
posts_lat <- array(NA, c(n.iter, season_length, N))
posts_lon <- array(NA, c(n.iter, season_length, N))
for(e in 1:N){
  for(i in Zidx[e]:season_length){
    posts_lat[,i,e] <- MOVE$BUGSoutput$sims.array[,,paste("x[", ((e-1)*season_length + i), "," , 1, "]", sep="")]
    posts_lon[,i,e] <- MOVE$BUGSoutput$sims.array[,,paste("x[", ((e-1)*season_length + i), "," , 2, "]", sep="")]
  }
}


#the model predicts location past our last detection, we don't want that, so we'll constrain the predictions to only the range of times we know the animal is present

start_x <- end_x <- c()
for(i in 1:e){

  start_x[i] <- allindvs_ordered_filtered_nodups$days_since[Yidx[i]]
  end_x[i] <- allindvs_ordered_filtered_nodups$days_since[Yidx[i + 1] - 1] - allindvs_ordered_filtered_nodups$days_since[Yidx[i]] + 1

}

#cut the predictions > the final data point

posts_lat_t <- posts_lat
posts_lon_t <- posts_lon

for(i in 1:e){

  for(j in 1:season_length){

    if(j > (start_x[i] + end_x[i])){
      posts_lat_t[ , j, i] <- NA
      posts_lon_t[ , j, i] <- NA}

  }


}

# read a file that has the extent of user-defined spatial units as rows, labeled "xmin", "ymax", "xmax", "ymin"
# can use the field calculator in QGIS to get a data file with the extent of each cell and centroid coordinates
# e.g. x_min($geometry) on polygons and $x on a .shp for centroids
# setwd("~/")
#BOEM <- read.csv('BOEM_XY.csv', header=TRUE)
# BOEM.sf <- st_read('I:/Dropbox/BRI/Ecological Modeling/USFWS Motus CRM/Motus CRM Movement Modeling/data',
#                    'BOEM_halfdeg_grid_latlon_2021')
BOEM.sf <- st_read('P:/SCRAM/BRI/Movement/SCRAM Movement Models/data',
                   'BOEM_halfdeg_grid_latlon_2021')
BOEM <- st_drop_geometry(BOEM.sf)

# label columns
colnames(BOEM)[1] <- "xmin"
colnames(BOEM)[2] <- "ymax"
colnames(BOEM)[3] <- "xmax"
colnames(BOEM)[4] <- "ymin"

# this script makes it possible to loop through every nth value
# useful for running through a spatial input file that has too many rows to run wihtin a reasonable time
nth <- function(x,n){
  x[x%%n==0]
}
x = 1:length(BOEM[,1])
index <- nth(x,1)

##Propagate uncertainty through the posterior estimates of position

# extrapolation includes JAGS model uncertainty and the uncertainty from estimating the proportion of
# individuals who likely crossed into the specified extent; gives estimates by month instead of year
posts_latb <- posts_lat_t[sample(1:20000, 1000), , ]
posts_lonb <- posts_lon_t[sample(1:20000, 1000), , ]

source('Pr_occupancy_season_trunc.R')
occ_post <- array(0, c(1000, 12, length(BOEM[,1])))
for(z in 1:length(index)){
  #for(z in 1:1){
  lat_input_min <- BOEM$ymin[z]
  lat_input_max <- BOEM$ymax[z]
  lon_input_min <- BOEM$xmin[z]
  lon_input_max <- BOEM$xmax[z]
  occ_post[, ,z] <- Pr_occupancy_season(posts_latb, posts_lonb, lat_input_min, lat_input_max, lon_input_min, lon_input_max, side, n.iter, N_bymonth, season_length, "time_step_bymo")
}

saveRDS(occ_post, 'occ_post_rekn.rds') #this file gets converted into what SCRAM needs to assess movement uncertainty

###Visualize the results###
#pull in a basemap
require(rnaturalearth)
require(viridis)

world <- ne_countries(continent = 'North America', returnclass = 'sf')
ocean <- ne_download(scale = 110, type = 'ocean', category = 'physical', returnclass = 'sf')

#add the crs
GCSWGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

#plot the position posteriors
x_post <- data.frame(x = c(posts_lonb[, , ]), y = c(posts_latb[, , ]))
x_post <- remove_missing(x_post)
x_post <- st_as_sf(x_post, coords = c(1, 2), crs = GCSWGS84)

bb <- st_bbox(x_post)

#intersect x_post with the BOEM shapefile to determine what the overall activity per grid cell should be

#look at the median estimates

med_lat <- apply(posts_latb, c(2,3), mean)
med_lon <- apply(posts_lonb, c(2,3), mean)

x_id <- data.frame(ID = rep(1:N, each = season_length), lat = c(med_lat[,]), lon = c(med_lon[,]))
x_id <- remove_missing(x_id)
x_id <- st_as_sf(x_id, coords = c(3, 2), crs = GCSWGS84)
x_id$Time <- 1:nrow(x_id)

plot(x_id['Time'])

bb <- st_bbox(x_id)

ggplot() +
  geom_sf(data = ocean)  +
  geom_sf(data = x_id, aes(col = ID)) + #coord_sf(xlim = bb[c(1,3)], ylim = bb[c(2,4)], expand = c(5,5)) +
  scale_color_viridis_c(option = "magma", begin = 0.1)

#plot occu_post features

occu_med <- apply(occ_post, c(3, 2), function(x) mean(x, na.rm = TRUE))
occu_sd <- apply(occ_post, c(3, 2), function(x) sd(x, na.rm = TRUE))
occu_cv <- occu_sd/occu_med

BOEM.sf$occu_med <- apply(occ_post, c(3, 2), function(x) mean(x, na.rm = TRUE))
BOEM.sf$occu_sd <- apply(occ_post, c(3, 2), function(x) sd(x, na.rm = TRUE))
BOEM.sf$occu_cv <- BOEM.sf$occu_sd/BOEM.sf$occu_med

BOEM.sf <- cbind(BOEM.sf, occu_med, occu_sd, occu_cv)

#example
plot(BOEM.sf['X12'], logz = TRUE)
plot(BOEM.sf['X12.1'])
plot(BOEM.sf['X12.2'], logz = TRUE)