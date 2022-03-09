# x_lower and x_upper specify the outer limits of the rotor circle at any point along the y-axis 
# (point along y-axis labeled x in the function)
x_lower <- function(xy){
  -(sqrt(1 - xy^2))
}
x_upper <- function(xy){
  sqrt(1 - xy^2)
} 


# create a function that interpolates width for any given point along the blade using simple LOESS regression
loess_blade <- function(r){
  blade_x <- 1:length(coverC$c)
  blade <- loess(coverC$c~blade_x)
  # get the radius for which a wdith is needed, on the 1:20 scale consistent with Masden et al. 
  # unlike Masden et al., allows for continuous values
  c_in <- (r*100)/5
  # make sure the minimum value is 1 to avoid making predictions outside the given blade widths
  c_in <- max(c_in, 1)
  c <- predict(blade, c_in)
  c
}


# pcoll estimates the probability of collision from the radius and angle (eq. 3 in Band 2012)
pcoll <- function(r, phi, updown,
                  inputRad = currentRad,
                  inputCirc = currentCirc,
                  inputRotorRadius = currentRotorRadius,
                  inputBladeWidth = currentBladeWidth,
                  inputRotorSpeed = currentRotorSpeed,
                  inputPitch = currentPitch,
                  inputFlight = currentFlightNumeric,
                  inputWingSpan = currentWingSpan,
                  inputFlightSpeed = currentFlightSpeed,
                  inputBirdLength = currentBirdLength,
                  inputBlades = currentBlades){
  # Masden's discrete approach to interpolating blade widths
  #cell_val <- ifelse (r > 1, 21, which(coverC$rad == (round(ceiling(r*100)/5)*5)/100))
  #upper <- coverC[cell_val,1]
  #lower <- coverC[cell_val - 1, 1]
  #p <- (r - lower) / (upper-lower)
  #c <- coverC[cell_val-1, 2] + p*(coverC[cell_val,2] - coverC[cell_val-1,2])
  # alternatively, use the loess_blade function to do an interpolation from a continuous function
  # avoids the use of logicals and the which function, which is helpful if this function is to be used in an integration
  c = loess_blade(r)
  
  radius <- inputRotorRadius*r
  chord <- inputBladeWidth*c
  omega <- inputRotorSpeed*2*pi/60
  pitch <- inputPitch*pi/180
  phi <- phi*pi/180
  
  direction <- ifelse(updown == "up", 1, -1)
  
  Wingspan2 <- ifelse(inputFlight == 1, inputWingSpan, inputWingSpan*abs(cos(phi)))
  
  # TurbineData is indexed with t, turbines, which is a loop in BandModel.R
  multiplier <- inputBlades*omega/(2*pi*inputFlightSpeed)
  alpha <- inputFlightSpeed/(radius*omega)
  CollideLength_1 <- abs(direction*chord*sin(pitch) + alpha*chord*cos(pitch))
  CollideLength2 <- ifelse(inputBirdLength > Wingspan2*alpha,
                           inputBirdLength, Wingspan2*alpha)
  # cf added min function
  min(ifelse(radius == 0, 1,  (multiplier*(CollideLength_1 + CollideLength2))), 1)
}


# pcollxy is the same as pcoll, but uses x, y coordinates instead of r and phi (see Fig. 7 in Band 2012)
pcollxy <- function(x, y, updown,
                    inputRad = currentRad,
                    inputCirc = currentCirc,
                    inputRotorRadius = currentRotorRadius,
                    inputBladeWidth = currentBladeWidth,
                    inputRotorSpeed = currentRotorSpeed,
                    inputPitch = currentPitch,
                    inputFlight = currentFlightNumeric,
                    inputWingSpan = currentWingSpan,
                    inputFlightSpeed = currentFlightSpeed,
                    inputBirdLength = currentBirdLength,
                    inputBlades = currentBlades){
  
  r <- (x*x + y*y)^0.5
  phi <- ifelse (y == 0, ifelse(x >= 0, pi/2, -pi/2), atan (x/y))
  phi2 <- ifelse (y < 0, phi + pi, phi)
  phi2 = phi2*180/pi
  pcoll(r, phi2, updown)
}
vpcollxy <- Vectorize(pcollxy)


# create an alternate version of pcollxy that reverses the indexing for y and x to match 'pracma' when doing full integration
pcollxyrev = function(y, x, updown,
                      inputRad = currentRad,
                      inputCirc = currentCirc,
                      inputRotorRadius = currentRotorRadius,
                      inputBladeWidth = currentBladeWidth,
                      inputRotorSpeed = currentRotorSpeed,
                      inputPitch = currentPitch,
                      inputFlight = currentFlightNumeric,
                      inputWingSpan = currentWingSpan,
                      inputFlightSpeed = currentFlightSpeed,
                      inputBirdLength = currentBirdLength,
                      inputBlades = currentBlades){
  
  # specify this in the function for now to simplify integral code
  r <- (y*y + x*x)^0.5
  phi <- ifelse(x == 0, ifelse(y >= 0, pi/2, -pi/2), atan(y/x))
  phi2 <- ifelse (x < 0, phi + pi, phi)
  phi2 <- phi2*180/pi
  pcoll(r, phi2, updown)
}
vpcollxyrev <- Vectorize(pcollxyrev)


# ind.coll estimates collisions by first sampling indiviudal heights from the full FHD, 
# using a cell probability at the end of the probability vector for all areas outside of the rotor's range
ind.coll <- function(x){
  HD.d.samp <- sample((length(HD.d.y) + 1), round(x), replace=T, prob=c(HD.d.y, 1-sum(HD.d.y)))
  # remove all individuals that ended up outside the rotor's range (i.e. in the last cell probability)
  # is there a faster option than the conditional?
  HD.d.samp <- HD.d.samp[HD.d.samp!=(length(HD.d.y) + 1)]
  # get the radius before normalization to the -1 to 1 scale
  radius.raw <-(height[length(height)] - height[1])/2
  # normalize samples from FHD to -1 to 1 scale
  flight_heights <- ((HD.d.samp + height[1]) - height[length(height)])/radius.raw + 1
  # get the x position of each flight; might not be necessary depending on how flux is handled
  flight_x <- runif(length(flight_heights), -1, 1)
  ind.probs <- vpcollxy(flight_x, flight_heights, "up")
  #ind.probs <- vpcollxyrev(flight_heights, flight_x)
  no.colls <- sum(rbinom(length(ind.probs[!is.na(ind.probs)]), 1, ind.probs[!is.na(ind.probs)]))
  no.colls
}


# a faster version of ind.coll that acheives faster run times by reorganizing the order of data manipulation
ind.coll_speed <- function(x, updown){
  HD.d.samp <- sample((length(HD.d.y) + 1), round(x), replace=T, prob=c(HD.d.y, 1-sum(HD.d.y)))
  # remove all individuals that ended up outside the rotor's range (i.e. in the last cell probability)
  # is there a faster option than the conditional?
  HD.d.samp <- HD.d.samp[HD.d.samp!=(length(HD.d.y) + 1)]
  # get the radius before normalization to the -1 to 1 scale
  radius.raw <-(height[length(height)] - height[1])/2
  # normalize samples from FHD to -1 to 1 scale
  flight_heights <- ((HD.d.samp + height[1]) - height[length(height)])/radius.raw + 1
  # get the x position of each flight; might not be necessary depending on how flux is handled
  flight_x <- runif(length(flight_heights), -1, 1)
  # remove all points outside of the circular rotor area (these individuals pass without colliding)
  flight_x2 <- flight_x[flight_x > x_lower(flight_heights) & flight_x < x_upper(flight_heights)]
  flight_heights2 <- flight_heights[flight_x > x_lower(flight_heights) & flight_x < x_upper(flight_heights)]
  ind.probs <- vpcollxy(flight_x2, flight_heights2, updown)
  no.colls <- sum(rbinom(length(ind.probs[!is.na(ind.probs)]), 1, ind.probs[!is.na(ind.probs)]))
  no.colls
}


# ind.coll.q estimates collisions using a single probability for flying at risk height
ind.coll.q <- function(x, updown){
  HD.d.samp <- sum(rbinom(round(x), 1, Q))
  flight_heights <- runif(HD.d.samp, -1, 1)
  # get the x position of each flight; might not be necessary depending on how flux is handled
  flight_x <- runif(length(flight_heights), -1, 1)
  # remove all points outside of the circular rotor area (these individuals pass without colliding)
  flight_x2 <- flight_x[flight_x > x_lower(flight_heights) & flight_x < x_upper(flight_heights)]
  flight_heights2 <- flight_heights[flight_x > x_lower(flight_heights) & flight_x < x_upper(flight_heights)]
  ind.probs <- vpcollxy(flight_x2, flight_heights2, updown)
  no.colls.q <- sum(rbinom(length(ind.probs[!is.na(ind.probs)]), 1, ind.probs[!is.na(ind.probs)]))
  no.colls.q
}


# xrisksum2 uses  pcollxy to estimate cumulative risk for a height band (y) across incremements (xinc) of x
# it is used in a loop in Option2 to calculate risk for each height band along the rotor
# this function does not directly use information from the flight height distribution
xrisksum2 <- function (y, xinc, updown) {
  # get the proprotion of the full rectangle for discretizing integration (full is 1, at the equator; poles are 0)
  xmax <- (1-y*y)^0.5
  # imax is the number of whole squares in the x dimension
  imax <- as.integer(xmax/xinc)
  # divided by 2 to get means?
  risk <- (pcollxy(imax*xinc, y, updown)/2 + pcollxy(xmax, y, updown)/2)*(xmax - imax*xinc)
  risk2 <- risk + (pcollxy(0,y,updown)/2 + pcollxy(imax*xinc, y, updown)/2)*xinc
  
  for (i in 1:(imax - 1)){
    risk2 <- risk2 + pcollxy(i*xinc, y, updown)*xinc
  }
  ifelse(imax > 0, 2*risk2, 2*risk)
}

