checkCounts <- mat.or.vec(2, 1)
    if(length(CountData[1, ]) == 25){
      checkCounts[1] <- 1
    }
    if(length(CountData[, 1]) >= 1){
      checkCounts[2] <- 1
    }
checkCounts <- sum(checkCounts)