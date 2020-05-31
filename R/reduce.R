reduceMbo = function(ids = NULL) {
  mlrMBOresult = reduceResultsList(ids, fun = reduceOptimize)
  for (i in 1:length(mlrMBOresult)) {
    names(mlrMBOresult[[i]]) = c("recommendedParameters","optimizationPathMBO")
  }
  return(mlrMBOresult)
}


reduceOptimize = function(result) {
  result
}


reduceRacing = function(ids = NULL, repls){
  racingResult = reduceResultsList(ids, fun = reduceOptimize)
  for (i in 1:length(racingResult)) {
    names(racingResult[[i]]) = "recommendedParameters"
  }
  return(racingResult)
}

reduceEs = function(ids = NULL, repls){
  esResult = reduceResultsList(ids, fun = reduceOptimize)
  for (i in 1:length(esResult)) {
    names(esResult[[i]]) = "recommendedParameters"
  }
  return(esResult)
}


reduceRandom = function(ids = NULL, repls) {
  randomResult = reduceResultsList(ids, fun = reduceOptimize)
  for (i in 1:length(randomResult)) {
    names(randomResult[[i]]) = c("recommendedParametersRandom","optimizationPathRandom")
    for (ii in 1:2) {
      randomResult[[i]][[ii]] = as.data.frame(randomResult[[i]][[ii]])
    }
  }
  return(randomResult)
}

