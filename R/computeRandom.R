computeRandom = function(reg, objNormal, funcEvals, repls = 1) {
  addAlgorithm(name = "random", fun = configRandomFunc, reg = reg)
  # add Experiments to registry
  addExperiments(prob.designs = objNormal, algo.designs = funcEvals, repls = repls, reg = reg)
}

configRandomFunc = function(instance, funcEvals = 50, ...) {

  x = t(sampleValues(getParamSet(instance[[1]]), funcEvals, trafo = FALSE))

  y = lapply(x, instance[[1]])

  y = as.data.frame(t(rbind(y)))
  x = data.table::rbindlist(x)

  colnames(y) = instance[[2]]$y.name
  colnames(x) = instance[[2]]$featureName
  for (i in 1:instance[[2]]$featureNumber) {
    if (instance[[2]]$featureType[i] == "discrete") {
      x[[instance[[2]]$featureName[i]]] =
        factor(x[[instance[[2]]$featureName[i]]],levels = instance[[2]]$discreteLevel[[instance[[2]]$featureName[i]]])
    }
  }

  optimizationPath = cbind(x,y)

  if (instance[[2]]$minimize == FALSE) {
    recommendedParameterRandom = optimizationPath[which.max(optimizationPath[, instance[[2]]$featureNumber+1]),]
  }
  if (instance[[2]]$minimize == TRUE) {
    recommendedParameterRandom = optimizationPath[which.min(optimizationPath[, instance[[2]]$featureNumber+1]),]
  }
  result = list(recommendedParameterRandom,optimizationPath)
  return(result)
}
