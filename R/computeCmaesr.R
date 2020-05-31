computeCMAESR = function(reg, objEncoded, configCmaesr, repls) {
  addAlgorithm(name = "cmaesr", fun = cmaesrFunc, reg = reg)
  # add Experiments to registry
  addExperiments(prob.designs = objEncoded, algo.designs = configCmaesr, repls = repls, reg = reg)
}

cmaesrFunc = function(instance, funcEvals = 50, sigma = 0.5, lambda = NULL, mu = NULL, ...) {

  optimizationPath = cmaesr::cmaes(
    instance[[1]],
    monitor = makeSimpleMonitor(max.params = instance[[2]]$featureNumber),
    control = list(
      sigma = sigma, # initial step size
      lambda = lambda, # number of offspring
      mu = mu, # number of individuals in each population
      stop.ons = list(cmaesr::stopOnMaxEvals(funcEvals))
    )
  )

  y = as.data.frame(optimizationPath[["best.fitness"]])
  x = as.data.frame(t(optimizationPath[["best.param"]]))

  for (i in 1:instance[[2]]$featureNumber) {

    if (instance[[2]]$featureType[i] == "integer") x[i] = as.integer(round(x[i]))

    if (instance[[2]]$featureType[i] == "discrete") {
      x[i] = round(x[i])
      levelEncoded = 1:length(instance[[2]]$discreteLevel[[instance[[2]]$featureName[i]]])
      for (ii in 1:length(instance[[2]]$discreteLevel[[instance[[2]]$featureName[i]]])) {
        if (x[i] == levelEncoded[ii]) {
          x[i] = instance[[2]]$discreteLevel[[instance[[2]]$featureName[i]]][[ii]]
        }
      }
      x[i] = factor(x[i], levels = instance[[2]]$discreteLevel[[instance[[2]]$featureName[i]]])
    }
  }

  y = y * instance[[2]]$p
  colnames(y) = instance[[2]]$y.name
  colnames(x) = instance[[2]]$featureName
  bestResult = cbind(x,y)
  res = list(bestResult,optimizationPath)
  return(res)
}
