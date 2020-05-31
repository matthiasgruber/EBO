computeDE = function(reg, objEncoded, configDE, repls) {
  addAlgorithm(name = "SpotDE", fun = configDEFunc, reg = reg)
  # add Experiments to registry
  addExperiments(prob.designs = objEncoded, algo.designs = configDE, repls = repls, reg = reg)
}

configDEFunc = function(instance, funcEvals = 50, populationSize = NULL, ...) {

  fun2 = function(xmat) {
    apply(xmat, 1, instance[[1]])
  }

  # define optimizer / algo

  psOpt = getParamSet(instance[[1]])

  res <- SPOT::optimDE(fun = fun2, lower = getLower(psOpt), upper = getUpper(psOpt),
                           control = list(funEvals = funcEvals,
                                          populationSize = ifelse(!is.null(populationSize), populationSize <- populationSize, populationSize <- 10*instance[[2]]$featureNumber)
                                          ))

  y = as.data.frame(res[["ybest"]])
  x = as.data.frame(res[["xbest"]])

  for (i in 1:instance[[2]]$featureNumber) {

    #if (instance[[2]]$featureType[i] == "integer") x[i] = as.integer(round(x[i]))

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
  result = list(cbind(x,y))
  return(result)
}
