computeEs = function(reg, objEncoded, configEs, repls) {
  addAlgorithm(name = "es", fun = configESFunc, reg = reg)
  # add Experiments to registry
  addExperiments(prob.designs = objEncoded, algo.designs = configEs, repls = repls, reg = reg)
}

configESFunc = function(instance, funcEvals = 50, nu = 10, mue = 10, sigmaInit = 1.0, nSigma = 1,
                        mutation = 1, tau = 1.0, stratReco = 2, objReco = 2, ...) {

  # wrapper in order to use SPOT optimization algorithms
  fun2 = function(xmat) {
    apply(xmat, 1, instance[[1]])
  }

  # define optimizer / algo

  psOpt = getParamSet(instance[[1]])

  res <- SPOT::optimES(fun = fun2, lower = getLower(psOpt), upper = getUpper(psOpt),
                 control = list(funEvals = funcEvals, nu = nu, mue = mue, sigmaInit = sigmaInit,
                                nSigma = nSigma, mutation = mutation, tau = tau,
                                stratReco = stratReco, objReco = objReco))

  y = as.data.frame(res[["ybest"]])
  x = as.data.frame(res[["xbest"]])

  # data transformations in the case of discrete variables
  for (i in 1:instance[[2]]$featureNumber) {

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
