optimizeCmaesr = function(configuration, objEncodedFunc, info, funcEvals) { #configuration, objEncoded, info, itersOptim

  # compute cmaes and define hyperparameters
  res = cmaesrFunc(instance = list(objEncodedFunc, info), #objEncoded, info
                      funcEvals = funcEvals,
                      sigma = configuration$sigma,
                      lambda = configuration$lambda)
  return(res)
}
