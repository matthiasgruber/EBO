optimizeCmaesr = function(configuration, objEncodedFunc, info, funcEvals) { #configuration, objEncoded, info, itersOptim

  # compute mlrMBO
  res = cmaesrFunc(instance = list(objEncodedFunc, info), #objEncoded, info
                      funcEvals = funcEvals,
                      sigma = configuration$sigma,
                      lambda = configuration$lambda)
  return(res)
}
