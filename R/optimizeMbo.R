optimizeMBO = function(configuration, objNormal, info, funcEvals) {
  # create MBOControl and MBOsurrogate (infillcrit + surrogate)
  mboControlLearner = createMboControlSurrogate(x = configuration)
  # compute mlrMBO


  res = configMboFunc(instance = list(objNormal, info),
                      funcEvals = funcEvals,
                      design = configuration$design,
                      amountDesign = configuration$amountDesign,
                      control = mboControlLearner[[1]],
                      surrogate = mboControlLearner[[2]])
  return(res)
}
