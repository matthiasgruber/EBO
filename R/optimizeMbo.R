#optimizeMBO = function(configuration, objNormal, info, funcEvals, model, psOpt) {
  # create MBOControl and MBOsurrogate (infillcrit + surrogate)
 # listControlLearner = createMboControlSurrogate(x = configuration) # funcEvals
  # compute mlrMBO

  #controlOpt = mlrMBO::setMBOControlTermination(listControlLearner[[1]],
  #                                              iters = funcEvals)
#
  #design = list(configuration$design)
#
  #paramsMBO = data.table(design = designOpt,
  #                       amountDesign = list(configuration$amountDesign),
  #                       control = list(controlOpt),
  #                       surrogate = list(listControlLearner[[2]]))
#
  #resMboBenchmark = benchmarkMbo(model, psOpt, funcEvals,
  #                               paramsMBO, minimize = FALSE, repls = 1,
  #                               ncpus = NA, seed = 1)
#
#
#
#  res = configMboFunc(instance = list(objNormal, info),
#                      itersMbo = funcEvals, design = get(configuration$initdesign, asNamespace("lhs")),
#                      amountDesign = configuration$amountDesign,
#                      control = mboControlLearner[[1]],
#                      surrogate = mboControlLearner[[2]])
#  return(resMboBenchmark)
#}

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
