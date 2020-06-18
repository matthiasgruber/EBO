benchmarkRacing = function(instance, psOpt, funcEvals = 50, minimize = TRUE, repls = 10, ncpus = NA, seed = 1) {
  info = getModelInfo(instance[[1]], psOpt, minimize)
  objNormal = createObjDesignNormal(instance, psOpt, info)
  funcEvals = createConfigRacing(funcEvals)
  # create registry
  reg = makeExperimentRegistry(file.dir = NA, seed = seed)
  # add the normal objective function to the registry
  batchtools::addProblem(name = "objNormal", fun = objNormalFunc, reg = reg)
  computeRacing(reg, objNormal, funcEvals, repls)
  executeComputation(reg, ncpus)
  resRacing = reduceRacing(repls = repls)
  batchtools::removeRegistry(0, reg)
  return(resRacing)
}

