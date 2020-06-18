
benchmarkRandom = function(instance, psOpt, funcEvals = 50, minimize = TRUE, repls = 10, ncpus = NA, seed = 1) {
  info = getModelInfo(instance[[1]], psOpt, minimize)
  objNormal = createObjDesignNormal(instance, psOpt, info)
  funcEvals = createConfigRandom(funcEvals)
  # create registry
  reg = batchtools::makeExperimentRegistry(file.dir = NA, seed = seed)
  # add the normal objective function to the registry
  batchtools::addProblem(name = "objNormal", fun = objNormalFunc, reg = reg)
  computeRandom(reg, objNormal, funcEvals, repls)
  executeComputation(reg, ncpus)
  resRandom = reduceRandom(repls = repls)
  batchtools::removeRegistry(0, reg)
  return(resRandom)
}
