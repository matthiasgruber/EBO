benchmarkRacing = function(instance, psOpt, funcEvals = 50, minimize = TRUE, repls = 10, ncpus = NA, seed = 1) {
  info = EBO::getModelInfo(instance[[1]], psOpt, minimize)
  objNormal = EBO::createObjDesignNormal(instance, psOpt, info)
  funcEvals = EBO::createConfigRacing(funcEvals)
  # create registry
  reg = makeExperimentRegistry(file.dir = NA, seed = seed)
  # add the normal objective function to the registry
  batchtools::addProblem(name = "objNormal", fun = objNormalFunc, reg = reg)
  EBO::computeRacing(reg, objNormal, funcEvals, repls)
  EBO::executeComputation(reg, ncpus)
  resRacing = EBO::reduceRacing(repls = repls)
  batchtools::removeRegistry(0, reg)
  return(resRacing)
}

