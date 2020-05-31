benchmarkMbo = function(instances, psOpt, funcEvals, paramsMBO = data.table::data.table(NULL), minimize = TRUE,
                        repls = 10, ncpus = NA, seed = 1, delReg = TRUE) {
  info = EBO::getModelInfo(instances[[1]], psOpt, minimize)
  objNormal = EBO::createObjDesignNormal(instances, psOpt, info)
  configMbo = EBO::createConfigMbo(funcEvals, paramsMBO)
  # create registry
  reg = batchtools::makeExperimentRegistry(file.dir = NA, seed = seed)
  # add the normal objective function to the registry
  batchtools::addProblem(name = "objNormal", fun = objNormalFunc, cache = TRUE, reg = reg)
  EBO::computeMBO(reg, objNormal, configMbo, info, repls)
  EBO::executeComputation(reg, ncpus)
  resMbo = EBO::reduceMbo()
  if(delReg) batchtools::removeRegistry(0, reg)
  return(resMbo)
}
