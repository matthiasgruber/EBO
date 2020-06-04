benchmarkMbo = function(instances, psOpt, funcEvals, paramsMBO = data.table::data.table(NULL), minimize = TRUE,
                        repls = 10, ncpus = NA, seed = 1, delReg = TRUE) {
  # get lots of information which will be used in further computations
  info = EBO::getModelInfo(instances[[1]], psOpt, minimize)
  # generate the design of the objective function
  objNormal = EBO::createObjDesignNormal(instances, psOpt, info)
  # generate the configuration(s) for mlrMBO::mbo()
  configMbo = EBO::createConfigMbo(funcEvals, paramsMBO)
  # create registry
  reg = batchtools::makeExperimentRegistry(file.dir = NA, seed = seed)
  # add the normal objective function to the registry
  batchtools::addProblem(name = "objNormal", fun = objNormalFunc, cache = TRUE, reg = reg)
  # add mlrMBO::mbo() algorithm to registry
  EBO::computeMBO(reg, objNormal, configMbo, info, repls)
  # axecute computations
  EBO::executeComputation(reg, ncpus)
  # reduce results
  resMbo = EBO::reduceMbo()
  # remove registry
  if(delReg) batchtools::removeRegistry(0, reg)
  # return result
  return(resMbo)
}
