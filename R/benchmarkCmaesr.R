benchmarkCmaesr = function(instance, psOpt, funcEvals = NULL, configCmaesr = data.table::data.table(NULL),
                           minimize = TRUE, repls = 10, ncpus = NA, seed = 1) {

  # get lots of information which will be used in further computations
  info = EBO::getModelInfo(instance[[1]], psOpt, minimize)
  # generate the design of the objective function
  objEncoded = EBO::createObjDesignEncoded(instance, psOpt, info)
  # generate the configuration (hyperparameters) of cmaesr::cmaes()
  configCmaesr = EBO::createConfigCmaesr(funcEvals, configCmaesr)
  # create registry for the computation
  reg = batchtools::makeExperimentRegistry(file.dir = NA, seed = seed)
  # add the encoded objective function to the registry
  batchtools::addProblem(name = "objEncoded", fun = objEncodedFunc, reg = reg)
  # add cmaesr:cmaes() as an algorithm to the registry
  EBO::computeCMAESR(reg, objEncoded, configCmaesr, repls)
  # compute the result
  EBO::executeComputation(reg, ncpus)
  # reduce the results
  resultCmeasr = batchtools::reduceResultsList(ids = NULL, fun = reduceOptimize)
  batchtools::removeRegistry(0, reg)
  return(resultCmeasr)
}
