#' @export
benchmarkDmbo = function(instances, psOpt, funcEvals, paramsMbo, minimize = TRUE,
                         repls = 10, ncpus = NA, seed = 1, delReg = TRUE, step, replsOrg) {
  # get instance info
  info = getModelInfo(instances[[1]], psOpt, minimize)
  # get number of instances
  numberInstances = length(instances)
  # in first step do
  if (step == 1) {
    # create the configuration for mlrMBO algorithm
    configMbo = createConfigMbo(funcEvals, paramsMbo)
    # create batchtool instance
    objNormal = createObjDesignNormal(instances, psOpt, info)
  }
  # in step greather then one do
  if (step > 1) {
    # split instances and create batchtool instance
    objNormal = list()
    for (i in 1:numberInstances) {
      objNormal[[i]] = createObjDesignNormal(list(instances[[i]]), psOpt, info)
    }
    # create the initial design configuration for mlrMBO algorithm
    configMbo = list()
    for (i in 1:numberInstances) {
      configMbo[[i]] = createConfigMbo(funcEvals, paramsMbo[c((replsOrg * (i-1) + 1) : (replsOrg * i)),])
    }
  }
  # create registry
  reg = batchtools::makeExperimentRegistry(file.dir = NA, seed = seed)
  # add the objective function  problem to the registry
  batchtools::addProblem(name = "objNormal", fun = objNormalFunc, cache = TRUE, reg = reg)
  # add the mlrMBO algorithm to the registry
  batchtools::addAlgorithm(name = "mlrMBO", fun = configMboFunc, reg = reg)
  # add Experiments for step one; all 3 instances at onces
  if (step == 1) {
    batchtools::addExperiments(prob.designs = objNormal, algo.designs = configMbo, repls = repls, reg = reg)
  }
  # add Experiments for step grather one
  # add instance in combination with the design from step one
  if (step > 1) {
    for (i in 1:numberInstances) {
      batchtools::addExperiments(prob.designs = objNormal[[i]], algo.designs = configMbo[[i]], repls = repls, reg = reg)
    }
  }
  # execute computation
  executeComputation(reg, ncpus)
  # reduce results
  resDmbo = reduceMbo()
  # delete registry
  if (delReg) batchtools::removeRegistry(0, reg)
  # return result of Benchmark
  return(resDmbo)
}
