#' @export
benchmarkDmbo = function(instances, psOpt, funcEvals, paramsMbo, minimize = TRUE,
                         repls = 10, ncpus = NA, seed = 1, delReg = TRUE, step, replsOrg) {
  # get instance info
  info = EBO::getModelInfo(instances[[1]], psOpt, minimize)
  # get number of instances
  numberInstances = length(instances)
  # in first step do
  if (step == 1) {
    # create the configuration for mlrMBO algorithm
    configMbo = EBO::createConfigMbo(funcEvals, paramsMbo)
    # create batchtool instance
    objNormal = EBO::createObjDesignNormal(instances, psOpt, info)
  }
  # in step greather then one do
  if (step > 1) {
    # split instances and create batchtool instance
    objNormal = list()
    for (i in 1:numberInstances) {
      objNormal[[i]] = EBO::createObjDesignNormal(list(instances[[i]]), psOpt, info)
    }
    # create the initial design configuration for mlrMBO algorithm
    configMbo = list()
    for (i in 1:numberInstances) {
      configMbo[[i]] = EBO::createConfigMbo(funcEvals, paramsMbo[c((replsOrg * (i-1) + 1) : (replsOrg * i)),])
    }


    #objNormal1 = EBO::createObjDesignNormal(list(instances[[1]]), psOpt, info)
    #objNormal2 = EBO::createObjDesignNormal(list(instances[[2]]), psOpt, info)
    #objNormal3 = EBO::createObjDesignNormal(list(instances[[3]]), psOpt, info)
    #configMbo1 = EBO::createConfigMbo(funcEvals, paramsMbo[c((replsOrg * 0 + 1) : (replsOrg * 1)),])
    #configMbo2 = EBO::createConfigMbo(funcEvals, paramsMbo[c((replsOrg * 1 + 1) : (replsOrg * 2)),])
    #configMbo3 = EBO::createConfigMbo(funcEvals, paramsMbo[c((replsOrg * 2 + 1) : (replsOrg * 3)),])
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
    #batchtools::addExperiments(prob.designs = objNormal1, algo.designs = configMbo1, repls = repls, reg = reg)
    #batchtools::addExperiments(prob.designs = objNormal2, algo.designs = configMbo2, repls = repls, reg = reg)
    #batchtools::addExperiments(prob.designs = objNormal3, algo.designs = configMbo3, repls = repls, reg = reg)
  }
  # execute computation
  EBO::executeComputation(reg, ncpus)
  # reduce results
  resDmbo = EBO::reduceMbo()
  # delete registry
  if (delReg) batchtools::removeRegistry(0, reg)
  # return result of Benchmark
  return(resDmbo)
}
