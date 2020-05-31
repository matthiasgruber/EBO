computeRacing = function(reg, objNormal, funcEvals, repls) {
  addAlgorithm(name = "irace", fun = configRacingFunc, reg = reg)
  addExperiments(prob.designs = objNormal, algo.designs = funcEvals, repls = repls, reg = reg)
}

configRacingFunc = function(instance, funcEvals = 50, minNbSurvival = 1, nbConfigurations = 2, ...) {

  oldWd = getwd()
  tempDirPath = tempdir()
  setwd(tempDirPath)

  instanceBatchtools = instance

  parameters = convertParamSetToIrace(getParamSet(instanceBatchtools[[1]]))

  target.runner = function(experiment, scenario) {

    debugLevel    = scenario$debugLevel
    configuration.id  = experiment$id.configuration
    instance.id   = experiment$id.instance
    seed          = experiment$seed
    configuration = experiment$configuration
    instance      = experiment$instance
    instance = data

    df = as.data.frame(configuration)
    colnames(df) = instanceBatchtools[[2]]$featureName

    y = instanceBatchtools[[1]](df)

    result = list(cost = instanceBatchtools[[2]]$p*(y), call = toString(experiment))

    return(result)
  }
  # create the default scenario aka hyperparameter setting for the algoritm
  scenario = irace::defaultScenario()
  # set directory for the irace.Rdata file to the temporary path
  scenario$execDir = tempDirPath
  # set seed --> there is no possability to not set the seed by the iRace package, but we
  # need to seed the seed for each replication to the corresponding seed of batchtools
  # we will do this in the target.runner function
  scenario$seed = NULL
  # set the target.runner function from above
  scenario$targetRunner = "target.runner"
  # our objective functions are deterministic
  scenario$deterministic = TRUE
  # number of runs of the target algorithm per iteration
  scenario$nbExperimentsPerIteration = 0 # --> default: 0
  # set number of maximum iterations
  scenario$maxExperiments = funcEvals
  # set number of instances (initial design) --> min 5 if deterministic == TRUE
  scenario$instances = c(1,2)
  # number of instances evaluated before the ﬁrst elimination test
  scenario$firstTest = 2
  # number of conﬁgurations to be sampled and evaluated at each iteration
  scenario$nbConfigurations = nbConfigurations
  # parameter used to deﬁne the number of conﬁgurations sampled and evaluated at each iteration
  scenario$mu = 3
  # minimum number of conﬁgurations needed to continue the execution of each iteration
  scenario$minNbSurvival = minNbSurvival
  # initialize computation
  result = irace::irace(scenario = scenario, parameters = parameters)

  x = result[2:(instanceBatchtools[[2]]$featureNumber+1)]
  for (i in 1:instance[[2]]$featureNumber) {
    if (instance[[2]]$featureType[i] == "discrete") {
      x[[instance[[2]]$featureName[i]]] =
        factor(x[[instance[[2]]$featureName[i]]],levels = instance[[2]]$discreteLevel[[instance[[2]]$featureName[i]]])
    }
    if (instance[[2]]$featureType[i] == "integer") x[i] = as.integer(round(x[i]))
  }
  y = instanceBatchtools[[1]](x)
  y = as.data.frame(y)
  colnames(y) = instanceBatchtools[[2]]$y.name
  res = list(cbind(x, y))
  setwd(oldWd)
  return(res)
}

