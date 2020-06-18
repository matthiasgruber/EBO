tuneDmboMbo = function(surrogateModel, minFuncEvals, funcEvals, psTune, itersMboTune,
                       minimize, repls, ncpus, seed, psOpt, info, steps) {

  step = 1L
  evals = 0L
  replsOrg = repls

  minFuncEvalsOrg = minFuncEvals

  resTuneDmbo = list()
  # steps:
  # initialize first tuning iteration with an initial design
  # initialize the next tuning iterations with the black-box function evaluations of step 1
  # as initial design for seconden step. the thirt step gets all black-box function evaluations
  # of the second step as initial design
  # and so on
  # compute until funcEvals is reached

  if (step == 1) minFuncEvals = minFuncEvals + info$featureNumber + 1

  stepOne = benchmarkMbo(list(surrogateModel), psOpt, minFuncEvals, paramsMBO = data.table::data.table(NULL),
                         minimize = minimize, repls = 1, ncpus = ncpus, seed = seed, delReg = TRUE)

  design = as.data.frame(stepOne[[1]][["optimizationPathMBO"]][["opt.path"]][["env"]][["path"]])

  surrogateModelTune = list(mlr::train(mlr::makeLearner("regr.randomForest", nodesize = 3),
                                       mlr::makeRegrTask(data = design, target = info$y.name)))

  bestHyperparams = data.frame()

  resTuneDmbo[[step]] = list(bestHyperparams, design)

  step = step + 1

  while (evals < funcEvals) {

    if(step == 2) designOpt = NULL

    if(step > 2) designOpt = list(resTuneDmbo[[step-2]][[2]])

    bestHyperparams = tuneMboMbo(surrogateModelTune, psOpt, minFuncEvals, psTune, itersMboTune = itersMboTune,
                                 minimize = minimize, repls = repls, ncpus = ncpus, seed = seed,
                                 designOpt = designOpt, maxTime = NULL)

    if (step != steps) minFuncEvals = (minFuncEvals + minFuncEvalsOrg)

    if (step == steps) minFuncEvals = funcEvals

    listControlLearner = createMboControlSurrogate(bestHyperparams)

    paramsMBO = data.table::data.table(design = list(design),
                                       amountDesign = list(NULL),
                                       control = list(listControlLearner[[1]]),
                                       surrogate = list(listControlLearner[[2]])
    )

    resComputeDmbo = benchmarkMbo(list(surrogateModel), psOpt, minFuncEvals, paramsMBO = paramsMBO,
                                  minimize = minimize, repls = 1, ncpus = ncpus, seed = seed, delReg = TRUE)

    evals = nrow(resComputeDmbo[[1]][["optimizationPathMBO"]][["opt.path"]][["env"]][["path"]])

    design = as.data.frame(resComputeDmbo[[1]][["optimizationPathMBO"]][["opt.path"]][["env"]][["path"]])

    surrogateModelTune = list(mlr::train(mlr::makeLearner("regr.randomForest", nodesize = 1),
                                         mlr::makeRegrTask(data = design, target = info$y.name)))

    resTuneDmbo[[step]] = list(bestHyperparams, design)

    step = step + 1
  }

  return(resTuneDmbo)
}
