computeDmbo = function(surrogateModel, minFuncEvals, funcEvals, psTune, itersMboTune,
                       minimize, repls, ncpus, seed, psOpt, info, hyperparamsDmbo, steps) {


  step = 1L
  evals = 0L
  replsOrg = repls

  minFuncEvalsOrg = minFuncEvals

  resTuneDmbo = list()

  if (step == 1) minFuncEvals = minFuncEvals + info$featureNumber + 1

  stepOne = benchmarkMbo(list(surrogateModel), psOpt, minFuncEvals, paramsMBO = data.table::data.table(NULL),
                         minimize = minimize, repls = repls, ncpus = ncpus, seed = seed, delReg = TRUE)

  design = list()

  for (i in 1:replsOrg) {
    design[[i]] = as.data.frame(stepOne[[i]][["optimizationPathMBO"]][["opt.path"]][["env"]][["path"]])
  }

  bestHyperparams = data.frame()

  resTuneDmbo[[step]] = list(bestHyperparams, design)

  step = step + 1

  for (i in 2:steps) {

    if (step == 2) repls = 1L

    if (step > 1) minFuncEvals = (minFuncEvals + minFuncEvalsOrg)

    if (step == steps) minFuncEvals = funcEvals

    listControlLearner = createMboControlSurrogate(hyperparamsDmbo[[step]])

    paramsDmboHyperparams = data.table::data.table(design = design,
                                                   amountDesign = list(NULL),
                                                   control = list(listControlLearner[[1]]),
                                                   surrogate = list(listControlLearner[[2]])
    )

    resComputeDmbo = benchmarkDmbo(list(surrogateModel), psOpt, minFuncEvals, paramsDmboHyperparams, minimize,
                                        repls, ncpus, seed, delReg = TRUE, step, replsOrg)

    evals = nrow(resComputeDmbo[[1]][["optimizationPathMBO"]][["opt.path"]][["env"]][["path"]])

    design = list()

    for (i in 1:replsOrg) {
      design[[i]] = as.data.frame(resComputeDmbo[[i]][["optimizationPathMBO"]][["opt.path"]][["env"]][["path"]])
    }

    resTuneDmbo[[step]] = list(hyperparamsDmbo[[step]], design)

    step = step + 1
  }

  return(resTuneDmbo)
}
