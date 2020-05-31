tuneDmboMboCompute = function(instancesTrain, psOpt, funcEvals, psTune, itersMboTune = 50,
                              minimize = TRUE, repls = 12, ncpus = NA, seed = 1,
                              designOpt = NULL, step, replsOrg) {

    set.seed(seed)
    info = getModelInfo(instancesTrain[[1]], psOpt, minimize)

    getMedianBenchmarkMbo = function(x) {

      listControlLearner = createMboControlSurrogate(x)

      if (is.null(designOpt)) designOpt = x$design

      paramsMbo = data.table(design = designOpt, ## list
                             amountDesign = x$amountDesign, ##list
                             control = list(listControlLearner[[1]]),
                             surrogate = list(listControlLearner[[2]])
      )

      resMboBenchmark = benchmarkDmbo(instancesTrain, psOpt, funcEvals, paramsMbo,
                                      minimize, repls, ncpus, seed, delReg = TRUE, step, replsOrg)

      results = NA
      for (i in 1:length(resMboBenchmark)) {
        results[i] = resMboBenchmark[[i]][["recommendedParameters"]][info$featureNumber+1]
      }
      if (length(resMboBenchmark) != repls) warning()

      median = median(unlist(results))
      return(median)
    }

    mboMboFuncMulti = makeSingleObjectiveFunction(
      name = "tunDmboMbo",
      fn = getMedianBenchmarkMbo,
      par.set = psTune,
      has.simple.signature = FALSE,
      minimize = minimize
    )

    controlTune = makeMBOControl(n.objectives = 1L, y.name = "y")
    controlTune = setMBOControlInfill(controlTune, crit = makeMBOInfillCritEI())
    controlTune = setMBOControlTermination(controlTune, iters = itersMboTune)

    resMboTune = mbo(mboMboFuncMulti, control = controlTune, show.info = TRUE)

    hyperparamsPath = as.data.frame(resMboTune[["opt.path"]][["env"]][["path"]])

    bestHyperparams = hyperparamsPath[which.max(hyperparamsPath$y),]

    return(bestHyperparams)
  }


