#' Benchmark and plot \code{mlrMBO::mbo()} optimization runs to investigate hyperparameters
#'
#' This functions benchmarks the \code{mlrMBO::mbo()} function on different configurations and
#' then plots them wrt the hyperparameters.
#' @export
#'
tuneMboMbo = function(instance, psOpt, funcEvals, psTune, itersMboTune = 10,
                      minimize = FALSE, repls = 15, ncpus = NA, seed = 1,
                      designOpt = NULL, maxTime = NULL) {

  set.seed(seed)
  info = EBO::getModelInfo(instance[[1]], psOpt, minimize)

  getMedianBenchmarkMbo = function(x) {

    listControlLearner = EBO::createMboControlSurrogate(x)

    if (is.null(designOpt)) designOpt = x$design

    paramsMBO = data.table::data.table(design = designOpt, ## list
                           amountDesign = x$amountDesign, ##list
                           control = list(listControlLearner[[1]]),
                           surrogate = list(listControlLearner[[2]])
    )

    resMboBenchmark = EBO::benchmarkMbo(instance, psOpt, funcEvals, paramsMBO, minimize, repls, ncpus, seed)

    results = NA
    for (i in 1:length(resMboBenchmark)) {
      results[i] = resMboBenchmark[[i]][["recommendedParameters"]][info$featureNumber+1]
    }
    #if (length(resMboBenchmark) != repls) warning() # add warning if a config failed

    median = median(unlist(results))
    return(median)
  }

  mboMboFuncMulti = smoof::makeSingleObjectiveFunction(
    name = "tuneMboMbo",
    fn = getMedianBenchmarkMbo,
    par.set = psTune,
    has.simple.signature = FALSE,
    minimize = minimize
  )

  controlTune = mlrMBO::makeMBOControl(n.objectives = 1L, y.name = "y")
  controlTune = mlrMBO::setMBOControlInfill(controlTune, crit = mlrMBO::makeMBOInfillCritEI())
  if (!is.null(itersMboTune)) controlTune = mlrMBO::setMBOControlTermination(controlTune, iters = itersMboTune)
  if (!is.null(maxTime)) controlTune = mlrMBO::setMBOControlTermination(controlTune, time.budget = maxTime)

  design = ParamHelpers::generateDesign(n = 12, par.set = psTune, fun = lhs::maximinLHS)

  resMboTune = mlrMBO::mbo(mboMboFuncMulti, design = design, control = controlTune, show.info = TRUE)

  hyperparamsPath = as.data.frame(resMboTune[["opt.path"]][["env"]][["path"]])

  bestHyperparams = hyperparamsPath[which.max(hyperparamsPath$y),]

  return(bestHyperparams)
}
