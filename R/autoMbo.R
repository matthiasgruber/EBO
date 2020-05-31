#' @export

autoMbo = function(data, target, minimize = FALSE, funcEvals = 45,
                   minFuncEvals = 10, itersMboTune = 10, repls = 10,
                   showInfo = TRUE, ncpus = NA, seed = 1) {

  set.seed(seed)
  startTime = Sys.time()

  data = as.data.frame(data) # in assertion rein

  # psTune default
  psTune = ParamHelpers::makeParamSet(

    ParamHelpers::makeDiscreteParam("crit", values = c("makeMBOInfillCritEI",
                                                       "makeMBOInfillCritAEI",
                                                       "makeMBOInfillCritCB",
                                                       "makeMBOInfillCritAdaCB")),

    ParamHelpers::makeIntegerParam("cb.lambda", lower = 1, upper = 5,
                                   requires = quote(crit == "makeMBOInfillCritCB")),

    ParamHelpers::makeIntegerParam("cb.lambda.start", lower = 3, upper = 10,
                                   requires = quote(crit == "makeMBOInfillCritAdaCB")),

    ParamHelpers::makeNumericParam("cb.lambda.end", lower = 0, upper = 3,
                                   requires = quote(crit == "makeMBOInfillCritAdaCB")),

    ParamHelpers::makeDiscreteParam("surrogate", values = c("regr.randomForest", "regr.km")),

    ParamHelpers::makeDiscreteParam("covtype" ,values = c("gauss","matern5_2",
                                                          "matern3_2","powexp"),
                                    requires = quote(surrogate == "regr.km"))
  )

  psOpt = EBO::generateParamSpace(data, target)

  surrogateModel = mlr::train(mlr::makeLearner("regr.randomForest", nodesize = 1),
                              mlr::makeRegrTask(data = data, target = target))

  info = EBO::getModelInfo(surrogateModel, psOpt, minimize)

  steps = as.integer(ceiling((funcEvals - info$featureNumber - 1) / minFuncEvals))

  resTuneDmbo = EBO::tuneDmboMbo(surrogateModel, minFuncEvals, funcEvals, psTune, itersMboTune,
                                 minimize, repls, ncpus, seed, psOpt, info, steps)

  repls = repls*3

  # benchmarkMbo default
  paramsSmboDefault = data.table::data.table(design = list("maximinLHS"),
                                            amountDesign = list(NULL),
                                            control = list(NULL),
                                            surrogate = list(NULL)
  )

  resMboDefault = EBO::benchmarkMbo(list(surrogateModel), psOpt, funcEvals, paramsSmboDefault,
                                    minimize, repls, ncpus, seed)


  # benchmarkDmbo
  hyperparamsDmbo = list()
  for (i in 1:steps) {
    hyperparamsDmbo[[i]] = resTuneDmbo[[i]][[1]]
  }

  resDmboTuned = EBO::computeDmbo(surrogateModel, minFuncEvals, funcEvals, psTune, itersMboTune,
                                  minimize, repls, ncpus, seed, psOpt, info, hyperparamsDmbo, steps)

  plot = EBO::plotAutoMbo(resMboDefault, resDmboTuned, minFuncEvals, funcEvals, repls, showInfo, info,
                          ncpus, seed, step, startTime, steps, hyperparamsDmbo)

  return(plot)
}
