#' Tune SMBO hyperparameters.
#'
#' This function tunes the SMBO hyperparameters. The user can
#' choose if the tuning is initialized with an initial design or not.
#'
#'
#' @param instance [\code{wrapped model}]\cr
#'  A trained mlr model.
#' @param psOpt [\code{ParamHelpers::ParamSet()}]\cr
#'  Parameter space for the optimization.
#' @param funcEvals [\code{integer(1)}]\cr
#'  Define the amount of black-box function evaluations.
#' @param psTune [\code{ParamHelpers::ParamSet()}]\cr
#'  The parameter space for the tuning.
#' @param itersMboTune [\code{integer(1)}]\cr
#'  Define the amount of black-box function evaluations.\cr
#'  Default is ten.
#' @param minimize [\code{logical(1)}]\cr
#'  Should the target be minimized? \cr
#'  Default is `FALSE`.
#' @param repls [\code{integer(1)}]\cr
#'  Define how often each configuration is replicated for the benchmark.\cr
#'  Default is ten.
#' @param ncpus [\code{numeric(1)}]\cr
#'  Define how many cpu cores are used for the benchmark.\cr
#'  Default is NA, which uses all cores minus one.
#' @param seed [\code{numeric(1)}]\cr
#'  Define the seed used for the computation. Will be set by \code{batchtools}.
#'  Which means the jobs get the seed plus the job.id as their unique seed. \cr
#'  Default is one.
#' @param designOpt [\code{data.frame(1)}]\cr
#'  A data.frame containing the initial design for the optimization.
#' @param maxTime [\code{integer(1)}]\cr
#'  This argument will overwrite itersMboTune and set a maximum time budget in seconds
#'  for the tuning.
#'
#' @return A data.frame containing the best suited hyperparameter set.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#' data <- data.frame(a=runif(50,10,100),b=runif(50,40,750),d=runif(50,0,90))
#' data$ratio <- rowSums(data^2)
#' data$ratio <- data$ratio/max(data$ratio)
#' colnames(data) <- c("power","time","pressure","ratio")
#'
#' model = list(mlr::train(mlr::makeLearner("regr.randomForest"),
#'              mlr::makeRegrTask(data = data, target = "ratio")))
#'
#' psOpt = ParamHelpers::makeParamSet(
#'   ParamHelpers::makeNumericParam("power", lower = 10, upper = 100),
#'   ParamHelpers::makeNumericParam("time", lower = 40, upper = 750),
#'   ParamHelpers::makeNumericParam("pressure", lower = 0, upper = 90)
#' )
#'
#' funcEvals = 10
#'
#' psTune = ParamHelpers::makeParamSet(
#'   ParamHelpers::makeDiscreteParam("design", values = c("maximinLHS",
#'                                                        "optimumLHS")),
#'
#'   ParamHelpers::makeDiscreteParam("crit", values = c("makeMBOInfillCritEI",
#'                                                      "makeMBOInfillCritAEI",
#'                                                      "makeMBOInfillCritCB",
#'                                                      "makeMBOInfillCritAdaCB")),
#'
#'   ParamHelpers::makeDiscreteParam("surrogate", values = c("regr.randomForest",
#'                                                           "regr.km")),
#'
#'   ParamHelpers::makeDiscreteParam("covtype" ,values = c("gauss","matern5_2",
#'                                                         "matern3_2","powexp"),
#'                                   requires = quote(surrogate == "regr.km"))
#' )
#'
#' itersMboTune = 2
#'
#' minimize = FALSE
#'
#' repls = 2
#'
#' resTune = tuneMboMbo(model, psOpt, funcEvals, psTune, itersMboTune, minimize, repls)
#' }
tuneMboMbo = function(instance, psOpt, funcEvals, psTune, itersMboTune = 10,
                      minimize = FALSE, repls = 10, ncpus = NA, seed = 1,
                      designOpt = NULL, maxTime = NULL) {

  set.seed(seed)
  info = EBO::getModelInfo(instance[[1]], psOpt, minimize)

  getMedianBenchmarkMbo = function(x) {

    listControlLearner = EBO::createMboControlSurrogate(x)

    if (is.null(designOpt)) designOpt = x$design

    paramsMBO = data.table::data.table(design = designOpt,
                           amountDesign = x$amountDesign,
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
