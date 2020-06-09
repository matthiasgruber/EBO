#' Tune SMBO hyperparameters by mlrMBO.
#'
#' This function tunes the SMBO hyperparameters by mlrMBO. The user can
#' choose if the tuning is initialized with an initial design or not.
#' You can either pass one instance or multiple instances of the same problem as a list.
#' We recommend to tune the hyperparameters after a large amount of black-box function
#' evaluations, as the tuning will fail if the user chooses an amount which is too low.
#' During the hyperparameter optimization of EBO::tuneMboMbo(), the user is able to see
#' in the console if the hyperparameters can be tuned successfully. If all hyperparameter
#' sets return approximately the same median of the target variable, we recommend to continue the SMBO
#' with the default hyperparameters.
#'
#' @param instance [\code{list(1)}]\cr
#'  A list containing the instance(s) as trained mlr learners.\cr
#'  You can either pass one or multiple instances of the same problem.
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

  # assertions
  checkmate::assertList(instance, any.missing = FALSE, min.len = 1)
  for (i in 1:length(instance)) {
    checkmate::assertClass(instance[[1]], classes = "WrappedModel", null.ok = FALSE)
  }
  EBO::assertReplsNcpusSeed(repls, ncpus, seed)
  checkmate::assertLogical(minimize, len = 1, any.missing = FALSE)
  checkmate::assertIntegerish(maxTime, lower = 1, any.missing = TRUE,
                              len = 1, null.ok = TRUE)
  checkmate::assertIntegerish(itersMboTune, lower = 1, any.missing = TRUE,
                              len = 1)
  designOpt = as.data.frame(designOpt)
  checkmate::assertClass(designOpt, classes = c("data.frame"))
  checkmate::assertIntegerish(funcEvals, lower = 1, any.missing = TRUE,
                              len = 1)
  EBO::assertPsTune(psTune)

  # start of function
  set.seed(seed)
  info = EBO::getModelInfo(instance[[1]], psOpt, minimize)

  # benchmark mlrMBO configuration
  getMedianBenchmarkMbo = function(x) {
    # convert character string to mlrMBO objects
    listControlLearner = EBO::createMboControlSurrogate(x)

    if (is.null(designOpt)) designOpt = x$design

    paramsMBO = data.table::data.table(design = designOpt,
                           amountDesign = x$amountDesign,
                           control = list(listControlLearner[[1]]),
                           surrogate = list(listControlLearner[[2]])
    )
    # benchmark mlrMBO configuration
    resMboBenchmark = EBO::benchmarkMbo(instance, psOpt, funcEvals, paramsMBO, minimize, repls, ncpus, seed)
    # get results
    results = NA
    for (i in 1:length(resMboBenchmark)) {
      results[i] = resMboBenchmark[[i]][["recommendedParameters"]][info$featureNumber+1]
    }
    #if (length(resMboBenchmark) != repls) warning() # add warning if a config failed
    # get median
    median = median(unlist(results))
    return(median)
  }
  # objective function for the tuning
  mboMboFuncMulti = smoof::makeSingleObjectiveFunction(
    name = "tuneMboMbo",
    fn = getMedianBenchmarkMbo,
    par.set = psTune,
    has.simple.signature = FALSE,
    minimize = minimize
  )
  # make mlrMBO contro objects
  controlTune = mlrMBO::makeMBOControl(n.objectives = 1L, y.name = "y")
  controlTune = mlrMBO::setMBOControlInfill(controlTune, crit = mlrMBO::makeMBOInfillCritEI())
  if (!is.null(itersMboTune)) controlTune = mlrMBO::setMBOControlTermination(controlTune, iters = itersMboTune)
  if (!is.null(maxTime)) controlTune = mlrMBO::setMBOControlTermination(controlTune, time.budget = maxTime)
  # generate initial design for the tuning
  design = ParamHelpers::generateDesign(n = 12, par.set = psTune, fun = lhs::maximinLHS)
  # execute tuning
  resMboTune = mlrMBO::mbo(mboMboFuncMulti, design = design, control = controlTune, show.info = TRUE)
  # get optimization path of the tuning
  hyperparamsPath = as.data.frame(resMboTune[["opt.path"]][["env"]][["path"]])
  # get best suited hyperparameter set
  if (minimize == FALSE) {
    bestHyperparams = hyperparamsPath[which.max(hyperparamsPath$y),]
  }
  if (minimize == TRUE) {
    bestHyperparams = hyperparamsPath[which.min(hyperparamsPath$y),]
  }
  return(bestHyperparams)
}
