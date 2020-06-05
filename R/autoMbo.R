#' Tune DMBO and plot vs SMBO default
#'
#' This function tunes the DMBO hyperparameters and then plots the best DMBO configuration
#' against the default SMBO configuration.
#'
#' @inheritParams tuneMboMbo
#'
#' @param data [\code{data.frame(1)}]\cr
#'  A data.frame containing the data for the blac-box function simulation.
#' @param target [\code{character}]\cr
#'  A character string containing the name of the target variable.
#' @param minFuncEvals [\code{integer(1)}]\cr
#'  An integer which defines when to switch between the hyperparameters.
#' @param showInfo [\code{logical(1)}]\cr
#'  Should information be plotted?\cr
#'  Default is `TRUE`.
#'
#' @return A ggplot2 object illustrating the benchmark of the tuned DMBO approach vs. the default SMBO.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#' data <- data.frame(a=runif(50,10,5555),b=runif(50,-30000,-500))
#' data$ratio <- rowSums(data)
#' data$ratio <- data$ratio/max(data$ratio)
#' colnames(data) <- c("a","t","y")
#'
#' target = c("y")
#'
#' minimize = FALSE
#' funcEvals = 13
#' minFuncEvals = 5
#' itersMboTune = 1
#'
#' repls = 2
#'
#' plot = EBO::autoMbo(data, target, minimize, funcEvals, minFuncEvals, itersMboTune, repls)
#' }
#'
#'
#' @import mlrMBO
#' @import mlr
#' @import ParamHelpers
#' @import lhs
#' @import batchtools
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @importFrom data.table data.table
#' @importFrom data.table CJ
#' @import cmaesr
#' @import irace
#' @import smoof

autoMbo = function(data, target, minimize = FALSE, funcEvals,
                   minFuncEvals = 10, itersMboTune = 10, repls = 10,
                   showInfo = TRUE, ncpus = NA, seed = 1) {

  data = as.data.frame(data)
  checkmate::assertClass(data, classes = c("data.frame"))
  checkmate::assertClass(target, classes = c("character"))
  EBO::assertReplsNcpusSeed(repls, ncpus, seed)
  checkmate::assertLogical(minimize, len = 1, any.missing = FALSE)
  checkmate::assertLogical(showInfo, len = 1, any.missing = FALSE)
  checkmate::assertIntegerish(itersMboTune, lower = 1, any.missing = TRUE,
                              len = 1)
  checkmate::assertIntegerish(minFuncEvals, lower = 1, any.missing = TRUE,
                              len = 1)
  checkmate::assertIntegerish(funcEvals, lower = 1, any.missing = TRUE,
                              len = 1)

  set.seed(seed)
  startTime = Sys.time()

  data = as.data.frame(data)

  # get parameter space
  psOpt = EBO::generateParamSpace(data, target)
  # train surrogate model of the black-box function
  surrogateModel = mlr::train(mlr::makeLearner("regr.randomForest", nodesize = 5),
                              mlr::makeRegrTask(data = data, target = target))
  # get info of the optimization problem
  info = EBO::getModelInfo(surrogateModel, psOpt, minimize)

  # psTune
  if (any(info$featureType == "factor")) {
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
      ParamHelpers::makeDiscreteParam("surrogate", values = c("regr.randomForest")))
  } else {
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
                                      requires = quote(surrogate == "regr.km")))
  }

  # drop kriging if parameter space of optimization is not only numeric or integer

  # compute number of steps
  steps = as.integer(ceiling((funcEvals - info$featureNumber - 1) / minFuncEvals))

  # execute DMBO tuning
  resTuneDmbo = EBO::tuneDmboMbo(surrogateModel, minFuncEvals, funcEvals, psTune, itersMboTune,
                                 minimize, repls, ncpus, seed, psOpt, info, steps)

  # double the amount of replications for the benchmarks
  repls = repls*2

  # benchmarkMbo default
  paramsSmboDefault = data.table::data.table(design = list("maximinLHS"),
                                            amountDesign = list(NULL),
                                            control = list(NULL),
                                            surrogate = list(NULL)
  )

  resMboDefault = EBO::benchmarkMbo(list(surrogateModel), psOpt, funcEvals, paramsSmboDefault,
                                    minimize, repls, ncpus, seed)


  # benchmark DMBO with the tuned hyperparameters
  hyperparamsDmbo = list()
  for (i in 1:steps) {
    hyperparamsDmbo[[i]] = resTuneDmbo[[i]][[1]]
  }

  resDmboTuned = EBO::computeDmbo(surrogateModel, minFuncEvals, funcEvals, psTune, itersMboTune,
                                  minimize, repls, ncpus, seed, psOpt, info, hyperparamsDmbo, steps)

  # plot DMBO vs. SMBO
  plot = EBO::plotAutoMbo(resMboDefault, resDmboTuned, minFuncEvals, funcEvals, repls, showInfo, info,
                          ncpus, seed, step, startTime, steps, hyperparamsDmbo)

  return(plot)
}
