#' Benchmark and plot all optimization algoritms connected to EBO.
#'
#'
#' This functions benchmarks the optimization algorithms: iRace, spotES, spotDE, spotGE, random, cmaesr and
#' mlrMBO and then plots them as boxplots.
#'
#' @inheritParams tuneMboMbo
#' @inheritParams generateConfigdata
#'
#' @param paramsES [\code{data.table::data.table()}]\cr
#'   A data.table containing the hyperparameters: nu, mue, sigmaInit, nSigma, mutation, tau, stratReco and objReco.\cr
#'   Default is [\code{data.table::data.table(NULL)}], which defines the default hyperparameters.
#' @param paramsDE [\code{data.table::data.table()}]\cr
#'   A data.table containing the hyperparameter: populationSize.\cr
#'   Default is [\code{data.table::data.table(NULL)}], which defines the default hyperparameters.
#' @param paramsGE [\code{data.table::data.table()}]\cr
#'   A data.table containing the hyperparameter: populationSize.\cr
#'   Default is [\code{data.table::data.table(NULL)}], which defines the default hyperparameters.
#' @param paramsCMAESR [\code{data.table::data.table()}]\cr
#'   A data.table containing the hyperparameters: sigma and lambda.\cr
#'   Default is [\code{data.table::data.table(NULL)}], which defines the default hyperparameters.
#'
#' @return A plot containing one boxplot for each algorithm.
#'
#' @export
#'
#'
#' @examples
#' \dontrun{
#'   set.seed(1)
#' data <- data.frame(a = runif(50,10,5555), b = runif(50,-30000,-500),
#'                   c = runif(50,0,1000),
#'                   d = sample(c("nitrogen","air","argon"), 50, replace = TRUE),
#'                   e = sample(c("cat1","cat2","cat3"), 50, replace = TRUE))
#' data$ratio <- rowSums(data[,1:3]^2)
#' data$ratio <- data$ratio/max(data$ratio)
#' colnames(data) <- c("power", "time", "pressure", "gas", "cat","testTarget")
#' instance = mlr::train(mlr::makeLearner("regr.randomForest"), mlr::makeRegrTask(data = data, target = "testTarget"))
#'
#' psOpt = ParamHelpers::makeParamSet(
#'   ParamHelpers::makeIntegerParam("power", lower = 10, upper = 5555),
#'   ParamHelpers::makeIntegerParam("time", lower = -30000, upper = -500),
#'   ParamHelpers::makeNumericParam("pressure", lower = 0, upper = 1000),
#'   ParamHelpers::makeDiscreteParam("gas", values = c("nitrogen", "air", "argon")),
#'   ParamHelpers::makeDiscreteParam("cat", values = c("cat1","cat2","cat3"))
#' )
#'
#' funcEvals = 60
#'
#' task = task(
#'   simulation = "regr.randomForest",
#'   data = data,
#'   target = "testTarget",
#'   psOpt = psOpt,
#'   minimize = FALSE
#' )
#' plotBenchmark2 = plotBenchmark(task, funcEvals, repls = 2, seed = 1)
#' }
#'
plotBenchmark = function(task, funcEvals = 65, paramsMBO = data.table::data.table(NULL),
                         paramsCMAESR = data.table::data.table(NULL), paramsES = data.table::data.table(NULL),
                         paramsDE = data.table::data.table(NULL), paramsGE = data.table::data.table(NULL),
                         repls = 25, showInfo = TRUE, ncpus = NA, seed = 5) {

  assertTask(task)
  assertReplsNcpusSeed(repls, ncpus, seed)
  checkmate::assertLogical(showInfo, len = 1, any.missing = FALSE)
  checkmate::assertIntegerish(funcEvals, lower = 56, any.missing = TRUE,
                              len = 1)
  checkmate::assertClass(paramsMBO, classes = c("data.table", "data.frame"))
  checkmate::assertClass(paramsCMAESR, classes = c("data.table", "data.frame"))
  checkmate::assertClass(paramsES, classes = c("data.table", "data.frame"))
  checkmate::assertClass(paramsDE, classes = c("data.table", "data.frame"))
  checkmate::assertClass(paramsGE, classes = c("data.table", "data.frame"))


  startTime <- Sys.time()
  set.seed(1)

  # create surrogate of the black-box function
  instance = mlr::train(mlr::makeLearner(task$simulation),
                        mlr::makeRegrTask(data = task$data, target = task$target))

  # get information of the optimization problem
  info = getModelInfo(instance, task$psOpt, task$minimize)

  # create registry
  reg = batchtools::makeExperimentRegistry(file.dir = NA, seed = seed)

  # create designs for objective function
  objEncoded = createObjDesignEncoded(list(instance), task$psOpt, info)
  objEncodedSpot = createObjDesignEncodedSpot(list(instance), task$psOpt, info)
  objNormal = createObjDesignNormal(list(instance), task$psOpt, info)

  # add the numeric encoded objective function to the registry
  batchtools::addProblem(name = "objEncoded", fun = objEncodedFunc, reg = reg)
  # add the numeric and integer encoded objective function to the registry
  batchtools::addProblem(name = "objEncodedSpot", fun = objSPOTFunc, reg = reg)
  # add the normal objective function to the registry
  batchtools::addProblem(name = "objNormal", fun = objNormalFunc, reg = reg)

  # create configurations for optimization algos
  configCmaesr = createConfigCmaesr(funcEvals, paramsCMAESR)
  configMbo = createConfigMbo(funcEvals, paramsMBO)
  configRandom = createConfigRandom(funcEvals)
  configRacing = createConfigRacing(funcEvals)
  configEs = createConfigEs(funcEvals, paramsES)
  configDe = createConfigDe(funcEvals, paramsDE)
  configGe = createConfigGe(funcEvals, paramsGE)

  # add optimization algos to registry
  computeRandom(reg, objNormal, configRandom, repls)
  computeMBO(reg, objNormal, configMbo, info, repls)
  computeRacing(reg, objNormal, configRacing, repls)
  computeCMAESR(reg, objEncoded, configCmaesr, repls)
  computeEs(reg, objEncodedSpot, configEs, repls)
  computeDE(reg, objEncodedSpot, configDe, repls)
  computeGE(reg, objEncodedSpot, configGe, repls)

  # execute computation
  executeComputation(reg, ncpus)

  # get error massages
  errors = batchtools::getErrorMessages()
  if (nrow(errors) > 0) return(errors)

  # reduce results
  resultsRandom = reduceRandom(ids = seq(from = 1, to = repls))
  resultsMbo = reduceMbo(ids = seq(from = (repls + 1), to = (repls*2)))
  resultsRacing = reduceRacing(ids = seq(from = ((repls*2)+1),
                                              to = (repls*3)))
  resultCmeasr = batchtools::reduceResultsList(ids = seq(from = (repls*3)+1,
                                                         to = (repls*4)), fun = reduceOptimize)
  resultEs = batchtools::reduceResultsList(ids = seq(from = (repls*4)+1,
                                                     to = (repls*5)), fun = reduceOptimize)
  resultDe = batchtools::reduceResultsList(ids = seq(from = (repls*5)+1,
                                                     to = (repls*6)), fun = reduceOptimize)
  resultGe = batchtools::reduceResultsList(ids = seq(from = (repls*6)+1,
                                                     to = (repls*7)), fun = reduceOptimize)
  # remove registry
  batchtools::removeRegistry(0, reg)
  # transform results for plot
  resultCmeasrPlotable = as.data.frame(NA)
  for (i in 1:repls) {
    resultCmeasrPlotable[i,1] = resultCmeasr[[i]][[1]][info$featureNumber+1]
  }
  resultCmeasrPlotable[,2] = "cmaesr"
  colnames(resultCmeasrPlotable) = c("y","method")

  resultRandomPlotable = as.data.frame(NA)
  for (i in 1:repls) {
    resultRandomPlotable[i,1] = resultsRandom[[i]][["recommendedParametersRandom"]][info$featureNumber+1][[1]]
  }
  resultRandomPlotable[,2] = "random"
  colnames(resultRandomPlotable) = c("y","method")

  resultMboPlotable = as.data.frame(NA)
  for (i in 1:repls) {
    resultMboPlotable[i,1] = resultsMbo[[i]][["recommendedParameters"]][info$featureNumber+1]
  }
  resultMboPlotable[,2] = "mlrMBO"
  colnames(resultMboPlotable) = c("y","method")

  resultRacingPlotable = NA
  for (i in 1:repls) {
    resultRacingPlotable[i] = resultsRacing[[i]][[1]][info$featureNumber+1][[1]]
  }
  resultRacingPlotable = as.data.frame(resultRacingPlotable)
  resultRacingPlotable[,2] = "iRace"
  colnames(resultRacingPlotable) = c("y","method")

  resultEsPlotable = as.data.frame(NA)
  for (i in 1:repls) {
    resultEsPlotable[i,1] = resultEs[[i]][[1]][info$featureNumber+1]
  }
  resultEsPlotable[,2] = "spotES"
  colnames(resultEsPlotable) = c("y","method")

  resultDePlotable = as.data.frame(NA)
  for (i in 1:repls) {
    resultDePlotable[i,1] = resultDe[[i]][[1]][info$featureNumber+1]
  }
  resultDePlotable[,2] = "spotDE"
  colnames(resultDePlotable) = c("y","method")

  resultGePlotable = as.data.frame(NA)
  for (i in 1:repls) {
    resultGePlotable[i,1] = resultGe[[i]][[1]][info$featureNumber+1]
  }
  resultGePlotable[,2] = "spotGE"
  colnames(resultGePlotable) = c("y","method")

  # combine results into one data frame
  resultsPlotable = as.data.frame(rbind(resultCmeasrPlotable,resultRandomPlotable,resultMboPlotable,
                                        resultRacingPlotable,resultEsPlotable,resultDePlotable,
                                        resultGePlotable))

  endTime <- Sys.time()
  timeTaken <- round(endTime - startTime,2)
  # plot
  plot = ggplot2::ggplot(resultsPlotable, aes(factor(method), y)) +
    geom_boxplot() +
    ylab(info$y.name)
  # add info to plot
  if (showInfo == TRUE) {
    plot = addInfo(plot, info, timeTaken, repls)
  }

  return(plot)
}
