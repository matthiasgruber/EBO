#' Benchmark and plot all optimization algoritms connected to EBO.
#'
#'
#' This functions benchmarks the optimization algorithms and
#' then plots them as boxplots.
#'
#' @inheritParams tuneMboMbo
#'
#' @return A plot containing one boxplot curve for each configurations benchmarked.
#'
#' @references Bernd Bischl, Jakob Richter, Jakob Bossek, Daniel Horn, Janek Thomas and Michel Lang; mlrMBO: A Modular Framework for Model-Based Optimization of Expensive Black-Box Functions, Preprint: \code{\link{https://arxiv.org/abs/1703.03373}} (2017).
#'
#' @export
#'
#'
#' @example
#' \dontrun{
#' }
plotBenchmark = function(task, funcEvals = 65, paramsMBO = data.table::data.table(NULL),
                         paramsCMAESR = data.table::data.table(NULL), paramsES = data.table::data.table(NULL),
                         paramsDE = data.table::data.table(NULL), paramsGE = data.table::data.table(NULL),
                         repls = 25, showInfo = TRUE, ncpus = NA, seed = 5) {

  EBO::assertReplsNcpusSeed(repls, ncpus, seed)
  checkmate::assertLogical(showInfo, len = 1, any.missing = FALSE)
  checkmate::assertIntegerish(funcEvals, lower = 60, any.missing = TRUE,
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
  info = EBO::getModelInfo(instance, task$psOpt, task$minimize)

  # create registry
  reg = batchtools::makeExperimentRegistry(file.dir = NA, seed = seed)

  # create designs for objective function
  objEncoded = EBO::createObjDesignEncoded(list(instance), task$psOpt, info)
  objEncodedSpot = EBO::createObjDesignEncodedSpot(list(instance), task$psOpt, info)
  objNormal = EBO::createObjDesignNormal(list(instance), task$psOpt, info)

  # add the numeric encoded objective function to the registry
  batchtools::addProblem(name = "objEncoded", fun = objEncodedFunc, reg = reg)
  # add the numeric and integer encoded objective function to the registry
  batchtools::addProblem(name = "objEncodedSpot", fun = objSPOTFunc, reg = reg)
  # add the normal objective function to the registry
  batchtools::addProblem(name = "objNormal", fun = objNormalFunc, reg = reg)

  # create configurations for optimization algos
  configCmaesr = EBO::createConfigCmaesr(funcEvals, paramsCMAESR)
  configMbo = EBO::createConfigMbo(funcEvals, paramsMBO)
  configRandom = EBO::createConfigRandom(funcEvals)
  configRacing = EBO::createConfigRacing(funcEvals)
  configEs = EBO::createConfigEs(funcEvals, paramsES)
  configDe = EBO::createConfigDe(funcEvals, paramsDE)
  configGe = EBO::createConfigGe(funcEvals, paramsGE)

  # add optimization algos to registry
  EBO::computeRandom(reg, objNormal, configRandom, repls)
  EBO::computeMBO(reg, objNormal, configMbo, info, repls)
  EBO::computeRacing(reg, objNormal, configRacing, repls)
  EBO::computeCMAESR(reg, objEncoded, configCmaesr, repls)
  EBO::computeEs(reg, objEncodedSpot, configEs, repls)
  EBO::computeDE(reg, objEncodedSpot, configDe, repls)
  EBO::computeGe(reg, objEncodedSpot, configGe, repls)

  # execute computation
  EBO::executeComputation(reg, ncpus)

  # reduce results
  resultsRandom = EBO::reduceRandom(ids = seq(from = 1, to = repls))
  resultsMbo = EBO::reduceMbo(ids = seq(from = (repls + 1), to = (repls*2)))
  resultsRacing = EBO::reduceRacing(ids = seq(from = ((repls*2)+1),
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
    plot = EBO::addInfo(plot, info, timeTaken, repls)
  }

  return(plot)
}
