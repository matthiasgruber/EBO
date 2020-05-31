#' Benchmark and plot all optimizers which are implemented in EBO.
#'
#'
#' This functions benchmarks the
#' then plots them as boxplots wrt to their iterations.
#'
#'
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
plotBenchmark = function(instance, psOpt, funcEvals = 65, paramsMBO = data.table::data.table(NULL),
                         paramsCMAESR = data.table::data.table(NULL), paramsES = data.table::data.table(NULL),
                         paramsDE = data.table::data.table(NULL), paramsGE = data.table::data.table(NULL),
                         minimize = TRUE, repls = 25,
                         showInfo = TRUE, ncpus = NA, seed = 1) {
  startTime <- Sys.time()
  info = EBO::getModelInfo(instance, psOpt, minimize)
  # create registry
  reg = batchtools::makeExperimentRegistry(file.dir = NA, seed = seed)

  objEncoded = EBO::createObjDesignEncoded(list(instance), psOpt, info)
  objEncodedSpot = EBO::createObjDesignEncodedSpot(list(instance), psOpt, info)
  objNormal = EBO::createObjDesignNormal(list(instance), psOpt, info)

  # add the encoded objective function to the registry
  batchtools::addProblem(name = "objEncoded", fun = objEncodedFunc, reg = reg)
  # add the encoded objective function to the registry
  batchtools::addProblem(name = "objEncodedSpot", fun = objSPOTFunc, reg = reg)
  # add the normal objective function to the registry
  batchtools::addProblem(name = "objNormal", fun = objNormalFunc, reg = reg)

  configCmaesr = EBO::createConfigCmaesr(funcEvals, paramsCMAESR)
  configMbo = EBO::createConfigMbo(funcEvals, paramsMBO)
  configRandom = EBO::createConfigRandom(funcEvals)
  configRacing = EBO::createConfigRacing(funcEvals)
  configEs = EBO::createConfigEs(funcEvals, paramsES)
  configDe = EBO::createConfigDe(funcEvals, paramsDE)
  configGe = EBO::createConfigGe(funcEvals, paramsGE)

  EBO::computeRandom(reg, objNormal, configRandom, repls)
  EBO::computeMBO(reg, objNormal, configMbo, info, repls)
  if (funcEvals >= 50) EBO::computeRacing(reg, objNormal, configRacing, repls)
  if (funcEvals >= 50) EBO::computeCMAESR(reg, objEncoded, configCmaesr, repls)
  if (funcEvals >= 50) EBO::computeEs(reg, objEncodedSpot, configEs, repls)
  if (funcEvals >= 50) EBO::computeDE(reg, objEncodedSpot, configDe, repls)
  if (funcEvals >= 50) EBO::computeGe(reg, objEncodedSpot, configGe, repls)

  EBO::executeComputation(reg, ncpus)

  resultsRandom = EBO::reduceRandom(ids = seq(from = 1, to = repls))

  resultsMbo = EBO::reduceMbo(ids = seq(from = (repls + 1), to = (repls*2)))

  if (funcEvals >= 50) resultsRacing = EBO::reduceRacing(ids = seq(from = ((repls*2)+1),
                                                              to = (repls*3)))

  if (funcEvals >= 50) resultCmeasr = batchtools::reduceResultsList(ids = seq(from = (repls*3)+1,
                                                                              to = (repls*4)), fun = reduceOptimize)

  if (funcEvals >= 50) resultEs = batchtools::reduceResultsList(ids = seq(from = (repls*4)+1,
                                                                              to = (repls*5)), fun = reduceOptimize)

  if (funcEvals >= 50) resultDe = batchtools::reduceResultsList(ids = seq(from = (repls*5)+1,
                                                                          to = (repls*6)), fun = reduceOptimize)

  if (funcEvals >= 50) resultGe = batchtools::reduceResultsList(ids = seq(from = (repls*6)+1,
                                                                          to = (repls*7)), fun = reduceOptimize)


  batchtools::removeRegistry(0, reg)


  resultCmeasrPlotable = as.data.frame(NA)
  if (funcEvals >= 50) {
    for (i in 1:repls) {
      resultCmeasrPlotable[i,1] = resultCmeasr[[i]][[1]][info$featureNumber+1]
    }
    resultCmeasrPlotable[,2] = "cmaesr"
    colnames(resultCmeasrPlotable) = c("y","method")
  }

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
  if (funcEvals >= 50) {
    for (i in 1:repls) {
      resultRacingPlotable[i] = resultsRacing[[i]][[1]][info$featureNumber+1][[1]]
    }
    resultRacingPlotable = as.data.frame(resultRacingPlotable)
    resultRacingPlotable[,2] = "iRace"
    colnames(resultRacingPlotable) = c("y","method")
  }

  resultEsPlotable = as.data.frame(NA)
  if (funcEvals >= 50) {
    for (i in 1:repls) {
      resultEsPlotable[i,1] = resultEs[[i]][[1]][info$featureNumber+1]
    }
    resultEsPlotable[,2] = "spotES"
    colnames(resultEsPlotable) = c("y","method")
  }

  resultDePlotable = as.data.frame(NA)
  if (funcEvals >= 50) {
    for (i in 1:repls) {
      resultDePlotable[i,1] = resultDe[[i]][[1]][info$featureNumber+1]
    }
    resultDePlotable[,2] = "spotDE"
    colnames(resultDePlotable) = c("y","method")
  }

  resultGePlotable = as.data.frame(NA)
  if (funcEvals >= 50) {
    for (i in 1:repls) {
      resultGePlotable[i,1] = resultGe[[i]][[1]][info$featureNumber+1]
    }
    resultGePlotable[,2] = "spotGE"
    colnames(resultGePlotable) = c("y","method")
  }

  resultsPlotable = as.data.frame(rbind(resultCmeasrPlotable,resultRandomPlotable,resultMboPlotable,
                                        resultRacingPlotable,resultEsPlotable,resultDePlotable,
                                        resultGePlotable))

  endTime <- Sys.time()
  timeTaken <- round(endTime - startTime,2)

  plot = ggplot2::ggplot(resultsPlotable, aes(factor(method), y)) +
    geom_boxplot() +
    ylab(info$y.name)

  if (showInfo == TRUE) {
    plot = EBO::addInfo(plot, info, timeTaken, repls)
  }

  return(plot)
}
