#' Generate benchmark data of \code{mlrMBO::mbo()} optimization runs
#'
#' This function benchmarks the \code{mlrMBO::mbo()} function on different configurations. The
#' resulting data can be used for several EBO functions such as EBO::boxplotCurve(), EBO::testAddIters(),
#' EBO::testConfigs().
#'
#'
#' @param paramsMBO [\code{data.table::data.table()}]\cr
#'   A data.table containing design, amountDesign, control and surrogate as lists.
#'   The data.table has to be defined as the expample below.
#' @param namesBoxplotCurve [\code{character}]\cr
#'   The names for the \code{mlrMBO} configurations
#'   Default is `default`.
#' @param repls [\code{integer(1)}]\cr
#'  Define how often each configuration is run for the benchmark.\cr
#'  Default is 20
#' @param funcEvals [\code{integer(1)}]\cr
#'  Define the number of function evaluations.\cr
#'  Default is 50.
#' @param showInfo [\code{logical(1)}]\cr
#'   Should some information be shown in the plot? \cr
#'   Default is `TRUE`.
#' @param ncpus [\code{numeric(1)}]\cr
#'  Define how many cpu cores are used for the benchmark.\cr
#'  Default is NA, which uses all cores and leave one for netflix.
#' @param seed [\code{numeric(1)}]\cr
#'  Define the seed used for the computation. Will be set by \code{batchtools}.
#'  Which means the jobs get the seed plus the job.id as their unique seed. \cr
#'  Default is one.
#' @param psOpt [\code{ParamHelpers::ParamSet()}]\cr
#'  Collection of parameters and their constraints for optimization.
#' @param simulation [\code{character}]\cr
#'  The black box function e.g. model for the \code{mlrMBO}
#'  Default is `regr.randomForest`.
#' @param minimize [\code{logical(1)}]\cr
#'  Should the target be minimized? \cr
#'  Default is `TRUE`.
#' @param task [\code{EBO:: task()}]\cr
#'  Task defines the problem setting.
#'
#'
#'
#' @return Benchmark data for each configuration.
#'
#' @references [\code{mlrMBO::mbo()}]
#' @references Bernd Bischl, Jakob Richter, Jakob Bossek, Daniel Horn, Janek Thomas and Michel Lang; mlrMBO: A Modular Framework for Model-Based Optimization of Expensive Black-Box Functions, Preprint: \code{\link{https://arxiv.org/abs/1703.03373}} (2017).
#'
#' @export
#'
#' @seealso \code{\link{optimize::plotBenchmark()}} \code{\link{optimize::plotMboContourPlot()}}
#'
#' @examples
#' \dontrun{
#'
#' set.seed(1)
#'
#' library(mlrMBO)
#' library(ParamHelpers)
#' library(mlr)
#'
#' # define infillCrit
#' ctrl = mlrMBO::makeMBOControl()
#' ctrl = mlrMBO::setMBOControlInfill(ctrl, crit = mlrMBO::makeMBOInfillCritEI())
#'
#' # define MBO configuration
#' paramsMBO = data.table::data.table(
#'  design = list("maximinLHS","randomLHS", "random"),
#'  amountDesign = list(12),
#'  control = list(ctrl),
#'  surrogate = list(mlr::makeLearner("regr.km", predict.type = "se"))
#' )
#'
#' namesBoxplot = c("maximinLHS",
#'                 "randomLHS",
#'                 "random")
#'
#' # define runs of each algorithm
#' repls = 10
#'
#' # define function evaluations
#' funcEvals = 32
#'
#'
#' data <- data.frame(a = runif(50,10,5555), b = runif(50,-30000,-500),
#'                    c = runif(50,0,1000))
#' data$ratio <- rowSums(data[,1:3]^2)
#' data$ratio <- data$ratio/max(data$ratio)
#' colnames(data) <- c("power", "time", "pressure","ratio")
#'
#'
#' psOpt = ParamHelpers::makeParamSet(
#'   ParamHelpers::makeIntegerParam("power", lower = 10, upper = 5555),
#'   ParamHelpers::makeIntegerParam("time", lower = -30000, upper = -500),
#'   ParamHelpers::makeNumericParam("pressure", lower = 0, upper = 1000),
#' )
#'
#' task = task(
#'  simulation = "regr.randomForest",
#'  data = data,
#'  target = "ratio",
#'  psOpt = psOpt,
#'  minimize = FALSE
#' )
#'
#' # generate configData
#' configResults = generateConfigdata(task, funcEvals = funcEvals, paramsMBO,
#'                                        namesBoxplot = namesBoxplot, repls = repls)
#' }
generateConfigdata = function(task, funcEvals = 50, paramsMBO = NULL,
                              namesBoxplot = c("default"),
                              repls = 20, showInfo = TRUE, ncpus = NA, seed = 1) {

  if (class(task) != "list") {
    stop("task must be a list!")
  }
  #check if correct number of elements
  if (length(task) != "7") {
    stop("task must have length 7!")
  }
  # check names and class of sublists
  if (names(task[2]) != "p") {
    stop("must pass p in task!")
  }
  if (class(task[[2]]) != "integer") {
    stop("p must be integer!")
  }

  if (names(task[3]) != "minimize") {
    stop("must pass minimize in task!")
  }
  if (class(task[[3]]) != "logical") {
    stop("minimize must be logical!")
  }

  if (names(task[4]) != "data") {
    stop("must pass data!")
  }
  if (class(task[[4]]) != "data.frame") {
    stop("data must be data.frame!")
  }

  if (names(task[5]) != "simulation") {
    stop("must pass simulation in task!")
  }
  if (class(task[[5]]) != "character") {
    stop("simulation must be character!")
  }

  # assertions for parameter space
  if (names(task[6]) != "psOpt") {
    stop("must pass a paramHelpers object in task!")
  }
  if (class(task[[6]]) != "ParamSet") {
    stop("parameter space must be a ParamSet object!")
  }
  # features from parameter space must be identical with features from data
  numberFeatures = ncol(task[[4]])-1
  if ((names(task[[6]][["pars"]][1:numberFeatures]) == names(task[[4]][1:numberFeatures]))==FALSE) {
    stop("data variables must be identical with ParamSet variables!")
  }

  # assertions for target
  if (names(task[7]) != "target") {
    stop("must pass a target in task!")
  }
  if (class(task[[7]]) != "character") {
    stop("target must be character!")
  }
  # targets from tasks must exist in data
  if((task[[7]] %in% names(task[[4]]))== FALSE) {
    stop("target from task must exist in data!")
  }

  EBO::assertReplsNcpusSeed(repls, ncpus, seed)

  checkmate::assertLogical(showInfo, len = 1, any.missing = FALSE)

  checkmate::assertClass(paramsMBO, classes = c("data.table", "data.frame"))

  # namesBoxplot must be array with characters, length must be identical with paramsMBO
  assertCharacter(namesBoxplot, len = nrow(paramsMBO), unique = TRUE, any.missing = FALSE, all.missing = FALSE)

  if (length(namesBoxplot) != nrow(paramsMBO)) {
    stop("namesBoxplot must have same length as paramsMBO")
  }


  startTime <- Sys.time()
  set.seed(seed)

  # define the black box function according to the task
  model = mlr::train(mlr::makeLearner(task$simulation), mlr::makeRegrTask(data = task$data, target = task$target))

  # get some infos
  info = getModelInfo(model, task$psOpt, minimize = task$minimize)

  # run the benchmark
  resMBO = benchmarkMbo(list(model), task$psOpt, funcEvals, paramsMBO, minimize = task$minimize,
                        repls, ncpus, seed, delReg = TRUE)

  ##################### apply data transformations, to get right data structure for further analysis (boxplotCurve(), testAddIters(), testConfigs())
  optimizationPath = as.list(NA)
  results = as.list(NA)
  targetColumn = info$featureNumber+1
  numberBoxplotCurve = length(namesBoxplot)

  for (i in 1:(repls*numberBoxplotCurve)) {
    optimizationPath[[i]] = as.data.frame(resMBO[[i]][["optimizationPathMBO"]]$opt.path)
    # compute the best y found so far. iteration 0 = initial data
    # add a column to the data frame optimization path to save the best y
    optimizationPath[[i]]["bestY"] = NA
    numberExperiments = length(optimizationPath[[i]]$dob)
    numberInitialDesign = sum(optimizationPath[[i]]$dob == 0)
    # compute the best y over the initial data
    if (info$minimize == FALSE) {
      optimizationPath[[i]]$bestY[1:numberInitialDesign] <-
        max(optimizationPath[[i]][1:numberInitialDesign,targetColumn])
    }
    if (info$minimize == TRUE) {
      optimizationPath[[i]]$bestY[1:numberInitialDesign] <-
        min(optimizationPath[[i]][1:numberInitialDesign,targetColumn])
    }
    for (ii in (numberInitialDesign + 1):numberExperiments) {
      if (info$minimize == FALSE) {
        optimizationPath[[i]]$bestY[ii] <-
          max(optimizationPath[[i]][[info$y.name]][ii], optimizationPath[[i]]$bestY[ii - 1])
      }
      if (info$minimize == TRUE) {

        optimizationPath[[i]]$bestY[ii] <-
          min(optimizationPath[[i]][[info$y.name]][ii], optimizationPath[[i]]$bestY[ii - 1])
      }
    }
    y = optimizationPath[[i]]$bestY[numberInitialDesign:numberExperiments]
    x = optimizationPath[[i]]$dob[numberInitialDesign:numberExperiments]

    result = cbind(data.frame(y),data.frame(x))
    results[[i]] = result
  }
  results = unclass(results)
  results = as.data.frame(results)

  itersMbo = resMBO[[1]][["optimizationPathMBO"]][["control"]][["iters"]]
  iterationCharacter = as.character(c(0:itersMbo))

  auxiliaryVariable = length(results)/numberBoxplotCurve

  resultsPlotable = NULL

  for (j in 1:numberBoxplotCurve) {
    if (j == 1) {
      for (i in 1:(itersMbo+1)) {
        resultsPlotable[[j]] =
          rbind(resultsPlotable[[j]], t(results[i,seq(1+(auxiliaryVariable*(j-1)),
                                                      auxiliaryVariable+(auxiliaryVariable*(j-1)),2)]))
      }
    }

    if (j >= 2) {
      resultsPlotable[[j]] = t(results[1,seq(1+(auxiliaryVariable*(j-1)),
                                             auxiliaryVariable+(auxiliaryVariable*(j-1)),2)])
      for (i in 2:(itersMbo+1)) {
        resultsPlotable[[j]] = rbind(resultsPlotable[[j]],
                                     t(results[i,seq(1+(auxiliaryVariable*(j-1)),
                                                     auxiliaryVariable+(auxiliaryVariable*(j-1)),2)]))
      }
    }
    resultsPlotable[[j]] = data.frame(resultsPlotable[[j]])
    resultsPlotable[[j]]$class = namesBoxplot[j]
    resultsPlotable[[j]]$iteration = c(rep(0:itersMbo, each = repls))
    resultsPlotable[[j]]$iteration = ordered(resultsPlotable[[j]]$iteration, levels = iterationCharacter)
  }


  resultsPlotable = data.table::rbindlist(resultsPlotable)
  resultsPlotable$class = as.factor(resultsPlotable$class)

  ##########################
  y.name = info$y.name
  colnames(resultsPlotable)[colnames(resultsPlotable) == 'X1'] = y.name


  return(resultsPlotable)
}
