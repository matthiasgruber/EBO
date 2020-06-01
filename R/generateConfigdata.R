#' plot boxplot curves of \code{mlrMBO::mbo()} optimization runs
#'
#' This functions benchmarks the \code{mlrMBO::mbo()} function on different configuration and
#' then plots them as boxplots wrt to their iterations.
#'
#' @param model [\code{wrapped model}]\cr
#'       A trained model.
#' @param psOpt [\code{ParamHelpers::ParamSet()}]\cr
#'   Collection of parameters and their constraints for optimization.
#' @param paramsMBO [\code{data.table::data.table()}]\cr
#'   A data.table containing itersMbo, design, amountDesign, control and surrogate as lists.
#'   The data.table has to be defined as the expample below.
#' @param minimize [\code{logical(1)}]\cr
#'   Should the target be minimized? \cr
#'   Default is `TRUE`.
#' @param namesBoxplotCurve [\code{character}]\cr
#'   The names for the \code{mlrMBO} configurations
#'   Default is `default`.
#' @param repls [\code{integer(1)}]\cr
#'  Define how often each configuration is replicated for the benchmark.\cr
#'  Default is ten.
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
#'
#'
#'
#' @return A plot containing one boxplot curve for each configurations benchmarked.
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
#'   set.seed(1)
#' data <- data.frame(a = runif(50,10,5555), b = runif(50,-30000,-500),
#'                    c = runif(50,0,1000),
#'                    d = sample(c("nitrogen","air","argon"), 50, replace = TRUE),
#'                    e = sample(c("cat1","cat2","cat3"), 50, replace = TRUE))
#' data$ratio <- rowSums(data[,1:3]^2)
#' data$ratio <- data$ratio/max(data$ratio)
#' colnames(data) <- c("power", "time", "pressure", "gas", "cat","ratio")
#' model = mlr::train(mlr::makeLearner("regr.randomForest"), mlr::makeRegrTask(data = data, target = "ratio"))
#'
#' psOpt = ParamHelpers::makeParamSet(
#'   ParamHelpers::makeIntegerParam("power", lower = 10, upper = 5555),
#'   ParamHelpers::makeIntegerParam("time", lower = -30000, upper = -500),
#'   ParamHelpers::makeNumericParam("pressure", lower = 0, upper = 1000),
#'   ParamHelpers::makeDiscreteParam("gas", values = c("nitrogen", "air", "argon")),
#'   ParamHelpers::makeDiscreteParam("cat", values = c("cat1","cat2","cat3"))
#' )
#'
#' task = (
#' simulation = "regr.randomForest",
#' target = "target",
#' psOpt = psOpt,
#' minimize = FALSE
#' )
#'
#' ctrl1 = mlrMBO::makeMBOControl()
#' ctrl1 = mlrMBO::setMBOControlInfill(ctrl1, crit = mlrMBO::makeMBOInfillCritAEI())
#' ctrl2 = mlrMBO::makeMBOControl()
#' ctrl2 = mlrMBO::setMBOControlInfill(ctrl1, crit = mlrMBO::makeMBOInfillCritEI())
#'
#' paramsMBO = data.table::data.table(itersMbo = list(3),
#'                                    design = list("maximinLHS"),
#'                                    amountDesign = list(9,
#'                                                        11),
#'                                    control = list(ctrl1,
#'                                                   ctrl2),
#'                                    surrogate = list(mlr::makeLearner("regr.randomForest", predict.type = "se"))
#' )
#'
#' namesBoxplot = c("Augmentet Expected Improvement + RandomForest + maximinLHS with amount = 9",
#'                  "Expected Improvement + RandomForest + maximinLHS with amount = 11")
#'
#' configResults = generateConfigdata(task, funcEvals = 20, paramsMBO,
#'                                namesBoxplot = namesBoxplot, repls = 5)
#'
#' boxplotCurve(configResults)
#'
#' configTest(configResults)
#'
#' addIterstest(configResults, baseIters = 10, addIters = 10, minimize = FALSE)
#'
#' }
generateConfigdata = function(task, funcEvals = NULL, paramsMBO = NULL,
                              namesBoxplot = c("default"),
                              repls = 12, showInfo = TRUE, ncpus = NA, seed = 1) {
  startTime <- Sys.time()
  set.seed(seed)

  #numberInstances = length(instanceList)

  #configDataFrame = vector(mode = "list", length = numberInstances)

  #model = vector(mode = "list", length = numberInstances)

  #for(r in 1:numberInstances) {

  # i = 2
  #  funcEvals = 7
  #  seed=1
  #  repls=2
  #  ncpus= NA
  model = mlr::train(mlr::makeLearner(task$simulation), mlr::makeRegrTask(data = task$data, target = task$target))

  info = getModelInfo(model, task$psOpt, minimize = task$minimize)

  resMBO = benchmarkMbo(list(model), task$psOpt, funcEvals, paramsMBO, minimize = task$minimize,
                        repls, ncpus, seed, delReg = TRUE)

  #####################
  optimizationPath = as.list(NA)
  results = as.list(NA)
  targetColumn = info$featureNumber+1
  numberBoxplotCurve = length(namesBoxplot)

  # i=1
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

  #configDataFrame[[r]] = resultsPlotable


  #configDataFrame = bind_rows(configDataFrame)

  return(resultsPlotable)
}
