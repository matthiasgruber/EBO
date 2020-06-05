#' Benchmark and plot \code{mlrMBO::mbo()} optimization runs to investigate hyperparameters
#'
#' This functions benchmarks the \code{mlrMBO::mbo()} function on different configurations and
#' then plots them wrt the hyperparameters.
#'
#' @inheritParams tuneMboMbo
#' @inheritParams autoMbo
#'
#' @param psParamPlot [\code{ParamHelpers::ParamSet()}]\cr
#'  Parameter space of the hyperparameters to investigate.
#' @param resolution [\code{integer(1) | integer(2)}]\cr
#'  The size of the grid for investigating the hyperparameter effect.
#'
#' @return A 1D or 2D ggplot2 object to investigate the effect of the hyperparameters.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#' data <- data.frame(a = runif(50,10,5555), b = runif(50,-30000,-500),
#'                    c = runif(50,0,1000),
#'                    d = sample(c("nitrogen","air","argon"), 50, replace = TRUE))
#' data$ratio <- rowSums(data[,1:3]^2)
#' data$ratio <- data$ratio/max(data$ratio)
#' colnames(data) <- c("power", "time", "pressure", "gas","ratio")
#' psOpt = ParamHelpers::makeParamSet(
#'   ParamHelpers::makeIntegerParam("power", lower = 10, upper = 5555),
#'   ParamHelpers::makeIntegerParam("time", lower = -30000, upper = -500),
#'   ParamHelpers::makeNumericParam("pressure", lower = 0, upper = 1000),
#'   ParamHelpers::makeDiscreteParam("gas", values = c("nitrogen", "air", "argon"))
#' )
#'
#' task = task(
#'   simulation = "regr.randomForest",
#'   data = data,
#'   target = "ratio",
#'   psOpt = psOpt,
#'   minimize = FALSE
#' )
#'
#' funcEvals = 10
#'
#' psParamPlot = ParamHelpers::makeParamSet(
#'   ParamHelpers::makeDiscreteParam("surrogate", values = ("regr.randomForest")),
#'   ParamHelpers::makeDiscreteParam("crit", values = ("makeMBOInfillCritAdaCB")),
#'   ParamHelpers::makeIntegerParam("cb.lambda.start", lower = 5, upper = 15,
#'                                  requires = quote(crit == "makeMBOInfillCritAdaCB")),
#'   ParamHelpers::makeNumericParam("cb.lambda.end", lower = 1, upper = 5,
#'                                  requires = quote(crit == "makeMBOInfillCritAdaCB"))
#' )
#'
#' resolution = 2
#'
#' repls = 2
#'
#' showInfo = TRUE
#'
#' ncpus = NA
#'
#' seed = 1
#'
#' contourPlot = plotMboHyperparams(task, funcEvals, psParamPlot, resolution,
#'                                  repls, showInfo, ncpus, seed)
#' }



plotMboHyperparams = function(task, funcEvals, psParamPlot, resolution,
                              repls, showInfo = TRUE, ncpus = NA, seed = 1) {

  EBO::assertReplsNcpusSeed(repls, ncpus, seed)
  checkmate::assertLogical(showInfo, len = 1, any.missing = FALSE)
  checkmate::assertIntegerish(funcEvals, lower = 1, any.missing = TRUE,
                              len = 1)
  checkmate::assertIntegerish(resolution, lower = 1, any.missing = TRUE,
                              len = 1)
  if (getParamNr(psParamPlot) == 0L) {
    stop("No hyperparameters were passed!")
  }

  set.seed(seed)

  startTime <- Sys.time()

  instancesTest = mlr::train(mlr::makeLearner(task$simulation),
                             mlr::makeRegrTask(data = task$data, target = task$target))

  info = EBO::getModelInfo(instancesTest, task$psOpt, task$minimize)

  designMbo = ParamHelpers::generateGridDesign(par.set = psParamPlot, resolution = resolution)
  resDesignMbo = EBO::computeDesignMbo(info, list(instancesTest), task$psOpt, designMbo, psParamPlot, funcEvals, task$minimize, repls, ncpus, seed)

  delete = NA
  for (i in 1:length(psParamPlot[["pars"]])) {
    if (sum(duplicated(resDesignMbo[i])) == (nrow(resDesignMbo[i]) - 1)){
      delete[i] = i
    }
  }

  resDesignMbo = resDesignMbo[-delete]

  numberParam = (length(resDesignMbo) - 1)

  endTime <- Sys.time()
  timeTaken <- round(endTime - startTime,2)

  names = names(resDesignMbo)

  if (numberParam == 1) {
    colnames(resDesignMbo) = c("x","y")
    contourPlot = ggplot2::ggplot(resDesignMbo, aes(x = x, y = y)) +
      geom_line() +
      ylab(paste("median",info$y.name)) +
      xlab(paste(names[1]))
  }

  if (numberParam == 2) {
    colnames(resDesignMbo) = c("x1","x2","y")
    contourPlot = ggplot2::ggplot(resDesignMbo, aes(x = x1, y = x2)) +
      geom_raster(aes(fill=y), interpolate = TRUE) +
      ylab(paste(names[2])) +
      xlab(paste(names[1]))

  }

  if (showInfo == TRUE) {
    contourPlot = addInfo(contourPlot, info, timeTaken, repls)
  }

  return(contourPlot)
}
