#' Benchmark and plot \code{mlrMBO::mbo()} optimization runs to investigate hyperparameters
#'
#' This functions benchmarks the \code{mlrMBO::mbo()} function on different configurations and
#' then plots them wrt the hyperparameters.
#'
#'
#' @return A ggplot2 object to investigate hyperparameters.
#'
#' @export
#'



plotMboHyperparams = function(task, funcEvals, psParamPlot, resolution, repls, showInfo = TRUE, ncpus = NA, seed = 1) {

  set.seed(seed)

  startTime <- Sys.time()

  instancesTest = list()

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
