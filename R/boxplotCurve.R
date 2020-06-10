#' plot boxplot curves of \code{mlrMBO::mbo()} optimization runs
#'
#' This function uses the EBO::generateConfigdata() result, to create boxplotCurves.
#'
#'
#' @param configResults [\code{EBO::generateConfigdata()}]\cr
#'   Benchmark data for different \code{mlrMBO::mbo() configurations.
#'
#'
#'
#' @return A plot containing one boxplot curve for each configuration.
#'
#' @references [configResults \code{mlrMBO::mbo()}]
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
#' #define infillCrit
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
#' # boxplot curve of configs
#' boxplotCurve(configResults)
#' }



boxplotCurve = function(configResults) {


  # assertions on configResults
  classConfigResults = class(configResults)
  classDemand = c("data.table", "data.frame")
  colnamesConfigResults = colnames(configResults)
  colnamesDemand = c("class","iteration")
  for(i in 1:2) {
    if (
      (classConfigResults[i] != classDemand[i]) | (ncol(configResults) != 3)
      | (colnamesConfigResults[i+1] != colnamesDemand[i])
    ) {
      stop("configResults must be an object of generateConfigdata()")
    }
  }

  # get some infos
  y.name = as.name(colnames(configResults)[1])
  numberBoxplotCurve = nlevels(configResults$class)
  repls = length(which(configResults$iteration == "0"))/numberBoxplotCurve
  namesBoxplot = levels(configResults$class)
  itersMbo = nlevels(configResults$iteration)-1

  colnames(configResults)[1] = 'y_best'

  # run boxplot curve
  bxp = ggplot2::ggplot(configResults, aes(x = iteration, y = y_best, fill = class)) +
    theme(legend.position = "bottom", legend.direction = "vertical") +
    geom_boxplot(outlier.shape = NA) +
    ylab(y.name) +
    stat_summary(fun = median, geom ="line", aes(group=class, color = class))

  return(bxp)
}
