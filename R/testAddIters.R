#' run statistical tests for \code{mlrMBO::mbo()} iterations
#'
#' This function benchmarks the \code{mlrMBO::mbo()} function on different iterations for different
#' configuration and run statistical tests for iterations.
#'
#'
#' @param configResults [\code{EBO::generateConfigdata()}]\cr
#'   Benchmark data for different \code{mlrMBO::mbo() configurations.
#' @param baseIters [\code{integer(1)}]\cr
#'  Define the number of baseline iterations.\cr
#'  Default is 10.
#' @param addIters [\code{integer(1)}]\cr
#'  Define the number of additional iterations.\cr
#'  Default is 10.
#' @param minimize [\code{logical(1)}]\cr
#'  Should the target be minimized? \cr
#'  Default is `TRUE`.

#'
#'
#'
#' @return Test results and summaries for effect of iterations.
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
#'
#' # analyze effect of additional iterations
#' testAddIters(configResults, baseIters = 10, addIters = 10, minimize = FALSE)
#' }


testAddIters = function(configResults, baseIters = 20, addIters = 10, minimize = TRUE) {

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

  checkmate::assertLogical(minimize, len = 1, any.missing = FALSE)

  # assertion on iters: sum of baseIters and addIters must result in itersMbo
  itersMbo = nlevels(configResults$iteration)-1
  if ( (baseIters + addIters) != itersMbo) {
    stop("sum of baseIters and addIters must be amount of iterations")
  }


  # get infos from the configResults dataframe
  y.name = as.name(colnames(configResults)[1])

  numberBoxplotCurve = nlevels(configResults$class)
  repls = length(which(configResults$iteration == "0"))/numberBoxplotCurve
  namesBoxplot = levels(configResults$class)

  colnames(configResults)[1] = 'y_best'

  # create date for running Tests

  testdata = configResults[((configResults$iteration == baseIters) | (configResults$iteration == (baseIters+addIters))),]

  colnames(testdata) = c("y_best", "class", "iters")

  testdata$iters = ordered(testdata$iters, levels = c(baseIters,(baseIters+addIters)))

  test = vector(mode = "list", length = numberBoxplotCurve)
  summary = vector(mode = "list", length = numberBoxplotCurve)

  # get some summaries of iterations for each configuration
  for(r in 1:numberBoxplotCurve) {
    test[[r]] = testdata[(testdata$class == namesBoxplot[r]),]
    names(test)[r] = namesBoxplot[r]
    summary[[r]] = test[[r]] %>%
      ggpubr::group_by(iters, class) %>%
      rstatix::get_summary_stats(y_best, type = "five_number")
  }

  # Run a one sided and paired wilcox test in order to get
  # statistical evidence, wether the results from baseline iterations differ to the results from additional iterations.
  # The direction of test depends on wether one wants to maximize or minimize the target
  min_max = ifelse(minimize == FALSE, "g", "l")

  testResults = vector(mode = "list", length = numberBoxplotCurve)

  for(r in 1:numberBoxplotCurve) {
    #testResults[[r]] = wilcox.test(test[[r]]$y_best ~ test[[r]]$iters, alternative = min_max, data = test[[r]], paired = TRUE)
    testResults[[r]] = coin::wilcoxsign_test(test[[r]]$y_best ~ test[[r]]$iters, alternative = min_max, zero.method = c("Pratt"),
                                       data = test[[r]], paired = TRUE)

    names(testResults)[r] = namesBoxplot[r]
  }

  p = ggplot(data = testdata, aes(x=class, y=y_best)) +
    geom_boxplot(aes(fill=iters))

  bxp = p + facet_wrap( ~ class, scales = "free")

  return(list(bxp, summary, testResults))
}
