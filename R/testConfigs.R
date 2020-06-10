#' run statistical tests for \code{mlrMBO::mbo()} configurations
#'
#' This function runs statistical tests for the \code{mlrMBO::mbo()} function on different
#' configurations.
#'
#'
#' @param configResults [\code{EBO::generateConfigdata()}]\cr
#'   Benchmark data for different \code{mlrMBO::mbo() configurations.
#'
#'
#'
#'
#' @return Test results and summaries for different \code{mlrMBO} configurations.
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
#' # boxplot curve of configs
#' boxplotCurve(configResults)
#'
#' # run statistical tests for configs
#' testConfigs(configResults)
#'
#' # analyze effect of additional iterations
#' testAddIters(configResults, baseIters = 10, addIters = 10, minimize = FALSE)
#' }


testConfigs = function(configResults) {

  # assertions on ConfigResults
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


  # get critical info from configResults dataframe
  y.name = as.name(colnames(configResults)[1])
  numberBoxplotCurve = nlevels(configResults$class)
  repls = length(which(configResults$iteration == "0"))/numberBoxplotCurve
  namesBoxplot = levels(configResults$class)
  itersMbo = nlevels(configResults$iteration)-1

  colnames(configResults)[1] = 'y_best'

  # create new test_dataframe
  testdata = configResults[configResults$iteration == itersMbo,]

  # get some summaries of SMBO configurations
  summary = testdata %>%
    ggpubr::group_by(class) %>%
    rstatix::get_summary_stats(y_best, type = c("five_number"))

  # in the case, there we have two configurations, a two sided wilcox test is run in order to get
  # statistical evidence, wether the configurations differ
  if(numberBoxplotCurve == "2") {

    boxplot = ggpubr::ggpaired(testdata, x = "class", y = "y_best", point.size = 0, line.size = 0,
                               order = namesBoxplot,
                               ylab = "y_best", xlab = "class")

    testdata$class = ordered(testdata$class, levels = namesBoxplot)

    wilcox.test(testdata$y_best ~ testdata$class, alternative = "two.sided", data = testdata, paired = FALSE)


    return(list(summary, plot(boxplot), test))


  } else {

    # in the case, there we have more configurations, a two sided test is run in order to get
    # statistical evidence, wether the configurations differ
    # furthermore a pairwise test is run to test two configurations each time

    boxplot = ggpubr::ggboxplot(testdata, x = "class", y = "y_best",
                                color = "class", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                                order = namesBoxplot,
                                ylab = "y_best", xlab = "class")

    testdata$class = ordered(testdata$class, levels = namesBoxplot)

    test = kruskal.test(y_best ~ class, data = testdata)

    comparison = pairwise.wilcox.test(testdata$y_best, testdata$class,
                                      p.adjust.method = "BH")

    return(list(summary, boxplot, test, comparison))
  }

}

