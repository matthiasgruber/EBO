library(testthat)
library(checkmate)

test_that("Test if works for mixed ps + every argument in use + maximize + 2D", {
  set.seed(1)
  data <- data.frame(a = runif(50,10,5555), b = runif(50,-30000,-500),
                     c = runif(50,0,1000),
                     d = sample(c("nitrogen","air","argon"), 50, replace = TRUE))
  data$ratio <- rowSums(data[,1:3]^2)
  data$ratio <- data$ratio/max(data$ratio)
  colnames(data) <- c("power", "time", "pressure", "gas","ratio")
  psOpt = ParamHelpers::makeParamSet(
    ParamHelpers::makeIntegerParam("power", lower = 10, upper = 5555),
    ParamHelpers::makeIntegerParam("time", lower = -30000, upper = -500),
    ParamHelpers::makeNumericParam("pressure", lower = 0, upper = 1000),
    ParamHelpers::makeDiscreteParam("gas", values = c("nitrogen", "air", "argon"))
  )


  task = task(
    simulation = "regr.randomForest",
    data = data,
    target = "ratio",
    psOpt = psOpt,
    minimize = FALSE
  )

  funcEvals = 10

  psParamPlot = ParamHelpers::makeParamSet(
    ParamHelpers::makeDiscreteParam("surrogate", values = ("regr.randomForest")),
    ParamHelpers::makeDiscreteParam("crit", values = ("makeMBOInfillCritAdaCB")),
    ParamHelpers::makeIntegerParam("cb.lambda.start", lower = 5, upper = 15, requires = quote(crit == "makeMBOInfillCritAdaCB")),
    ParamHelpers::makeNumericParam("cb.lambda.end", lower = 1, upper = 5, requires = quote(crit == "makeMBOInfillCritAdaCB"))
  )

  resolution = 2

  repls = 2

  showInfo = TRUE

  ncpus = NA

  seed = 1

  contourPlot = plotMboHyperparams(task, funcEvals, psParamPlot, resolution, repls, showInfo, ncpus, seed)

  expect_equal(class(contourPlot), c("gg", "ggplot"))
  expect_equal(length(contourPlot), 9)
  expect_equal(length(contourPlot$data), 3)
  expect_equal(class(contourPlot$data$x1), "integer")
  expect_equal(class(contourPlot$data$x2), "numeric")
  expect_equal(length(contourPlot$data$x1), 4) # resolution ^ numberParams --> 2^2 = 4
}
)

#test_that("Test if works for mixed ps + every argument is default + minimize + 1D", {
#  set.seed(1)
#  data <- data.frame(a = runif(50,10,5555), b = runif(50,-30000,-500),
#                     c = runif(50,0,1000),
#                     d = sample(c("nitrogen","air","argon"), 50, replace = TRUE),
#                     e = sample(c("cat1","cat2","cat3"), 50, replace = TRUE))
#  data$ratio <- rowSums(data[,1:3]^2)
#  data$ratio <- data$ratio/max(data$ratio)
#  colnames(data) <- c("power", "time", "pressure", "gas", "cat","ratio")
#  model = train(makeLearner("regr.randomForest"), makeRegrTask(data = data, target = "ratio"))##
#
#  psOpt = makeParamSet(
#    makeIntegerParam("power", lower = 10, upper = 5555),
#    makeIntegerParam("time", lower = -30000, upper = -500),
#    makeNumericParam("pressure", lower = 0, upper = 1000),
#    makeDiscreteParam("gas", values = c("nitrogen", "air", "argon")),
#    makeDiscreteParam("cat", values = c("cat1","cat2","cat3"))
#  )
#
#  psParamPlot = makeParamSet(
#    makeDiscreteParam("surrogate", values = ("regr.randomForest")),
#    makeIntegerParam("infill.opt.restarts", lower = 3, upper = 12)
#   )
#
#  itersMboOpt = 1
#
#  resolution = 5
#
#  minimize = FALSE
#
#  repls = 3
#
#  showInfo = TRUE
#
#  ncpus = NA
#
#  seed = 1
#
#  contourPlot = plotMboContourPlot(model, psOpt, psParamPlot, itersMboOpt, resolution, minimize, repls, showInfo, ncpus, seed)
#
#  expect_equal(class(contourPlot), c("gg", "ggplot"))
#  expect_equal(length(contourPlot), 9)
#  expect_equal(length(contourPlot$data), 2)
#  expect_equal(class(contourPlot$data$x), "integer")
#  expect_equal(length(contourPlot$data$y), 5) # resolution ^ numberParams --> 5^1 = 5
#}
#)
