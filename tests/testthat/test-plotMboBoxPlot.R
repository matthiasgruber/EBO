library(testthat)
library(checkmate)

test_that("Test if works for mixed ps + every argument in use + maximize", {
  set.seed(1)
  data <- data.frame(a = runif(50,10,5555), b = runif(50,-30000,-500),
                     c = runif(50,0,1000),
                     d = sample(c("nitrogen","air","argon"), 50, replace = TRUE),
                     e = sample(c("cat1","cat2","cat3"), 50, replace = TRUE))
  data$ratio <- rowSums(data[,1:3]^2)
  data$ratio <- data$ratio/max(data$ratio)
  colnames(data) <- c("power", "time", "pressure", "gas", "cat","ratio")
  model = list(mlr::train(mlr::makeLearner("regr.randomForest"), mlr::makeRegrTask(data = data, target = "ratio")))

  psOpt = ParamHelpers::makeParamSet(
    ParamHelpers::makeIntegerParam("power", lower = 10, upper = 5555),
    ParamHelpers::makeIntegerParam("time", lower = -30000, upper = -500),
    ParamHelpers::makeNumericParam("pressure", lower = 0, upper = 1000),
    ParamHelpers::makeDiscreteParam("gas", values = c("nitrogen", "air", "argon")),
    ParamHelpers::makeDiscreteParam("cat", values = c("cat1","cat2","cat3"))
  )

  funcEvals = 28

  ctrl1 = mlrMBO::makeMBOControl(resample.at = 40)
  ctrl2 = mlrMBO::makeMBOControl()

  ctrl1 = mlrMBO::setMBOControlInfill(ctrl1, opt.restarts = 6,
                                      crit = mlrMBO::makeMBOInfillCritCB(3))

  ctrl2 = mlrMBO::setMBOControlInfill(ctrl2, crit = mlrMBO::makeMBOInfillCritCB())

  paramsMBO = data.table::data.table(
                         design = list(c(0,0.2),c(0.8,1)),
                         amountDesign = list(25),
                         control = list(ctrl1,
                                        ctrl2),
                         surrogate = list(mlr::makeLearner("regr.randomForest", predict.type = "se"))
  )

  minimize = FALSE

  namesBoxPlot = c("mlrMBO tuned",
                   "mlrMBO default")

  repls = 3

  showInfo = TRUE

  boxplotCurve = plotMboBoxPlot(model, psOpt, funcEvals, paramsMBO, minimize, namesBoxPlot, repls, showInfo)

  expect_equal(class(boxplotCurve), c("gg", "ggplot"))
  expect_equal(length(boxplotCurve), 9)
  expect_equal(length(boxplotCurve$data), 3)
  expect_equal(class(boxplotCurve$data$class), "factor")
  expect_equal(class(boxplotCurve$data$iteration), c("ordered", "factor"))
  expect_equal(length(boxplotCurve$data$X1), 24) # mbo's * repls * (iters + 1) --> 2 * 3 * (3 + 1) = 24
}
)

#test_that("Test if works for mixed ps + every argument is deafult + minimize", {
#  set.seed(1)
#  data <- data.frame(a=runif(50,10,5555),c=runif(50,-30000,-500))
#  data$ratio <- rowSums(data^2)
#  data$ratio <- data$ratio/max(data$ratio)
#  colnames(data) <- c("a","c","target")
#  model = train(makeLearner("regr.randomForest"), makeRegrTask(data = data, target = "target"))#

#  psOpt = makeParamSet(
#    makeIntegerParam("a", lower = 10, upper = 5555),
#    makeNumericParam("c", lower = -30000, upper = -500)
#  )

#  boxplotCurve = plotMboBoxPlot(model, psOpt)

#  expect_equal(class(boxplotCurve), c("gg", "ggplot"))
#  expect_equal(length(boxplotCurve), 9)
#  expect_equal(length(boxplotCurve$data), 3)
#  expect_equal(class(boxplotCurve$data$class), "factor")
#  expect_equal(class(boxplotCurve$data$iteration), c("ordered", "factor"))
#  expect_equal(length(boxplotCurve$data$X1), 110) # mbo's * repls * (iters + 1) --> 1 * 10 * (50 + 1) = 510
#}
#)
