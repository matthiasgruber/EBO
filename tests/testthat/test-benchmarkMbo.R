library(testthat)
library(checkmate)

test_that("Test if works for mixed ps + every argument in use + maximize", {
  set.seed(1)
  data <- data.frame(a = runif(50,10,5555), b = runif(50,-30000,-500),
                     c = runif(50,0,1000))#,
                     #d = sample(c("nitrogen","air","argon"), 50, replace = TRUE),
                     #e = sample(c("cat1","cat2","cat3"), 50, replace = TRUE))
  data$ratio <- rowSums(data[,1:3]^2)
  data$ratio <- data$ratio/max(data$ratio)
  colnames(data) <- c("power", "time", "pressure","ratio")
  model = mlr::train(mlr::makeLearner("regr.randomForest"), mlr::makeRegrTask(data = data, target = "ratio"))

  psOpt = ParamHelpers::makeParamSet(
    ParamHelpers::makeIntegerParam("power", lower = 10, upper = 5555),
    ParamHelpers::makeIntegerParam("time", lower = -30000, upper = -500),
    ParamHelpers::makeNumericParam("pressure", lower = 0, upper = 1000)
  )

  minimize = FALSE

  ctrl1 = mlrMBO::makeMBOControl(resample.at = 3)
  ctrl1 = mlrMBO::setMBOControlInfill(ctrl1, crit = mlrMBO::makeMBOInfillCritCB(cb.lambda = 2))

  funcEvals = 25

  paramsMBO = data.table::data.table(
      design = list("maximinLHS"),
      amountDesign = list(23),
      control = list(ctrl1),
      surrogate = list(mlr::makeLearner("regr.randomForest", predict.type = "se"))
      )

  repls = 5

  resMBO = benchmarkMbo(list(model), psOpt, funcEvals, paramsMBO, minimize, repls)

  expect_equal(length(resMBO), 5)
  expect_equal(length(resMBO[[1]]),2)
  expect_equal(length(resMBO[[1]][[2]]),9)
  expect_equal(length(resMBO[[1]][[1]]),4)
  expect_equal(median(resMBO[[1]][[1]]$ratio), 0.799, tolerance = .001)
  expect_integerish(resMBO[[1]][[1]]$power)
  expect_integerish(resMBO[[1]][[1]]$time)
  expect_double(resMBO[[1]][[1]]$pressure)
}
)
