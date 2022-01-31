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
  data$gas = as.factor(data$gas)
  data$cat = as.factor(data$cat)
  model = list(mlr::train(mlr::makeLearner("regr.randomForest"), mlr::makeRegrTask(data = data, target = "ratio")))

  psOpt = ParamHelpers::makeParamSet(
    ParamHelpers::makeIntegerParam("power", lower = 10, upper = 5555),
    ParamHelpers::makeIntegerParam("time", lower = -30000, upper = -500),
    ParamHelpers::makeNumericParam("pressure", lower = 0, upper = 1000),
    ParamHelpers::makeDiscreteParam("gas", values = c("nitrogen", "air", "argon")),
    ParamHelpers::makeDiscreteParam("cat", values = c("cat1","cat2","cat3"))
  )

  minimize = FALSE

  funcEvals = 100

  repls = 4

  resRacing = benchmarkRacing(model, psOpt, funcEvals, minimize, repls)

  expect_equal(length(resRacing), 4)
  expect_equal(length(resRacing[[1]]),1)
  expect_equal(resRacing[[1]][[1]]$ratio, 0.573, tolerance = .01)
  expect_integerish(resRacing[[1]][[1]]$power)
  expect_integerish(resRacing[[1]][[1]]$time)
  expect_double(resRacing[[1]][[1]]$pressure)
  expect_factor(resRacing[[1]][[1]]$gas)
  expect_factor(resRacing[[1]][[1]]$cat)
}
)
