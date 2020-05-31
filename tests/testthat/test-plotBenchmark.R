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
  colnames(data) <- c("power", "time", "pressure", "gas", "cat","testTarget")
  instance = mlr::train(mlr::makeLearner("regr.randomForest"), mlr::makeRegrTask(data = data, target = "testTarget"))

  psOpt = ParamHelpers::makeParamSet(
    ParamHelpers::makeIntegerParam("power", lower = 10, upper = 5555),
    ParamHelpers::makeIntegerParam("time", lower = -30000, upper = -500),
    ParamHelpers::makeNumericParam("pressure", lower = 0, upper = 1000),
    ParamHelpers::makeDiscreteParam("gas", values = c("nitrogen", "air", "argon")),
    ParamHelpers::makeDiscreteParam("cat", values = c("cat1","cat2","cat3"))
  )
  funcEvals = 60

  plotBenchmark2 = plotBenchmark(instance, psOpt, funcEvals, minimize = FALSE, repls = 2, seed = 1)

  expect_equal(class(plotBenchmark), c("gg", "ggplot"))
  expect_equal(length(plotBenchmark), 9)
  expect_equal(length(plotBenchmark$data), 2)
  expect_equal(class(plotBenchmark$data$method), "character")
  expect_equal(class(plotBenchmark$data$y), "numeric")
  expect_equal(length(plotBenchmark$data$y), 15) # numberOptimizers * repls --> 5 * 3 = 15
}
)
