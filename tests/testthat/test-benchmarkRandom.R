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

  funcEvals = 10

  minimize = FALSE

  repls = 4

  resRandom = benchmarkRandom(model, psOpt, funcEvals, minimize, repls)

  expect_equal(length(resRandom), 4)
  expect_equal(length(resRandom[[1]]),2)
  expect_equal(length(resRandom[[1]][["recommendedParametersRandom"]]), 6)
  expect_integerish(resRandom[[1]][["recommendedParametersRandom"]]$power[[1]])
  expect_integerish(resRandom[[1]][["recommendedParametersRandom"]][["time"]][[1]])
  expect_double(resRandom[[1]][["recommendedParametersRandom"]][["pressure"]][[1]])
  expect_double(resRandom[[1]][["recommendedParametersRandom"]][["ratio"]][[1]])
  expect_factor(resRandom[[1]][["recommendedParametersRandom"]][["cat"]][[1]])
  expect_factor(resRandom[[1]][["recommendedParametersRandom"]][["gas"]][[1]])
  expect_equal(length(resRandom[[1]][["optimizationPathRandom"]]), 6)
  expect_integerish(resRandom[[1]][["optimizationPathRandom"]]$power[[1]])
  expect_integerish(resRandom[[1]][["optimizationPathRandom"]][["time"]][[1]])
  expect_double(resRandom[[1]][["optimizationPathRandom"]][["pressure"]][[1]])
  expect_double(resRandom[[1]][["optimizationPathRandom"]][["ratio"]][[1]])
  expect_factor(resRandom[[1]][["optimizationPathRandom"]][["cat"]][[1]])
  expect_factor(resRandom[[1]][["optimizationPathRandom"]][["gas"]][[1]])

}
)

