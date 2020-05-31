library(testthat)
library(checkmate)

test_that("Test that every optim algo generate same Output in 1st list entry + default arguments + minimize", {
  set.seed(1)
  data <- data.frame(a=runif(50,10,5555),b=runif(50,-30000,-500))
  data$ratio <- rowSums(data)
  data$ratio <- data$ratio/max(data$ratio)
  colnames(data) <- c("a","t","y")
  model = list(mlr::train(mlr::makeLearner("regr.randomForest"), mlr::makeRegrTask(data = data, target = "y")))

  psOpt = ParamHelpers::makeParamSet(
    ParamHelpers::makeIntegerParam("a", lower = 10, upper = 5555),
    ParamHelpers::makeNumericParam("t", lower = -30000, upper = -500)
  )

  repls = 1

  resCmaesr = benchmarkCmaesr(model, psOpt, 50, repls = repls)
  resMBO = benchmarkMbo(model, psOpt, 10, repls = repls)
  resRandom = benchmarkRandom(model, psOpt, 10, repls = repls)
  resRacing = benchmarkRacing(model, psOpt, 56, repls = repls)

  expect_integerish(as.numeric(resCmaesr[[1]][[1]]["a"]))
  expect_integerish(as.numeric(resMBO[[1]][[1]]["a"]))
  expect_integerish(as.numeric(resRandom[[1]][[1]]["a"]))
  expect_integerish(as.numeric(resRacing[[1]][[1]]["a"]))

  expect_double(as.numeric(resCmaesr[[1]][[1]]["t"]))
  expect_double(as.numeric(resMBO[[1]][[1]]["t"]))
  expect_double(as.numeric(resRandom[[1]][[1]]["t"]))
  expect_double(as.numeric(resRacing[[1]][[1]]["t"]))

  expect_double(as.numeric(resCmaesr[[1]][[1]]["y"]))
  expect_double(as.numeric(resMBO[[1]][[1]]["y"]))
  expect_double(as.numeric(resRandom[[1]][[1]]["y"]))
  expect_double(as.numeric(resRacing[[1]][[1]]["y"]))
}
)
