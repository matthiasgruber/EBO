library(testthat)
library(checkmate)

test_that("Test if works for mixed ps + every argument in use + maximize", {
  set.seed(1)
  data <- data.frame(a=runif(50,10,100),b=runif(50,40,750),d=runif(50,0,90))
  data$ratio <- rowSums(data^2)
  data$ratio <- data$ratio/max(data$ratio)
  colnames(data) <- c("power","time","pressure","ratio")

  model = list(mlr::train(mlr::makeLearner("regr.randomForest"), mlr::makeRegrTask(data = data, target = "ratio")))

  psOpt = ParamHelpers::makeParamSet(
    ParamHelpers::makeNumericParam("power", lower = 10, upper = 100),
    ParamHelpers::makeNumericParam("time", lower = 40, upper = 750),
    ParamHelpers::makeNumericParam("pressure", lower = 0, upper = 90)
  )

  funcEvals = 10

  psTune = ParamHelpers::makeParamSet(
    ParamHelpers::makeIntegerParam("amountDesign", lower = 5, upper = 6),
    ParamHelpers::makeDiscreteParam("design", values = c("maximinLHS",
                                                         "optimumLHS")),

    ParamHelpers::makeDiscreteParam("crit", values = c("makeMBOInfillCritEI",
                                                       "makeMBOInfillCritAEI",
                                                       "makeMBOInfillCritCB",
                                                       "makeMBOInfillCritAdaCB")),

    ParamHelpers::makeDiscreteParam("surrogate", values = c("regr.randomForest", "regr.km")),

    ParamHelpers::makeDiscreteParam("covtype" ,values = c("gauss","matern5_2","matern3_2","powexp"),
                                    requires = quote(surrogate == "regr.km"))
  )

  itersMboTune = 2

  minimize = FALSE

  repls = 2

  resTune = tuneMboMbo(model, psOpt, funcEvals, psTune, itersMboTune, minimize, repls)

  expect_equal(class(resTune), "data.frame")
  expect_equal(nrow(resTune), 1)
  expect_equal(length(resTune), 6)
  expect_equal(class(resTune[1,1]), "integer")
  expect_equal(class(resTune[1,2]), "character")
  expect_equal(class(resTune[1,3]), "character")
  expect_equal(class(resTune[1,4]), "character")
  expect_equal(class(resTune[1,5]), "character")
  expect_equal(class(resTune[1,6]), "numeric")
})
