# get the data
synthesis = openxlsx::read.xlsx("examples/data/synthesis.xlsx")

psOpt = ParamHelpers::makeParamSet(
  ParamHelpers::makeNumericParam("f", lower = 0, upper = 0.25),
  ParamHelpers::makeNumericParam("k", lower = 0, upper = 0.1),
  ParamHelpers::makeNumericParam("du", lower = 0, upper = 1),
  ParamHelpers::makeNumericParam("dv", lower = 0, upper = 1)
)

synthesisTask = task(
  simulation = "regr.randomForest",
  data = synthesis,
  target = "interface",
  psOpt = psOpt,
  minimize = FALSE
)


funcEvals = 56

plotBenchmark2 = EBO::plotBenchmark(synthesisTask, funcEvals, repls = 20)


# get the data
kaptonArgon = openxlsx::read.xlsx("examples/data/kaptonArgon.xlsx")

psOpt = ParamHelpers::makeParamSet(
  ParamHelpers::makeIntegerParam("power", lower = 10, upper = 5555),
  ParamHelpers::makeIntegerParam("time", lower = 500, upper = 20210),
  ParamHelpers::makeIntegerParam("pressure", lower = 0, upper = 1000)
)

kaptonTask = task(
  simulation = "regr.randomForest",
  data = kaptonArgon,
  target = "ratio",
  psOpt = psOpt,
  minimize = FALSE
)

funcEvals = 56

plotBenchmark1 = plotBenchmark(kaptonTask, funcEvals, repls = 20, seed = 5)
