synthesis = as.data.frame(readxl::read_excel("tests/testthat/data/synthesis.xlsx"))

psOpt = ParamHelpers::makeParamSet(
  ParamHelpers::makeNumericParam("f", lower = 0, upper = 0.25),
  ParamHelpers::makeNumericParam("k", lower = 0, upper = 0.1),
  ParamHelpers::makeNumericParam("du", lower = 0, upper = 1),
  ParamHelpers::makeNumericParam("dv", lower = 0, upper = 1)
)

task = task(
  simulation = "regr.randomForest",
  data = synthesis,
  target = "interface",
  psOpt = psOpt,
  minimize = FALSE
)

funcEvals = 42

psParamPlot = ParamHelpers::makeParamSet(
  ParamHelpers::makeDiscreteParam("surrogate", values = ("regr.km")),
  ParamHelpers::makeDiscreteParam("crit", values = ("makeMBOInfillCritCB")),
  ParamHelpers::makeIntegerParam("amountDesign", lower = 5, upper = 35)
)

resolution = 5

plot1 = EBO::plotMboHyperparams(task, funcEvals, psParamPlot, resolution, repls = 20, ncpus = 20, seed = 1)



kapton = as.data.frame(readxl::read_excel("tests/testthat/data/kapton_argon.xlsx"))

psOpt = ParamHelpers::makeParamSet(
  ParamHelpers::makeIntegerParam("power", lower = 10, upper = 5555),
  ParamHelpers::makeIntegerParam("time", lower = 500, upper = 20210),
  ParamHelpers::makeIntegerParam("pressure", lower = 0, upper = 1000)
)

task = task(
  simulation = "regr.randomForest",
  data = kapton,
  target = "ratio",
  psOpt = psOpt,
  minimize = FALSE
)

funcEvals = 42

psParamPlot = ParamHelpers::makeParamSet(
  ParamHelpers::makeDiscreteParam("surrogate", values = ("regr.km")),
  ParamHelpers::makeDiscreteParam("crit", values = ("makeMBOInfillCritCB")),
  ParamHelpers::makeIntegerParam("amountDesign", lower = 5, upper = 35)
)

resolution = 5

plot2 = EBO::plotMboHyperparams(task, funcEvals, psParamPlot, resolution, repls = 20, ncpus = 20, seed = 1)
