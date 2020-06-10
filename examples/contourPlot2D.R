synthesis = as.data.frame(readxl::read_excel("examples/data/synthesis.xlsx"))

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
  ParamHelpers::makeDiscreteParam("surrogate", values = ("regr.randomForest")),
  ParamHelpers::makeDiscreteParam("crit", values = ("makeMBOInfillCritAdaCB")),
  ParamHelpers::makeNumericParam("cb.lambda.start", lower = 3, upper = 10, requires = quote(crit == "makeMBOInfillCritAdaCB")),
  ParamHelpers::makeNumericParam("cb.lambda.end", lower = 0, upper = 3, requires = quote(crit == "makeMBOInfillCritAdaCB"))
)

resolution = 3

plot1 = EBO::plotMboHyperparams(task, funcEvals, psParamPlot, resolution, repls = 10,
                                ncpus = 20, seed = 1)



kapton = as.data.frame(readxl::read_excel("examples/data/kaptonArgon.xlsx"))

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
  ParamHelpers::makeDiscreteParam("surrogate", values = ("regr.randomForest")),
  ParamHelpers::makeDiscreteParam("crit", values = ("makeMBOInfillCritAdaCB")),
  ParamHelpers::makeNumericParam("cb.lambda.start", lower = 3, upper = 10, requires = quote(crit == "makeMBOInfillCritAdaCB")),
  ParamHelpers::makeNumericParam("cb.lambda.end", lower = 0, upper = 3, requires = quote(crit == "makeMBOInfillCritAdaCB"))
)

resolution = 3

plot2 = EBO::plotMboHyperparams(task, funcEvals, psParamPlot, resolution, repls = 10, ncpus = 20, seed = 1)
