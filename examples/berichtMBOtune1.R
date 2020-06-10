library(ParamHelpers)
library(readr)


# define problem 1
kapton <- as.data.frame(readxl::read_excel("examples/data/kaptonArgon.xlsx"))

psOpt = makeParamSet(
  makeIntegerParam("power", lower = 10, upper = 5555),
  makeIntegerParam("time", lower = 500, upper = 20210),
  makeIntegerParam("pressure", lower = 0, upper = 1000)
)

task_Kapton = task(
  simulation = "regr.randomForest",
  data = kapton,
  target = "ratio",
  psOpt = psOpt,
  minimize = FALSE
)

# define problem 2
synthesis <- as.data.frame(readxl::read_excel("examples/data/synthesis.xlsx"))

psOpt = ParamHelpers::makeParamSet(
  ParamHelpers::makeNumericParam("f", lower = 0, upper = 0.25),
  ParamHelpers::makeNumericParam("k", lower = 0, upper = 0.1),
  ParamHelpers::makeNumericParam("du", lower = 0, upper = 1),
  ParamHelpers::makeNumericParam("dv", lower = 0, upper = 1)
)

task_Synthesis = task(
  simulation = "regr.randomForest",
  data = synthesis,
  target = "interface",
  psOpt = psOpt,
  minimize = FALSE
)

################## Define problemList #############

problemList = generateProblemList(task_Kapton, task_Synthesis)

### 2.4 tuneMBO
set.seed(1)

psTune = ParamHelpers::makeParamSet(

  ParamHelpers::makeDiscreteParam("design", values = c("maximinLHS",
                                                       "optimumLHS")),

  ParamHelpers::makeDiscreteParam("crit", values = c("makeMBOInfillCritEI",
                                                     "makeMBOInfillCritAEI",
                                                     "makeMBOInfillCritCB",
                                                     "makeMBOInfillCritAdaCB")),

  ParamHelpers::makeIntegerParam("cb.lambda", lower = 1, upper = 5,
                                 requires = quote(crit == "makeMBOInfillCritCB")),

  ParamHelpers::makeIntegerParam("cb.lambda.start", lower = 3, upper = 10,
                                 requires = quote(crit == "makeMBOInfillCritAdaCB")),

  ParamHelpers::makeNumericParam("cb.lambda.end", lower = 0, upper = 3,
                                 requires = quote(crit == "makeMBOInfillCritAdaCB")),

  ParamHelpers::makeDiscreteParam("surrogate", values = c("regr.randomForest", "regr.km")),

  ParamHelpers::makeDiscreteParam("covtype" ,values = c("gauss","matern5_2",
                                                        "matern3_2","powexp"),
                                  requires = quote(surrogate == "regr.km"))
)

tuneResults = optimizertuneRace("optimizeMBO", psTune,
                                funcEvals = 55, itersTune = 1000, trainInstanceList = problemList,
                                minimize = FALSE, configurationsFile = "examples/configurations.txt",
                                plotAblation = TRUE, ablationFile = "ablationMBOPlot.pdf", seed = 1)





