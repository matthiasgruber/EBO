library(ParamHelpers)
library(readr)

set.seed(1)

# define problem 1
graphene <- as.data.frame(readxl::read_excel("tests/testthat/data/grapheneArgon.xlsx"))

psOpt = makeParamSet(

  makeIntegerParam("power", lower = 10, upper = 5555),

  makeIntegerParam("time", lower = 500, upper = 20210),

  makeIntegerParam("pressure", lower = 0, upper = 100)
)

task_Graphene = task(
  simulation = "regr.randomForest",
  data = graphene,
  target = "ratio",
  psOpt = psOpt,
  minimize = FALSE
)

# define problem 2
synthesis <- as.data.frame(readxl::read_excel("tests/testthat/data/synthesis.xlsx"))

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


taskList = list(task_Graphene, task_Synthesis)

################## Define problemList #############

problemList = generateProblemList(taskList)



### 2.4 tuneMBO

psTune = ParamHelpers::makeParamSet(

  ParamHelpers::makeDiscreteParam("design", values = c("maximinLHS",
                                                       "optimumLHS")),

  ParamHelpers::makeDiscreteParam("surrogate", values = c("regr.km")),

  ParamHelpers::makeDiscreteParam("covtype", values = c("matern5_2","matern3_2", "powexp", "gauss")),

  ParamHelpers::makeDiscreteParam("crit", values = c("makeMBOInfillCritCB", "makeMBOInfillCritAdaCB","makeMBOInfillCritEI")),

  ParamHelpers::makeNumericParam("cb.lambda", lower = 0.7, upper = 1.3,
                                 requires = quote(crit == "makeMBOInfillCritCB")),

  ParamHelpers::makeIntegerParam("cb.lambda.start", lower = 2, upper = 200,
                                 requires = quote(crit == "makeMBOInfillCritAdaCB")),

  ParamHelpers::makeNumericParam("cb.lambda.end", lower = 0.001, upper = 1,
                                 requires = quote(crit == "makeMBOInfillCritAdaCB"))
)



tuneResults2 = optimizertuneRace("optimizeMBO", psTune,
                                funcEvals = 55, itersTune = 1000, trainInstanceList = problemList,
                                minimize = FALSE, configurationsFile = "configurations.txt",
                                plotAblation = TRUE, ablationFile = "ablationMBOPlot2.pdf", seed = 1)



