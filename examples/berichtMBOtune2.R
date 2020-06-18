# define problem 1
graphene <- as.data.frame(readxl::read_excel("examples/data/grapheneArgon.xlsx"))

psOpt = ParamHelpers::makeParamSet(
  ParamHelpers::makeIntegerParam("power", lower = 10, upper = 5555),
  ParamHelpers::makeIntegerParam("time", lower = 500, upper = 20210),
  ParamHelpers::makeIntegerParam("pressure", lower = 0, upper = 100)
)

task_Graphene = task(
  simulation = "regr.randomForest",
  data = graphene,
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

problemList = generateProblemList(task_Graphene, task_Synthesis)



### 2.4 tuneMBO

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


# execute tuning
tuneResults2 = optimizertuneRace("optimizeMBO", psTune,
                                funcEvals = 55, itersTune = 1000, trainInstanceList = problemList,
                                minimize = FALSE, configurationsFile = "examples/configurations.txt",
                                plotAblation = TRUE, ablationFile = "ablationMBOPlot2.pdf", seed = 1)



