library(ParamHelpers)
library(readr)

set.seed(1)


# define infillCrit
ctrl = mlrMBO::makeMBOControl()
ctrl = mlrMBO::setMBOControlInfill(ctrl, crit = mlrMBO::makeMBOInfillCritEI())

# define MBO configuration
paramsMBO = data.table::data.table(
  design = list(c(0.8,1), c(0,0.2)),
  amountDesign = list(12),
  control = list(ctrl),
  surrogate = list(mlr::makeLearner("regr.randomForest", predict.type = "se"))
)

namesBoxplot = c("good",
                 "bad")

# define runs of each algorithm
repls = 20

# define function evaluations
funcEvals = 42


#### synthesis problem ###
synthesis <- as.data.frame(readxl::read_excel("data/synthesis.xlsx"))

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

# generate configData
synthGB_ConfigResults = generateConfigdata(task_Synthesis, funcEvals = funcEvals, paramsMBO,
                                        namesBoxplot = namesBoxplot, repls = repls)
# boxplot curve of configs
boxplotCurve(synthGB_ConfigResults)

# run statistical tests for configs
testConfigs(synthGB_ConfigResults)

# analyze effect of additional iterations
testAddIters(synthGB_ConfigResults, baseIters = 20, addIters = 10, minimize = FALSE)



#### kapton problem ###
kapton = as.data.frame(readxl::read_excel("data/kaptonArgon.xlsx"))

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

# generate configData
kaptGBConfigResults = generateConfigdata(task_Kapton, funcEvals = funcEvals, paramsMBO,
                                       namesBoxplot = namesBoxplot, repls = repls)
# boxplot curve of configs

boxplotCurve(kaptGBConfigResults)

# run statistical tests for configs
testConfigs(kaptGBConfigResults)

# analyze effect of additional iterations
testAddIters(kaptGBConfigResults, baseIters = 20, addIters = 10, minimize = FALSE)


#### graphene problem ###
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

# generate configData
graphGBConfigResults = generateConfigdata(task_Graphene, funcEvals = funcEvals, paramsMBO,
                                        namesBoxplot = namesBoxplot, repls = repls)
# boxplot curve of configs

boxplotCurve(graphGBConfigResults)

# run statistical tests for configs
testConfigs(graphGBConfigResults)

# analyze effect of additional iterations
testAddIters(graphGBConfigResults, baseIters = 20, addIters = 10, minimize = FALSE)










good_badSynthConfigResults = generateConfigdata(task, funcEvals = funcEvals, paramsMBO,
                                           namesBoxplot = namesBoxplot, repls = repls)

boxplotCurve(good_badSynthConfigResults)

testConfigs(good_badSynthConfigResults)

testAddIters(good_badSynthConfigResults, baseIters = 20, addIters = 10, minimize = FALSE)





#### synthesis problem ###
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

# generate configData
synthConfigResults = generateConfigdata(task_Synthesis, funcEvals = funcEvals, paramsMBO,
                                        namesBoxplot = namesBoxplot, repls = repls)
# boxplot curve of configs
boxplotCurve(synthConfigResults)

# run statistical tests for configs
testConfigs(synthConfigResults)

# analyze effect of additional iterations
testAddIters(synthConfigResults, baseIters = 20, addIters = 10, minimize = FALSE)



#### kapton problem ###
kapton <- as.data.frame(readxl::read_excel("tests/testthat/data/kapton_argon.xlsx"))

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

# generate configData
kaptConfigResults = generateConfigdata(task_Kapton, funcEvals = funcEvals, paramsMBO,
                                       namesBoxplot = namesBoxplot, repls = repls)
# boxplot curve of configs

boxplotCurve(kaptConfigResults)

# run statistical tests for configs
testConfigs(kaptConfigResults)

# analyze effect of additional iterations
testAddIters(kaptConfigResults, baseIters = 20, addIters = 10, minimize = FALSE)


#### graphene problem ###
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

# generate configData
graphConfigResults = generateConfigdata(task_Graphene, funcEvals = funcEvals, paramsMBO,
                                        namesBoxplot = namesBoxplot, repls = repls)
# boxplot curve of configs

boxplotCurve(graphConfigResults)

# run statistical tests for configs
testConfigs(graphConfigResults)

# analyze effect of additional iterations
testAddIters(graphConfigResults, baseIters = 20, addIters = 10, minimize = FALSE)

