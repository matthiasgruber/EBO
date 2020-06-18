
# define infillCrit
ctrl = mlrMBO::makeMBOControl()
ctrl = mlrMBO::setMBOControlInfill(ctrl, crit = mlrMBO::makeMBOInfillCritEI())

# define MBO configuration
paramsMBO = data.table::data.table(
  design = list(c(0.8,1),
                c(0,0.2)),
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

# generate configData
synthGB_ConfigResults = generateConfigdata(task_Synthesis, funcEvals = funcEvals, paramsMBO,
                                        namesBoxplot = namesBoxplot, repls = repls)
# boxplot curve of configs
boxplotCurve(synthGB_ConfigResults)



#### kapton problem ###
kapton = as.data.frame(readxl::read_excel("examples/data/kaptonArgon.xlsx"))

psOpt = ParamHelpers::makeParamSet(
  ParamHelpers::makeIntegerParam("power", lower = 10, upper = 5555),
  ParamHelpers::makeIntegerParam("time", lower = 500, upper = 20210),
  ParamHelpers::makeIntegerParam("pressure", lower = 0, upper = 1000)
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
