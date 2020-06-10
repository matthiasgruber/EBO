# define infillCrit
ctrl = mlrMBO::makeMBOControl()
ctrl = mlrMBO::setMBOControlInfill(ctrl, crit = mlrMBO::makeMBOInfillCritEI())
# define MBO configuration
paramsMBO = data.table::data.table(
  design = list("maximinLHS","randomLHS", "random"),
  amountDesign = list(12),
  control = list(ctrl),
  surrogate = list(mlr::makeLearner("regr.km", predict.type = "se"))
)
# define names
namesBoxplot = c("maximinLHS",
                 "randomLHS",
                 "random")
# define function evaluations
funcEvals = 42

# kapton problem
kapton = as.data.frame(readxl::read_excel("examples/data/kaptonArgon.xlsx"))
# define parameter spce
psOpt = ParamHelpers::makeParamSet(
  ParamHelpers::makeIntegerParam("power", lower = 10, upper = 5555),
  ParamHelpers::makeIntegerParam("time", lower = 500, upper = 20210),
  ParamHelpers::makeIntegerParam("pressure", lower = 0, upper = 1000)
)
# create task
task_Kapton = EBO::task(
  simulation = "regr.randomForest",
  data = kapton,
  target = "ratio",
  psOpt = psOpt,
  minimize = FALSE
)
# generate Data
kaptConfigResults = EBO::generateConfigdata(task_Kapton, funcEvals = funcEvals, paramsMBO,
                                            namesBoxplot = namesBoxplot, repls = 20)

testConfigs(kaptConfigResults)

# boxplot curve of configs
boxplotCurve(kaptConfigResults)

# analyze effect of additional iterations
EBO::testAddIters(kaptConfigResults, baseIters = 20, addIters = 10, minimize = FALSE)
