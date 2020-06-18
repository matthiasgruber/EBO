grapheneArgon = as.data.frame(readxl::read_excel("examples/data/grapheneArgon.xlsx"))

# define parameter spce
psOpt = ParamHelpers::makeParamSet(
  ParamHelpers::makeIntegerParam("power", lower = 10, upper = 5555),
  ParamHelpers::makeIntegerParam("time", lower = 500, upper = 20210),
  ParamHelpers::makeIntegerParam("pressure", lower = 0, upper = 100)
)

# create task
task_grapheneArgon = EBO::task(
  simulation = "regr.randomForest",
  data = grapheneArgon,
  target = "ratio",
  psOpt = psOpt,
  minimize = FALSE
)


ctrl = mlrMBO::makeMBOControl()
ctrl = mlrMBO::setMBOControlInfill(ctrl, crit = mlrMBO::makeMBOInfillCritCB(4))

# define MBO configuration
paramsMBO = data.table::data.table(
  design = list(NULL,"optimumLHS"),
  amountDesign = list(4),
  control = list(NULL,
                 ctrl),
  surrogate = list(NULL,
                   mlr::makeLearner("regr.km", predict.type = "se", covtype = "powexp", nugget.estim = TRUE))
)

# define names
namesBoxplot = c("mlrMBO default",
                 "mlrMBO tuned")

# define function evaluations
funcEvals = 55

# generate Data
grapConfigResults = EBO::generateConfigdata(task_grapheneArgon, funcEvals = funcEvals, paramsMBO,
                                            namesBoxplot = namesBoxplot, repls = 20, seed = 1235)


eval1 = EBO::boxplotCurve(grapConfigResults)












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


ctrl = mlrMBO::makeMBOControl()
ctrl = mlrMBO::setMBOControlInfill(ctrl, crit = mlrMBO::makeMBOInfillCritCB(3))

# define MBO configuration
paramsMBO = data.table::data.table(
  design = list(NULL,
                "optimumLHS"),
  amountDesign = list(4),
  control = list(NULL,
                 ctrl),
  surrogate = list(NULL,
                   mlr::makeLearner("regr.km", predict.type = "se", covtype = "powexp", nugget.estim = TRUE))
)

# define names
namesBoxplot = c("mlrMBO default",
                 "mlrMBO tuned")

# define function evaluations
funcEvals = 55

# generate Data
kaptConfigResults = EBO::generateConfigdata(task_Kapton, funcEvals = funcEvals, paramsMBO,
                                            namesBoxplot = namesBoxplot, repls = 20, seed = 1)


eval2 = EBO::boxplotCurve(kaptConfigResults)









synthesis = openxlsx::read.xlsx("examples/data/synthesis.xlsx")

# define parameter spce

psOpt = ParamHelpers::makeParamSet(
  ParamHelpers::makeNumericParam("f", lower = 0, upper = 0.25),
  ParamHelpers::makeNumericParam("k", lower = 0, upper = 0.1),
  ParamHelpers::makeNumericParam("du", lower = 0, upper = 1),
  ParamHelpers::makeNumericParam("dv", lower = 0, upper = 1)
)

# create task
task_synt = EBO::task(
  simulation = "regr.randomForest",
  data = synthesis,
  target = "interface",
  psOpt = psOpt,
  minimize = FALSE
)


ctrl = mlrMBO::makeMBOControl()
ctrl = mlrMBO::setMBOControlInfill(ctrl, crit = mlrMBO::makeMBOInfillCritCB(3))

# define MBO configuration
paramsMBO = data.table::data.table(
  design = list(NULL),
  amountDesign = list(NULL),
  control = list(NULL,
                 ctrl),
  surrogate = list(NULL,
                   mlr::makeLearner("regr.km", predict.type = "se", covtype = "powexp", nugget.estim = TRUE))
)

# define names
namesBoxplot = c("mlrMBO default",
                 "mlrMBO tuned")

# define function evaluations
funcEvals = 55

# generate Data
syntConfigResults = EBO::generateConfigdata(task_synt, funcEvals = funcEvals, paramsMBO,
                                            namesBoxplot = namesBoxplot, repls = 20, seed = 1)


eval3 = EBO::boxplotCurve(syntConfigResults)
