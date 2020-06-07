library(ggplot2)
set.seed(1)
# objective function
objFun = smoof::makeAckleyFunction(1)
# initial design
design = data.frame(x = c(-18, -12, 18, 29))
design$y = apply(design, 1, objFun)
# mlrMBO default run
# surrogate learner: regr.km
# infill crit: cb
set.seed(1)
control = mlrMBO::makeMBOControl()
control = mlrMBO::setMBOControlTermination(control, iters = 8L)
runN = mlrMBO::exampleRun(objFun, design = design, control = control, show.info = FALSE)
mlrMBO::plotExampleRun(runN, iters = c(8L), pause = FALSE, xlim = c(-35,35), ylim = c(0,25))

set.seed(1)
# objective function
objFun = smoof::makeAckleyFunction(1)
# initial design
design = data.frame(x = c(-18, -12, 18, 29))
design$y = apply(design, 1, objFun)
# mlrMBO run
# surrogate learner: regr.random.Forest
# infill crit: ei
set.seed(1)
surrogateRf = mlr::makeLearner("regr.randomForest", predict.type = "se")
control = mlrMBO::makeMBOControl()
control = mlrMBO::setMBOControlTermination(control, iters = 8L)# more then 2 and its getting worse
control = mlrMBO::setMBOControlInfill(control, crit = mlrMBO::makeMBOInfillCritEI())
runRf = mlrMBO::exampleRun(objFun, design = design, learner = surrogateRf, control = control, show.info = FALSE)
mlrMBO::plotExampleRun(runRf, iters = c(8L), pause = FALSE, xlim = c(-35,35), ylim = c(0,25))


#########DMBO
# a sequential tuned mlrMBO run

# same initial design
# random forest
# 2 function evaluation
set.seed(1)
surrogateRf = mlr::makeLearner("regr.randomForest", predict.type = "se")
control = mlrMBO::makeMBOControl()
control = mlrMBO::setMBOControlTermination(control, iters = 2L)# more then 2 and its getting worse
control = mlrMBO::setMBOControlInfill(control, crit = mlrMBO::makeMBOInfillCritEI())
runRf = mlrMBO::exampleRun(objFun, design = design, learner = surrogateRf, control = control, show.info = FALSE)
mlrMBO::plotExampleRun(runRf, iters = c(2L), pause = FALSE, xlim = c(-35,35), ylim = c(0,25))

# the initial design for the second step are the evaluated points from the first step
# krigging with high coef.trend
# 1 function evaluation
set.seed(1)
designKm = runRf[["mbo.res"]][["opt.path"]][["env"]][["path"]][1:6,]
#surrogate = mlr::makeLearner("regr.km", predict.type = "se", covtype = "matern3_2",
#                             coef.trend = 5, control = list(trace = FALSE)) # 150
control = mlrMBO::makeMBOControl()
control = mlrMBO::setMBOControlTermination(control, iters = 1L)
#control = mlrMBO::setMBOControlInfill(control, crit = mlrMBO::makeMBOInfillCritEI())
runKm = mlrMBO::exampleRun(objFun, design = designKm,  control = control, show.info = FALSE)#learner = surrogate,
mlrMBO::plotExampleRun(runKm, iters = c(1L), pause = FALSE, xlim = c(-35,35), ylim = c(0,25))
