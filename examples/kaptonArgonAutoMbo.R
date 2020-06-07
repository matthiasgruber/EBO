# get the data
kapton = openxlsx::read.xlsx("tests/testthat/data/kaptonArgon.xlsx")

# define the target
target = c("ratio")

# define optimization direction
minimize = FALSE

# define the maximum amount of black-box function evaluations
funcEvals = 84

# define how many black-box function evaluations should be performed
# with the same hyperparameter set
minFuncEvals = 40

# execute computations
plot1 = EBO::autoMbo(kapton, target, minimize, funcEvals, minFuncEvals, seed = 6, itersMboTune = 2, repls = 10, ncpus = 20)
plot2 = EBO::autoMbo(kapton, target, minimize, funcEvals, minFuncEvals, seed = 7, itersMboTune = 2, repls = 10, ncpus = 20)
plot3 = EBO::autoMbo(kapton, target, minimize, funcEvals, minFuncEvals, seed = 8, itersMboTune = 10, repls = 10, ncpus = 20)
plot4 = EBO::autoMbo(kapton, target, minimize, funcEvals, minFuncEvals, seed = 9, itersMboTune = 2, repls = 10, ncpus = 20)
plot5 = EBO::autoMbo(kapton, target, minimize, funcEvals, minFuncEvals, seed = 11, itersMboTune = 2, repls = 10, ncpus = 20)
plot7 = EBO::autoMbo(kapton, target, minimize, funcEvals, minFuncEvals, seed = 12, itersMboTune = 2, repls = 10, ncpus = 20)
plot8 = EBO::autoMbo(kapton, target, minimize, funcEvals, minFuncEvals, seed = 13, itersMboTune = 2, repls = 10, ncpus = 20)

