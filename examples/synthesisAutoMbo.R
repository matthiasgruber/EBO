# get the data
synthesis = openxlsx::read.xlsx("examples/data/synthesis.xlsx")

# define the target
target = c("interface")

# define optimization direction
minimize = FALSE

# define the maximum amount of black-box function evaluations
funcEvals = 205

# define how many black-box function evaluations should be performed
# with the same hyperparameter set
minFuncEvals = 100

# execute computations
plot1 = EBO::autoMbo(synthesis, target, minimize, funcEvals, minFuncEvals, seed = 1, itersMboTune = 2, repls = 10, ncpus = 20)
plot2 = EBO::autoMbo(synthesis, target, minimize, funcEvals, minFuncEvals, seed = 2, itersMboTune = 2, repls = 10, ncpus = 20)
plot3 = EBO::autoMbo(synthesis, target, minimize, funcEvals, minFuncEvals, seed = 3, itersMboTune = 10, repls = 10, ncpus = 20)
plot4 = EBO::autoMbo(synthesis, target, minimize, funcEvals, minFuncEvals, seed = 4, itersMboTune = 2, repls = 10, ncpus = 20)
plot5 = EBO::autoMbo(synthesis, target, minimize, funcEvals, minFuncEvals, seed = 5, itersMboTune = 2, repls = 10, ncpus = 20)
plot7 = EBO::autoMbo(synthesis, target, minimize, funcEvals, minFuncEvals, seed = 7, itersMboTune = 2, repls = 10, ncpus = 20)
plot8 = EBO::autoMbo(synthesis, target, minimize, funcEvals, minFuncEvals, seed = 8, itersMboTune = 2, repls = 10, ncpus = 20)
plot9 = EBO::autoMbo(synthesis, target, minimize, funcEvals, minFuncEvals, seed = 9, itersMboTune = 2, repls = 10, ncpus = 20)

