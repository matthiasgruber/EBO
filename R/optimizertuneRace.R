#' Tuning of optimization algorithms
#'
#' This functions enables users to tune the hyperparameters of different optimization algorithms.
#'
#' @param minimize [\code{logical(1)}]\cr
#'  Should the target be minimized? \cr
#'  Default is `TRUE`.
#' @param trainInstanceList [\code{list()}]\cr
#'  Define list with instances / problems, which were defined with EBO::generateProblemList().\cr
#' @param funcEvals [\code{integer(1)}]\cr
#'  Define the number of function evaluations.\cr
#'  Default is 50.
#' @param optimizer [\code{character}]\cr
#'  optimization algorithm which user want to tune \cr
#'  possible optimization algorithms are: "optimizeMBO", "optimizeES", "optimizeDE",
#'  "optimizeGenoud", "optimizeCmaesr"
#'  Default is `NA`.
#' @param plotAblation [\code{logical(1)}]\cr
#'  Should an ablation analysis be run with the tuning Result? \cr
#'  Default is `FALSE`.
#' @param ablationFile [\code{character}]\cr
#'  Saving path for the ablation analysis. \cr
#'  Default is `NA`.
#' @param configurationsFile [\code{character}]\cr
#'  Use a .txt file to define the source for ablation analysis \cr
#'  We recommend using the default algorithm setting.
#'  Default is `NA`.
#' @param seed [\code{numeric(1)}]\cr
#'  Define the seed used for the computation. Will be set by \code{irace}.
#'  Default is one.
#' @param psTune [\code{ParamHelpers::ParamSet()}]\cr
#'  Collection of hyperparameters and their constraints for the tuning, e.g. tuning of optimizer\cr
#'  In the following, the hyperparamter of the optimizer, which one can tune are summarized.\cr
#'  "optimizeMBO":\cr
#'     - design[string]: "maximinLHS", "randomLHS", "random", "optimumLHS", "augmentLHS",
#'                       "geneticLHS", "improvedLHS", "optAugmentLHS"\cr
#'     - surrogate[string]: "regr.randomForest", "regr.km"\cr
#'     - amountDesign[integer]: defines number of initial design points. default is number of features + 1.\cr
#'     - covtype[string]: "matern5_2","matern3_2", "powexp", "gauss"\cr
#'     - nodesize[integer]: default is ...\cr
#'     - mtry[integer]: default is ...\cr
#'     - crit[string]: "makeMBOInfillCritAEI","makeMBOInfillCritCB", "makeMBOInfillCritAdaCB","makeMBOInfillCritEI",
#'                     "makeMBOInfillCritEQI", "makeMBOInfillCritMeanResponse", "makeMBOInfillCritStandardError"\cr
#'     - cb.lambda[numeric]: defines cb.lambda from makeMBOInfillCritCB; default is 1 for fully numeric parameter set and 2 otherwise\cr
#'     - cb.lambda.start[numeric]: defines cb.lambda.start from makeMBOInfillCritAdaCB\cr
#'     - cb.lambda.end[numeric]:  defines cb.lambda.end from makeMBOInfillCritAdaCB\cr
#'     - eqi.beta[numeric]: Beta parameter for expected quantile improvement criterion. Default is 0.75.\cr
#'  "optimizeES":\cr
#'     - nu[integer]: selection pressure. That means, number of offspring (lambda) is
#'           mue multiplied with nu. Default is 10\cr
#'     - mue[integer]: number of parents, default is 10\cr
#'     - sigmaInit[numeric]: initial sigma value (step size), default is 1.0\cr
#'     - nSigma[integer]: number of different sigmas, default is 1\cr
#'     - mutation[integer]: string of mutation type, default is 1\cr
#'     - tau[numeric]: number, learning parameter for self adaption,
#'            i.e. the local multiplier for step sizes (for each dimension).default is 1.0\cr
#'     - stratReco[integer]: Recombination operator for strategy variables. 1: none.
#'                  2: dominant/discrete (default). 3: intermediate.
#'                  4: variation of intermediate recombination.\cr
#'     - objReco[integer]: Recombination operator for object variables. 1: none. 2: dominant/discrete (default).
#'                3: intermediate. 4: variation of intermediate recombination.\cr
#'  "optimizeGenoud":\cr
#'     - populationSize[integer]: Number of individuals in the population. Default is 10*dimension.\cr
#'  "optimizeDE":\cr
#'     - populationSize[integer]: Number of particles in the population. Default is 10*dimension.\cr
#'  "optimizeCmaesr":\cr
#'     - sigma[numeric]: Initial step-size. Default is 0.5.\cr
#'     - lambda[integer]: Number of offspring generated in each generation.\cr
#'
#' @param itersTune [\code{integer(1)}]\cr
#'   Define the tuning budget used for tuning with \code{irace}\cr
#'   Default is 1000.
#' @param firstTest [\code{integer(1)}]\cr
#'   defines how many instances are evaluated, before the first test \code{irace}\cr
#'   Default is 6.
#' @param test [\code{character}]\cr
#'  Defines the test used for the iRace tuning procedure \cr
#'  Default is `F-test`.
#'
#'
#' @return Elite configurations for defined tasks and an ablation analysis if plotAblation = TRUE.
#'
#' @references [\code{mlrMBO::mbo()}]
#' @references Manuel López-Ibànez, Leslie Pérez Cáceres, Jérémie Dubois-Lacoste, Thomas Stützle and Mauro Birattari, The irace Package:
#' User Guide. Preprint: \code{\link{https://cran.r-project.org/web/packages/irace/vignettes/irace-package.pdf}} (2019).
#'
#' @export
#'
#' @seealso \code{\link{optimize::plotBenchmark()}} \code{\link{optimize::plotMboContourPlot()}}
#'
#' @examples
#' \dontrun{
#'
#' set.seed(1)
#'
#' library(mlrMBO)
#' library(ParamHelpers)
#' library(mlr)
#'
#' set.seed(1)
#'
#' # define problem 1
#' data1 <- data.frame(a = runif(50,10,5555), b = runif(50,-30000,-500),
#'                    c = runif(50,0,1000))
#' data1$ratio <- rowSums(data1[,1:3]^2)
#' data1$ratio <- data1$ratio/max(data1$ratio)
#' colnames(data1) <- c("power", "time", "pressure","ratio")
#'
#' psOpt = makeParamSet(
#'
#'  makeIntegerParam("power", lower = 10, upper = 5555),
#'
#'  makeIntegerParam("time", lower = -30000, upper = -500),
#'
#'  makeIntegerParam("pressure", lower = 0, upper = 1000)
#')
#'
#' task1 = task(
#'  simulation = "regr.randomForest",
#'  data = data1,
#'  target = "ratio",
#'  psOpt = psOpt,
#'  minimize = FALSE
#' )
#'
#'
#' # define problem 2
#' data2 <- data.frame(a = runif(80,0,0.25), b = runif(80, 0,0.1),
#'                    c = runif(80,0,1), d = runif(80,0,1))
#' data2$interface <- rowSums((data2[,1:4]*8)^2)
#' data2$interface <- data2$interface/max(data2$interface)
#' colnames(data2) <- c("f", "k", "du","dv")
#'
#'
#' psOpt = ParamHelpers::makeParamSet(
#'  ParamHelpers::makeNumericParam("f", lower = 0, upper = 0.25),
#'  ParamHelpers::makeNumericParam("k", lower = 0, upper = 0.1),
#'  ParamHelpers::makeNumericParam("du", lower = 0, upper = 1),
#'  ParamHelpers::makeNumericParam("dv", lower = 0, upper = 1)
#' )
#'
#' task2 = task(
#'  simulation = "regr.randomForest",
#'  data = data2,
#'  target = "interface",
#'  psOpt = psOpt,
#'  minimize = FALSE
#' )
#'
#'
#' ################## Define problemList #############
#'
#' problemList = generateProblemList(task1, task2)
#'
#'
#' ### tune SMBO algorithm
#'
#' psTune = ParamHelpers::makeParamSet(
#'
#'  ParamHelpers::makeDiscreteParam("design", values = c("maximinLHS",
#'                                                       "optimumLHS")),
#'
#'  ParamHelpers::makeDiscreteParam("surrogate", values = c("regr.randomForest", "regr.km")),
#'
#'  ParamHelpers::makeDiscreteParam("covtype", values = c("matern5_2","matern3_2", "powexp", "gauss")),
#'
#'  ParamHelpers::makeDiscreteParam("crit", values = c("makeMBOInfillCritAEI","makeMBOInfillCritCB", "makeMBOInfillCritAdaCB","makeMBOInfillCritEI")),
#'
#'  ParamHelpers::makeNumericParam("cb.lambda", lower = 1, upper = 5,
#'                                 requires = quote(crit == "makeMBOInfillCritCB")),
#'
#'  ParamHelpers::makeIntegerParam("cb.lambda.start", lower = 3, upper = 10,
#'                                 requires = quote(crit == "makeMBOInfillCritAdaCB")),
#'
#'  ParamHelpers::makeNumericParam("cb.lambda.end", lower = 0, upper = 3,
#'                                 requires = quote(crit == "makeMBOInfillCritAdaCB"))
#' )
#'
#'
#'
#' tuneResultsMBO = optimizertuneRace("optimizeMBO", psTune,
#'                                funcEvals = 55, itersTune = 1000, trainInstanceList = problemList,
#'                                minimize = FALSE,
#'                                plotAblation = TRUE, ablationFile = "ablationMBOPlot.pdf", seed = 1)
#'
#'
#' ## tune ES algorithm
#'
#' psTune = ParamHelpers::makeParamSet(
#'
#' ParamHelpers::makeIntegerParam("nu", lower = 5, upper = 15),
#'
#' ParamHelpers::makeIntegerParam("mue", lower = 5, upper = 15),
#'
#' ParamHelpers::makeNumericParam("sigmaInit", lower = 0.7, upper = 1.3),
#'
#' ParamHelpers::makeNumericParam("tau", lower = 0.7, upper = 1.3),
#'
#' ParamHelpers::makeIntegerParam("stratReco", lower = 1, upper = 4),
#'
#' ParamHelpers::makeIntegerParam("objReco", lower = 1, upper = 4)
#' )
#'
#' tuneResultsES = optimizertuneRace("optimizeES", psTune,
#'                                funcEvals = 65, itersTune = 1000, trainInstanceList = problemList,
#'                                minimize = FALSE,
#'                                plotAblation = TRUE, ablationFile = "ablationESPlot.pdf", seed = 1)
#'
#' }

optimizertuneRace = function(optimizer,
                             psTune, funcEvals = 50, itersTune = 1000,
                             trainInstanceList, minimize = TRUE, configurationsFile = NA,
                             plotAblation = FALSE, ablationFile = NA, firstTest = 6,
                             test = "F-test", seed = 1) {

  # assertions
  checkmate::assertChoice(optimizer, c("optimizeMBO", "optimizeES", "optimizeDE", "optimizeGenoud",
                                       "optimizeCmaesr"))
  # assertion on funcevals
  # if ES, Genoud, DE, Cmaesr, then: funcevals must be at least 50
  #if (optimizer == "optimizeES" | optimizer == "optimizeGenoud"| optimizer == "optimizeDE" | optimizer == "optimizeCmaesr"){
  #  checkmate::assertIntegerish(funcEvals, lower = 60, any.missing = TRUE,
  #                              len = 1)
  #} else {
  #checkmate::assertIntegerish(funcEvals, lower = amount + 1, any.missing = TRUE,
  #                            len = 1)
  #}
  #itersTune assertions works within the iRace package

  # assertion on psTune
  if (optimizer == "optimizeMBO") {EBO::assertPsTune(psTune)}

  if (optimizer == "optimizeES") {EBO::assertPsTuneES(psTune)}

  if (optimizer == "optimizeGenoud"| optimizer == "optimizeDE") {
    checkmate::assertChoice(names(psTune$pars), c("populationSize"))
    if (getParamNr(psTune) == 0L) {
      stop("No hyperparameters were passed!")
    }

    # check if populationSize is passed correctly
    if (!is.null(psTune[["pars"]]$populationSize)) {
      # check if populationSize is passed as integer
      if (!psTune[["pars"]]$populationSize$type == "integer") {
        stop("Tuning populationSize only works if it is passed as a integer parameter!")
      }
    }
  }

  if (optimizer == "optimizeCmaesr") {
    name = names(psTune$pars)
    for (i in 1:length(name)) {
      checkmate::assertChoice(name[i], c("lambda", "sigma"))
    }
    if (getParamNr(psTune) == 0L) {
      stop("No hyperparameters were passed!")
    }

    # check if lambda is passed correctly
    if (!is.null(psTune[["pars"]]$lambda)) {
      # check if lambda is passed as integer
      if (!psTune[["pars"]]$lambda$type == "integer") {
        stop("Tuning lambda only works if it is passed as a integer parameter!")
      }
    }
    # check if sigma is passed correctly
    if (!is.null(psTune[["pars"]]$sigma)) {
      # check if sigma is passed as numeric
      if (!psTune[["pars"]]$sigma$type == "numeric") {
        stop("Tuning sigma only works if it is passed as a numeric parameter!")
      }
    }
  }

  checkmate::assertClass(trainInstanceList, classes = c("list"))

  numberProblems = length(trainInstanceList)

  for(i in 1:numberProblems) {
    #check if correct class
    if (class(problemList[[i]]) != "list") {
      stop("each problem must be a list!")
    }
    #check if correct number of elements
    if (length(problemList[[i]]) != "4") {
      stop("each problem must have length 4!")
    }
    # check names and class of sublists
    # assertion for data
    if (names(problemList[[i]][1]) != "data") {
      stop("problemList must contain data!")
    }
    if (class(problemList[[i]][[1]]) != "data.frame") {
      stop("data in problemList must be a data.frame!")
    }

    # assertions for parameter space
    if (names(problemList[[i]][2]) != "psOpt") {
      stop("problemList must contain parameter space psOpt!")
    }
    if (class(problemList[[i]][[2]]) != "ParamSet") {
      stop("parameter space must be a paramHelpers object!")
    }
    # features from parameter space must be identical with features from data
    numberFeatures = ncol(problemList[[i]][[1]])-1
    for(r in 1:numberFeatures) {
      if (isFALSE(names(problemList[[i]][[2]][["pars"]][["pars"]][r]) %in% names(problemList[[i]][[1]][1:numberFeatures]))) {
        stop("data features must be identical with psOpt features!")
      }
    }

    # assertions for target
    if (names(problemList[[i]][3]) != "target") {
      stop("problemList must contain target!")
    }
    if (class(problemList[[i]][[3]]) != "character") {
      stop("target in problemList must be character!")
    }
    # targets from tasks must exist in data
    if(isFALSE(problemList[[i]][[3]] %in% names(problemList[[i]][[1]]))) {
      stop("target must be identical with target in data!")
    }
    # assertion for simulation
    if (names(problemList[[i]][4]) != "simulation") {
      stop("problemList must contain simulation!")
    }
    if (class(problemList[[i]][[4]]) != "character") {
      stop("simulation in problemList must be character!")
    }

  }

  checkmate::assertLogical(minimize, len = 1, any.missing = FALSE)

  ## check if configurationsFile has right file output
  if (!is.na(configurationsFile)) {

    if (class(configurationsFile) != "character") {
      stop("configurationsFile must be character!")
    }

    if (isFALSE(grepl(".txt", configurationsFile))) {
      stop("configurationsFile must be .txt file")
    }
  }

  checkmate::assertLogical(plotAblation, len = 1, any.missing = FALSE)

  ## check if ablationFile has right file output
  if (!is.na(ablationFile)) {

    if (class(ablationFile) != "character") {
      stop("ablationFile must be character!")
    }

    if (isFALSE(grepl(".pdf", ablationFile))) {
      stop("ablationFile must be .pdf file")
    }
  }

  checkmate::assertIntegerish(firstTest, lower = 2, any.missing = FALSE,
                              len = 1)

  checkmate::assertChoice(test, c("F-test", "t-test"))

  checkmate::assertIntegerish(seed, lower = 1, any.missing = FALSE,
                              len = 1)

  # transform into irace parameter set
  psTune = ParamHelpers::convertParamSetToIrace(psTune)


  # define target.runner of iRace
  target.runner = function(experiment, scenario) {

    debugLevel    = scenario$debugLevel

    configuration.id  = experiment$id.configuration

    instance.id   = experiment$id.instance

    seed          = experiment$seed

    configuration = experiment$configuration

    set.seed(seed)

    instance = experiment$instance

    # define trainInstanceList as iRace instances
    trainInstanceList = instance

    # build model for each instance (based on the informations of trainInstanceList)
    model = mlr::train(mlr::makeLearner(instance$simulation), mlr::makeRegrTask(data = instance$data, target = instance$target))

    info = getModelInfo(model, instance$psOpt, minimize = minimize)

    # different tuning algorithms, with different objective functions
    if (optimizer == "optimizeCmaesr") {

      objEncoded = objEncodedFunc(model, instance$psOpt, info)

      res = do.call(optimizer, list(configuration, objEncoded[[1]], info, funcEvals))

    }

    if (optimizer == "optimizeES" | optimizer == "optimizeGenoud"| optimizer == "optimizeDE") {

      objEncoded = objSPOTFunc(model, instance$psOpt, info)

      res = do.call(optimizer, list(configuration, objEncoded[[1]], info, funcEvals))
    }

    if (optimizer == "optimizeMBO") {

      objNormal = objNormalFunc(model, instance$psOpt, info)

      res = do.call(optimizer, list(configuration, objNormal[[1]], info, funcEvals))
    }


    # get best y of the optimizer run (all optimizers return the same result!)
    y = as.numeric(res[[1]][info$y.name])

    # multiply with p, to allow maximization
    result = list(cost = (info$p)*y, call = toString(experiment))

    return(result)
  }
  ########### Configuration Scenario ########

  scenario                = irace::defaultScenario()

  scenario$targetRunner   = "target.runner"

  # start the iRace procedure with specified configurations
  # we highly recommend to specify the default setting as configurationFile, to define the
  # source of the ablation analysis
  scenario$configurationsFile = configurationsFile

  # define tuning budget
  scenario$maxExperiments = itersTune

  scenario$instances      = trainInstanceList

  # parallelisation
  nc = floor(parallel::detectCores()/2) + 1
  scenario$parallel = nc
  #define  number of configs which are testet in each instance
  scenario$nbConfigurations <- 20
  #define test type
  scenario$testType = test

  # define how many instances are evaluated, before first test
  scenario$firstTest = firstTest

  scenario$seed = seed

  irace::irace(scenario = scenario, parameters = psTune)

  load("irace.Rdata")

  # run ablation analysis
  if(plotAblation == TRUE) {

    ablation(iraceResults = iraceResults , pdf.file = ablationFile)

    load("log-ablation.Rdata")

    # convert to maximization problem
    ab.log[["experiments"]] = ab.log[["experiments"]]*(ifelse(minimize == TRUE, (1), (-1)))

    ablation = plotAblation(ab.log, pdf.file = ablationFile)
  }
  # make ablation plot

  # return optimation.path from tuning
  result = as.data.frame(iraceResults[["state"]][["eliteConfigurations"]])

  return(list(result, iraceResults))
}
