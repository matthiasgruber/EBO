
optimizertuneRace = function(optimizer,
                             psTune, funcEvals, itersTune,
                             trainInstanceList, minimize = TRUE, configurationsFile = NA,
                             plotAblation = TRUE, ablationFile = NA,
                             test = "F-test", seed = 1)
 {

  psTune <- ParamHelpers::convertParamSetToIrace(psTune)


  #oldWd = getwd()
  #tempDirPath = tempdir()
  #setwd(tempDirPath)




  target.runner <- function(experiment, scenario) {
    debugLevel    <- scenario$debugLevel
    configuration.id  <- experiment$id.configuration
    instance.id   <- experiment$id.instance
    seed          = experiment$seed
    configuration <- experiment$configuration

    set.seed(seed)

    instance <- experiment$instance

    trainInstanceList = instance

    model = mlr::train(mlr::makeLearner("regr.randomForest"), mlr::makeRegrTask(data = instance$data, target = instance$target))
    #model2 = mlr::train(mlr::makeLearner(task2$simulation), mlr::makeRegrTask(data = instance$problem2, target = task2$target))

    info = getModelInfo(model, instance$psOpt, minimize = minimize)
    #info2 = getModelInfo(model2, task2$psOpt2, task2$minimize)

    if (optimizer == "optimizeCmaesr") {

      objEncoded = objEncodedFunc(model, instance$psOpt, info)
      #objEncoded2 = objEncodedFunc(model2, task2$psOpt2, info2)

      res = do.call(optimizer, list(configuration, objEncoded[[1]], info, funcEvals))

      #res = do.call(optimizer, list(configuration, objEncoded2[[1]], info2, funcEvals))

    }

    if (optimizer == "optimizeES" | optimizer == "optimizeGenoud"| optimizer == "optimizeDE") {

      objEncoded = objSPOTFunc(model, instance$psOpt, info)
      #objEncoded2 = objEncodedFunc(model2, task2$psOpt2, info2)

      res = do.call(optimizer, list(configuration, objEncoded[[1]], info, funcEvals))
    }

    if (optimizer == "optimizeMBO") {

      objNormal = objNormalFunc(model, instance$psOpt, info)

      res = do.call(optimizer, list(configuration, objNormal[[1]], info, funcEvals))
    }


    # get best y of the optimizer run (all optimizers return the same result!)
    y = as.numeric(res[[1]][info$y.name])
    #y = as.numeric(res[[1]][[1]][,info$y.name])

    result <- list(cost = (info$p)*y, call = toString(experiment))

    return(result)
  }
  ########### Run-Function / Irace Options / Configuration Scenario ########

  scenario                <- irace::defaultScenario()

  scenario$targetRunner   <- "target.runner"

  scenario$configurationsFile = configurationsFile

  # define tuning iterations
  scenario$maxExperiments <- itersTune
  # define trainInstances
  scenario$instances      <- trainInstanceList
  # define testInstances
  #scenario$testInstances      <- testInstanceList

  #testIterationElites = 0
  #testNbElites = 1
  # parallelisation
  nc = floor(parallel::detectCores()/2) + 1
  scenario$parallel = nc
  #define  number of configs testet at each instance
  scenario$nbConfigurations <- 20
  #define test type
  scenario$testType <- test

  scenario$firstTest = 6

  #scenario$execDir = tempDirPath
  #scenario$configurationsFile <- configurationsFile
  scenario$seed = seed
  #scenario$seed = seed
  irace::irace(scenario = scenario, parameters = psTune)

  load("irace.Rdata")

  # return testing
  #testing.main(logFile = "./irace.Rdata")
  #testResult = iraceResults$testing


 if(plotAblation == TRUE) {

    ablation(iraceResults = iraceResults , pdf.file = ablationFile)

    load("log-ablation.Rdata")

    ab.log[["experiments"]] <- ab.log[["experiments"]]*(ifelse(minimize == TRUE, (1), (-1)))

    ablation = plotAblation(ab.log, pdf.file = ablationFile)
  }
    # make ablation plot

  # return optimation.path from tuning
  result = as.data.frame(iraceResults[["state"]][["eliteConfigurations"]])

  return(list(result, iraceResults))
}

