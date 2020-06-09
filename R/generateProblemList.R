generateProblemList = function(...) {

  taskList = list(...)
  numberProblems = length(taskList)

  # check if correct class
  for(i in 1:numberProblems) {
    #check if correct class
    if (class(taskList[[i]]) != "list") {
      stop("each task must be a list!")
    }
    #check if correct number of elements
    if (length(taskList[[i]]) != "7") {
      stop("each task must have length 7!")
    }
    # check names and class of sublists
    if (names(taskList[[i]][2]) != "p") {
      stop("must pass p!")
    }
    if (class(taskList[[i]][[2]]) != "integer") {
      stop("p must be integer!")
    }

    if (names(taskList[[i]][3]) != "minimize") {
      stop("must pass minimize!")
    }
    if (class(taskList[[i]][[3]]) != "logical") {
      stop("minimize must be logical!")
    }

    if (names(taskList[[i]][4]) != "data") {
      stop("must pass data!")
    }
    if (class(taskList[[i]][[4]]) != "data.frame") {
      stop("data must be data.frame!")
    }

    if (names(taskList[[i]][5]) != "simulation") {
      stop("must pass simulation!")
    }
    if (class(taskList[[i]][[5]]) != "character") {
      stop("simulation must be character!")
    }

    # assertions for parameter space
    if (names(taskList[[i]][6]) != "psOpt") {
      stop("must pass a paramHelpers object!")
    }
    if (class(taskList[[i]][[6]]) != "ParamSet") {
      stop("parameter space must be a ParamSet object!")
    }
    # features from parameter space must be identical with features from data
    numberFeatures = ncol(taskList[[i]][[4]])-1
    if ((names(taskList[[i]][[6]][["pars"]][1:numberFeatures]) == names(taskList[[i]][[4]][1:numberFeatures]))==FALSE) {
      stop("data variables must be identical with ParamSet variables!")
    }

    # assertions for target
    if (names(taskList[[i]][7]) != "target") {
      stop("must pass a target!")
    }
    if (class(taskList[[i]][[7]]) != "character") {
      stop("target must be character!")
    }
    # targets from tasks must exist in data
    if((taskList[[i]][[7]] %in% names(taskList[[i]][[4]]))== FALSE) {
      stop("target from task must exist in data!")
    }

    #checkmate::assertNames(taskList[[i]][2], identical.to = c("p"))
    #checkmate::assertNames(taskList[[i]][3], identical.to = c("minimize"))
    #checkmate::assertNames(taskList[[i]][4], identical.to = c("data"))
    #checkmate::assertNames(taskList[[i]][5], identical.to = c("simulation"))
    #checkmate::assertNames(taskList[[i]][6], identical.to = c("psOpt"))
    #checkmate::assertNames(taskList[[i]][7], identical.to = c("target"))
    #check if correct class of listelements
  }



  problemList = vector(mode = "list", length = numberProblems)

  for(i in 1:numberProblems) {

    problemList[[i]] = vector(mode = "list", length = 4)

    # name sublist
    names(problemList[[i]])[[1]] = 'data'
    names(problemList[[i]])[[2]] = 'psOpt'
    names(problemList[[i]])[[3]] = 'target'
    names(problemList[[i]])[[4]] = 'simulation'

    # define data frames
    problemList[[i]][[1]] = taskList[[i]]$data

    # define parameter spaces
    problemList[[i]][[2]] = taskList[[i]]$psOpt

    # define targets
    problemList[[i]][[3]] = taskList[[i]]$target

    #define blackbox function
    problemList[[i]][[4]] = taskList[[i]]$simulation
  }
  return(problemList)
}
