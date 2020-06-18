#' Generate instances for EBO::optimizertuneRace()
#'
#' This functions enables users to create an instance list with different problems, which are
#' used for EBO::optimizertuneRace().
#'
#' @param ... [\code{EBO:: task()}]\cr
#'  Task defines the problem setting.
#'
#' @return List of instances with different problems / tasks.
#'
#' @export
#'
#' @examples
#' \dontrun{
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
#' }



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
    if (length(taskList[[i]]) != 7) {
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
    for(r in 1:numberFeatures) {
      if (isFALSE(names(taskList[[i]][[6]][["pars"]][r]) %in% names(taskList[[i]][[4]][1:numberFeatures]))) {
        stop("data variables must be identical with ParamSet variables!")
      }
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
