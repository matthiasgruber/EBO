#' Generate task
#'
#' This functions enables users to create a task, which mainly describes the optimization problem.
#' Task is required for many EBO functions, such as EBO::optimizertuneRace(), EBO::generateConfigdata()
#' or EBO::generateProblemList().
#'
#' @param psOpt [\code{ParamHelpers::ParamSet()}]\cr
#'  Collection of parameters and their constraints for problem optimization
#' @param simulation [\code{character}]\cr
#'  The black box function e.g. model for the \code{mlrMBO}
#'  Default is `regr.randomForest`.
#' @param data [\code{data.frame}]\cr
#'  Data of problem. \cr
#' @param target [\code{character}]\cr
#'  The variable, which one wants to minimize (maximize) \code{mlrMBO}.
#' @param minimize [\code{logical(1)}]\cr
#'  Should the target be minimized? \cr
#'  Default is `TRUE`.
#' @param task [\code{EBO:: task()}]\cr
#'  Task defines the problem setting.
#'
#' @return List with relevant informations of a specific problem.
#'
#'  @examples
#' \dontrun{
#' set.seed(1)
#'
#' library(mlrMBO)
#' library(ParamHelpers)
#' library(mlr)
#'
#' set.seed(1)
#'
#' # define problem / task
#' data <- data.frame(a = runif(50,10,5555), b = runif(50,-30000,-500),
#'                    c = runif(50,0,1000))
#' data$ratio <- rowSums(data[,1:3]^2)
#' data$ratio <- data$ratio/max(data$ratio)
#' colnames(data) <- c("power", "time", "pressure","ratio")
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
#' task = task(
#'  simulation = "regr.randomForest",
#'  data = data,
#'  target = "ratio",
#'  psOpt = psOpt,
#'  minimize = FALSE
#' )
#' }





task = function(simulation = "regr.randomForest", data, target, psOpt, minimize = TRUE) {
  # define a list named task

  task = as.list("task")

  checkmate::assertChoice(simulation, c("regr.randomForest", "regr.km"))

  checkmate::assertClass(data, classes = c("data.frame"))

  checkmate::assertClass(target, classes = c("character"))

  checkmate::assertClass(psOpt, classes = c("ParamSet"))

  checkmate::assertLogical(minimize, len = 1, any.missing = FALSE)

  # features from psOpt must exist in data
  numberFeatures = ncol(data)-1
  for(i in 1:numberFeatures) {
    if (isFALSE(names(psOpt[["pars"]][i]) %in% names(data[1:numberFeatures]))) {
      stop("data variables must be identical with ParamSet variables!")
    }
  }

  # target from task must exist in data
  if(isFALSE(target %in% names(data))) {
    stop("target from task must exist in data!")
  }

  if (minimize == FALSE) task$p = as.integer(-1)
  if (minimize == TRUE) task$p = as.integer(1)
  # add minimize as TRUE or FALSE
  task$minimize = minimize

  # add data
  task$data = data

  # add simulation for black box function
  task$simulation = simulation

  # add parameter space of problem
  task$psOpt = psOpt

  # add target of problem
  task$target = target

  return(task)
}
