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
  if ((names(psOpt[["pars"]][1:numberFeatures]) == names(data[1:numberFeatures]))==FALSE) {
    stop("data variables must be identical with ParamSet variables!")
  }

  # target from task must exist in data
  if((target %in% names(data))== FALSE) {
    stop("target from task must exist in data!")
  }

  if (minimize == FALSE) task$p = as.integer(-1)
  if (minimize == TRUE) task$p = as.integer(1)
  # add minimize as TRUE or FALSE
  task$minimize = minimize

  task$data = data

  task$simulation = simulation

  task$psOpt = psOpt

  task$target = target

  return(task)
}
