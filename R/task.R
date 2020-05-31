#' @export
task = function(simulation, data, target, psOpt, minimize) {
  # define a list named info
  task = as.list("info")
  # add the name of the target variable
  #task$y.name = model[["task.desc"]][["target"]]
  # add the name of the feature(s)
  #task$featureName = model[["features"]]
  # add numbers of feature(s)
  #task$featureNumber = length(model[["features"]])
  # add type / class of feature(s)
  #task$featureType = getParamTypes(psOpt)
  # add the name of the problem (name of the dataset)
  #task$dataName = model[["task.desc"]][["id"]]
  # add the learners name
  #task$learner = model[["learner"]][["id"]]
  # add an auxiliary named p which defines min or max by 1 and -1
  if (minimize == FALSE) task$p = (-1)
  if (minimize == TRUE) task$p = (1)
  # add minimize as TRUE or FALSE
  task$minimize = minimize
  # add the levels of the discretes variables
  #task$model = model

  task$data = data

  task$simulation = simulation

  task$psOpt = psOpt

  task$target = target

  return(task)
}
