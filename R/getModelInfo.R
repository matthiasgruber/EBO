getModelInfo = function(model, psOpt, minimize) {
  # define a list named info
  info = as.list("info")
  # add the name of the target variable
  info$y.name = model[["task.desc"]][["target"]]
  # add the name of the feature(s)
  info$featureName = model[["features"]]
  # add numbers of feature(s)
  info$featureNumber = length(model[["features"]])
  # add type / class of feature(s)
  info$featureType = ParamHelpers::getParamTypes(psOpt)
  # add the name of the problem (name of the dataset)
  info$dataName = model[["task.desc"]][["id"]]
  # add the learners name
  info$learner = model[["learner"]][["id"]]
  # add an auxiliary named p which defines min or max by 1 and -1
  if (minimize == FALSE) info$p = (-1)
  if (minimize == TRUE) info$p = (1)
  # add minimize as TRUE or FALSE
  info$minimize = minimize
  # add the levels of the discretes variables
  info$discreteLevel = ParamHelpers::getValues(psOpt)
  return(info)
}
