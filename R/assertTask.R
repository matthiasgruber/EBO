assertTask = function(task) {
  if (class(task) != "list") {
    stop("task must be a list!")
  }
  #check if correct number of elements
  if (length(task) != "7") {
    stop("task must have length 7!")
  }
  # check names and class of sublists
  if (names(task[2]) != "p") {
    stop("must pass p in task!")
  }
  if (class(task[[2]]) != "integer") {
    stop("p must be integer!")
  }

  if (names(task[3]) != "minimize") {
    stop("must pass minimize in task!")
  }
  if (class(task[[3]]) != "logical") {
    stop("minimize must be logical!")
  }

  if (names(task[4]) != "data") {
    stop("must pass data!")
  }
  if (class(task[[4]]) != "data.frame") {
    stop("data must be data.frame!")
  }

  if (names(task[5]) != "simulation") {
    stop("must pass simulation in task!")
  }
  if (class(task[[5]]) != "character") {
    stop("simulation must be character!")
  }

  # assertions for parameter space
  if (names(task[6]) != "psOpt") {
    stop("must pass a paramHelpers object in task!")
  }
  if (class(task[[6]]) != "ParamSet") {
    stop("parameter space must be a ParamSet object!")
  }
  # features from parameter space must be identical with features from data
  numberFeatures = ncol(task[[4]])-1
  if (isFALSE(names(task[[6]][["pars"]][1:numberFeatures]) == names(task[[4]][1:numberFeatures]))) {
    stop("data variables must be identical with ParamSet variables!")
  }

  # assertions for target
  if (names(task[7]) != "target") {
    stop("must pass a target in task!")
  }
  if (class(task[[7]]) != "character") {
    stop("target must be character!")
  }
  # targets from tasks must exist in data
  if((task[[7]] %in% names(task[[4]]))== FALSE) {
    stop("target from task must exist in data!")
  }
}
