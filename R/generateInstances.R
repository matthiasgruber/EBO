# create instances
# generate Instances
generateInstances = function(task, numberInstances = 5, seed = 1) {



  # if we choose observations with high Quantiles from the dataset (to generate goodInitDesign for a maximization Problem),
  # we often get observations with a higher target value than the maximum of our objective function / Simulation.
  # to avoid this, we create a data.frame evaluated on our provided simulation / objective function.

  model = mlr::train(mlr::makeLearner(task$simulation), mlr::makeRegrTask(data = task$data, target = task$target))

  info = getModelInfo(model, task$psOpt, task$minimize)

  target = task$target

  n = info$featureNumber*1000

  # damit bei jeden run nicht das selbe mit for schleife seed updaten lassen

  instanceList = vector(mode = "list", length = numberInstances)

  for(i in 1:numberInstances) {

    seed = (seed + (i*750))

    set.seed(seed)

    experiments = ParamHelpers::generateRandomDesign(n, task$psOpt)

    experiments = split(experiments, seq(nrow(experiments)))

    computeRandomDesign = function(x) {

      x = as.data.frame(x)

      response = mlr::getPredictionResponse(predict(model, newdata = x))

      return(response)

    }

    simul_data = lapply(experiments, computeRandomDesign)

    simul_data = as.data.frame(do.call(rbind,simul_data))

    colnames(simul_data) = target

    simul_data = cbind(as.data.frame(do.call(rbind,experiments)), simul_data)

    instanceList[[i]] = simul_data
  }
  return(instanceList)
}
