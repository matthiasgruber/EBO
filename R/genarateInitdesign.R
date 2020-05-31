
generateInitdesign = function(data, model, psOpt,
                              amountDesign, minimize, quantiles = c(0.8,0.2), seed) {


  # if we choose observations with high Quantiles from the dataset (to generate goodInitDesign for a maximization Problem),
  # we often get observations with a higher target value than the maximum of our objective function / Simulation.
  # to avoid this, we create a data.frame evaluated on our provided simulation / objective function.

  info = getModelInfo(model, psOpt, minimize)

  target = info$y.name

  n = info$featureNumber*150

  set.seed(seed)

  experiments = ParamHelpers::generateRandomDesign(n, psOpt)

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

  if (minimize == FALSE) {

    ### ich muss good so ändern, dass nur 0.6-0.8 quantil, sonst findet er einen wert, der höher ist als maximum der objective function
    goodInit = dplyr::sample_n((simul_data[simul_data[[target]] >= quantile(simul_data[[target]],
                                                      probs = c(quantiles[1]), na.rm = FALSE,
                                                      names = TRUE, type = 7), ]), amountDesign)

    badInit = dplyr::sample_n((simul_data[simul_data[[target]] <= quantile(simul_data[[target]],
                                                     probs = c(quantiles[2]), na.rm = FALSE,
                                                     names = TRUE, type = 7), ]), amountDesign)
  } else {
    goodInit = dplyr::sample_n((simul_data[simul_data[[target]] <= quantile(simul_data[[target]],
                                                      probs = c(quantiles[1]), na.rm = FALSE,
                                                      names = TRUE, type = 7), ]), amountDesign)

    badInit = dplyr::sample_n((simul_data[simul_data[[target]] >= quantile(simul_data[[target]],
                                                     probs = c(quantiles[2]), na.rm = FALSE,
                                                     names = TRUE, type = 7), ]), amountDesign)
  }

  return(list(goodInit, badInit))
}


