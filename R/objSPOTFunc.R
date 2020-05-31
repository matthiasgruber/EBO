objSPOTFunc = function(instances, psOpt, info, ...) {
  #
  fun = function(x) {
    # as the function get x as an vector, transpose it
    df = t(x)
    # make as data frame
    df = as.data.frame(df)

    for (i in 1:info$featureNumber) {
      #if (info$featureType[i] == "integer") {
      #  df[i] = round(df[i])
      #}
      if (info$featureType[i] == "discrete") {
        df[i] = round(df[i])
        levelEncoded = 1:length(info$discreteLevel[[info$featureName[i]]])
        for (ii in 1:length(info$discreteLevel[[info$featureName[i]]])) {
          if (df[i] == levelEncoded[ii]) {
            df[i] = info$discreteLevel[[info$featureName[i]]][[ii]]
          }
        }
        df[i] = factor(df[i], levels = info$discreteLevel[[info$featureName[i]]])
      }
    }

    colnames(df) = info$featureName

    return(getPredictionResponse(predict(instances, newdata = df))*info$p)
  }

  for (i in 1:info$featureNumber) {
    #if (info$featureType[i] == "integer") {
    #  psOpt$pars[[i]]$type = "numeric"
    #}
    if (info$featureType[i] == "discrete") {
      encodedParam = makeNumericParam(info$featureName[i], lower = 0.5001, upper = (0.5+length(psOpt$pars[[i]]$values)-0.001))
      dropParams(psOpt, info$featureName[i])
      psOpt$pars[[info$featureName[i]]] = encodedParam
    }
  }

  objfun = smoof::makeSingleObjectiveFunction(
    name = "default",
    fn = fun,
    par.set = psOpt,
    has.simple.signature = FALSE,
    minimize = info$minimize
  )

  return(list(objfun, info))
}
