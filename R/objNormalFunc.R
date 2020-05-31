# define the objective function and the initial design for the mbo process
objNormalFunc = function(instances, psOpt, info, ...) {

  fun = function(x) {
    df = as.data.frame(x)
    for (i in 1:info$featureNumber) {
      if (info$featureType[i] == "discrete") {
        df[,info$featureName[i]] = factor(df[,info$featureName[i]], levels = instances[["factor.levels"]][[info$featureName[i]]])
      }
    }
    return(getPredictionResponse(predict(instances, newdata = df)))
  }

  objfun = smoof::makeSingleObjectiveFunction(
    name = info$y.name,
    fn = fun,
    par.set = psOpt,
    has.simple.signature = FALSE,
    minimize = info$minimize)

  return(list(objfun,info))
}

