generateParamSpace = function(data, target) {

  data[,target] = NULL

  featureName = colnames(data)
  featureNumber = length(colnames(data))

  psOpt = ParamHelpers::makeParamSet()

  for (i in 1:featureNumber) {
    featureTyp = class(data[,i])

    if (featureTyp == "numeric") {
      psOpt[["pars"]][[featureName[i]]] = makeNumericParam(featureName[i],
                                                           lower = min(data[,featureName[i]]),
                                                           upper = max(data[,featureName[i]]))
    }
    if (featureTyp == "integer") {
      psOpt[["pars"]][[featureName[i]]] = makeIntegerParam(featureName[i],
                                                           lower = min(data[,featureName[i]]),
                                                           upper = max(data[,featureName[i]]))
    }
    if (featureTyp == "factor") {
      level = levels(data[,featureName[i]])
      psOpt[["pars"]][[featureName[i]]] = makeDiscreteParam(featureName[i], values = c(level))
    }
  }

  return(psOpt)
}
