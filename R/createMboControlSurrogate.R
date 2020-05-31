createMboControlSurrogate = function(x) {

  controlOpt = mlrMBO::makeMBOControl()

  if (!is.null(x$crit)) {
    controlOpt$infill.crit = do.call(get(as.character(x$crit), asNamespace("mlrMBO")), list())
    infillParams = names(controlOpt[["infill.crit"]][["params"]])
    numberInfillParams = length(infillParams)
    if (!numberInfillParams == 0) {
      for (i in 1:numberInfillParams) {
        if (!is.null(x[[infillParams[i]]])) {
          controlOpt$infill.crit$params[[infillParams[i]]] = x[[infillParams[i]]]
        }
      }
    }
  }

  infillOptParams = names(controlOpt[22:38])
  numberInfillOptParams = length(infillOptParams)
  if (!numberInfillOptParams == 0) {
    for (i in 1:numberInfillOptParams) {
      if (!is.null(x[[infillOptParams[i]]])) {
        controlOpt[[infillOptParams[i]]] = x[[infillOptParams[i]]]
      }
    }
  }

  if (!is.null(x[["resample.at"]])) {
    controlOpt[["resample.at"]] = as.integer(x$resample.at)
  }

  if (!is.null(x$surrogate)) {
    surrogateOpt = mlr::makeLearner(as.character(x$surrogate), predict.type = "se")
    surrogateParams = names(surrogateOpt[["par.set"]]$pars)
    numberSurrogateParams = length(surrogateParams)
    for (i in 1:numberSurrogateParams) {
      if (!is.null(x[[surrogateParams[i]]])) {
        surrogateOpt[["par.vals"]][[surrogateParams[i]]] = x[[surrogateParams[i]]]
      }
    }

    if (x$surrogate == "regr.km") surrogateOpt[["par.vals"]][["nugget.estim"]] = TRUE
  }

  if (is.null(x$surrogate)) {
    surrogateOpt = NULL
  }

  return(list(controlOpt, surrogateOpt))
}

