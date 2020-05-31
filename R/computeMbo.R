computeMBO = function(reg, objNormal, configMbo, info, repls = 1) {
  batchtools::addAlgorithm(name = "mlrMBO", fun = configMboFunc, reg = reg)
  batchtools::addExperiments(prob.designs = objNormal, algo.designs = configMbo, repls = repls, reg = reg)
}

configMboFunc = function(instance, funcEvals = 50, design = NULL, amountDesign = NULL,
                         control = NULL, surrogate = NULL, ...) {

  if (is.null(control)) {
    control = mlrMBO::makeMBOControl(y.name = instance[[2]]$y.name)
    control = mlrMBO::setMBOControlInfill(control)
  }

  if (!is.null(control)) {
    control$y.name = instance[[2]]$y.name
  }

  if (is.null(amountDesign)) {
    amountDesign = (instance[[2]]$featureNumber + 1)
  }

  if (is.data.frame(design)) amountDesign = nrow(design)

  control = mlrMBO::setMBOControlTermination(control, iters = as.integer(funcEvals - amountDesign))


  if (is.null(design)) {
    design = ParamHelpers::generateDesign(n = amountDesign, par.set = ParamHelpers::getParamSet(instance[[1]]), fun = lhs::maximinLHS)
  }

  if (is.character(design) & !(design == "random")) {
    design = ParamHelpers::generateDesign(n = amountDesign, par.set = ParamHelpers::getParamSet(instance[[1]]), fun = get(design))
  }

  if (is.character(design) & (design == "random"))  {
     design = ParamHelpers::generateRandomDesign(n = amountDesign, par.set = ParamHelpers::getParamSet(instance[[1]]), trafo = FALSE)
  }


  if (is.numeric(design)) {
    amountSample = instance[[2]]$featureNumber*1500
    designQ = ParamHelpers::generateRandomDesign(n = amountSample, par.set = ParamHelpers::getParamSet(instance[[1]]), trafo = FALSE)

    y = as.data.frame(instance[[1]](designQ))
    colnames(y) = instance[[2]]$y.name
    designQ = cbind(designQ,y)

    bounderyUp = quantile(designQ[[instance[[2]]$y.name]],
                          probs = c(design[2]), na.rm = FALSE,
                          names = TRUE, type = 7)
    bounderyDown = quantile(designQ[[instance[[2]]$y.name]],
                            probs = c(design[1]), na.rm = FALSE,
                            names = TRUE, type = 7)

    designQ = designQ[(designQ[[instance[[2]]$y.name]] <= bounderyUp), ]
    designQ = designQ[(designQ[[instance[[2]]$y.name]] >= bounderyDown), ]

    design = sample_n(tbl = designQ, size = amountDesign)
  }

  control$store.model.at = 1

  optimizationPath = mlrMBO::mbo(fun = instance[[1]], design = design, learner = surrogate, control = control, show.info = TRUE)

  y = as.data.frame(optimizationPath[["y"]])
  colnames(y) = instance[[2]]$y.name
  x = optimizationPath[["x"]]

  for (i in 1:instance[[2]]$featureNumber) {
    if (instance[[2]]$featureType[i] == "discrete") {
      x[[instance[[2]]$featureName[i]]] =
        factor(x[[instance[[2]]$featureName[i]]],levels = instance[[2]]$discreteLevel[[instance[[2]]$featureName[i]]])
    }
  }

  bestResult = cbind(x,y)

  res = list(bestResult,optimizationPath)
  return(res)
}
