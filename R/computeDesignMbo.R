computeDesignMbo = function(info, instancesTest, psOpt, designMbo, psParamPlot, funcEvals, minimize, repls, ncpus, seed) {

  resDesignTune = designMbo
  numberDesignTune = nrow(designMbo)

  controlOpt = list()
  surrogateOpt = list()


  if (is.null(psParamPlot[["pars"]][["amountDesign"]])) amountDesign = list(NULL)
  if (!is.null(psParamPlot[["pars"]][["amountDesign"]])) {
    amountDesign = list()
  }

  if (is.null(psParamPlot[["pars"]][["design"]])) design = list(NULL)

  for (j in 1:numberDesignTune) {

    designTune = designMbo[j,]

    if (is.integer(psParamPlot[["pars"]][["amountDesign"]])) {
      amountDesign[[j]] = designTune$amountDesign
    }
    mboControlSurrogate = createMboControlSurrogate(designTune)

    controlOpt[[j]] = mboControlSurrogate[[1]]
    surrogateOpt[[j]] = mboControlSurrogate[[2]]
  }



  paramsMBO = data.table(design = design,
                         amountDesign = amountDesign,
                         control = controlOpt,
                         surrogate = surrogateOpt)


  resMboDesign = benchmarkMbo(instancesTest, psOpt, funcEvals, paramsMBO, minimize, repls, ncpus, seed, delReg = FALSE)
  errors = batchtools::findErrors()
  batchtools::removeRegistry(0)

  designMboExtended = designMbo[rep(1:nrow(designMbo),each = repls),]
  designMboExtended$y = NA
  if (!nrow(errors)==0) designMboExtended = designMboExtended[-unlist(errors),]
  for (i in 1:nrow(designMboExtended)) {
    designMboExtended$y[i] = resMboDesign[[i]][["recommendedParameters"]][[info$y.name]]
  }
  if (!nrow(errors)==0) {
    for (i in 1:nrow(errors)) {
      designMboExtended = add_row(designMboExtended, .after = as.integer(errors[i]-1))
    }
  }

  for (i in 1:nrow(designMbo)) {
    resDesignTune$y[i] = median(c(designMboExtended$y[(repls*(i-1)+1):(repls*i)])
                                  ,na.rm = TRUE)
  }

  return(resDesignTune)
}
